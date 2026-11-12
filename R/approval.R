# Approval of paid tool calls. Pure functions, tested in
# tests/test-approval.R; app.R wires them into the chat.

paid_tools <- c(
  "orion_run_bq_query", "orion_export_bq_query",
  "orion_query_to_table", "orion_save_result_to_bq"
)

# A message approves only if the whole message is an approval: optional
# "please", an approval word, optionally "run it", "run #2" or "2", then
# nothing but punctuation.
approval_regex <- local({
  please <- "(?:please|bitte)"
  word <- str_flatten(c(
    "yes", "yep", "y", "ja", "jep", "ok", "okay", "sure", "go ahead", "go",
    "run it", "run", "proceed", "do it", "please do", "please run"
  ), "|")
  number <- paste0(
    "(?:run(?:\\s+it)?\\s*)?",
    "(?:(?:no\\.?|number|nr\\.?)\\s*)?",
    "#?\\s*(\\d+)"
  )
  regex(
    as.character(glue(
      "^\\s*(?:{please}[\\s,]+)?(?:{word})",
      "(?:[\\s,:;-]+(?:{number}|run(?:\\s+it)?))?",
      "(?:[\\s,]+{please})?[\\s.!,]*$"
    )),
    ignore_case = TRUE
  )
})

# list(approved = TRUE/FALSE, number = integer or NA)
parse_approval <- function(text) {
  text <- text %||% ""
  if (length(text) != 1 || is.na(text)) text <- ""
  match <- str_match(text, approval_regex)
  list(
    approved = !is.na(match[1, 1]),
    number = as.integer(match[1, 2])
  )
}

# The key under which a dry-run is recorded. Whitespace does not count, as
# in the orion server; the hash keeps long SQL usable as a key. NULL if the
# tool call has no usable SQL.
sql_key <- function(sql) {
  if (!is.character(sql) || length(sql) != 1 || is.na(sql)) {
    return(NULL)
  }
  sql <- str_squish(sql)
  if (sql == "") {
    return(NULL)
  }
  rlang::hash(sql)
}

# The record of dry-runs and approvals for one browser session. It is an
# environment so that every chat client of the session (one per model)
# writes to the same record.
#   records:   per SQL key, list(number, bytes, seen_at, last_at); seen_at
#              and last_at are the number of user messages when the SQL was
#              first and last dry-run
#   approvals: user-message counts of messages that released a paid call
#   n_user_messages: the count when the current tool call was requested
new_estimate_log <- function() {
  log <- new.env(parent = emptyenv())
  log$records <- list()
  log$approvals <- integer()
  log$n_user_messages <- 0L
  log
}

# Record a successful dry-run and return its number. Running the same SQL
# again keeps its number.
record_estimate <- function(log, sql, bytes, n_user_messages) {
  key <- sql_key(sql)
  if (is.null(key)) {
    return(NA_integer_)
  }
  record <- log$records[[key]]
  if (is.null(record)) {
    record <- list(
      number = length(log$records) + 1L,
      bytes = bytes,
      seen_at = n_user_messages
    )
  }
  record$last_at <- n_user_messages
  log$records[[key]] <- record
  record$number
}

record_approval <- function(log, n_user_messages) {
  log$approvals <- union(log$approvals, n_user_messages)
}

gate_allow <- function() list(allow = TRUE, message = NULL)
gate_reject <- function(...) list(allow = FALSE, message = paste0(...))

ask_again <- paste0(
  "Show the SQL, the estimated cost and the estimate number, ask ",
  "'Shall I run this query?', and wait for a yes."
)

# Whether a tool call may run. `estimated` is the log (or a list with the
# same fields), `approval` the result of parse_approval() for the latest
# user message, `n_user_messages` the number of user messages so far.
gate_decision <- function(tool, sql_key, estimated, n_user_messages, approval) {
  if (!tool %in% paid_tools) {
    return(gate_allow())
  }

  # No SQL argument: saves an already stored result.
  if (tool == "orion_save_result_to_bq") {
    if (isTRUE(approval$approved)) {
      return(gate_allow())
    }
    return(gate_reject(
      "Not run: the user has not approved this yet. Say what will be saved ",
      "where, ask whether to go ahead, and wait for a yes."
    ))
  }

  if (is.null(sql_key)) {
    return(gate_reject(
      "Not run: the tool call has no SQL in its 'query' argument. Pass the ",
      "SQL that was dry-run with orion_estimate_query_cost."
    ))
  }
  record <- estimated$records[[sql_key]]
  if (is.null(record)) {
    return(gate_reject(
      "Not run: this SQL has not been dry-run. Call orion_estimate_query_cost ",
      "with exactly this SQL. ", ask_again
    ))
  }
  if (record$seen_at >= n_user_messages) {
    return(gate_reject(
      "Not run: this exact SQL was dry-run after the user's last message, ",
      "so the user has not seen it. ", ask_again
    ))
  }
  if (!isTRUE(approval$approved)) {
    return(gate_reject(
      "Not run: the user has not approved this yet. ", ask_again
    ))
  }

  if (!is.na(approval$number)) {
    if (identical(as.integer(record$number), approval$number)) {
      return(gate_allow())
    }
    return(gate_reject(
      "Not run: the user approved estimate #", approval$number, ", but this ",
      "SQL is estimate #", record$number, ". Run the SQL of estimate #",
      approval$number, " exactly as it was dry-run."
    ))
  }

  # A plain "yes" counts only if one estimate was made since the previous
  # approval (and before the user's latest message).
  approvals <- estimated$approvals
  previous <- max(c(0L, approvals[approvals < n_user_messages]))
  pending <- keep(
    estimated$records,
    \(r) r$last_at >= previous && r$last_at < n_user_messages
  )
  if (length(pending) == 1 && names(pending) == sql_key) {
    return(gate_allow())
  }
  numbers <- sort(map_int(pending, "number"))
  open <- if (length(numbers) > 0) {
    paste0(" (open estimates: #", str_flatten(numbers, ", #"), ")")
  }
  gate_reject(
    "Not run: it is not clear which estimate the user approved", open,
    ". Ask the user which estimate number to run, for example 'run #2'."
  )
}

# Messages the user typed (tool results are stored as user turns without text).
user_messages <- function(turns) {
  keep(turns, \(turn) turn@role == "user" && str_trim(turn@text) != "")
}

latest_user_text <- function(turns) {
  messages <- user_messages(turns)
  if (length(messages) == 0) "" else messages[[length(messages)]]@text
}

# Replace orion_estimate_query_cost by a tool that records each successful
# dry-run in `log` and tells the model its number. Tool errors come back as
# a ContentToolResult and pass through unchanged.
number_estimates <- function(tools, log) {
  map(tools, \(tool) {
    if (tool@name != "orion_estimate_query_cost") {
      return(tool)
    }
    ellmer::tool(
      function(query) {
        # Pass the value, not the symbol: mcptools tools re-evaluate their
        # arguments from match.call().
        result <- do.call(tool, list(query = query))
        if (!is.character(result) || length(result) != 1) {
          return(result)
        }
        bytes <- tryCatch(
          jsonlite::fromJSON(result)$bytes_processed,
          error = \(e) NULL
        ) %||% NA_real_
        number <- record_estimate(log, query, bytes, log$n_user_messages)
        if (is.na(number)) {
          return(result)
        }
        paste0(
          result, "\n\nThis is Estimate #", number, ". Show 'Estimate #",
          number, "' with the cost, so the user can approve it by number."
        )
      },
      name = tool@name,
      description = tool@description,
      arguments = tool@arguments@properties,
      convert = tool@convert,
      annotations = tool@annotations
    )
  })
}
