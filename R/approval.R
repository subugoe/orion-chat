# Approval of paid tool calls. Pure functions, tested in
# tests/test-approval.R; app.R wires them into the chat.

paid_tools <- c(
  "orion_run_bq_query", "orion_export_bq_query",
  "orion_query_to_table", "orion_save_result_to_bq"
)
# These write a BigQuery table, and can replace an existing one.
table_tools <- c("orion_query_to_table", "orion_save_result_to_bq")

# A message approves only if the whole message is an approval: optional
# "please", an approval word, then nothing but estimate numbers ("#2",
# "2 and 3"), "both" or "all", filler such as "run the query", and
# punctuation. "ok, but use Crossref instead" is not an approval.
approval_start <- regex(
  paste0(
    "^(?:(?:please|bitte)[\\s,]+)?",
    "(go ahead|please do|please run|do it|proceed|okay|sure|yes|yep|jep|",
    "run|ok|ja|go|y)(?=$|[\\s,.:;!#-])(.*)$"
  ),
  ignore_case = TRUE
)
# Fillers of several words come first, so that "go ahead" is not left with
# "go".
approval_filler <- regex(
  paste0(
    "\\b(?:go ahead|thank you|run|it|them|both|all|beide|alle|of|and|und|",
    "with|estimates?|numbers?|please|bitte|thanks|danke|the|this|that|",
    "these|query|queries|one|now|do|for|yes|okay|ok|sure)\\b"
  ),
  ignore_case = TRUE
)

# A message of estimate numbers only: "2", "#2", "#2 and #3", "2, 3". The
# gate counts it as an approval only if these are open estimates.
approval_bare <- regex(
  "^#?\\d+(?:(?:\\s*(?:,|&|\\+|and|und)\\s*|\\s+)#?\\d+)*[.!]?$",
  ignore_case = TRUE
)

approval_all <- regex("\\b(?:both|all|beide|alle)\\b", ignore_case = TRUE)

# The numbers in a text, as integers.
numbers_in <- function(text) {
  suppressWarnings(as.integer(str_extract_all(text, "\\d+")[[1]]))
}

# list(approved = TRUE/FALSE, numbers = the estimate numbers named,
#      all = TRUE if the message says "both" or "all",
#      bare = TRUE if the message consists of numbers only)
parse_approval <- function(text) {
  text <- text %||% ""
  if (length(text) != 1 || is.na(text)) text <- ""
  text <- str_squish(text)
  if (str_detect(text, approval_bare)) {
    return(list(
      approved = TRUE, numbers = numbers_in(text), all = FALSE, bare = TRUE
    ))
  }
  rest <- str_match(text, approval_start)[1, 3]
  if (is.na(rest)) {
    return(list(
      approved = FALSE, numbers = integer(), all = FALSE, bare = FALSE
    ))
  }
  # "Run #4?" echoes the model's question; a "?" counts only after a number,
  # so "yes?" stays a question.
  if (length(numbers_in(rest)) > 0) {
    rest <- str_remove(rest, "\\?$")
  }
  leftover <- rest |>
    str_remove_all("\\d+") |>
    str_remove_all(approval_filler) |>
    str_remove_all("[\\s#,.;:!&+-]")
  list(
    approved = leftover == "",
    numbers = numbers_in(rest),
    all = str_detect(rest, approval_all),
    bare = FALSE
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
#   records:   per SQL key, list(number, sql, bytes, at, released_at);
#              `sql` and `bytes` are those of the latest dry-run, `at`
#              holds the number of user messages at each dry-run of the
#              SQL, `released_at` that number when the gate last let a paid
#              call for it through (NULL before)
#   approvals: user-message counts of messages that released a paid call
#   n_user_messages: the count when the current tool call was requested
#   auto_run_bytes: estimates below this size run with orion_run_bq_query
#              without asking (small lookups); 0 turns this off
new_estimate_log <- function(auto_run_bytes = 0) {
  log <- new.env(parent = emptyenv())
  log$records <- list()
  log$approvals <- integer()
  log$n_user_messages <- 0L
  log$auto_run_bytes <- auto_run_bytes
  log
}

# ORION_AUTO_RUN_GB as bytes: queries whose estimate is below it run
# without asking. Default 0.1 GB; 0 turns it off.
auto_run_bytes <- function(value = Sys.getenv("ORION_AUTO_RUN_GB", "0.1")) {
  gb <- suppressWarnings(as.numeric(value))
  if (length(gb) != 1 || is.na(gb) || gb < 0) {
    stop(
      "ORION_AUTO_RUN_GB must be a number of GB, 0 or more (got '", value,
      "').",
      call. = FALSE
    )
  }
  gb * 1e9
}

# Whether an estimate is small enough to run without asking.
runs_unasked <- function(record, log) {
  limit <- log$auto_run_bytes %||% 0
  bytes <- record$bytes
  is.numeric(bytes) && length(bytes) == 1 && !is.na(bytes) && bytes < limit
}

# Record a successful dry-run and return its number. Running the same SQL
# again keeps its number, but the record takes the text and size of the
# latest dry-run: the key ignores spacing that BigQuery does not (inside
# string literals), and the data may have grown since.
record_estimate <- function(log, sql, bytes, n_user_messages) {
  key <- sql_key(sql)
  if (is.null(key)) {
    return(NA_integer_)
  }
  record <- log$records[[key]]
  if (is.null(record)) {
    record <- list(
      number = length(log$records) + 1L,
      at = integer()
    )
  }
  record$sql <- sql
  record$bytes <- bytes
  record$at <- union(record$at, as.integer(n_user_messages))
  log$records[[key]] <- record
  record$number
}

# The user-message count at which the user last saw this estimate before
# message `n`: the latest dry-run before it. NA if the SQL was only dry-run
# after the user's latest message.
shown_at <- function(record, n) {
  before <- record$at[record$at < n]
  if (length(before) == 0) NA_integer_ else max(before)
}

# The gate let a paid call for this estimate through at message `n`.
record_release <- function(log, key, n_user_messages) {
  if (!is.null(key) && !is.null(log$records[[key]])) {
    log$records[[key]]$released_at <- as.integer(n_user_messages)
  }
  invisible(log)
}

# The estimates a plain approval at message `n` can refer to: shown before
# `n` and since the previous approval, and not run since they were last
# shown. A release at `n` itself does not close an estimate, so that the
# same approval covers a retry in the same reply. Queries small enough to
# run unasked do not count, except `current`, the one being decided.
open_estimates <- function(log, n, current = NULL) {
  approvals <- log$approvals
  previous <- max(c(0L, approvals[approvals < n]))
  keep(log$records, \(r) {
    shown <- shown_at(r, n)
    released <- r$released_at
    !is.na(shown) && shown >= previous &&
      (is.null(released) || released >= n || shown > released) &&
      (!runs_unasked(r, log) || identical(r$number, current$number))
  })
}

# The key of estimate number `number`, or NULL.
estimate_key <- function(log, number) {
  numbers <- map_int(log$records, "number")
  key <- names(numbers)[numbers == number]
  if (length(key) == 1) key
}

# The `estimate` argument of a tool call as an integer: a whole number, or
# text of digits with an optional "#". NA for anything else. ellmer converts
# arguments to their declared types only after the gate has run, so a model
# that sends "1" reaches the gate with text.
estimate_number <- function(value) {
  if (length(value) != 1 || is.na(value)) {
    return(NA_integer_)
  }
  if (is.character(value)) {
    value <- str_match(str_trim(value), "^#?(\\d+)$")[1, 2]
  } else if (!is.numeric(value) || value != trunc(value)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(value))
}

pass_integer <- paste0(
  "pass the number of the estimate as an integer, for example ",
  "estimate = 1."
)

# The key for a paid tool call: that of its `estimate` number, or NULL. In
# this app the query tools take no SQL, only the number of an estimate.
request_key <- function(arguments, log) {
  number <- estimate_number(arguments$estimate)
  if (!is.na(number)) {
    estimate_key(log, number)
  }
}

# "#1, #2" for the estimates in the log, or "none".
estimate_list <- function(log) {
  numbers <- sort(map_int(log$records, "number"))
  if (length(numbers) == 0) "none" else glue("#{str_flatten(numbers, ', #')}")
}

record_approval <- function(log, n_user_messages) {
  log$approvals <- union(log$approvals, n_user_messages)
}

# `unasked`: allowed because the query is small, not because the user
# approved it; such runs do not count as approvals.
gate_allow <- function(unasked = FALSE) {
  list(allow = TRUE, message = NULL, unasked = unasked)
}
gate_reject <- function(...) list(allow = FALSE, message = paste0(...))

ask_again <- paste0(
  "Show the SQL, the estimated cost and the estimate number, end with a ",
  "question that repeats the number and cost, such as 'Shall I run ",
  "Estimate #n (x GB, about $y)?', and wait for a yes."
)

how_to_approve <- paste0(
  "The user approves with 'yes' (if your last message showed one ",
  "estimate), with the numbers ('run #2', 'run #2 and #3', or just '#2') ",
  "or with 'run all' (all estimates of your last message)."
)

ask_table <- paste0(
  "Name the full table (project.dataset.table), say whether an existing ",
  "table will be replaced, ask whether to go ahead, and wait for a yes."
)

overwrite_words <- regex(
  "overwrit|replac|überschreib|ersetz",
  ignore_case = TRUE
)

# The gate sees `overwrite` before ellmer converts it, so it may be TRUE or
# "true". Anything that is not clearly false counts as a request to replace.
asks_overwrite <- function(value) {
  is_false <- isFALSE(value) ||
    (is.character(value) && length(value) == 1 &&
      str_to_lower(value) %in% c("false", ""))
  !is.null(value) && !is_false
}

# Why a table write must not run although the user approved, or NULL. The
# model chooses `destination` and `overwrite` in the tool call, so the
# approval counts only if `reply`, the text the user answered, named the
# table and said that an existing table will be replaced.
table_problem <- function(destination, overwrite, reply) {
  reply <- str_to_lower(reply %||% "")
  valid <- is.character(destination) && length(destination) == 1 &&
    !is.na(destination) && str_trim(destination) != ""
  if (!valid) {
    return(glue("Not run: the call names no destination table. {ask_table}"))
  }
  destination <- str_trim(destination)
  if (!str_detect(reply, fixed(str_to_lower(destination)))) {
    return(glue(
      "Not run: the user approved, but your last message did not name the ",
      "destination table '{destination}'. {ask_table}"
    ))
  }
  if (asks_overwrite(overwrite) && !str_detect(reply, overwrite_words)) {
    return(glue(
      "Not run: the call sets overwrite, but your last message did not say ",
      "that the existing table '{destination}' will be replaced. {ask_table}"
    ))
  }
  NULL
}

# Whether a tool call may run. `estimated` is the log (or a list with the
# same fields), `approval` the result of parse_approval() for the latest
# user message, `n_user_messages` the number of user messages so far.
# `estimate` is the estimate number the tool call names, if any. `reply` is
# the assistant text the user answered (last_reply_text()); `destination`
# and `overwrite` are the arguments of a call that writes a table.
gate_decision <- function(tool, sql_key, estimated, n_user_messages, approval,
                          estimate = NULL, reply = NULL, destination = NULL,
                          overwrite = NULL) {
  if (!tool %in% paid_tools) {
    return(gate_allow())
  }
  decision <- approval_decision(
    tool, sql_key, estimated, n_user_messages, approval, estimate,
    reply = reply
  )
  if (decision$allow && tool %in% table_tools) {
    problem <- table_problem(destination, overwrite, reply)
    if (!is.null(problem)) {
      return(gate_reject(problem))
    }
  }
  decision
}

# Whether the user approved this call of a paid tool.
# Whether `text` names estimate `number` ("Estimate #4", "#4").
names_estimate <- function(text, number) {
  str_detect(text %||% "", glue("#{number}(?!\\d)"))
}

approval_decision <- function(tool, sql_key, estimated, n_user_messages,
                              approval, estimate = NULL, reply = NULL) {
  record <- if (!is.null(sql_key)) estimated$records[[sql_key]]

  # A message of numbers only approves if all of them are open estimates.
  # "5" as the answer to another question approves nothing.
  if (isTRUE(approval$bare)) {
    open <- open_estimates(estimated, n_user_messages, current = record)
    approval$approved <- all(approval$numbers %in% map_int(open, "number"))
  }

  # No SQL argument: saves an already stored result.
  if (tool == "orion_save_result_to_bq") {
    if (isTRUE(approval$approved)) {
      return(gate_allow())
    }
    return(gate_reject(glue(
      "Not run: the user has not approved this yet. Say which result will ",
      "be saved. {ask_table}"
    )))
  }

  if (is.null(record) && !is.null(estimate)) {
    number <- estimate_number(estimate)
    if (is.na(number)) {
      return(gate_reject(glue(
        "Not run: {pass_integer} Existing estimates: ",
        "{estimate_list(estimated)}."
      )))
    }
    return(gate_reject(glue(
      "Not run: there is no Estimate #{number}. Existing estimates: ",
      "{estimate_list(estimated)}. Only orion_estimate_query_cost gives ",
      "estimate numbers and costs. Dry-run the SQL with it, show the user ",
      "the number and cost it returns, and wait for approval."
    )))
  }
  if (is.null(record)) {
    return(gate_reject(glue(
      "Not run: pass the number of an approved estimate as 'estimate'. ",
      "This tool takes no SQL; dry-run new SQL with ",
      "orion_estimate_query_cost first. {ask_again}"
    )))
  }
  # Small lookups run without asking. Exports and tables always need an
  # approval, since they write files or tables.
  if (tool == "orion_run_bq_query" && runs_unasked(record, estimated)) {
    return(gate_allow(unasked = TRUE))
  }
  if (is.na(shown_at(record, n_user_messages))) {
    return(gate_reject(glue(
      "Not run: this exact SQL was dry-run after the user's last message, ",
      "so the user has not seen it. {ask_again}"
    )))
  }
  if (!isTRUE(approval$approved)) {
    # Not NULL: glue() returns an empty string when a value is NULL.
    table_note <- if (tool %in% table_tools) glue("{ask_table} ") else ""
    return(gate_reject(glue(
      "Not run: the user has not approved this yet. {ask_again} ",
      "{table_note}{how_to_approve}"
    )))
  }

  if (length(approval$numbers) > 0) {
    if (record$number %in% approval$numbers) {
      return(gate_allow())
    }
    return(gate_reject(glue(
      "Not run: the user approved estimate ",
      "#{str_flatten(approval$numbers, ', #')}, but this SQL is estimate ",
      "#{record$number}. Run only the approved estimates."
    )))
  }

  # A plain "yes" refers to the latest step: the open estimates that were
  # shown last. If the reply the user answered names some of them by
  # number, only those: a dry-run the model dropped without showing it
  # does not make the yes ambiguous. It covers the call if there is just
  # one; "all" and "both" cover every estimate of that step.
  open <- open_estimates(estimated, n_user_messages, current = record)
  shown <- map_int(open, \(r) shown_at(r, n_user_messages))
  latest <- open[shown == max(shown, -Inf)]
  named <- keep(latest, \(r) names_estimate(reply, r$number))
  if (length(named) > 0) {
    latest <- named
  }
  covered <- length(latest) == 1 || isTRUE(approval$all)
  if (sql_key %in% names(latest) && covered) {
    return(gate_allow())
  }
  numbers <- sort(map_int(latest, "number"))
  listed <- if (length(numbers) > 0) {
    glue(" (open estimates: #{str_flatten(numbers, ', #')})")
  } else {
    ""
  }
  gate_reject(glue(
    "Not run: it is not clear which estimate the user approved{listed}. ",
    "Ask which to run. {how_to_approve}"
  ))
}

# Messages the user typed (tool results are stored as user turns without text).
is_user_message <- function(turn) {
  turn@role == "user" && str_trim(turn@text) != ""
}

user_messages <- function(turns) {
  keep(turns, is_user_message)
}

latest_user_text <- function(turns) {
  messages <- user_messages(turns)
  if (length(messages) == 0) "" else messages[[length(messages)]]@text
}

# What the user read before answering: the text of the assistant turns
# between the previous user message and the latest one.
last_reply_text <- function(turns) {
  typed <- which(map_lgl(turns, is_user_message))
  if (length(typed) == 0) {
    return("")
  }
  latest <- typed[length(typed)]
  previous <- if (length(typed) > 1) typed[length(typed) - 1] else 0L
  before <- seq_len(latest - 1)
  turns[before[before > previous]] |>
    keep(\(turn) turn@role == "assistant") |>
    map_chr(\(turn) turn@text) |>
    str_flatten("\n")
}

# The question that ends a message presenting estimate `number`, with the
# size and cost from the estimate `result` (JSON from orion-mcp), so the
# user sees the cost next to the question. NULL if the result lacks them.
run_question <- function(number, result) {
  fields <- tryCatch(jsonlite::fromJSON(result), error = \(e) NULL)
  gb <- fields$gb_processed
  cost <- fields$cost_usd_estimate
  is_number <- \(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  if (!is_number(gb) || !is_number(cost)) {
    return(NULL)
  }
  # Not 1e-04 for small costs.
  plain <- \(x) format(x, scientific = FALSE, trim = TRUE)
  glue("Shall I run Estimate #{number} ({plain(gb)} GB, about ${plain(cost)})?")
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
        if (runs_unasked(log$records[[sql_key(query)]], log)) {
          return(glue(
            "{result}\n\nThis is Estimate #{number}. It is small enough to ",
            "run with orion_run_bq_query (estimate = {number}) without ",
            "asking the user. Ignore the instruction above to present it ",
            "and ask; use it as a lookup and mention it in one line."
          ))
        }
        question <- run_question(number, result)
        question_rule <- if (is.null(question)) {
          "repeating the estimate number and cost"
        } else {
          glue("'{question}'")
        }
        glue(
          "{result}\n\nThis is Estimate #{number}. Show 'Estimate ",
          "#{number}' with the cost, so the user can approve it by number. ",
          "End your message with the question, so the cost stays next to ",
          "it: {question_rule} (for several estimates, one question that ",
          "names each with its cost). Once approved, run it by passing ",
          "estimate = {number} instead of the SQL."
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

# The orion-mcp descriptions are written for clients that send SQL and ask
# before every query. Remove or replace the two sentences that contradict
# this app. A sentence that is not found (another server version) is left
# alone; the text run_by_estimate() appends still holds.
app_description <- function(description, auto_run_bytes = 0) {
  one_argument <- paste0(
    "Takes exactly one argument — query — which must be the complete, ",
    "fully-qualified SQL string. "
  )
  never_skip <- "Never skip the cost confirmation step, even for small queries."
  description <- str_replace(description, fixed(one_argument), "")
  if (auto_run_bytes > 0) {
    description <- str_replace(
      description, fixed(never_skip),
      glue(
        "In this app, queries whose estimate is under ",
        "{auto_run_bytes / 1e9} GB run without asking; all others need the ",
        "user's approval."
      )
    )
  }
  description
}

# The tools that take SQL take the number of a cost estimate instead, and
# run its SQL exactly as it was dry-run. The model cannot retype the SQL
# (it often changed it slightly) or run SQL that was never estimated.
run_by_estimate <- function(tools, log) {
  map(tools, \(tool) {
    arguments <- tool@arguments@properties
    if (!tool@name %in% paid_tools || is.null(arguments$query)) {
      return(tool)
    }
    arguments <- c(
      list(estimate = ellmer::type_integer(
        "Number of the approved cost estimate (Estimate #n) to run."
      )),
      arguments[names(arguments) != "query"]
    )
    fun <- function() {
      args <- compact(as.list(environment()))
      number <- estimate_number(args$estimate)
      if (is.na(number)) {
        return(ellmer::ContentToolResult(
          error = glue("Not run: {pass_integer}")
        ))
      }
      key <- estimate_key(log, number)
      if (is.null(key)) {
        return(ellmer::ContentToolResult(
          error = glue("There is no Estimate #{number}.")
        ))
      }
      args$query <- log$records[[key]]$sql
      args$estimate <- NULL
      do.call(tool, args)
    }
    formals(fun) <- rep(list(NULL), length(arguments)) |>
      set_names(names(arguments))
    ellmer::tool(
      fun,
      name = tool@name,
      description = glue(
        "{app_description(tool@description, log$auto_run_bytes %||% 0)} ",
        "In this app the tool takes no SQL: pass `estimate`, the number of ",
        "an approved cost estimate, and its SQL runs exactly as it was ",
        "dry-run."
      ),
      arguments = arguments,
      convert = tool@convert,
      annotations = tool@annotations
    )
  })
}

# Both wrappers, for the tools of one browser session.
wrap_orion_tools <- function(tools, log) {
  tools |>
    number_estimates(log) |>
    run_by_estimate(log)
}
