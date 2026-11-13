test_that("parse_approval() accepts whole-message approvals", {
  # text, numbers named, "both" or "all"
  cases <- list(
    list("yes", integer(), FALSE),
    list("Yes.", integer(), FALSE),
    list("  ok!  ", integer(), FALSE),
    list("ja", integer(), FALSE),
    list("Ja, bitte", integer(), FALSE),
    list("y", integer(), FALSE),
    list("go ahead", integer(), FALSE),
    list("please run it", integer(), FALSE),
    list("please do", integer(), FALSE),
    list("yes please", integer(), FALSE),
    list("yes, run it.", integer(), FALSE),
    list("yes, 2", 2L, FALSE),
    list("run #2", 2L, FALSE),
    list("yes, run #12!", 12L, FALSE),
    list("ok 3", 3L, FALSE),
    list("Yes, number 2", 2L, FALSE),
    list("run #3 and run #2", c(3L, 2L), FALSE),
    list("run #2 and #3", c(2L, 3L), FALSE),
    list("yes, 2, 3", c(2L, 3L), FALSE),
    list("run 2 & 3", c(2L, 3L), FALSE),
    list("ja, beide", integer(), TRUE),
    list("yes, both", integer(), TRUE),
    list("run all of them", integer(), TRUE),
    list("go ahead with both", integer(), TRUE),
    list("yes, run both, thanks", integer(), TRUE),
    list("Yes, run the query", integer(), FALSE),
    list("run the query", integer(), FALSE),
    list("yes, go ahead", integer(), FALSE),
    list("ok, do it", integer(), FALSE),
    list("yes please do", integer(), FALSE),
    list("yes run this one", integer(), FALSE),
    list("ok run it now", integer(), FALSE),
    list("go for it", integer(), FALSE),
    list("sure, run that one", integer(), FALSE),
    list("ok, yes", integer(), FALSE),
    list("run these queries", integer(), FALSE),
    list("run all", integer(), TRUE),
    # "for" is filler since "go for it" is accepted.
    list("yes for 2", 2L, FALSE)
  )
  for (case in cases) {
    result <- parse_approval(case[[1]])
    expect_true(result$approved, info = case[[1]])
    expect_identical(result$numbers, case[[2]], info = case[[1]])
    expect_identical(result$all, case[[3]], info = case[[1]])
    expect_false(result$bare, info = case[[1]])
  }
})

test_that("parse_approval() accepts a message of estimate numbers only", {
  cases <- list(
    list("2", 2L),
    list("#2", 2L),
    list(" #2. ", 2L),
    list("#2 and #3", c(2L, 3L)),
    list("2, 3", c(2L, 3L)),
    list("2 3", c(2L, 3L)),
    list("#1 & 4!", c(1L, 4L)),
    list("2 und 3", c(2L, 3L))
  )
  for (case in cases) {
    result <- parse_approval(case[[1]])
    expect_true(result$approved, info = case[[1]])
    expect_true(result$bare, info = case[[1]])
    expect_identical(result$numbers, case[[2]], info = case[[1]])
    expect_false(result$all, info = case[[1]])
  }
})

test_that("parse_approval() rejects messages that only start like one", {
  for (text in c(
    "ok, but use Crossref instead", "go back to the first idea",
    "run a different analysis", "y axis should be log", "yes but only 2020",
    "no", "", "   ", "yesterday", "okay so what about OpenAIRE?",
    "run #2 but with 2024", "all good, but not yet",
    "go with the cheaper one", "yes, but only for 2020",
    "run this query: SELECT 1", "run the query with Crossref",
    "2020 to 2024", "2 years", "#2 looks wrong", "the second one",
    "do it for the other one", "yes if it is free", "ok, what does it cost?",
    "thanks", "# 2", "2.5", "2 or 3", "#"
  )) {
    result <- parse_approval(text)
    expect_false(result$approved, info = text)
    expect_false(result$bare, info = text)
  }
  expect_false(parse_approval(NULL)$approved)
  expect_false(parse_approval(NA_character_)$approved)
})

test_that("sql_key() ignores whitespace and handles long or missing SQL", {
  expect_identical(sql_key("SELECT  1\n FROM t"), sql_key("SELECT 1 FROM t"))
  expect_false(identical(sql_key("SELECT 1"), sql_key("SELECT 2")))
  columns <- str_flatten(rep("a_long_column_name", 2000), ", ")
  long <- paste("SELECT", columns, "FROM t")
  expect_gt(str_length(long), 10000)
  log <- new_estimate_log()
  expect_identical(record_estimate(log, long, 1, 1L), 1L)
  expect_false(is.null(log$records[[sql_key(long)]]))
  expect_null(sql_key(NULL))
  expect_null(sql_key(""))
  expect_null(sql_key("  "))
  expect_null(sql_key(NA_character_))
  expect_null(sql_key(list("SELECT 1")))
})

test_that("record_estimate() numbers estimates and keeps the first number", {
  log <- new_estimate_log()
  expect_identical(record_estimate(log, "SELECT 1", 10, 1L), 1L)
  expect_identical(record_estimate(log, "SELECT 2", 20, 1L), 2L)
  expect_identical(record_estimate(log, "SELECT   1", 10, 3L), 1L)
  record <- log$records[[sql_key("SELECT 1")]]
  expect_identical(record$at, c(1L, 3L))
  # A second dry-run at the same message is recorded once.
  record_estimate(log, "SELECT 1", 10, 3L)
  expect_identical(log$records[[sql_key("SELECT 1")]]$at, c(1L, 3L))
  expect_identical(record$bytes, 10)
})

run <- "orion_run_bq_query"

# The gate decision for `tool` with SQL key `key`, when the latest of `n`
# user messages is `text`.
# `...`: reply, destination and overwrite, for the tools that write a table.
decide <- function(log, key, n, text, tool = run, ...) {
  gate_decision(tool, key, log, n, parse_approval(text), ...)
}

# A log built from a description of what happened. `at` gives, per estimate
# number, the user-message counts at which its SQL was dry-run; `released`
# the count at which it was run, if it was.
key_of <- \(number) sql_key(paste("SELECT", number))
log_of <- function(at, released = list(), approvals = integer(),
                   bytes = 15e9, auto_run_bytes = 0) {
  log <- new_estimate_log(auto_run_bytes)
  bytes <- rep_len(bytes, length(at))
  for (number in seq_along(at)) {
    for (count in at[[number]]) {
      record_estimate(log, paste("SELECT", number), bytes[[number]], count)
    }
    if (number <= length(released) && !is.null(released[[number]])) {
      record_release(log, key_of(number), released[[number]])
    }
  }
  for (count in approvals) record_approval(log, count)
  log
}

test_that("gate_decision() lets free tools through", {
  log <- new_estimate_log()
  expect_true(decide(log, NULL, 1L, "hi", tool = "orion_list_tables")$allow)
  estimate <- "orion_estimate_query_cost"
  expect_true(decide(log, sql_key("SELECT 1"), 1L, "hi", tool = estimate)$allow)
})

test_that("gate_decision() with a single estimate", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  key <- sql_key("SELECT 1")
  # No estimate number: the tools take no SQL.
  expect_false(decide(log, sql_key("SELECT 9"), 2L, "yes")$allow)
  expect_match(
    decide(log, NULL, 2L, "yes")$message,
    "This tool takes no SQL"
  )
  # Dry-run after the latest user message: the user has not seen it.
  expect_match(
    decide(log, key, 1L, "yes")$message,
    "after the user's last message"
  )
  # Not an approval.
  expect_false(decide(log, key, 2L, "ok, but use Crossref")$allow)
  # Plain yes, or yes with the right number.
  expect_true(decide(log, key, 2L, "yes")$allow)
  expect_true(decide(log, key, 2L, "run #1")$allow)
  expect_false(decide(log, key, 2L, "run #2")$allow)
})

test_that("gate_decision() with several estimates needs a number", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  record_estimate(log, "SELECT 3", 30, 1L)
  k1 <- sql_key("SELECT 1")
  k2 <- sql_key("SELECT 2")
  plain <- decide(log, k2, 2L, "yes")
  expect_false(plain$allow)
  expect_match(plain$message, "#1, #2, #3")
  expect_true(decide(log, k2, 2L, "yes, 2")$allow)
  expect_false(decide(log, k1, 2L, "yes, 2")$allow)
  expect_match(decide(log, k1, 2L, "run #2")$message, "estimate #1")
})

test_that("gate_decision() accepts several numbers or 'all'", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  record_estimate(log, "SELECT 3", 30, 1L)
  k1 <- sql_key("SELECT 1")
  k2 <- sql_key("SELECT 2")
  k3 <- sql_key("SELECT 3")
  expect_true(decide(log, k3, 2L, "run #3 and run #2")$allow)
  expect_true(decide(log, k2, 2L, "run #3 and run #2")$allow)
  expect_false(decide(log, k1, 2L, "run #3 and run #2")$allow)
  for (key in list(k1, k2, k3)) {
    expect_true(decide(log, key, 2L, "yes, run all")$allow)
  }
  # "all" means the estimates since the previous approval, not older ones.
  record_approval(log, 2L)
  record_estimate(log, "SELECT 4", 40, 2L)
  record_estimate(log, "SELECT 5", 50, 2L)
  expect_false(decide(log, sql_key("SELECT 5"), 3L, "both")$allow)
  expect_true(decide(log, sql_key("SELECT 5"), 3L, "yes, both")$allow)
  expect_false(decide(log, k1, 3L, "yes, both")$allow)
})

test_that("a message of numbers approves only open estimates", {
  log <- log_of(at = list(1L, 1L))
  expect_true(decide(log, key_of(2), 2L, "#2")$allow)
  expect_true(decide(log, key_of(2), 2L, "2")$allow)
  expect_true(decide(log, key_of(2), 2L, "#1 and #2")$allow)
  # The number names another estimate.
  expect_match(decide(log, key_of(1), 2L, "#2")$message, "estimate #1")
  # "5" answers another question: there is no open #5.
  five <- decide(log, key_of(2), 2L, "5")
  expect_false(five$allow)
  expect_match(five$message, "has not approved this yet")
  expect_false(decide(log, key_of(2), 2L, "2, 5")$allow)
  # An estimate that has run is not open; it still runs with "run #1".
  ran <- log_of(at = list(1L, 2L), released = list(2L), approvals = 2L)
  expect_false(decide(ran, key_of(1), 3L, "#1")$allow)
  expect_true(decide(ran, key_of(1), 3L, "run #1")$allow)
  expect_true(decide(ran, key_of(2), 3L, "#2")$allow)
  # A small query counts as open for the call that exports it.
  small <- log_of(at = list(1L), bytes = 5e6, auto_run_bytes = 1e8)
  export <- "orion_export_bq_query"
  expect_true(decide(small, key_of(1), 2L, "#1", tool = export)$allow)
  # Saving a stored result has no estimate; numbers must still be open.
  save <- "orion_save_result_to_bq"
  five <- decide(
    log, NULL, 2L, "5",
    tool = save, reply = "Save q1 to p.d.t?", destination = "p.d.t"
  )
  expect_match(five$message, "has not approved this yet")
})

test_that("the rejection tells the model how the user can approve", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  message <- decide(log, sql_key("SELECT 1"), 2L, "yes")$message
  expect_match(message, "#1, #2", fixed = TRUE)
  expect_match(message, "run #2 and #3", fixed = TRUE)
  expect_match(message, "run all", fixed = TRUE)
  expect_match(message, "or just '#2'", fixed = TRUE)
})

test_that("an estimate number the tool never gave is refused clearly", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  # The model names "Estimate #3" for SQL it never dry-ran.
  key <- request_key(list(estimate = 3L), log)
  expect_null(key)
  result <- gate_decision(
    run, key, log, 2L, parse_approval("yes"),
    estimate = 3L
  )
  expect_false(result$allow)
  expect_match(result$message, "there is no Estimate #3", fixed = TRUE)
  expect_match(result$message, "Existing estimates: #1, #2", fixed = TRUE)
  expect_match(result$message, "Only orion_estimate_query_cost", fixed = TRUE)
  empty <- gate_decision(
    run, NULL, new_estimate_log(), 2L, parse_approval("yes"),
    estimate = 1L
  )
  expect_match(empty$message, "Existing estimates: none", fixed = TRUE)
})

test_that("request_key() goes by the estimate number only", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  expect_identical(request_key(list(estimate = 2L), log), sql_key("SELECT 2"))
  expect_identical(request_key(list(estimate = 2), log), sql_key("SELECT 2"))
  # SQL in the call does not count, even when it was estimated.
  expect_null(request_key(list(query = "SELECT 2"), log))
  expect_identical(
    request_key(list(estimate = 1L, query = "SELECT 2"), log),
    sql_key("SELECT 1")
  )
  expect_null(request_key(list(estimate = 9L), log))
  expect_null(request_key(list(), log))
})

test_that("gate_decision() counts only estimates since the previous approval", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  k1 <- sql_key("SELECT 1")
  expect_true(decide(log, k1, 2L, "yes")$allow)
  record_approval(log, 2L)
  # The same approval still covers a retry in the same reply.
  expect_true(decide(log, k1, 2L, "yes")$allow)
  # A second SQL dry-run within that reply is not covered by that "yes".
  record_estimate(log, "SELECT 2", 20, 2L)
  k2 <- sql_key("SELECT 2")
  expect_false(decide(log, k2, 2L, "yes")$allow)
  # The next "yes" covers it: it is the only estimate since the approval.
  expect_true(decide(log, k2, 3L, "yes")$allow)
  expect_false(decide(log, k1, 3L, "yes")$allow)
  # The older one can still be run by number.
  expect_true(decide(log, k1, 3L, "run #1")$allow)
})

test_that("yes after a revision runs the revised estimate", {
  log <- log_of(at = list(1L, 2L))
  expect_true(decide(log, key_of(2), 3L, "yes")$allow)
})

test_that("yes after a revision does not run the earlier estimate", {
  log <- log_of(at = list(1L, 2L))
  result <- decide(log, key_of(1), 3L, "yes")
  expect_false(result$allow)
  expect_match(result$message, "(open estimates: #2)", fixed = TRUE)
})

test_that("yes still counts when the model dry-runs the SQL again", {
  log <- log_of(at = list(c(1L, 2L)))
  expect_true(decide(log, key_of(1), 2L, "yes")$allow)
})

test_that("yes is unclear when one step showed two estimates", {
  log <- log_of(at = list(1L, 1L))
  result <- decide(log, key_of(2), 2L, "yes")
  expect_false(result$allow)
  expect_match(result$message, "(open estimates: #1, #2)", fixed = TRUE)
})

test_that("'yes, both' covers the two estimates of one step", {
  log <- log_of(at = list(1L, 1L))
  expect_true(decide(log, key_of(2), 2L, "yes, both")$allow)
  expect_true(decide(log, key_of(1), 2L, "yes, both")$allow)
})

test_that("yes to a follow-up runs it, after an earlier estimate has run", {
  log <- log_of(
    at = list(c(1L, 2L), 2L), released = list(2L), approvals = 2L
  )
  expect_true(decide(log, key_of(2), 3L, "yes")$allow)
})

test_that("yes to a follow-up does not run the earlier estimate again", {
  log <- log_of(
    at = list(c(1L, 2L), 2L), released = list(2L), approvals = 2L
  )
  result <- decide(log, key_of(1), 3L, "yes")
  expect_false(result$allow)
  expect_match(result$message, "(open estimates: #2)", fixed = TRUE)
})

test_that("an estimate left over from an earlier approval needs its number", {
  log <- log_of(
    at = list(1L, 1L), released = list(NULL, 2L), approvals = 2L
  )
  result <- decide(log, key_of(1), 3L, "ok")
  expect_false(result$allow)
  expect_no_match(result$message, "open estimates")
  expect_true(decide(log, key_of(1), 3L, "run #1")$allow)
})

test_that("yes still counts after a question in between", {
  log <- log_of(at = list(1L))
  expect_true(decide(log, key_of(1), 3L, "yes")$allow)
})

test_that("an estimate dry-run only after the yes is not covered by it", {
  log <- log_of(at = list(2L))
  result <- decide(log, key_of(1), 2L, "yes")
  expect_false(result$allow)
  expect_match(result$message, "after the user's last message")
})

test_that("a small lookup beside the main query leaves the yes clear", {
  log <- log_of(
    at = list(1L, 1L), bytes = c(5e6, 15e9), auto_run_bytes = 1e8
  )
  result <- decide(log, key_of(2), 2L, "yes")
  expect_true(result$allow)
  expect_false(result$unasked)
})

test_that("an old estimate can be run by its number", {
  log <- log_of(at = list(1L, 3L), approvals = 2L)
  expect_true(decide(log, key_of(1), 5L, "run #1")$allow)
})

test_that("a release covers a retry in the same reply, not a later yes", {
  log <- log_of(at = list(1L), released = list(2L), approvals = 2L)
  expect_identical(log$records[[key_of(1)]]$released_at, 2L)
  expect_true(decide(log, key_of(1), 2L, "yes")$allow)
  expect_false(decide(log, key_of(1), 3L, "yes")$allow)
  # Shown again after the run: open again.
  record_estimate(log, "SELECT 1", 15e9, 3L)
  expect_true(decide(log, key_of(1), 4L, "yes")$allow)
  # Unknown keys are ignored.
  expect_no_error(record_release(log, NULL, 2L))
  expect_no_error(record_release(log, "no-such-key", 2L))
  expect_length(log$records, 1)
})

test_that("gate_decision() rejects an estimate made after the approval", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  # The user says yes (message 2); the model then dry-runs new SQL and
  # tries to run it in the same reply.
  record_estimate(log, "SELECT 1 LIMIT 10", 10, 2L)
  key <- sql_key("SELECT 1 LIMIT 10")
  expect_false(decide(log, key, 2L, "yes")$allow)
  expect_false(decide(log, key, 2L, "run #2")$allow)
})

test_that("gate_decision() needs a plain approval to save a result", {
  log <- new_estimate_log()
  save <- "orion_save_result_to_bq"
  asked <- "Shall I save q1 as `my-project.results.oa_by_year`?"
  save_to <- \(text, ...) {
    decide(
      log, NULL, 2L, text,
      tool = save, destination = "my-project.results.oa_by_year", ...
    )
  }
  expect_false(save_to("save q1 please", reply = asked)$allow)
  expect_true(save_to("yes", reply = asked)$allow)
  # The "yes" answered a message that did not name the table.
  hidden <- save_to("yes", reply = "Shall I save q1 to BigQuery?")
  expect_false(hidden$allow)
  expect_match(hidden$message, "did not name the destination table")
  expect_match(hidden$message, "project.dataset.table", fixed = TRUE)
  expect_false(save_to("yes")$allow)
  expect_false(decide(log, NULL, 2L, "yes", tool = save, reply = asked)$allow)
})

test_that("a table is written only to the destination the user saw", {
  log <- log_of(at = list(1L))
  table <- "orion_query_to_table"
  to_table <- \(text, reply, destination = "my-project.results.works", ...) {
    decide(
      log, key_of(1), 2L, text,
      tool = table, reply = reply, destination = destination, ...
    )
  }
  shown <- paste(
    "Estimate #1 scans 15 GB. Shall I write the result to",
    "`My-Project.results.works`?"
  )
  expect_true(to_table("yes", shown)$allow)
  expect_true(to_table("run #1", shown)$allow)
  expect_true(to_table("yes", shown, overwrite = FALSE)$allow)
  expect_true(to_table("yes", shown, overwrite = "false")$allow)
  # Approved, but the model picked a table the user did not see.
  other <- to_table("yes", shown, destination = "my-project.results.other")
  expect_false(other$allow)
  expect_match(other$message, "'my-project.results.other'", fixed = TRUE)
  expect_match(other$message, "wait for a yes")
  expect_false(to_table("yes", "Estimate #1 scans 15 GB. Run it?")$allow)
  expect_false(to_table("yes", shown, destination = NULL)$allow)
  expect_false(to_table("yes", shown, destination = "")$allow)
  # Not approved: the rejection already asks for the table.
  expect_match(to_table("hm", shown)$message, "project.dataset.table")
})

test_that("an existing table is replaced only if the user was told", {
  log <- log_of(at = list(1L))
  replace <- \(reply, overwrite = TRUE, tool = "orion_query_to_table") {
    decide(
      log, key_of(1), 2L, "yes",
      tool = tool, reply = reply, destination = "p.d.works",
      overwrite = overwrite
    )
  }
  silent <- replace("Shall I write Estimate #1 to p.d.works?")
  expect_false(silent$allow)
  expect_match(silent$message, "will be replaced")
  expect_false(replace("Shall I write it to p.d.works?", "true")$allow)
  for (reply in c(
    "This will overwrite p.d.works. Go ahead?",
    "The existing table p.d.works will be replaced. Go ahead?",
    "Replacing p.d.works. Go ahead?",
    "Soll ich p.d.works überschreiben?",
    "Die Tabelle p.d.works wird ersetzt. Einverstanden?"
  )) {
    expect_true(replace(reply)$allow, info = reply)
    expect_true(
      replace(reply, tool = "orion_save_result_to_bq")$allow,
      info = reply
    )
  }
  save <- replace("Save q1 to p.d.works?", tool = "orion_save_result_to_bq")
  expect_false(save$allow)
})

test_that("running and exporting do not depend on the reply or a table", {
  log <- log_of(at = list(1L))
  for (tool in c("orion_run_bq_query", "orion_export_bq_query")) {
    expect_true(decide(log, key_of(1), 2L, "yes", tool = tool)$allow)
    result <- decide(
      log, key_of(1), 2L, "yes",
      tool = tool, reply = "", destination = "p.d.t", overwrite = TRUE
    )
    expect_true(result$allow, info = tool)
  }
})

test_that("last_reply_text() is what the user read before answering", {
  request <- ContentToolRequest(id = "1", name = "orion_list_tables")
  turns <- list(
    SystemTurn(list(ContentText("system prompt with p.d.t"))),
    UserTurn(list(ContentText("first"))),
    AssistantTurn(list(ContentText("An early reply."))),
    UserTurn(list(ContentText("save it"))),
    AssistantTurn(list(ContentText("Checking."), request)),
    UserTurn(list(ContentToolResult("tables", request = request))),
    AssistantTurn(list(ContentText("Shall I save q1 to p.d.t?"))),
    UserTurn(list(ContentText("yes"))),
    AssistantTurn(list(ContentText("Saving to p.d.other.")))
  )
  expect_identical(
    last_reply_text(turns),
    "Checking.\nShall I save q1 to p.d.t?"
  )
  # One user message: the user has read no reply yet.
  expect_identical(last_reply_text(turns[1:3]), "")
  expect_identical(last_reply_text(list()), "")
})

test_that("user_messages() skips turns that carry only tool results", {
  request <- ContentToolRequest(id = "1", name = "orion_list_tables")
  turns <- list(
    UserTurn(list(ContentText("first"))),
    AssistantTurn(list(request)),
    UserTurn(list(ContentToolResult("tables", request = request))),
    AssistantTurn(list(ContentText("Here they are."))),
    UserTurn(list(ContentText("yes")))
  )
  expect_length(user_messages(turns), 2)
  expect_identical(latest_user_text(turns), "yes")
  expect_identical(latest_user_text(list()), "")
})

test_that("number_estimates() numbers successful dry-runs only", {
  fake <- ellmer::tool(
    # Reads its arguments as mcptools' tool functions do.
    function(query) {
      query <- lapply(match.call()[-1], eval)$query
      if (query == "bad") {
        return(ContentToolResult(error = "refused"))
      }
      jsonlite::toJSON(list(bytes_processed = 123), auto_unbox = TRUE)
    },
    name = "orion_estimate_query_cost", description = "Estimate.",
    arguments = list(query = type_string("SQL"))
  )
  other <- ellmer::tool(
    \() "x",
    name = "orion_list_datasets", description = "List."
  )
  log <- new_estimate_log()
  log$n_user_messages <- 1L
  tools <- number_estimates(list(fake, other), log)
  expect_identical(tools[[2]], other)
  expect_identical(tools[[1]]@name, "orion_estimate_query_cost")
  expect_match(tools[[1]](query = "SELECT 1"), "Estimate #1")
  expect_match(tools[[1]](query = "SELECT 2"), "Estimate #2")
  expect_match(tools[[1]](query = "SELECT  1"), "Estimate #1")
  expect_s3_class(tools[[1]](query = "bad"), "ellmer::ContentToolResult")
  expect_length(log$records, 2)
  expect_identical(log$records[[sql_key("SELECT 1")]]$bytes, 123L)
})

test_that("run_by_estimate() replaces the SQL by an estimate number", {
  seen <- NULL
  run <- ellmer::tool(
    function(query, filename = NULL) {
      args <- lapply(match.call()[-1], eval)
      seen <<- args
      "done"
    },
    name = "orion_export_bq_query", description = "Export.",
    arguments = list(
      query = type_string("SQL"),
      filename = type_string("File", required = FALSE)
    )
  )
  free <- ellmer::tool(
    \() "x",
    name = "orion_list_datasets", description = "List."
  )
  log <- new_estimate_log()
  record_estimate(log, "SELECT doi\nFROM works", 10, 1L)
  tools <- run_by_estimate(list(run, free), log)
  expect_identical(tools[[2]], free)
  wrapped <- tools[[1]]
  # The model sees no query argument, and must give an estimate.
  expect_named(wrapped@arguments@properties, c("estimate", "filename"))
  expect_true(wrapped@arguments@properties$estimate@required)
  expect_match(wrapped@description, "takes no SQL", fixed = TRUE)

  expect_identical(wrapped(estimate = 1L, filename = "a.csv"), "done")
  expect_identical(
    seen,
    list(query = "SELECT doi\nFROM works", filename = "a.csv")
  )
  expect_error(wrapped(query = "SELECT 1"))
  missing <- wrapped(estimate = 7L)
  expect_s3_class(missing, "ellmer::ContentToolResult")
  expect_match(missing@error, "no Estimate #7")
})

test_that("auto_run_bytes() reads ORION_AUTO_RUN_GB", {
  expect_identical(auto_run_bytes("0.1"), 1e8)
  expect_identical(auto_run_bytes("0"), 0)
  expect_identical(auto_run_bytes("2"), 2e9)
  expect_error(auto_run_bytes("lots"), "ORION_AUTO_RUN_GB")
  expect_error(auto_run_bytes("-1"), "ORION_AUTO_RUN_GB")
})

test_that("small queries run without asking, only with orion_run_bq_query", {
  log <- new_estimate_log(auto_run_bytes = 1e8)
  # A lookup dry-run in this very reply (message 1), never shown to the user.
  record_estimate(log, "SELECT ror_id FROM institution", 5e6, 1L)
  small <- sql_key("SELECT ror_id FROM institution")
  result <- decide(log, small, 1L, "What share of works ...")
  expect_true(result$allow)
  expect_true(result$unasked)
  # Writing a file or a table always needs an approval.
  export <- "orion_export_bq_query"
  expect_false(decide(log, small, 1L, "What share ...", tool = export)$allow)
  table <- "orion_query_to_table"
  expect_false(decide(log, small, 1L, "What share ...", tool = table)$allow)
  # Large or unknown size: approval as before.
  record_estimate(log, "SELECT big", 15e9, 1L)
  record_estimate(log, "SELECT unknown", NA_real_, 1L)
  expect_false(decide(log, sql_key("SELECT big"), 1L, "anything")$allow)
  expect_false(decide(log, sql_key("SELECT unknown"), 1L, "anything")$allow)
  # Turned off.
  off <- new_estimate_log(auto_run_bytes = 0)
  record_estimate(off, "SELECT 1", 5e6, 1L)
  expect_false(decide(off, sql_key("SELECT 1"), 1L, "anything")$allow)
})

test_that("small lookups do not make a plain yes ambiguous", {
  log <- new_estimate_log(auto_run_bytes = 1e8)
  record_estimate(log, "SELECT lookup", 5e6, 1L)
  record_estimate(log, "SELECT main", 15e9, 1L)
  main <- sql_key("SELECT main")
  approved <- decide(log, main, 2L, "yes")
  expect_true(approved$allow)
  expect_false(approved$unasked)
  # Two large estimates still need a number.
  record_estimate(log, "SELECT other", 20e9, 1L)
  expect_false(decide(log, main, 2L, "yes")$allow)
  expect_true(decide(log, main, 2L, "run #2")$allow)
})

test_that("the estimate result tells the model when it may run unasked", {
  fake <- ellmer::tool(
    function(query) {
      query <- lapply(match.call()[-1], eval)$query
      size <- if (query == "SELECT small") 5e6 else 15e9
      jsonlite::toJSON(list(bytes_processed = size), auto_unbox = TRUE)
    },
    name = "orion_estimate_query_cost", description = "Estimate.",
    arguments = list(query = type_string("SQL"))
  )
  log <- new_estimate_log(auto_run_bytes = 1e8)
  estimate <- number_estimates(list(fake), log)[[1]]
  expect_match(estimate(query = "SELECT small"), "without asking the user")
  big <- estimate(query = "SELECT big")
  expect_match(big, "so the user can approve it by number")
  expect_no_match(big, "without asking")
})
