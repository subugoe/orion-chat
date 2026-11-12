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
    list("yes, run both, thanks", integer(), TRUE)
  )
  for (case in cases) {
    result <- parse_approval(case[[1]])
    expect_true(result$approved, info = case[[1]])
    expect_identical(result$numbers, case[[2]], info = case[[1]])
    expect_identical(result$all, case[[3]], info = case[[1]])
  }
})

test_that("parse_approval() rejects messages that only start like one", {
  for (text in c(
    "ok, but use Crossref instead", "go back to the first idea",
    "run a different analysis", "y axis should be log", "yes but only 2020",
    "no", "2", "#2", "", "   ", "yesterday", "okay so what about OpenAIRE?",
    "run #2 but with 2024", "yes for 2", "all good, but not yet",
    "go with the cheaper one"
  )) {
    expect_false(parse_approval(text)$approved, info = text)
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
  expect_identical(record$seen_at, 1L)
  expect_identical(record$last_at, 3L)
  expect_identical(record$bytes, 10)
})

run <- "orion_run_bq_query"

# The gate decision for `tool` with SQL key `key`, when the latest of `n`
# user messages is `text`.
decide <- function(log, key, n, text, tool = run) {
  gate_decision(tool, key, log, n, parse_approval(text))
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
  # Never dry-run, or no SQL at all.
  expect_false(decide(log, sql_key("SELECT 9"), 2L, "yes")$allow)
  expect_match(decide(log, NULL, 2L, "yes")$message, "neither")
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

test_that("the rejection tells the model how the user can approve", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  message <- decide(log, sql_key("SELECT 1"), 2L, "yes")$message
  expect_match(message, "#1, #2", fixed = TRUE)
  expect_match(message, "run #2 and #3", fixed = TRUE)
  expect_match(message, "run all", fixed = TRUE)
})

test_that("request_key() takes the estimate number before the SQL", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  expect_identical(request_key(list(estimate = 2L), log), sql_key("SELECT 2"))
  expect_identical(request_key(list(estimate = 2), log), sql_key("SELECT 2"))
  expect_identical(
    request_key(list(estimate = 1L, query = "SELECT 2"), log),
    sql_key("SELECT 1")
  )
  expect_identical(
    request_key(list(query = "SELECT  2"), log),
    sql_key("SELECT 2")
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
  expect_false(decide(log, NULL, 1L, "save q1 please", tool = save)$allow)
  expect_true(decide(log, NULL, 1L, "yes", tool = save)$allow)
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

test_that("run_by_estimate() passes the dry-run SQL for an estimate number", {
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
  expect_named(wrapped@arguments@properties, c("query", "filename", "estimate"))
  expect_false(wrapped@arguments@properties$query@required)
  expect_match(wrapped@description, "estimate", fixed = TRUE)

  expect_identical(wrapped(estimate = 1L, filename = "a.csv"), "done")
  expect_identical(
    seen,
    list(query = "SELECT doi\nFROM works", filename = "a.csv")
  )
  wrapped(query = "SELECT 1")
  expect_identical(seen, list(query = "SELECT 1"))
  missing <- wrapped(estimate = 7L)
  expect_s3_class(missing, "ellmer::ContentToolResult")
  expect_match(missing@error, "no estimate #7")
})
