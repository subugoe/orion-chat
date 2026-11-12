test_that("parse_approval() accepts whole-message approvals", {
  cases <- list(
    list("yes", TRUE, NA),
    list("Yes.", TRUE, NA),
    list("  ok!  ", TRUE, NA),
    list("ja", TRUE, NA),
    list("Ja, bitte", TRUE, NA),
    list("y", TRUE, NA),
    list("go ahead", TRUE, NA),
    list("please run it", TRUE, NA),
    list("please do", TRUE, NA),
    list("yes please", TRUE, NA),
    list("yes, run it.", TRUE, NA),
    list("yes, 2", TRUE, 2L),
    list("run #2", TRUE, 2L),
    list("yes, run #12!", TRUE, 12L),
    list("ok 3", TRUE, 3L),
    list("Yes, number 2", TRUE, 2L)
  )
  for (case in cases) {
    result <- parse_approval(case[[1]])
    expect_identical(result$approved, case[[2]], info = case[[1]])
    expect_identical(result$number, as.integer(case[[3]]), info = case[[1]])
  }
})

test_that("parse_approval() rejects messages that only start like one", {
  for (text in c(
    "ok, but use Crossref instead", "go back to the first idea",
    "run a different analysis", "y axis should be log", "yes but only 2020",
    "no", "2", "#2", "", "   ", "yesterday", "okay so what about OpenAIRE?"
  )) {
    expect_false(parse_approval(text)$approved, info = text)
  }
  expect_false(parse_approval(NULL)$approved)
  expect_false(parse_approval(NA_character_)$approved)
})

test_that("sql_key() ignores whitespace and handles long or missing SQL", {
  expect_identical(sql_key("SELECT  1\n FROM t"), sql_key("SELECT 1 FROM t"))
  expect_false(identical(sql_key("SELECT 1"), sql_key("SELECT 2")))
  long <- paste("SELECT", str_flatten(rep("a_long_column_name", 2000), ", "), "FROM t")
  expect_gt(nchar(long), 10000)
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

approve <- function(text) parse_approval(text)
run <- "orion_run_bq_query"

test_that("gate_decision() lets free tools through", {
  log <- new_estimate_log()
  expect_true(gate_decision("orion_list_tables", NULL, log, 1L, approve("hi"))$allow)
  expect_true(gate_decision("orion_estimate_query_cost", sql_key("SELECT 1"), log, 1L, approve("hi"))$allow)
})

test_that("gate_decision() with a single estimate", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  key <- sql_key("SELECT 1")
  # Never dry-run, or no SQL at all.
  expect_false(gate_decision(run, sql_key("SELECT 9"), log, 2L, approve("yes"))$allow)
  expect_match(gate_decision(run, NULL, log, 2L, approve("yes"))$message, "no SQL")
  # Dry-run after the latest user message: the user has not seen it.
  expect_match(gate_decision(run, key, log, 1L, approve("yes"))$message, "after the user's last message")
  # Not an approval.
  expect_false(gate_decision(run, key, log, 2L, approve("ok, but use Crossref"))$allow)
  # Plain yes, or yes with the right number.
  expect_true(gate_decision(run, key, log, 2L, approve("yes"))$allow)
  expect_true(gate_decision(run, key, log, 2L, approve("run #1"))$allow)
  expect_false(gate_decision(run, key, log, 2L, approve("run #2"))$allow)
})

test_that("gate_decision() with several estimates needs a number", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  record_estimate(log, "SELECT 2", 20, 1L)
  record_estimate(log, "SELECT 3", 30, 1L)
  k1 <- sql_key("SELECT 1")
  k2 <- sql_key("SELECT 2")
  plain <- gate_decision(run, k2, log, 2L, approve("yes"))
  expect_false(plain$allow)
  expect_match(plain$message, "#1, #2, #3")
  expect_true(gate_decision(run, k2, log, 2L, approve("yes, 2"))$allow)
  expect_false(gate_decision(run, k1, log, 2L, approve("yes, 2"))$allow)
  expect_match(gate_decision(run, k1, log, 2L, approve("run #2"))$message, "estimate #1")
})

test_that("gate_decision() counts only estimates since the previous approval", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  k1 <- sql_key("SELECT 1")
  expect_true(gate_decision(run, k1, log, 2L, approve("yes"))$allow)
  record_approval(log, 2L)
  # The same approval still covers a retry in the same reply.
  expect_true(gate_decision(run, k1, log, 2L, approve("yes"))$allow)
  # A second SQL dry-run within that reply is not covered by that "yes".
  record_estimate(log, "SELECT 2", 20, 2L)
  k2 <- sql_key("SELECT 2")
  expect_false(gate_decision(run, k2, log, 2L, approve("yes"))$allow)
  # The next "yes" covers it: it is the only estimate since the approval.
  expect_true(gate_decision(run, k2, log, 3L, approve("yes"))$allow)
  expect_false(gate_decision(run, k1, log, 3L, approve("yes"))$allow)
  # The older one can still be run by number.
  expect_true(gate_decision(run, k1, log, 3L, approve("run #1"))$allow)
})

test_that("gate_decision() rejects an estimate made after the approval", {
  log <- new_estimate_log()
  record_estimate(log, "SELECT 1", 10, 1L)
  # The user says yes (message 2); the model then dry-runs new SQL and
  # tries to run it in the same reply.
  record_estimate(log, "SELECT 1 LIMIT 10", 10, 2L)
  key <- sql_key("SELECT 1 LIMIT 10")
  expect_false(gate_decision(run, key, log, 2L, approve("yes"))$allow)
  expect_false(gate_decision(run, key, log, 2L, approve("run #2"))$allow)
})

test_that("gate_decision() needs a plain approval to save a result", {
  log <- new_estimate_log()
  save <- "orion_save_result_to_bq"
  expect_false(gate_decision(save, NULL, log, 1L, approve("save q1 to my dataset"))$allow)
  expect_true(gate_decision(save, NULL, log, 1L, approve("yes"))$allow)
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
    function(query) {
      if (query == "bad") return(ContentToolResult(error = "refused"))
      jsonlite::toJSON(list(bytes_processed = 123), auto_unbox = TRUE)
    },
    name = "orion_estimate_query_cost", description = "Estimate.",
    arguments = list(query = type_string("SQL"))
  )
  other <- ellmer::tool(\() "x", name = "orion_list_datasets", description = "List.")
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
