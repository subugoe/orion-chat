meta <- list(
  title = "ORION-DBs AI chat interface",
  started = as.POSIXct("2026-11-12 14:05:00"),
  models = c("qwen3.5-397b-a17b", "glm-5.3-flash")
)

estimate <- ContentToolRequest(
  id = "call_1", name = "orion_estimate_query_cost",
  arguments = list(query = "SELECT doi\nFROM openalex.works\nLIMIT 10")
)

test_that("text only: header, headings, no system prompt", {
  turns <- list(
    SystemTurn(list(ContentText("SECRET SYSTEM PROMPT"))),
    UserTurn(list(ContentText("What datasets are there?"))),
    AssistantTurn(list(ContentText("OpenAlex, Crossref and OpenAIRE.")))
  )
  md <- render_transcript(turns, meta)
  expect_match(md, "^# ORION-DBs AI chat interface")
  expect_match(md, "Started: 2026-11-12 14:05", fixed = TRUE)
  expect_match(md, "Models: qwen3.5-397b-a17b, glm-5.3-flash", fixed = TRUE)
  expect_match(md, "## You\n\nWhat datasets are there?", fixed = TRUE)
  expect_match(
    md, "## Assistant\n\nOpenAlex, Crossref and OpenAIRE.",
    fixed = TRUE
  )
  expect_no_match(md, "SECRET")
})

test_that("a tool request shows the SQL verbatim, other arguments as a list", {
  request <- ContentToolRequest(
    id = "call_2", name = "orion_export_result",
    arguments = list(name = "q1", format = "csv")
  )
  turns <- list(
    AssistantTurn(list(ContentText("Checking the cost."), estimate, request))
  )
  md <- render_transcript(turns, meta)
  expect_match(md, "**Tool call:** `orion_estimate_query_cost`", fixed = TRUE)
  expect_match(
    md, "```sql\nSELECT doi\nFROM openalex.works\nLIMIT 10\n```",
    fixed = TRUE
  )
  expect_match(md, "- name: q1\n- format: csv", fixed = TRUE)
})

test_that("tool results go under the assistant turn, with no empty heading", {
  turns <- list(
    UserTurn(list(ContentText("How much would it cost?"))),
    AssistantTurn(list(estimate)),
    UserTurn(list(
      ContentToolResult('{"gb_processed": 1.2}', request = estimate)
    )),
    AssistantTurn(list(ContentText("It scans 1.2 GB. Estimate #1.")))
  )
  md <- render_transcript(turns, meta)
  expect_match(
    md,
    paste0(
      "**Tool result** (`orion_estimate_query_cost`):\n\n",
      "```\n{\"gb_processed\": 1.2}\n```"
    ),
    fixed = TRUE
  )
  expect_identical(str_count(md, fixed("## You")), 1L)
  expect_identical(str_count(md, fixed("## Assistant")), 1L)
  expect_no_match(md, "## You\n\n## ")
  expect_no_match(md, "## You\n*$")
  # Order: question, call, result, answer.
  positions <- map_int(
    c("How much", "Tool call", "Tool result", "It scans"),
    \(x) str_locate(md, fixed(x))[1, 1]
  )
  expect_false(is.unsorted(positions))
})

test_that("errors and gate rejections are marked", {
  run <- ContentToolRequest(
    id = "c3", name = "orion_run_bq_query",
    arguments = list(query = "SELECT 1")
  )
  rejected <- structure(
    class = c("ellmer_tool_reject", "rlang_error", "error", "condition"),
    list(message = paste(
      "Tool call rejected.",
      "Not run: the user has not approved this yet."
    ))
  )
  turns <- list(
    AssistantTurn(list(run)),
    UserTurn(list(
      ContentToolResult(error = rejected, request = run),
      ContentToolResult(error = simpleError("Table not found"), request = run),
      ContentToolResult(error = "plain string error", request = run)
    ))
  )
  md <- render_transcript(turns, meta)
  expect_match(
    md, "**Rejected by the approval gate** (`orion_run_bq_query`)",
    fixed = TRUE
  )
  expect_match(md, "not approved this yet", fixed = TRUE)
  expect_match(
    md, "**Tool error** (`orion_run_bq_query`):\n\n```\nTable not found",
    fixed = TRUE
  )
  expect_match(md, "plain string error", fixed = TRUE)
})

test_that("long results are cut with a note giving the full length", {
  long <- strrep("x", 5000)
  turns <- list(
    AssistantTurn(list(estimate)),
    UserTurn(list(ContentToolResult(long, request = estimate)))
  )
  md <- render_transcript(turns, meta)
  expect_match(md, "(Cut at 2,000 of 5,000 characters.)", fixed = TRUE)
  expect_false(str_detect(md, strrep("x", 2001)))
  expect_true(str_detect(md, strrep("x", 2000)))
})

# A dry-run result as orion_estimate_query_cost of orion-mcp returns it
# (checked against the server on 2026-11-13), with the note that
# number_estimates() appends.
estimate_result <- function(sql, number) {
  fake <- ellmer::tool(
    function(query) {
      query <- lapply(match.call()[-1], eval)$query
      as.character(jsonlite::toJSON(
        list(
          bytes_processed = 327602034, gb_processed = 0.328,
          cost_usd_estimate = 0.0019, sql = query,
          message = paste(
            "This query will scan 0.328 GB (estimated cost: $0.0019).",
            strrep("Present this to the user in exactly this order. ", 22)
          )
        ),
        auto_unbox = TRUE, pretty = TRUE
      ))
    },
    name = "orion_estimate_query_cost", description = "Estimate.",
    arguments = list(query = type_string("SQL"))
  )
  log <- new_estimate_log()
  for (i in seq_len(number - 1)) record_estimate(log, paste("SELECT", i), 1, 1L)
  number_estimates(list(fake), log)[[1]](query = sql)
}

test_that("an estimate result shows its number in the label, cut or not", {
  columns <- str_flatten(paste0("column_", 1:60), ",\n  ")
  long_sql <- paste0("SELECT\n  ", columns, "\nFROM openalex.works")
  long <- estimate_result(long_sql, 2)
  expect_gt(str_length(long), transcript_result_limit)
  # The note with the number is beyond the cut.
  expect_gt(
    str_locate(long, fixed("This is Estimate #2"))[1, 1],
    transcript_result_limit
  )
  turns <- list(
    AssistantTurn(list(estimate)),
    UserTurn(list(ContentToolResult(long, request = estimate)))
  )
  md <- render_transcript(turns, meta)
  expect_match(
    md, "**Tool result** (`orion_estimate_query_cost`, Estimate #2):",
    fixed = TRUE
  )
  expect_match(md, "(Cut at 2,000 of", fixed = TRUE)
  expect_no_match(md, "This is Estimate #2", fixed = TRUE)

  short <- estimate_result("SELECT doi FROM openalex.works", 1)
  expect_lt(str_length(short), transcript_result_limit)
  turns[[2]] <- UserTurn(list(ContentToolResult(short, request = estimate)))
  md <- render_transcript(turns, meta)
  expect_match(
    md, "**Tool result** (`orion_estimate_query_cost`, Estimate #1):",
    fixed = TRUE
  )
  # The text of the result is unchanged.
  expect_match(md, "This is Estimate #1.", fixed = TRUE)
})

test_that("only estimate results get a number in the label", {
  run <- ContentToolRequest(
    id = "c4", name = "orion_run_bq_query", arguments = list(estimate = 3L)
  )
  turns <- list(
    AssistantTurn(list(run, estimate)),
    UserTurn(list(
      ContentToolResult(
        "Rows for Estimate #3. This is Estimate #3.",
        request = run
      ),
      # SQL that mentions a number, without the note.
      ContentToolResult("SELECT 'Estimate #7' AS label", request = estimate),
      ContentToolResult(
        error = "Refused. This is Estimate #9.", request = estimate
      )
    ))
  )
  md <- render_transcript(turns, meta)
  expect_match(md, "**Tool result** (`orion_run_bq_query`):", fixed = TRUE)
  expect_match(
    md, "**Tool result** (`orion_estimate_query_cost`):",
    fixed = TRUE
  )
  expect_match(
    md, "**Tool error** (`orion_estimate_query_cost`):",
    fixed = TRUE
  )
  expect_no_match(md, ", Estimate #")
})

test_that("non-text results, backticks and unknown content do not fail", {
  turns <- list(
    AssistantTurn(list(estimate)),
    UserTurn(list(ContentToolResult(
      list(rows = 3, cols = list("a", "b")),
      request = estimate
    ))),
    AssistantTurn(list(
      ContentText("Here is code:\n```r\nx <- 1\n```"),
      ContentThinking("hidden reasoning"),
      ContentImageRemote("https://example.org/a.png")
    ))
  )
  md <- render_transcript(turns, meta)
  expect_match(md, "\"rows\": 3", fixed = TRUE)
  expect_match(md, "```r\nx <- 1\n```", fixed = TRUE)
  expect_no_match(md, "hidden reasoning")
  expect_match(md, "_(ContentImageRemote not shown.)_", fixed = TRUE)
})

test_that("a result whose text contains a fence gets a longer fence", {
  turns <- list(
    AssistantTurn(list(estimate)),
    UserTurn(list(
      ContentToolResult("Show it in a ```sql block", request = estimate)
    ))
  )
  expect_match(
    render_transcript(turns, meta),
    "````\nShow it in a ```sql block\n````",
    fixed = TRUE
  )
})

test_that("save_transcript() replaces the file in one step", {
  dir <- file.path(tempfile(), "transcripts")
  path <- file.path(dir, "orion-chat-test.md")
  save_transcript("first", path)
  save_transcript("second – ü", path)
  expect_identical(readLines(path, encoding = "UTF-8"), "second – ü")
  expect_identical(
    list.files(dir, all.files = TRUE, no.. = TRUE),
    "orion-chat-test.md"
  )
})
