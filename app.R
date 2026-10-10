# ORION-DBs chat: GWDG-hosted model + orion-mcp, as a Shiny app.

library(shiny)
library(shinychat)
library(ellmer)
library(mcptools)
library(glue)
library(purrr)
library(stringr)

# Setup
# mcptools waits only 4 s per MCP reply; orion-mcp needs ~10 s to start.
local({
  f <- mcptools:::mcp_transport_stdio_send
  src <- str_flatten(deparse(body(f)), "\n")
  if (!str_detect(src, fixed("max_attempts <- 1500"))) {
    stopifnot(str_detect(src, fixed("max_attempts <- 20")))
    body(f) <- str2lang(str_replace(src, fixed("max_attempts <- 20"), "max_attempts <- 1500"))
    assignInNamespace("mcp_transport_stdio_send", f, ns = "mcptools")
  }
})

# Enforce approval

paid_tools <- c(
  "orion_run_bq_query", "orion_export_bq_query",
  "orion_query_to_table", "orion_save_result_to_bq"
)

approval_pattern <- regex(
  "^\\s*(yes|y|ja|jep|ok|okay|sure|go|go ahead|run|run it|proceed|do it|please do|please run)\\b",
  ignore_case = TRUE
)

# Messages the user typed (tool results are stored as user turns without text).
user_messages <- function(chat) {
  keep(chat$get_turns(), \(turn) turn@role == "user" && str_trim(turn@text) != "")
}

latest_user_text <- function(chat) {
  messages <- user_messages(chat)
  if (length(messages) == 0) "" else messages[[length(messages)]]@text
}

# Remember when each SQL first passed a dry run (as a count of user messages),
# ignoring whitespace like the orion server does.
remember_estimate <- function(result, chat, estimated) {
  request <- result@request
  if (request@name == "orion_estimate_query_cost" && is.null(result@error)) {
    key <- str_squish(request@arguments$query)
    estimated[[key]] <- estimated[[key]] %||% length(user_messages(chat))
  }
}

# Paid tools need a "yes" written after the user could see the dry-run SQL.
# The orion server itself refuses SQL that has not been dry-run at all.
approval_gate <- function(request, chat, estimated) {
  if (!request@name %in% paid_tools) return(invisible())

  sql <- request@arguments$query
  if (!is.null(sql)) {
    seen_at <- estimated[[str_squish(sql)]]
    if (is.null(seen_at)) {
      tool_reject(glue(
        "Not run: this SQL has not been dry-run. Call orion_estimate_query_cost ",
        "with exactly this SQL, show the SQL and the estimated cost, ask ",
        "'Shall I run this query?', and wait for a yes."
      ))
    }
    if (seen_at >= length(user_messages(chat))) {
      tool_reject(glue(
        "Not run: this exact SQL was dry-run after the user's last message, ",
        "so the user has not seen it. Show the SQL and the estimated cost, ",
        "ask 'Shall I run this query?', and wait for a yes."
      ))
    }
  }
  if (!str_detect(latest_user_text(chat), approval_pattern)) {
    tool_reject(glue(
      "Not run: the user has not approved this yet. Show the SQL and the ",
      "estimated cost, ask 'Shall I run this query?', and wait for a yes."
    ))
  }
}

# Chat client 
my_chat <- chat_openai_compatible(
  base_url = "https://chat-ai.academiccloud.de/v1",
  model = "qwen3.5-397b-a17b",
  credentials = \() Sys.getenv("GWDG_API_KEY"),
  system_prompt = glue(
    "You help researchers query ORION-DBs (open research information on ",
    "Google BigQuery) through the orion_* tools. Follow the tool ",
    "descriptions exactly. Queries only run after the user explicitly ",
    "approves them in the chat.\n\n",
    "When the user is exploring or the best approach is unclear, act as an ",
    "advisor: propose two or three alternative strategies (e.g. different ",
    "datasets, identifiers such as ROR IDs or DOIs, or ways to filter), ",
    "dry-run each with orion_estimate_query_cost, and compare them in a ",
    "short table with data source, GB scanned, estimated cost, and pros and ",
    "cons. Point out ways to scan less data, such as filtering on partition ",
    "columns or selecting fewer columns. Then ask which strategy to run. ",
    "When the request is already specific, skip the comparison and follow ",
    "the normal workflow."
  )
)

estimated <- new.env()
my_chat$on_tool_result(\(result) remember_estimate(result, my_chat, estimated))
my_chat$on_tool_request(\(request) approval_gate(request, my_chat, estimated))

message("Starting the orion-mcp container (10-30 s) ...")
my_chat$set_tools(mcp_tools())

# App 
examples <- c(
  "Which datasets are available?",
  "Who provides OpenAlex data, and which snapshot is the most recent?",
  "In that snapshot, which column holds the publication type, and how do I filter by it?",
  "Using that table: how many works were published in 2024, by type?",
  "Export the result as CSV."
)

# "1\." instead of "1." keeps markdown from making a list, which shinychat
# would render as suggestion cards.
steps <- glue('{seq_along(examples)}\\. <span class="suggestion">{examples}</span>')

greeting <- glue(
  "Hi! I can write and run BigQuery SQL against **ORION-DBs**. Every query ",
  "is dry-run first; nothing runs (or costs money) until you answer **yes**.",
  "\n\nA typical session, step by step:\n\n",
  "{str_flatten(steps, '<br>\n')}"
)

chat_app(
  my_chat,
  title = "ORION-DBs chat (GWDG)",
  icon = tags$img(src = "logo-sub.svg", alt = "SUB Göttingen", class = "sub-logo"),
  greeting = greeting,
  history = FALSE,
  bookmark_store = "disable",
  theme = bslib::bs_bundle(page_chat_theme(), sass::sass_layer_file("gau.scss"))
)
