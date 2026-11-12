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

# Models
gwdg_url <- "https://chat-ai.academiccloud.de/v1"
gwdg_key <- \() Sys.getenv("GWDG_API_KEY")
default_model <- "glm-5.3-flash"

# Models GWDG serves right now, as c("Display name" = "id"). Falls back to
# the default model if the list can't be fetched.
gwdg_models <- function() {
  models <- tryCatch(
    httr2::request(gwdg_url) |>
      httr2::req_url_path_append("models") |>
      httr2::req_auth_bearer_token(gwdg_key()) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      pluck("data") |>
      keep(\(m) (m$status %||% "ready") == "ready"),
    error = \(e) {
      warning("Could not list GWDG models: ", conditionMessage(e), call. = FALSE)
      list()
    }
  )
  ids <- c(map_chr(models, "id"), default_model)
  names <- c(map_chr(models, \(m) m$name %||% m$id), default_model)
  choices <- set_names(ids, names)[!duplicated(ids)]
  choices[order(names(choices))]
}

model_choices <- gwdg_models()

# A tiny probe request: can this model take tools? Returns NULL if it can
# (or if the problem is only rate limiting, which the chat will show),
# otherwise a short message for the user.
tool_choice_problem <- function(model) {
  res <- tryCatch(
    httr2::request(gwdg_url) |>
      httr2::req_url_path_append("chat/completions") |>
      httr2::req_auth_bearer_token(gwdg_key()) |>
      httr2::req_body_json(list(
        model = model,
        messages = list(list(role = "user", content = "Hi")),
        tools = list(list(
          type = "function",
          `function` = list(
            name = "noop",
            description = "Do nothing.",
            parameters = list(type = "object", properties = list())
          )
        )),
        tool_choice = "auto",
        max_tokens = 1
      )) |>
      # Include the body, or the error message is only "HTTP 400 Bad Request."
      httr2::req_error(body = \(resp) httr2::resp_body_string(resp)) |>
      httr2::req_perform(),
    error = \(e) conditionMessage(e)
  )
  if (inherits(res, "httr2_response")) return(NULL)
  if (
    str_detect(res, fixed("400")) &&
      str_detect(res, regex("tool choice|tool-call-parser", ignore_case = TRUE))
  ) {
    glue(
      "{model} is not available right now: it can't call tools, which this ",
      "app needs. Try another model, or try again later."
    )
  }
}

# Chat client
system_prompt <- glue(
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
  "the normal workflow.\n\n",
  "Stored results (q1, q2, ...) live inside the orion-mcp server, a ",
  "separate R process in a Docker container. The user cannot reach them ",
  "from their own R session, so never say the data is 'in your R session' ",
  "and never write code that uses q1, q2, ... as R objects. When the user ",
  "wants to plot or analyse a result in R, export it with ",
  "orion_export_result (free) and give R code that reads the exported ",
  "file, e.g. read.csv(\"oa_by_year.csv\"), noting that the file is in the ",
  "export folder they mounted for orion-mcp.\n\n",
  "Write in a plain, factual style. Never use emojis or decorative ",
  "symbols (such as ✅, ❌, 🚀, ⚠️, 📊), not even in headings, lists or ",
  "tables; use words instead (e.g. 'yes'/'no', 'Note:')."
)

message("Starting the orion-mcp container (10-30 s) ...")
orion_tools <- mcp_tools()

# A chat with the orion tools and the approval gate. `estimated` is shared
# across model switches, so dry runs stay valid after changing the model.
new_chat <- function(model, estimated) {
  chat <- chat_openai_compatible(
    base_url = gwdg_url,
    model = model,
    credentials = gwdg_key,
    system_prompt = system_prompt
  )
  chat$set_tools(orion_tools)
  chat$on_tool_result(\(result) remember_estimate(result, chat, estimated))
  chat$on_tool_request(\(request) approval_gate(request, chat, estimated))
  chat
}

# App 
examples <- c(
  "Explore the data" = "What datasets are available in ORION-DBs?",
  "Check the cost" = "What share of works from the University of Göttingen and UMG were open access between 2018 and 2025?",
  "Export your results" = "Export the result as a CSV file."
)

# A numbered list of suggestions renders as numbered cards; the title
# attribute is the card heading, and clicking sends only the body text.
steps <- glue('{seq_along(examples)}. <span class="suggestion" title="{names(examples)}">{examples}</span>')

greeting <- glue(
  "Hi there! I can help you explore **ORION-DBs**, open research ",
  "information on Google BigQuery. Before running any query, I'll show ",
  "you the SQL and what it will cost. Nothing runs until you say **yes**.",
  "\n\n{str_flatten(steps, '\n')}"
)

# .noWS: no space between a link and the comma after it.
footer_link <- \(text, href) tags$a(href = href, target = "_blank", text, .noWS = "outside")

ui <- tagList(
  tags$head(tags$script(src = "tables.js")),
  page_chat(
    id = "chat",
    title = "ORION-DBs AI chat interface",
    icon = tags$img(src = "logo-sub.svg", alt = "SUB Göttingen", class = "sub-logo"),
    sidebar = FALSE,
    # Top bar as on sub.uni-goettingen.de: short links, then the contrast toggle.
    toolbar_global = bslib::toolbar(
      tags$nav(
        class = "sub-meta-nav", `aria-label` = "Links",
        tags$a(href = "https://orion-dbs.community/", target = "_blank", "ORION-DBs"),
        tags$a(href = "https://docs.hpc.gwdg.de/services/ai-services/index.html", target = "_blank", "GWDG AI"),
        tags$a(href = "https://github.com/subugoe/orion-chat", target = "_blank", "GitHub")
      ),
      bslib::input_dark_mode()
    ),
    # Model menu directly below the message box.
    toolbar_input = bslib::toolbar(
      align = "left",
      bslib::toolbar_input_select(
        "model", "GWDG-hosted model",
        choices = model_choices,
        selected = default_model,
        show_label = TRUE
      )
    ),
    footer = tags$div(
      class = "sub-contact",
      tags$p(
        "Contact: Najko Jahn, ",
        tags$a(href = "mailto:najko.jahn@sub.uni-goettingen.de", "najko.jahn@sub.uni-goettingen.de")
      ),
      tags$p(
        "Built in R with ",
        footer_link("shiny", "https://shiny.posit.co/"), ", ",
        footer_link("shinychat", "https://posit-dev.github.io/shinychat/"), ", ",
        footer_link("ellmer", "https://ellmer.tidyverse.org/"), ", ",
        footer_link("mcptools", "https://posit-dev.github.io/mcptools/"), " and ",
        footer_link("bslib", "https://rstudio.github.io/bslib/"),
        ". Models by ",
        footer_link("GWDG", "https://docs.hpc.gwdg.de/services/ai-services/index.html"),
        ", data via ",
        footer_link("orion-mcp", "https://github.com/subugoe/orion-mcp"), "."
      )
    ),
    # Wider than the default (680-760px) so SQL and tables have room;
    # gau.scss narrows prose and user messages within it.
    width = "min(clamp(720px, 60vw, 880px), 100%)",
    theme = bslib::bs_bundle(page_chat_theme(), sass::sass_layer_file("gau.scss"))
  )
)

server <- function(input, output, session) {
  # Each browser session gets its own chat and dry-run record.
  estimated <- new.env()
  # The model the menu is on (the revert target when a switch fails).
  selected_model <- default_model
  chat <- chat_server(
    "chat", new_chat(default_model, estimated),
    greeting = greeting, history = FALSE
  )

  # Switching keeps the conversation; the new model sees the earlier turns.
  # A switch during a reply takes effect once the reply is done. Some models
  # can't call tools; the probe reverts those before the chat fails.
  observeEvent(input$model, ignoreInit = TRUE, {
    model <- input$model
    problem <- tool_choice_problem(model)
    if (!is.null(problem)) {
      showNotification(problem, type = "error", duration = 10)
      updateSelectInput(session, "model", selected = selected_model)
      return()
    }
    chat$set_client(new_chat(model, estimated))
    selected_model <<- model
    label <- names(model_choices)[model_choices == model]
    showNotification(glue("Now using {label}."), duration = 3)
  })
}

shinyApp(ui, server, enableBookmarking = "disable")
