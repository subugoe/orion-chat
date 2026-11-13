# ORION-DBs chat: a hosted model (GWDG by default) + orion-mcp, as a Shiny app.

library(shiny)
library(shinychat)
library(ellmer)
library(mcptools)
library(glue)
library(purrr)
library(stringr)

# Title and logo in the top bar. The logo file is in www/; replace both to
# use the app elsewhere (the SUB logo is not covered by the MIT licence).
app_title <- "ORION-DBs AI chat interface"
app_logo <- c(src = "logo-sub.svg", alt = "SUB Göttingen")

# Setup (see R/setup.R)
source("R/setup.R")
patch_mcptools_timeout()

# Enforce approval: paid tools run only after the user approved the
# numbered estimate (see R/approval.R). The orion server itself refuses SQL
# that has not been dry-run at all.
source("R/approval.R")
source("R/transcript.R")

approval_gate <- function(request, chat, log) {
  turns <- chat$get_turns()
  n <- length(user_messages(turns))
  log$n_user_messages <- n
  key <- request_key(request@arguments, log)
  decision <- gate_decision(
    request@name, key, log, n,
    parse_approval(latest_user_text(turns)),
    estimate = request@arguments$estimate,
    reply = last_reply_text(turns),
    destination = request@arguments$destination,
    overwrite = request@arguments$overwrite
  )
  if (!decision$allow) tool_reject(decision$message)
  if (request@name %in% paid_tools && !decision$unasked) {
    record_approval(log, n)
    record_release(log, key, n)
  }
}

# Model service: GWDG unless .Renviron says otherwise (see R/provider.R)
source("R/provider.R")
provider <- provider_settings()

# Chat client
# Queries under this size (ORION_AUTO_RUN_GB, default 0.1 GB) run without
# asking; see R/approval.R.
auto_run <- auto_run_bytes()
lookup_rule <- if (auto_run > 0) {
  glue(
    "Dry-run the lookup too; if its estimate is under {auto_run / 1e9} GB, ",
    "run it right away with its estimate number, without asking the user, ",
    "and mention it in one line."
  )
} else {
  "Lookups need the user's approval like any other query."
}
small_main_rule <- if (auto_run > 0) {
  glue(
    " If the estimate of this query is also under {auto_run / 1e9} GB, ",
    "run it right away instead, and show the SQL and its cost with the ",
    "answer."
  )
} else {
  ""
}
approval_rule <- if (auto_run > 0) {
  glue(
    "Queries under {auto_run / 1e9} GB run right away; larger queries, ",
    "exports and new tables run only after the user approves them in the ",
    "chat."
  )
} else {
  "Queries only run after the user explicitly approves them in the chat."
}

# orion-mcp, started with a config that holds only the orion-dbs server.
docker <- find_docker()
key_set <- nzchar(provider$ellmer) || nzchar(provider$key())
billing_project <- Sys.getenv("BQ_BILLING_PROJECT")
check_startup(key_set, docker, billing_project = billing_project)
model_choices <- list_models(provider)
# The configured model, or another one if the service no longer offers it.
provider$model <- start_model(model_choices, provider$model)
export_dir <- Sys.getenv("ORION_EXPORT_DIR", "~/Downloads/orion-exports") |>
  path.expand()
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
# One transcript per browser session, rewritten after each reply.
save_transcripts <- Sys.getenv("ORION_SAVE_TRANSCRIPTS", "1") != "0"
transcript_dir <- file.path(export_dir, "transcripts")
mcp_config <- if (!nzchar(billing_project)) orion_entry_from_file()
from_file <- !is.null(mcp_config)
if (!from_file) {
  if (!nzchar(billing_project)) {
    message("BQ_BILLING_PROJECT is not set: only schema browsing will work.")
  }
  mcp_config <- orion_mcp_config(docker, billing_project, export_dir)
} else {
  message("Using the orion-dbs entry in ~/.config/mcptools/config.json.")
}
message("Starting the orion-mcp container ...")
orion_tools <- mcp_tools(config = write_mcp_config(mcp_config))

home_path <- \(path) str_replace(path, fixed(path.expand("~")), "~")
# Where exported files appear, as named in the system prompt.
export_folder <- export_folder_phrase(home_path(export_dir), from_file)

system_prompt <- glue(
  "You help researchers answer questions with ORION-DBs (open research ",
  "information on Google BigQuery) through the orion_* tools. Follow the ",
  "tool descriptions exactly. {approval_rule}\n\n",
  "Aim for one query that answers the question, and get there in few ",
  "steps:\n",
  "1. Read the schema only of the tables you will use. Do not list or ",
  "inspect more than you need.\n",
  "2. Identify organisations by identifier, not by name patterns. Never ",
  "type identifiers such as ROR IDs, OpenAlex IDs or DOIs from memory; ",
  "they are often wrong. If the user did not give them, find the records ",
  "with one small lookup (for example on the institution table, matching ",
  "the name) and pick the right ones. {lookup_rule} Then write one query ",
  "that answers the whole ",
  "question and filters on the ROR IDs you found (numeric IDs in results ",
  "can be missing).\n",
  "3. Dry-run it and present it once: the SQL, one sentence on what it ",
  "does, the estimate number and cost, and the choices you made that ",
  "affect the result (data source and snapshot, the institutions with ",
  "their names and ROR IDs, how open access is defined, years) as a short ",
  "list the user can correct. ",
  "Then ask whether to run it.{small_main_rule}\n",
  "4. After it has run, answer the question in the first sentence, with ",
  "the numbers, then give a short table if useful. Report what the data ",
  "shows; do not suggest causes, such as policies or events, that it does ",
  "not show.\n\n",
  "Do not calculate numbers yourself. Put shares, percentages, totals and ",
  "differences into the SQL (for example SAFE_DIVIDE, and ROLLUP for ",
  "totals), so they come back in the result. For breakdowns of a stored ",
  "result use orion_result_count, orion_result_summary or ",
  "orion_result_slice (free). Every number you report must appear in a ",
  "tool result; if one is missing, say so or get it with another ",
  "query.\n\n",
  "Do not describe each tool call as you make it. If a tool call fails, ",
  "fix the cause once; if it fails again, stop and tell the user in one ",
  "sentence what went wrong. Ask the user a question before starting only ",
  "if the request is ambiguous in a way that changes the result, and then ",
  "ask one question with a suggested default. Offer alternative ",
  "strategies (at most two, each dry-run, compared in a short table) only ",
  "when the user asks for options.\n\n",
  "Each cost estimate has a number (Estimate #1, #2, ...), given by ",
  "orion_estimate_query_cost. Never give a query a number or a cost ",
  "yourself: every query you ask the user to approve must have been ",
  "dry-run, and you show the number and cost the tool returned. Show the ",
  "number with each estimate and, if there are several, ask which to run. The ",
  "user approves with 'yes' (one estimate), with numbers ('run #2', ",
  "'run #2 and #3') or with 'run all'. The run, export and save-as-table ",
  "tools take no SQL: pass the number of the approved estimate as ",
  "`estimate`.\n\n",
  "Stored results (q1, q2, ...) live inside the orion-mcp server, a ",
  "separate R process in a Docker container. The user cannot reach them ",
  "from their own R session, so never say the data is 'in your R session' ",
  "and never write code that uses q1, q2, ... as R objects. When the user ",
  "wants to plot or analyse a result in R, export it with ",
  "orion_export_result (free) and give R code in tidyverse style that ",
  "reads the exported file, e.g. readr::read_csv(\"oa_by_year.csv\") ",
  "followed by dplyr and ggplot2, noting that the file is ",
  "{export_folder}.\n\n",
  "Write in a plain, factual style. Never use emojis or decorative ",
  "symbols (such as ✅, ❌, 🚀, ⚠️, 📊), not even in headings, lists or ",
  "tables; use words instead (e.g. 'yes'/'no', 'Note:')."
)

# A chat with the orion tools and the approval gate. `log` and `tools` are
# shared by all clients of a browser session, so estimates stay valid after
# changing the model.
new_chat <- function(model, log, tools) {
  chat <- new_client(model, provider, system_prompt)
  chat$set_tools(tools)
  chat$on_tool_request(\(request) approval_gate(request, chat, log))
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
  "you the SQL and what it will cost. Nothing runs until you say **yes**",
  if (auto_run > 0) {
    glue(", except queries under {auto_run / 1e9} GB.")
  } else {
    "."
  },
  "\n\n{str_flatten(steps, '\n')}"
)

# .noWS: no space between a link and the comma after it.
footer_link <- \(text, href) {
  tags$a(href = href, target = "_blank", text, .noWS = "outside")
}

# Help next to the model menu: how to use the chat, in the order a first
# conversation goes. Set-up details stay in the README.
help_item <- \(lead, ...) tags$li(tags$b(lead), " ", ...)
help_text <- tags$ul(
  class = "sub-help-list",
  help_item(
    "Ask.", "Write your question in plain language. The model finds the ",
    "tables and writes the SQL."
  ),
  help_item(
    "Approve.", "You see the SQL and its cost first. Reply yes to run it. ",
    "If there are several, reply with the numbers, such as run #2 and #3, ",
    "or run all. Cost checks are free; queries are billed to your Google ",
    "Cloud project.",
    if (auto_run > 0) {
      glue(
        " Queries under {auto_run / 1e9} GB, such as finding an ",
        "institution's ROR ID, run without asking."
      )
    }
  ),
  help_item(
    "Export.", "Ask for a CSV file to get the full result. Files go to ",
    tags$code(home_path(export_dir), .noWS = "after"), "."
  ),
  help_item(
    "Switch models.", "Pick another model in the menu at any time; the ",
    "conversation continues."
  ),
  help_item(
    "Save chat.", "Downloads the conversation as a Markdown file.",
    if (save_transcripts) {
      paste(
        " A copy is also saved after each reply, in the transcripts",
        "folder next to your exports."
      )
    }
  ),
  help_item(
    "Privacy.",
    glue(
      "Your messages, the SQL and short result previews go to ",
      "{provider$label}",
      if (nzchar(provider$privacy_url)) " (" else ". "
    ),
    if (nzchar(provider$privacy_url)) {
      tagList(footer_link("how they handle data", provider$privacy_url), "). ")
    },
    "Full results stay on your computer."
  )
)

ui <- tagList(
  tags$head(tags$script(src = "tables.js")),
  page_chat(
    id = "chat",
    title = app_title,
    icon = tags$img(
      src = app_logo[["src"]], alt = app_logo[["alt"]], class = "sub-logo"
    ),
    sidebar = FALSE,
    # Top bar as on sub.uni-goettingen.de: short links, then the contrast
    # toggle.
    toolbar_global = bslib::toolbar(
      tags$nav(
        class = "sub-meta-nav", `aria-label` = "Links",
        tags$a(
          href = "https://orion-dbs.community/", target = "_blank", "ORION-DBs"
        ),
        tags$a(href = provider$url, target = "_blank", provider$label),
        tags$a(
          href = "https://github.com/subugoe/orion-chat", target = "_blank",
          "GitHub"
        )
      ),
      bslib::input_dark_mode()
    ),
    # Below the message box: the model on the left; on the right, what acts
    # on this conversation (save it, explain it). The top bar holds only
    # site links.
    toolbar_input = bslib::toolbar(
      align = "left",
      bslib::toolbar_input_select(
        "model", glue("{provider$label}-hosted model"),
        choices = model_choices,
        selected = provider$model,
        show_label = TRUE
      ),
      bslib::toolbar_spacer(),
      downloadButton(
        "save_transcript", tags$span(class = "sub-chat-label", "Save chat"),
        icon = icon("download"), class = "btn-sm sub-chat-action",
        title = "Save this chat as a Markdown file",
        `aria-label` = "Save chat"
      ),
      bslib::popover(
        tags$button(
          type = "button", class = "btn btn-sm sub-chat-action",
          `aria-label` = "Help", icon("circle-question")
        ),
        title = "How this chat works",
        help_text,
        placement = "top",
        options = list(customClass = "sub-help")
      )
    ),
    footer = tags$div(
      class = "sub-contact",
      tags$p(
        "Contact: Najko Jahn, ",
        tags$a(
          href = "mailto:najko.jahn@sub.uni-goettingen.de",
          "najko.jahn@sub.uni-goettingen.de"
        )
      ),
      tags$p(
        "Built in R with ",
        footer_link("shiny", "https://shiny.posit.co/"), ", ",
        footer_link("shinychat", "https://posit-dev.github.io/shinychat/"),
        ", ",
        footer_link("ellmer", "https://ellmer.tidyverse.org/"), ", ",
        footer_link("mcptools", "https://posit-dev.github.io/mcptools/"),
        " and ",
        footer_link("bslib", "https://rstudio.github.io/bslib/"),
        ". Models by ",
        footer_link(provider$label, provider$url),
        ", data via ",
        footer_link("orion-mcp", "https://github.com/subugoe/orion-mcp"), "."
      )
    ),
    # Wider than the default (680-760px) so SQL and tables have room;
    # gau.scss narrows prose and user messages within it.
    width = "min(clamp(720px, 60vw, 880px), 100%)",
    theme = bslib::bs_bundle(
      page_chat_theme(),
      sass::sass_layer_file("gau.scss")
    )
  )
)

server <- function(input, output, session) {
  # Each browser session gets its own chat and dry-run record. The orion-mcp
  # container is shared by all sessions of this R process, and with it the
  # stored results (q1, q2, ...) and the server's dry-run cache.
  estimated <- new_estimate_log(auto_run)
  tools <- wrap_orion_tools(orion_tools, estimated)
  # The model the menu is on (the revert target when a switch fails).
  selected_model <- provider$model
  chat <- chat_server(
    "chat", new_chat(provider$model, estimated, tools),
    greeting = greeting, history = FALSE
  )

  # Transcript. chat$client is the current client; a model switch copies
  # all turns to the new client, also when it waits for a reply to end.
  started <- Sys.time()
  models_used <- provider$model
  transcript <- \() {
    render_transcript(
      chat$client$get_turns(),
      list(title = app_title, started = started, models = models_used)
    )
  }
  output$save_transcript <- downloadHandler(
    filename = \() format(Sys.time(), "orion-chat-%Y%m%d-%H%M.md"),
    content = \(file) writeLines(transcript(), file, useBytes = TRUE)
  )
  if (save_transcripts) {
    transcript_path <- file.path(
      transcript_dir,
      format(started, "orion-chat-%Y%m%d-%H%M%S.md")
    )
    observeEvent(chat$status(), ignoreInit = TRUE, {
      if (chat$status() == "idle") {
        tryCatch(
          save_transcript(transcript(), transcript_path),
          error = \(e) {
            warning(
              "Transcript not saved: ", conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    })
  }

  # Switching keeps the conversation; the new model sees the earlier turns.
  # A switch during a reply takes effect once the reply is done. Some models
  # can't call tools; the probe reverts those before the chat fails.
  observeEvent(input$model, ignoreInit = TRUE, {
    model <- input$model
    problem <- tool_choice_problem(model, provider)
    if (!is.null(problem)) {
      showNotification(problem, type = "error", duration = 10)
      # `selected` alone is ignored; the choices must come with it.
      bslib::update_toolbar_input_select(
        "model",
        choices = model_choices, selected = selected_model
      )
      return()
    }
    chat$set_client(new_chat(model, estimated, tools))
    selected_model <<- model
    if (model != models_used[length(models_used)]) {
      models_used <<- c(models_used, model)
    }
    label <- names(model_choices)[model_choices == model]
    showNotification(glue("Now using {label}."), duration = 3)
  })
}

shinyApp(ui, server, enableBookmarking = "disable")
