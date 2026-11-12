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
  decision <- gate_decision(
    request@name, sql_key(request@arguments$query), log, n,
    parse_approval(latest_user_text(turns))
  )
  if (!decision$allow) tool_reject(decision$message)
  if (request@name %in% paid_tools) record_approval(log, n)
}

# Model service: GWDG unless .Renviron says otherwise (see R/provider.R)
source("R/provider.R")
provider <- provider_settings()

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
  "Each cost estimate has a number (Estimate #1, #2, ...). Show the number ",
  "with each estimate and, if there are several, ask which number to run.\n\n",
  "Stored results (q1, q2, ...) live inside the orion-mcp server, a ",
  "separate R process in a Docker container. The user cannot reach them ",
  "from their own R session, so never say the data is 'in your R session' ",
  "and never write code that uses q1, q2, ... as R objects. When the user ",
  "wants to plot or analyse a result in R, export it with ",
  "orion_export_result (free) and give R code that reads the exported ",
  "file, e.g. read.csv(\"oa_by_year.csv\"), noting that the file is in the ",
  "app's export folder (by default ~/Downloads/orion-exports).\n\n",
  "Write in a plain, factual style. Never use emojis or decorative ",
  "symbols (such as ✅, ❌, 🚀, ⚠️, 📊), not even in headings, lists or ",
  "tables; use words instead (e.g. 'yes'/'no', 'Note:')."
)

# orion-mcp, started with a config that holds only the orion-dbs server.
docker <- find_docker()
key_set <- nzchar(provider$ellmer) || nzchar(provider$key())
check_startup(key_set, docker)
model_choices <- list_models(provider)
export_dir <- Sys.getenv("ORION_EXPORT_DIR", "~/Downloads/orion-exports") |>
  path.expand()
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
# One transcript per browser session, rewritten after each reply.
save_transcripts <- Sys.getenv("ORION_SAVE_TRANSCRIPTS", "1") != "0"
transcript_dir <- file.path(export_dir, "transcripts")
billing_project <- Sys.getenv("BQ_BILLING_PROJECT")
mcp_config <- if (!nzchar(billing_project)) orion_entry_from_file()
if (is.null(mcp_config)) {
  if (!nzchar(billing_project)) {
    message("BQ_BILLING_PROJECT is not set: only schema browsing will work.")
  }
  mcp_config <- orion_mcp_config(docker, billing_project, export_dir)
} else {
  message("Using the orion-dbs entry in ~/.config/mcptools/config.json.")
}
message("Starting the orion-mcp container ...")
orion_tools <- mcp_tools(config = write_mcp_config(mcp_config))

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
  "you the SQL and what it will cost. Nothing runs until you say **yes**.",
  "\n\n{str_flatten(steps, '\n')}"
)

# .noWS: no space between a link and the comma after it.
footer_link <- \(text, href) {
  tags$a(href = href, target = "_blank", text, .noWS = "outside")
}

# Help next to the model menu: how to use the chat, in the order a first
# conversation goes. Set-up details stay in the README.
home_path <- \(path) str_replace(path, fixed(path.expand("~")), "~")
help_item <- \(lead, ...) tags$li(tags$b(lead), " ", ...)
help_text <- tags$ul(
  class = "sub-help-list",
  help_item(
    "Ask.", "Write your question in plain language. The model finds the ",
    "tables and writes the SQL."
  ),
  help_item(
    "Approve.", "You see the SQL and its cost first. Reply yes to run it, ",
    "or run #2 if there are several. Cost checks are free; queries are ",
    "billed to your Google Cloud project."
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
  estimated <- new_estimate_log()
  tools <- number_estimates(orion_tools, estimated)
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
      updateSelectInput(session, "model", selected = selected_model)
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
