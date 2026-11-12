# The conversation as a readable Markdown transcript. Pure functions apart
# from save_transcript(); tested in tests/test-transcript.R.

transcript_result_limit <- 2000

# A fenced code block whose fence is longer than any run of backticks in
# the text.
fence <- function(text, lang = "") {
  runs <- str_extract_all(text, "`+")[[1]]
  ticks <- strrep("`", max(3, nchar(runs) + 1))
  paste0(ticks, lang, "\n", text, "\n", ticks)
}

result_text <- function(value) {
  if (is.character(value)) return(str_flatten(value, "\n"))
  tryCatch(
    as.character(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE)),
    error = \(e) str_flatten(utils::capture.output(print(value)), "\n")
  )
}

format_argument <- function(name, value) {
  text <- if (is.character(value) && length(value) == 1) value else result_text(value)
  text <- str_squish(text)
  if (nchar(text) > 200) text <- paste0(str_sub(text, 1, 200), " ...")
  glue("- {name}: {text}")
}

format_request <- function(request) {
  args <- request@arguments
  sql <- args$query
  args$query <- NULL
  str_flatten(c(
    glue("**Tool call:** `{request@name}`"),
    if (is.character(sql)) fence(str_trim(str_flatten(sql, "\n")), "sql"),
    if (length(args) > 0) {
      str_flatten(imap_chr(args, \(value, name) format_argument(name, value)), "\n")
    }
  ), "\n\n")
}

format_result <- function(result, limit = transcript_result_limit) {
  name <- if (S7::S7_inherits(result@request)) result@request@name else "unknown tool"
  error <- result@error
  if (!is.null(error)) {
    text <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error)
    label <- if (inherits(error, "ellmer_tool_reject") || str_starts(text, fixed("Tool call rejected."))) {
      "Rejected by the approval gate"
    } else {
      "Tool error"
    }
  } else {
    text <- result_text(result@value)
    label <- "Tool result"
  }
  text <- str_flatten(text, "\n")
  note <- NULL
  if (nchar(text) > limit) {
    note <- glue(
      "(Cut at {format(limit, big.mark = ',')} of ",
      "{format(nchar(text), big.mark = ',')} characters.)"
    )
    text <- str_sub(text, 1, limit)
  }
  str_flatten(c(glue("**{label}** (`{name}`):"), fence(text), note), "\n\n")
}

format_content <- function(content) {
  if (S7::S7_inherits(content, ellmer::ContentToolRequest)) return(format_request(content))
  if (S7::S7_inherits(content, ellmer::ContentToolResult)) return(format_result(content))
  # Reasoning is not shown in the chat either.
  if (S7::S7_inherits(content, ellmer::ContentThinking)) return(NULL)
  if (S7::S7_inherits(content, ellmer::ContentText)) {
    text <- str_trim(content@text)
    return(if (nzchar(text)) text)
  }
  glue("_({sub('^.*::', '', class(content)[[1]])} not shown.)_")
}

# One Markdown string. `meta` is list(title, started, models): the session
# start as POSIXct and the models used, in order.
render_transcript <- function(turns, meta) {
  out <- c(
    glue("# {meta$title}"),
    glue("Started: {format(meta$started, '%Y-%m-%d %H:%M')}  \nModels: {str_flatten(meta$models, ', ')}")
  )
  heading <- ""
  for (turn in turns) {
    if (turn@role == "system") next
    contents <- turn@contents
    is_result <- map_lgl(contents, \(c) S7::S7_inherits(c, ellmer::ContentToolResult))
    # Tool results come back as user turns; print them under the assistant
    # turn that asked for them.
    out <- c(out, compact(map(contents[is_result], format_content)))
    blocks <- unlist(compact(map(contents[!is_result], format_content)))
    if (length(blocks) == 0) next
    role <- if (turn@role == "user") "You" else "Assistant"
    # Several assistant turns in one reply (text, tool calls, more text)
    # share one heading.
    if (role != heading) out <- c(out, glue("## {role}"))
    heading <- role
    out <- c(out, blocks)
  }
  paste0(str_flatten(out, "\n\n"), "\n")
}

# Write via a temporary file in the same folder, so a reader never sees a
# half-written transcript.
save_transcript <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".orion-chat-", tmpdir = dirname(path), fileext = ".md")
  writeLines(text, tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not write ", path, call. = FALSE)
  }
  invisible(path)
}
