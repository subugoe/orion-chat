# The model service. GWDG by default; .Renviron settings point the app at
# another OpenAI-compatible service or at a provider ellmer supports.
# Tested in tests/test-provider.R.

# The settings, read once. `key` is a function, so the key is read only
# when a request needs it and never stored in the list.
provider_settings <- function(get = Sys.getenv) {
  setting <- \(name, default = "") {
    value <- get(name)
    if (nzchar(value)) value else default
  }
  list(
    base_url = setting("ORION_CHAT_BASE_URL", "https://chat-ai.academiccloud.de/v1"),
    key = \() setting("ORION_CHAT_API_KEY", get("GWDG_API_KEY")),
    model = setting("ORION_CHAT_MODEL", "glm-5.3-flash"),
    label = setting("ORION_CHAT_PROVIDER", "GWDG"),
    url = setting(
      "ORION_CHAT_PROVIDER_URL",
      "https://docs.hpc.gwdg.de/services/ai-services/index.html"
    ),
    # How the service handles data; linked in the footer if known.
    privacy_url = setting(
      "ORION_CHAT_PRIVACY_URL",
      if (nzchar(get("ORION_CHAT_PROVIDER"))) "" else
        "https://docs.hpc.gwdg.de/services/ai-services/chat-ai/data-privacy/index.html"
    ),
    ellmer = setting("ORION_CHAT_ELLMER")
  )
}

# The only place that builds a chat client. On the ellmer route the key
# comes from that provider's own variable (e.g. ANTHROPIC_API_KEY).
new_client <- function(model, provider = provider_settings(), system_prompt = NULL) {
  if (nzchar(provider$ellmer)) {
    return(ellmer::chat(paste0(provider$ellmer, "/", model), system_prompt = system_prompt))
  }
  ellmer::chat_openai_compatible(
    base_url = provider$base_url,
    name = provider$label,
    model = model,
    credentials = provider$key,
    system_prompt = system_prompt
  )
}

# Menu choices c("Display name" = "id") from the `data` of a /models reply:
# chat models that are ready and take and give text, plus the configured
# model, sorted by name. GWDG reports `status`, `input` and `output`; a
# service that does not is not filtered on them.
model_choices_from <- function(models, configured) {
  has <- \(field, value) is.null(field) || value %in% unlist(field)
  models <- keep(models, \(m) {
    is.character(m$id) &&
      (m$status %||% "ready") == "ready" &&
      has(m$input, "text") && has(m$output, "text")
  })
  ids <- c(map_chr(models, "id"), configured)
  labels <- c(map_chr(models, \(m) m$name %||% m$id), configured)
  choices <- set_names(ids, labels)[!duplicated(ids)]
  choices[order(names(choices))]
}

# The models the service offers now. If the list cannot be fetched, only
# the configured model.
list_models <- function(provider = provider_settings()) {
  fallback <- \(e) {
    warning("Could not list ", provider$label, " models: ", conditionMessage(e), call. = FALSE)
    set_names(provider$model, provider$model)
  }
  if (nzchar(provider$ellmer)) {
    lister <- paste0("models_", provider$ellmer)
    if (!lister %in% getNamespaceExports("ellmer")) {
      return(set_names(provider$model, provider$model))
    }
    return(tryCatch(
      {
        ids <- getExportedValue("ellmer", lister)()$id
        choices <- unique(c(ids, provider$model))
        set_names(choices, choices)
      },
      error = fallback
    ))
  }
  tryCatch(
    httr2::request(provider$base_url) |>
      httr2::req_url_path_append("models") |>
      httr2::req_auth_bearer_token(provider$key()) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      pluck("data") |>
      model_choices_from(provider$model),
    error = fallback
  )
}

# A tiny probe request: can this model take tools? Returns NULL if it can,
# or if the probe fails for another reason (rate limits, a service without
# this error), otherwise a short message for the user. Skipped on the
# ellmer route.
tool_choice_problem <- function(model, provider = provider_settings()) {
  if (nzchar(provider$ellmer)) return(NULL)
  res <- tryCatch(
    httr2::request(provider$base_url) |>
      httr2::req_url_path_append("chat/completions") |>
      httr2::req_auth_bearer_token(provider$key()) |>
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
