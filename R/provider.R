# The model service. GWDG by default; .Renviron settings point the app at
# another OpenAI-compatible service or at a provider ellmer supports.
# Tested in tests/test-provider.R.

gwdg_privacy_url <- paste0(
  "https://docs.hpc.gwdg.de/services/ai-services/chat-ai/",
  "data-privacy/index.html"
)

# The settings, read once. `key` is a function, so the key is read only
# when a request needs it and never stored in the list.
provider_settings <- function(get = Sys.getenv) {
  setting <- \(name, default = "") {
    value <- get(name)
    if (nzchar(value)) value else default
  }
  list(
    base_url = setting(
      "ORION_CHAT_BASE_URL", "https://chat-ai.academiccloud.de/v1"
    ),
    key = \() setting("ORION_CHAT_API_KEY", get("GWDG_API_KEY")),
    # Fastest of the GWDG models tested on the query workflow (2026-11-12).
    model = setting("ORION_CHAT_MODEL", "deepseek-v4.1-flash-1017"),
    label = setting("ORION_CHAT_PROVIDER", "GWDG"),
    url = setting(
      "ORION_CHAT_PROVIDER_URL",
      "https://docs.hpc.gwdg.de/services/ai-services/index.html"
    ),
    # How the service handles data; linked in the footer if known.
    privacy_url = setting(
      "ORION_CHAT_PRIVACY_URL",
      if (!nzchar(get("ORION_CHAT_PROVIDER"))) gwdg_privacy_url else ""
    ),
    ellmer = setting("ORION_CHAT_ELLMER")
  )
}

# The only place that builds a chat client. On the ellmer route the key
# comes from that provider's own variable (e.g. ANTHROPIC_API_KEY).
new_client <- function(model,
                       provider = provider_settings(),
                       system_prompt = NULL) {
  if (nzchar(provider$ellmer)) {
    return(ellmer::chat(
      paste0(provider$ellmer, "/", model),
      system_prompt = system_prompt
    ))
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
# chat models that are ready and take and give text, sorted by name. GWDG
# reports `status`, `input` and `output`; a service that does not is not
# filtered on them. The configured model is not added: if the service does
# not list it, it is out of service (see start_model()). Only if no model
# is left is the configured one offered, as when the list cannot be fetched.
model_choices_from <- function(models, configured) {
  has <- \(field, value) is.null(field) || value %in% unlist(field)
  models <- keep(models, \(m) {
    is.character(m$id) &&
      (m$status %||% "ready") == "ready" &&
      has(m$input, "text") && has(m$output, "text")
  })
  if (length(models) == 0) {
    return(set_names(configured, configured))
  }
  ids <- map_chr(models, "id")
  labels <- map_chr(models, \(m) m$name %||% m$id)
  choices <- set_names(ids, labels)[!duplicated(ids)]
  choices[order(names(choices))]
}

# Models to start with when the configured one is not offered, in this
# order: the GWDG models that handled the query workflow in tests.
preferred_models <- c(
  "deepseek-v4.1-flash-1017", "glm-5.3-flash", "qwen3.5-397b-a17b"
)

# The model to start with: the configured one if it is in the menu,
# otherwise the first preferred model that is, otherwise the first in the
# menu. Says so on the console, since the default id carries a date and
# will go out of service at some point.
start_model <- function(choices, configured, preferred = preferred_models) {
  if (configured %in% choices || length(choices) == 0) {
    return(configured)
  }
  model <- c(intersect(preferred, choices), unname(choices))[[1]]
  message(
    "The model '", configured, "' is not offered at the moment; starting ",
    "with '", model, "'. Set ORION_CHAT_MODEL in ~/.Renviron to choose ",
    "another."
  )
  model
}

# The models the service offers now. If the list cannot be fetched, only
# the configured model; on the ellmer route the configured model is always
# in the list.
list_models <- function(provider = provider_settings()) {
  fallback <- \(e) {
    warning(
      "Could not list ", provider$label, " models: ", conditionMessage(e),
      call. = FALSE
    )
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
  if (nzchar(provider$ellmer)) {
    return(NULL)
  }
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
  if (inherits(res, "httr2_response")) {
    return(NULL)
  }
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
