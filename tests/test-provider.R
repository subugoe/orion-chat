# A getter that reads from a named vector instead of the environment.
fake_env <- function(...) {
  values <- c(...)
  \(name) if (name %in% names(values)) values[[name]] else ""
}

test_that("provider_settings() defaults to GWDG", {
  p <- provider_settings(fake_env(GWDG_API_KEY = "gwdg-key"))
  expect_identical(p$base_url, "https://chat-ai.academiccloud.de/v1")
  expect_identical(p$label, "GWDG")
  expect_match(p$url, "docs.hpc.gwdg.de", fixed = TRUE)
  expect_identical(p$ellmer, "")
  expect_true(nzchar(p$model))
  expect_type(p$key, "closure")
  expect_identical(p$key(), "gwdg-key")
})

test_that("each setting overrides its default", {
  p <- provider_settings(fake_env(
    ORION_CHAT_BASE_URL = "https://llm.example.org/v1",
    ORION_CHAT_API_KEY = "own-key",
    ORION_CHAT_MODEL = "some-model",
    ORION_CHAT_PROVIDER = "Example",
    ORION_CHAT_PROVIDER_URL = "https://example.org/llm",
    ORION_CHAT_ELLMER = "anthropic"
  ))
  expect_identical(p$base_url, "https://llm.example.org/v1")
  expect_identical(p$key(), "own-key")
  expect_identical(p$model, "some-model")
  expect_identical(p$label, "Example")
  expect_identical(p$url, "https://example.org/llm")
  expect_identical(p$ellmer, "anthropic")
})

test_that("ORION_CHAT_API_KEY wins over GWDG_API_KEY, which is the fallback", {
  both <- provider_settings(
    fake_env(ORION_CHAT_API_KEY = "own", GWDG_API_KEY = "gwdg")
  )
  expect_identical(both$key(), "own")
  empty <- provider_settings(
    fake_env(ORION_CHAT_API_KEY = "", GWDG_API_KEY = "gwdg")
  )
  expect_identical(empty$key(), "gwdg")
  none <- provider_settings(fake_env())
  expect_identical(none$key(), "")
})

test_that("the key is not stored in the settings", {
  p <- provider_settings(fake_env(GWDG_API_KEY = "secret-value"))
  others <- unlist(p[names(p) != "key"])
  expect_false(any(str_detect(others, fixed("secret-value"))))
})

test_that("new_client() builds an OpenAI-compatible chat by default", {
  p <- provider_settings(fake_env(GWDG_API_KEY = "dummy"))
  chat <- new_client("some-model", p, system_prompt = "Be brief.")
  provider <- chat$get_provider()
  expect_true(S7::S7_inherits(provider, ellmer:::ProviderOpenAICompatible))
  expect_identical(provider@name, "GWDG")
  expect_identical(provider@base_url, "https://chat-ai.academiccloud.de/v1")
  expect_identical(chat$get_model(), "some-model")
  expect_identical(chat$get_system_prompt(), "Be brief.")
})

test_that("new_client() builds an Anthropic chat via ORION_CHAT_ELLMER", {
  withr::local_envvar(ANTHROPIC_API_KEY = "dummy")
  p <- provider_settings(fake_env(ORION_CHAT_ELLMER = "anthropic"))
  chat <- new_client("claude-sonnet-5-5", p, system_prompt = "Be brief.")
  expect_true(S7::S7_inherits(chat$get_provider(), ellmer:::ProviderAnthropic))
  expect_identical(chat$get_model(), "claude-sonnet-5-5")
})

test_that("model_choices_from() keeps ready models only", {
  models <- list(
    list(id = "b-model", name = "b model", status = "ready"),
    list(id = "a-model", status = "ready"),
    list(id = "down", name = "Down", status = "offline"),
    list(id = "plain")
  )
  # A configured model the service does not list is not added.
  choices <- model_choices_from(models, "configured")
  expect_identical(unname(choices), c("a-model", "b-model", "plain"))
  expect_identical(names(choices), c("a-model", "b model", "plain"))
  expect_false("down" %in% choices)
  # A listed one stays, once.
  expect_identical(model_choices_from(models, "b-model"), choices)
  expect_length(model_choices_from(list(list(id = "x")), "x"), 1)
  # No usable list: the configured model, as when the list cannot be fetched.
  expect_identical(model_choices_from(list(), "x"), c(x = "x"))
  offline <- list(list(id = "down", status = "offline"))
  expect_identical(model_choices_from(offline, "x"), c(x = "x"))
})

test_that("start_model() keeps the configured model if it is offered", {
  choices <- c("B" = "b-model", "DeepSeek" = "deepseek-v4.1-flash-1017")
  expect_silent(
    model <- start_model(choices, "b-model")
  )
  expect_identical(model, "b-model")
  # The list could not be fetched, or the ellmer route: it is always there.
  expect_silent(start_model(c(m1 = "m1"), "m1"))
})

test_that("start_model() replaces a retired model and says so", {
  retired <- "deepseek-v4.1-flash-1017"
  # The first preferred model on offer, in the order of preferred_models.
  choices <- model_choices_from(
    list(
      list(id = "another-model", name = "A model"),
      list(id = "qwen3.5-397b-a17b", name = "Qwen 3.5"),
      list(id = "glm-5.3-flash", name = "GLM 5.3 Flash")
    ),
    retired
  )
  expect_false(retired %in% choices)
  expect_message(
    model <- start_model(choices, retired),
    "ORION_CHAT_MODEL"
  )
  expect_identical(model, "glm-5.3-flash")
  expect_message(start_model(choices, retired), retired, fixed = TRUE)
  expect_message(start_model(choices, retired), "glm-5.3-flash", fixed = TRUE)
  # No preferred model on offer: the first in the menu.
  others <- model_choices_from(
    list(
      list(id = "z-model", name = "Zeta"),
      list(id = "m-model", name = "Mu")
    ),
    retired
  )
  expect_message(model <- start_model(others, retired), "ORION_CHAT_MODEL")
  expect_identical(model, "m-model")
  expect_identical(preferred_models[[1]], provider_settings(fake_env())$model)
})

test_that("tool_choice_problem() is skipped on the ellmer route", {
  p <- provider_settings(fake_env(ORION_CHAT_ELLMER = "anthropic"))
  expect_null(tool_choice_problem("any", p))
})

test_that("list_models() offers only the configured model without a lister", {
  p <- provider_settings(
    fake_env(ORION_CHAT_ELLMER = "nosuchprovider", ORION_CHAT_MODEL = "m1")
  )
  expect_identical(list_models(p), c(m1 = "m1"))
})

test_that("model_choices_from() keeps only models with text in and out", {
  models <- list(
    list(
      id = "chat",
      input = list("text", "image"), output = list("text", "thought")
    ),
    list(id = "embed", input = list("text"), output = list("embedding")),
    list(id = "speech", input = list("audio"), output = list("text")),
    list(id = "image-gen", input = list("text"), output = list("image"))
  )
  expect_identical(unname(model_choices_from(models, "chat")), "chat")
})

test_that("the default model is the one the README names", {
  p <- provider_settings(fake_env())
  readme <- readLines("../README.md")
  expect_true(any(str_detect(readme, fixed(p$model))))
})

test_that("the privacy link defaults to GWDG's page only for GWDG", {
  privacy_url <- \(...) provider_settings(fake_env(...))$privacy_url
  expect_match(privacy_url(), "chat-ai/data-privacy", fixed = TRUE)
  expect_identical(privacy_url(ORION_CHAT_PROVIDER = "Example"), "")
  expect_identical(
    privacy_url(
      ORION_CHAT_PROVIDER = "Example",
      ORION_CHAT_PRIVACY_URL = "https://e.org/p"
    ),
    "https://e.org/p"
  )
})
