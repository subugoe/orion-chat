test_that("check_startup() stops with one plain line per problem", {
  ok <- \(args) 0L
  fails_on <- \(command) \(args) if (args[1] == command) 1L else 0L
  expect_error(check_startup(FALSE, "/bin/docker", run = ok), "No API key")
  expect_error(check_startup(TRUE, "", run = ok), "Docker not found")
  expect_error(
    check_startup(TRUE, "/bin/docker", run = fails_on("info")),
    "not running"
  )
  expect_error(
    check_startup(TRUE, "/bin/docker", run = fails_on("image")),
    "docker pull ghcr.io/subugoe/orion-mcp:latest"
  )
  expect_true(check_startup(TRUE, "/bin/docker", run = ok))
})

test_that("check_startup() needs a Google Cloud login to bill queries", {
  ok <- \(args) 0L
  check <- \(billing_project, credentials) {
    check_startup(
      TRUE, "/bin/docker",
      run = ok, billing_project = billing_project, credentials = credentials
    )
  }
  missing <- file.path(tempfile(), "application_default_credentials.json")
  expect_error(
    check("my-project", missing),
    "No Google Cloud login found. Run: gcloud auth application-default login",
    fixed = TRUE
  )
  # Docker creates a folder when the file is missing; that is no login.
  dir.create(missing, recursive = TRUE)
  expect_error(check("my-project", missing), "No Google Cloud login found")
  login <- tempfile(fileext = ".json")
  file.create(login)
  expect_true(check("my-project", login))
  # Without a billing project the file is not mounted and not needed.
  expect_true(check("", file.path(tempfile(), "none.json")))
})

test_that("find_docker() takes the first binary that exists", {
  real <- tempfile()
  file.create(real)
  expect_identical(find_docker(c("", "/no/such/docker", real)), real)
  expect_identical(find_docker(c("", "/no/such/docker")), "")
})

test_that("orion_mcp_config() holds only orion-dbs, with expanded paths", {
  config <- orion_mcp_config("/usr/bin/docker", "my-project", "~/exports")
  expect_named(config$mcpServers, "orion-dbs")
  args <- config$mcpServers$`orion-dbs`$args
  expect_identical(config$mcpServers$`orion-dbs`$command, "/usr/bin/docker")
  home <- path.expand("~")
  expect_true(paste0(home, "/exports:/data/exports") %in% args)
  # Only the login file is mounted, read-only, not the gcloud folder.
  login <- "/.config/gcloud/application_default_credentials.json"
  expect_true(paste0(home, login, ":/root", login, ":ro") %in% args)
  expect_false(any(str_detect(args, "gcloud:")))
  expect_identical(sum(args == "-v"), 2L)
  expect_false(any(str_detect(args, fixed("~"))))
  expect_true("BQ_BILLING_PROJECT=my-project" %in% args)
  expect_true("SCHEMA_REFRESH=0" %in% args)
  expect_identical(args[length(args)], orion_image)

  no_billing <- orion_mcp_config("/usr/bin/docker", "", "~/exports") |>
    pluck("mcpServers", "orion-dbs", "args")
  expect_false(any(str_detect(no_billing, "BQ_BILLING_PROJECT")))
  # No billing project: no credentials in the container.
  expect_false(any(str_detect(no_billing, "gcloud")))
  expect_identical(sum(no_billing == "-v"), 1L)

  custom <- orion_mcp_config(
    "/usr/bin/docker", "p", "/tmp/x",
    credentials = "/tmp/login.json"
  )
  expect_true(
    paste0(
      "/tmp/login.json:",
      "/root/.config/gcloud/application_default_credentials.json:ro"
    ) %in% custom$mcpServers$`orion-dbs`$args
  )
})

test_that("write_mcp_config() writes JSON that mcptools can read", {
  path <- write_mcp_config(orion_mcp_config("/usr/bin/docker", "p", "/tmp/x"))
  read <- jsonlite::fromJSON(path)
  expect_identical(read$mcpServers$`orion-dbs`$command, "/usr/bin/docker")
  expect_type(read$mcpServers$`orion-dbs`$args, "character")
})

test_that("orion_entry_from_file() keeps only the orion-dbs entry", {
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(list(mcpServers = list(
    other = list(command = "x"),
    `orion-dbs` = list(command = "docker", args = list("run", "img"))
  )), path, auto_unbox = TRUE)
  entry <- orion_entry_from_file(path)
  expect_named(entry$mcpServers, "orion-dbs")
  expect_identical(entry$mcpServers$`orion-dbs`$args, list("run", "img"))

  jsonlite::write_json(
    list(mcpServers = list(other = list(command = "x"))),
    path,
    auto_unbox = TRUE
  )
  expect_null(orion_entry_from_file(path))
  expect_null(orion_entry_from_file(tempfile()))
})

test_that("patch_mcptools_timeout() raises the limit and is idempotent", {
  skip_if_not_installed("mcptools")
  expect_true(suppressWarnings(patch_mcptools_timeout()))
  send <- utils::getFromNamespace("mcp_transport_stdio_send", "mcptools")
  src <- str_flatten(deparse(body(send)), "\n")
  expect_match(src, "max_attempts <- 1500", fixed = TRUE)
  expect_true(patch_mcptools_timeout())
})

test_that("patch_mcptools_timeout() warns instead of failing on unknown code", {
  skip_if_not_installed("mcptools")
  # 1500 is in place now, so ask for a limit whose pattern cannot match.
  expect_warning(
    result <- patch_mcptools_timeout(attempts = 7),
    "Could not raise"
  )
  expect_false(result)
})

test_that("export_folder_phrase() names the folder in use", {
  expect_identical(
    as.character(export_folder_phrase("~/data/orion")),
    "in ~/data/orion"
  )
  # The orion-dbs entry from the mcptools config mounts a folder the app
  # does not know.
  from_file <- export_folder_phrase("~/Downloads/orion-exports", from_file = TRUE)
  expect_match(from_file, "orion-dbs entry of ~/.config/mcptools/config.json",
               fixed = TRUE)
  expect_no_match(from_file, "Downloads", fixed = TRUE)
})
