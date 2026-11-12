test_that("check_startup() stops with one plain line per problem", {
  ok <- \(args) 0L
  expect_error(check_startup(FALSE, "/bin/docker", run = ok), "API_KEY is not set")
  expect_error(check_startup(TRUE, "", run = ok), "Docker not found")
  expect_error(
    check_startup(TRUE, "/bin/docker", run = \(args) if (args[1] == "info") 1L else 0L),
    "not running"
  )
  expect_error(
    check_startup(TRUE, "/bin/docker", run = \(args) if (args[1] == "image") 1L else 0L),
    "docker pull ghcr.io/subugoe/orion-mcp:latest"
  )
  expect_true(check_startup(TRUE, "/bin/docker", run = ok))
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
  expect_true(paste0(path.expand("~/exports"), ":/data/exports") %in% args)
  expect_true(paste0(path.expand("~/.config/gcloud"), ":/root/.config/gcloud:ro") %in% args)
  expect_false(any(str_detect(args, fixed("~"))))
  expect_true("BQ_BILLING_PROJECT=my-project" %in% args)
  expect_true("SCHEMA_REFRESH=0" %in% args)
  expect_identical(args[length(args)], orion_image)

  no_billing <- orion_mcp_config("/usr/bin/docker", "", "~/exports")$mcpServers$`orion-dbs`$args
  expect_false(any(str_detect(no_billing, "BQ_BILLING_PROJECT")))
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

  jsonlite::write_json(list(mcpServers = list(other = list(command = "x"))), path, auto_unbox = TRUE)
  expect_null(orion_entry_from_file(path))
  expect_null(orion_entry_from_file(tempfile()))
})

test_that("patch_mcptools_timeout() raises the limit and is idempotent", {
  skip_if_not_installed("mcptools")
  expect_true(suppressWarnings(patch_mcptools_timeout()))
  src <- str_flatten(deparse(body(utils::getFromNamespace("mcp_transport_stdio_send", "mcptools"))), "\n")
  expect_match(src, "max_attempts <- 1500", fixed = TRUE)
  expect_true(patch_mcptools_timeout())
})

test_that("patch_mcptools_timeout() warns instead of failing on unknown code", {
  skip_if_not_installed("mcptools")
  # 1500 is in place now, so ask for a limit whose pattern cannot match.
  expect_warning(result <- patch_mcptools_timeout(attempts = 7), "Could not raise")
  expect_false(result)
})
