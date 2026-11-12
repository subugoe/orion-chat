# Start-up: checks, the orion-mcp server config, the mcptools timeout.
# Tested in tests/test-setup.R.

orion_image <- "ghcr.io/subugoe/orion-mcp:latest"

# mcptools (1.0.3, and the development version on 2026-11-12) waits at most
# 20 x 0.2 s for each reply from a stdio MCP server, including replies to
# tool calls. BigQuery queries and exports often take longer, so raise the
# limit to 1500 x 0.2 s. This does not depend on how fast orion-mcp starts
# (with SCHEMA_REFRESH=0 it answers in under 2 s); the patch can go once
# mcptools has a timeout setting for stdio servers. If mcptools changes the
# function, warn and continue with its own limit.
patch_mcptools_timeout <- function(attempts = 1500) {
  version <- as.character(utils::packageVersion("mcptools"))
  failed <- \(why) {
    warning(
      "Could not raise the mcptools reply timeout (mcptools ", version, ": ",
      why, "). Tool calls that take longer than 4 s will fail.",
      call. = FALSE
    )
    invisible(FALSE)
  }
  tryCatch(
    {
      f <- utils::getFromNamespace("mcp_transport_stdio_send", "mcptools")
      src <- str_flatten(deparse(body(f)), "\n")
      if (str_detect(src, fixed(paste("max_attempts <-", attempts)))) {
        return(invisible(TRUE))
      }
      if (!str_detect(src, fixed("max_attempts <- 20"))) {
        return(failed("the expected code was not found"))
      }
      body(f) <- str2lang(str_replace(
        src, fixed("max_attempts <- 20"), paste("max_attempts <-", attempts)
      ))
      utils::assignInNamespace("mcp_transport_stdio_send", f, ns = "mcptools")
      invisible(TRUE)
    },
    error = \(e) failed(conditionMessage(e))
  )
}

# The docker binary: on the PATH, or where Docker Desktop puts it on macOS.
# "" if there is none.
find_docker <- function(candidates = c(Sys.which("docker"), "~/.docker/bin/docker")) {
  candidates <- path.expand(candidates[nzchar(candidates)])
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) "" else unname(found[[1]])
}

# Stop with a one-line message if something the app needs is missing.
# `run` runs a docker command and returns its exit status.
check_startup <- function(key_set, docker, image = orion_image,
                          run = \(args) system2(docker, args, stdout = FALSE, stderr = FALSE)) {
  stop_plain <- \(...) stop(paste0(...), call. = FALSE)
  if (!key_set) {
    stop_plain(
      "No API key for the model service. Add ORION_CHAT_API_KEY=... ",
      "(or GWDG_API_KEY=...) to ~/.Renviron and restart R."
    )
  }
  if (docker == "") {
    stop_plain("Docker not found. Install Docker Desktop (or Docker Engine) and start it.")
  }
  if (run("info") != 0) {
    stop_plain("Docker is not running. Start Docker Desktop and try again.")
  }
  if (run(c("image", "inspect", image)) != 0) {
    stop_plain("The orion-mcp image is missing. Run: docker pull ", image)
  }
  invisible(TRUE)
}

# The mcptools config for the orion-dbs server only, as a list.
# SCHEMA_REFRESH=0 makes orion-mcp use the schemas built into the image
# instead of downloading them at start-up; `docker pull` updates them.
orion_mcp_config <- function(docker, billing_project, export_dir,
                             gcloud_dir = "~/.config/gcloud", image = orion_image) {
  args <- c(
    "run", "--rm", "-i",
    "-v", paste0(path.expand(gcloud_dir), ":/root/.config/gcloud:ro"),
    "-v", paste0(path.expand(export_dir), ":/data/exports"),
    "-e", "SCHEMA_REFRESH=0"
  )
  if (nzchar(billing_project)) {
    args <- c(args, "-e", paste0("BQ_BILLING_PROJECT=", billing_project))
  }
  list(mcpServers = list(`orion-dbs` = list(
    command = docker,
    args = c(args, image)
  )))
}

# An existing mcptools config file (the route used before 2026-11), reduced
# to its orion-dbs entry so that no other server is attached. NULL if the
# file has no such entry.
orion_entry_from_file <- function(path = "~/.config/mcptools/config.json") {
  if (!file.exists(path)) return(NULL)
  config <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = \(e) NULL
  )
  entry <- config$mcpServers$`orion-dbs`
  if (is.null(entry)) return(NULL)
  list(mcpServers = list(`orion-dbs` = entry))
}

write_mcp_config <- function(config, path = tempfile("orion-mcp-", fileext = ".json")) {
  jsonlite::write_json(config, path, auto_unbox = TRUE, pretty = TRUE)
  path
}
