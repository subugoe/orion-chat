# orion-chat

Shiny app for asking questions about [ORION-DBs](https://orion-dbs.community/) in plain language using [GWDG AI services](https://docs.hpc.gwdg.de/services/ai-services/index.html).

ORION-DBs is a growing collection of open research information on Google BigQuery, including OpenAlex, Crossref and OpenAIRE. The language model writes the SQL queries, which then run on BigQuery.
Safeguards check every query, show its cost in advance, and run nothing without approval.

![The app showing a dry-run SQL query on the open access status of articles by the University of Göttingen and UMG, 2020–2025, with its estimated cost, and asking for approval before running it](home.png)

## Setup

The app needs Docker Desktop, the [orion-mcp](https://github.com/subugoe/orion-mcp) image with Google Cloud credentials (see its README), and a [GWDG API key](https://docs.hpc.gwdg.de/services/ai-services/saia/).

Tested on macOS only. The packages and their minimum versions are listed in `DESCRIPTION`:

```r
install.packages("pak")
pak::local_install_deps()  # run in the orion-chat folder
file.edit("~/.Renviron")
```

Add these lines to `~/.Renviron`, then restart R:

```
GWDG_API_KEY=...
BQ_BILLING_PROJECT=your-google-cloud-project-id
# Optional, the folder for exports; this is the default:
ORION_EXPORT_DIR=~/Downloads/orion-exports
```

The app starts the orion-mcp container itself. It mounts your Google Cloud credentials from `~/.config/gcloud` (read-only) and the export folder, which it creates if needed. Without `BQ_BILLING_PROJECT` the app still starts, but only schema browsing works. The container uses the table schemas built into the image; `docker pull ghcr.io/subugoe/orion-mcp:latest` updates the image and the schemas.

If `BQ_BILLING_PROJECT` is not set and `~/.config/mcptools/config.json` has an `orion-dbs` entry (the set-up described in earlier versions of this README), the app uses that entry and ignores the other servers in the file.

## Usage

Start Docker Desktop, and run in R:

```r
shiny::runApp()
```

The app is meant to run locally for one person. All browser sessions of the app share one orion-mcp container and its stored results, so it should not be deployed to a shared server in this form.

The model menu below the message box lists the models GWDG currently offers. Switching keeps the conversation, so you can ask the same question of another model. The app starts with GLM 5.3 Flash (`glm-5.3-flash`); `default_model` in `app.R` changes this. Not every GWDG model handles tool calls well; Qwen 3.5 (`qwen3.5-397b-a17b`) handled the query workflow most reliably in earlier tests. Estimating costs is free; running queries is billed to the Google Cloud project.

## AI assistance

Code and documentation were written with the help of [Claude Code](https://claude.com/claude-code) (Anthropic). The app itself does not use Claude.

## License

MIT. The logos in `www/` (SUB Göttingen, University of Göttingen, GWDG) are not covered by the MIT licence. To use the app elsewhere, replace them and change `app_title` and `app_logo` at the top of `app.R`.

## Contact

Najko Jahn <najko.jahn@sub.uni-goettingen.de>
