# orion-chat

Ask questions about [ORION-DBs](https://orion-dbs.community/) in plain language, 
with a language model that runs at [GWDG](https://gwdg.de/) instead of at a commercial AI provider.

Retrieve data from a growing collection of open research information, including OpenAlex, Crossref and OpenAIRE. 

Built-in safeguards check every query, show its cost in advance, and run nothing without approval.

![The app answering a question about who provides OpenAlex data in ORION-DBs](home.png)

## Setup

You need Docker Desktop, the [orion-mcp](https://github.com/subugoe/orion-mcp) image with Google Cloud credentials (see its README), and a [GWDG API key](https://docs.hpc.gwdg.de/services/ai-services/saia/).

```r
install.packages(c("shiny", "shinychat", "ellmer", "mcptools", "bslib", "sass", "glue", "purrr", "stringr"))
usethis::edit_r_environ()  # add GWDG_API_KEY=your-key, then restart R
```

Then copy the `orion-dbs` entry from your Claude Desktop config (or the orion-mcp README) into `~/.config/mcptools/config.json`, using the full path to `docker`.

## Usage

```r
shiny::runApp("orion-chat")
```

Try "Which datasets are available?" or, if you are unsure how to approach a question, ask the app to compare options. Estimating costs is free; running queries is billed to your Google Cloud project.

## License

MIT

## Contact

Najko Jahn <mailto:najko.jahn@sub.uni-goettingen.de>
