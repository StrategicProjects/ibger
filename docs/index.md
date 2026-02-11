# ibger

Tidyverse-friendly interface to the [IBGE Aggregate Data
API](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)
(version 3), which powers [SIDRA](https://sidra.ibge.gov.br/), the
automatic data retrieval system for all surveys and censuses conducted
by IBGE (Brazilian Institute of Geography and Statistics).

## Installation

``` r

# install.packages("remotes")
remotes::install_github("StrategicProjects/ibger")
```

> **Note**: if you encounter the error
> `curl_modify_url is not an exported object`, update the curl package
> with `install.packages("curl")`. Version 6.0.0 or higher is required.

## Quick start

``` r

library(ibger)

# Browse available aggregates
ibge_aggregates()

# Inspect an aggregate (full metadata)
meta <- ibge_metadata(7060)
meta
meta$variables
meta$classifications

# Available periods and localities
ibge_periods(7060)
ibge_localities(7060, level = "N6")

# Get data — the main function
ibge_variables(7060, localities = "BR")
```

## Automatic validation

[`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md)
and
[`ibge_localities()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_localities.md)
validate parameters against the aggregate metadata **before** querying.
Invalid parameters stop execution with a clear error showing the allowed
values:

``` r

ibge_variables(7060, localities = "N3")
#> Error:
#> ! Geographic level(s) "N3" not available for aggregate 7060.
#> ℹ Available levels: "N1", "N6", and "N7".
```

## Examples

``` r

# Monthly IPCA in Brazil, last 12 months
ibge_variables(7060, variable = 63, periods = -12, localities = "BR")

# IPCA by product group for specific municipalities
ibge_variables(
  aggregate      = 7060,
  variable       = 63,
  periods        = -6,
  localities     = list(N6 = c(1200401, 2800308)),
  classification = list("315" = c(7169, 7170, 7445))
)
```

## Value column and IBGE conventions

In accordance with official IBGE data standards, the `value` column
returned by the API may be of type `character` rather than numeric.

This occurs because IBGE uses special symbols to represent specific data
conditions, which are part of the statistical dissemination standard and
should not be treated as errors.

The possible values are:

| Value | Meaning                                                |
|-------|--------------------------------------------------------|
| `-`   | Numeric zero (not resulting from rounding)             |
| `..`  | Not applicable                                         |
| `...` | Data not available                                     |
| `X`   | Suppressed to avoid identifying individual respondents |

As a consequence, users should not assume that the `value` column is
always numeric. When numerical analysis is required, these symbols must
be handled explicitly before coercion.

## Design

- **Tidyverse native**: all functions return tidy tibbles
- **snake_case**: function and column names
- **Natural interface**: classifications as named lists, flexible
  localities
- **Validation**: checks metadata before querying, stops with clear
  errors
- **Caching**: metadata cached in memory to avoid duplicate calls
- **Feedback**: clear cli messages at each step
- **Robust**: automatic retry via httr2

## Functions

### Aggregates API (data retrieval)

| Function | Description |
|----|----|
| [`ibge_aggregates()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_aggregates.md) | List aggregates grouped by survey |
| [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md) | Full metadata for an aggregate |
| [`ibge_periods()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_periods.md) | Available periods |
| [`ibge_localities()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_localities.md) | Localities by level (with validation) |
| [`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md) | Get data (with validation) |
| [`ibge_subjects()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_subjects.md) | Look up IBGE subject (theme) codes |

### Survey catalog (Metadata API)

| Function | Description |
|----|----|
| [`ibge_surveys()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_surveys.md) | List all IBGE surveys with status and category |
| [`ibge_survey_periods()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_survey_periods.md) | Available periods for a survey’s metadata |
| [`ibge_survey_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_survey_metadata.md) | Institutional/methodological metadata for a survey |

### Utilities

| Function | Description |
|----|----|
| [`parse_sidra_url()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/parse_sidra_url.md) | Parse SIDRA URL into ibger parameters |
| [`fetch_sidra_url()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/fetch_sidra_url.md) | Fetch data directly from a SIDRA URL |
| [`parse_ibge_value()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/parse_ibge_value.md) | Convert value column to numeric |
| [`ibge_clear_cache()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_clear_cache.md) | Clear metadata cache |

## Learn more

- [`vignette("getting-started")`](https://monitoramento.sepe.pe.gov.br/ibger/articles/getting-started.md)
  — full walkthrough
- [`vignette("api-concepts")`](https://monitoramento.sepe.pe.gov.br/ibger/articles/api-concepts.md)
  — understanding the IBGE API data model
- [`vignette("ipca-example")`](https://monitoramento.sepe.pe.gov.br/ibger/articles/ipca-example.md)
  — real-world IPCA inflation analysis
- [`vignette("tutorial")`](https://monitoramento.sepe.pe.gov.br/ibger/articles/tutorial.md)
  — tracking state GDP components with IBGE data

## Interactive Aggregate Explorer

The function
[`ibge_explorer()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_explorer.md)
launches an interactive Shiny application that allows you to browse,
filter, and export the full catalog of IBGE aggregates available via the
API.

It is designed to make exploration easier before calling functions such
as
[`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md),
[`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md),
or
[`ibge_aggregates()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_aggregates.md).

### ✨ Features

- 📊 Summary value boxes with total counts  
- 🔍 Global and column-level table filtering  
- 🧭 Filter by survey  
- 📥 CSV download of filtered results  
- 🆔 Click a row to display the corresponding
  [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md)
  call

------------------------------------------------------------------------

### 🚀 Usage

``` r

# Open in RStudio Viewer (default behavior)
ibge_explorer()

# Open in your system browser
ibge_explorer(launch.browser = TRUE)
```

------------------------------------------------------------------------

### 📦 Dependencies

The explorer requires the following packages:

- `shiny`
- `DT`
- `bslib`
- `bsicons`

If any of them are not installed,
[`ibge_explorer()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_explorer.md)
will display a friendly CLI error message.

------------------------------------------------------------------------

### 💡 When to Use It?

Use
[`ibge_explorer()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_explorer.md)
when you:

- Don’t remember an aggregate ID  
- Want to search by survey name  
- Need a quick way to copy the correct
  [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md)
  call  
- Prefer a visual workflow before writing code

## Comparison with other packages

Several R packages provide access to IBGE data. Here is how ibger
differs:

| Feature | ibger | sidrar | PNADcIBGE / SIPDIBGE |
|----|----|----|----|
| Data source | IBGE Aggregates API (SIDRA tables) | IBGE Aggregates API (SIDRA tables) | IBGE microdata (FTP/download) |
| Output | Tibbles (tidy, long format) | data.frames | survey design objects (`survey`) |
| Parameter format | Named R lists | Strings with SIDRA codes | File paths / year + quarter |
| Validation | Pre-flight check against metadata | None (errors come from the API) | — |
| Metadata browsing | [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md), [`ibge_periods()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_periods.md), [`ibge_localities()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_localities.md) | — | — |
| Survey catalog | [`ibge_surveys()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_surveys.md), [`ibge_survey_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_survey_metadata.md) | — | — |
| Special values | [`parse_ibge_value()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/parse_ibge_value.md) | Manual handling | — |
| Caching | In-memory metadata cache | None | Local file cache |
| Feedback | cli progress messages | None | None |
| Retry on failure | Automatic (via httr2) | None | — |

### vs sidrar

[sidrar](https://github.com/rpradosiqueira/sidrar/) is the closest
alternative — it wraps the same IBGE Aggregates API. The key
differences:

- **Parameter ergonomics**: sidrar requires a single string with raw API
  codes (`"/t/1705/n3/all/v/284/p/last%206"`), while ibger uses natural
  R objects (`ibge_variables(1705, variable = 284, localities = "N3")`).
- **Pre-flight validation**: ibger checks your parameters (levels,
  periods, variables, classifications) against the aggregate metadata
  *before* hitting the API. Invalid requests fail fast with clear error
  messages showing the allowed values, instead of returning an opaque
  API error.
- **Metadata exploration**: ibger exposes
  [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md),
  [`ibge_periods()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_periods.md),
  and
  [`ibge_localities()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_localities.md)
  so you can discover what an aggregate offers without leaving R.
- **Modern stack**: ibger uses httr2 (automatic retry, structured error
  handling), cli (progress messages), and returns tibbles by default.

### vs PNADcIBGE and SIPDIBGE

These packages serve a fundamentally different purpose. They download
and process **microdata** (individual survey responses) from household
surveys like PNAD Contínua, POF, and MUNIC, returning `survey` design
objects suitable for complex survey analysis with the `survey` package.

ibger works with **aggregate data** — the pre-tabulated summary tables
published through SIDRA. If you need individual-level records and proper
survey weights, use PNADcIBGE or SIPDIBGE. If you need ready-made
indicators, time series, and cross-tabulations (such as IPCA, GDP,
census totals, or agricultural production by municipality), use ibger.

## Disclaimer

This package is an independent, open-source project and is **not**
affiliated with, endorsed by, or officially connected to the Instituto
Brasileiro de Geografia e Estatística (IBGE) in any way.

All data retrieved through this package is sourced from the [IBGE
Aggregates
API](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3) and
the [IBGE Metadata
API](https://servicodados.ibge.gov.br/api/docs/metadados?versao=2) and
remains the intellectual property of IBGE. Users must comply with IBGE’s
[terms of
use](https://www.ibge.gov.br/acesso-informacao/institucional/termos-de-uso.html)
when using, publishing, or redistributing the data.

The data is provided **as-is**, without warranty of any kind. The
package authors are not responsible for the accuracy, completeness, or
timeliness of the data returned by the API. For official statistics and
methodology, always refer to [ibge.gov.br](https://www.ibge.gov.br).

API availability, rate limits, and response formats are controlled by
IBGE and may change without notice.
