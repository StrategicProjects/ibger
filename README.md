# ibger <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

Tidyverse-friendly interface to the [IBGE Aggregate Data
API](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)
(version 3), which powers [SIDRA](https://sidra.ibge.gov.br/), the
automatic data retrieval system for all surveys and censuses conducted by
IBGE (Brazilian Institute of Geography and Statistics).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("StrategicProjects/ibger")
```

> **Note**: if you encounter the error `curl_modify_url is not an exported object`,
> update the curl package with `install.packages("curl")`. Version 6.0.0 or
> higher is required.

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
ibge_localities(7060, level = "N7")

# Get data — the main function
ibge_variables(7060, localities = "BR")
```

## Automatic validation

`ibge_variables()` and `ibge_localities()` validate parameters against the
aggregate metadata **before** querying. Invalid parameters stop execution
with a clear error showing the allowed values:

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
  localities     = list(N6 = c(3550308, 3304557)),
  classification = list("315" = c(7169, 7170, 7445))
)
```

## Value column and IBGE conventions

In accordance with official IBGE data standards, the `value` column returned by the API may be
of type `character` rather than numeric.

This occurs because IBGE uses special symbols to represent specific data conditions, which are
part of the statistical dissemination standard and should not be treated as errors.

The possible values are:

| Value | Meaning                                                        |
|-------|----------------------------------------------------------------|
| `-`   | Numeric zero (not resulting from rounding)                     |
| `..`  | Not applicable                                                 |
| `...` | Data not available                                             |
| `X`   | Suppressed to avoid identifying individual respondents         |

As a consequence, users should not assume that the `value` column is always numeric.
When numerical analysis is required, these symbols must be handled explicitly before coercion.

## Design

- **Tidyverse native**: all functions return tidy tibbles
- **snake\_case**: function and column names
- **Natural interface**: classifications as named lists, flexible localities
- **Validation**: checks metadata before querying, stops with clear errors
- **Caching**: metadata cached in memory to avoid duplicate calls
- **Feedback**: clear cli messages at each step
- **Robust**: automatic retry via httr2

## Functions

| Function             | Description                            |
|----------------------|----------------------------------------|
| `ibge_aggregates()`  | List aggregates grouped by survey      |
| `ibge_metadata()`    | Full metadata for an aggregate         |
| `ibge_periods()`     | Available periods                      |
| `ibge_localities()`  | Localities by level (with validation)  |
| `ibge_variables()`   | Get data (with validation)             |
| `parse_sidra_url()`  | Parse SIDRA URL into ibger parameters  |
| `fetch_sidra_url()`  | Fetch data directly from a SIDRA URL   |
| `parse_ibge_value()` | Convert value column to numeric        |
| `ibge_clear_cache()` | Clear metadata cache                   |

## Learn more

- `vignette("getting-started")` — full walkthrough
- `vignette("api-concepts")` — understanding the IBGE API data model
- `vignette("ipca-example")` — real-world IPCA inflation analysis
- `vignette("tutorial")` — tracking state GDP components with IBGE data

## Comparison with other packages

Several R packages provide access to IBGE data. Here is how ibger differs:

| Feature | ibger | sidrar | PNADcIBGE / SIPDIBGE |
|---------|-------|--------|----------------------|
| Data source | IBGE Aggregates API (SIDRA tables) | IBGE Aggregates API (SIDRA tables) | IBGE microdata (FTP/download) |
| Output | Tibbles (tidy, long format) | data.frames | survey design objects (`survey`) |
| Parameter format | Named R lists | Strings with SIDRA codes | File paths / year + quarter |
| Validation | Pre-flight check against metadata | None (errors come from the API) | — |
| Metadata browsing | `ibge_metadata()`, `ibge_periods()`, `ibge_localities()` | — | — |
| Special values | `parse_ibge_value()` | Manual handling | — |
| Caching | In-memory metadata cache | None | Local file cache |
| Feedback | cli progress messages | None | None |
| Retry on failure | Automatic (via httr2) | None | — |

### vs sidrar

[sidrar](https://github.com/rpradosiqueira/sidrar/) is the closest
alternative — it wraps the same IBGE Aggregates API. The key differences:

- **Parameter ergonomics**: sidrar requires a single string with raw API
  codes (`"/t/1705/n3/all/v/284/p/last%206"`), while ibger uses natural
  R objects (`ibge_variables(1705, variable = 284, localities = "N3")`).
- **Pre-flight validation**: ibger checks your parameters (levels, periods,
  variables, classifications) against the aggregate metadata *before*
  hitting the API. Invalid requests fail fast with clear error messages
  showing the allowed values, instead of returning an opaque API error.
- **Metadata exploration**: ibger exposes `ibge_metadata()`,
  `ibge_periods()`, and `ibge_localities()` so you can discover what an
  aggregate offers without leaving R.
- **Modern stack**: ibger uses httr2 (automatic retry, structured error
  handling), cli (progress messages), and returns tibbles by default.

### vs PNADcIBGE and SIPDIBGE

These packages serve a fundamentally different purpose. They download and
process **microdata** (individual survey responses) from household surveys
like PNAD Contínua, POF, and MUNIC, returning `survey` design objects
suitable for complex survey analysis with the `survey` package.

ibger works with **aggregate data** — the pre-tabulated summary tables
published through SIDRA. If you need individual-level records and proper
survey weights, use PNADcIBGE or SIPDIBGE. If you need ready-made
indicators, time series, and cross-tabulations (such as IPCA, GDP, census
totals, or agricultural production by municipality), use ibger.

## Disclaimer

This package is an independent, open-source project and is **not** affiliated
with, endorsed by, or officially connected to the Instituto Brasileiro de
Geografia e Estatística (IBGE) in any way.

All data retrieved through this package is sourced from the
[IBGE Aggregates API](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)
and remains the intellectual property of IBGE. Users must comply with IBGE's
[terms of use](https://www.ibge.gov.br/acesso-informacao/institucional/termos-de-uso.html)
when using, publishing, or redistributing the data.

The data is provided **as-is**, without warranty of any kind. The package
authors are not responsible for the accuracy, completeness, or timeliness of
the data returned by the API. For official statistics and methodology, always
refer to [ibge.gov.br](https://www.ibge.gov.br).

API availability, rate limits, and response formats are controlled by IBGE
and may change without notice.
