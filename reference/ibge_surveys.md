# List IBGE surveys with metadata

Retrieves the full catalog of IBGE surveys (statistical operations) that
have associated metadata in the IBGE Metadata API, including status,
category, collection and publication frequency, and thematic
classifications.

## Usage

``` r
ibge_surveys(thematic_classifications = TRUE)
```

## Arguments

- thematic_classifications:

  Logical. If `TRUE` (default), includes a list-column
  `thematic_classifications` with the classification details for each
  survey. Set to `FALSE` for a simpler flat tibble.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- id:

  Survey code (e.g. `"SC"`, `"CD"`)

- name:

  Survey name in Portuguese

- name_en:

  Survey name in English (may be `NA`)

- status:

  Survey status: Ativa, Encerrada, etc.

- category:

  Conjuntural, Estrutural, or Especial

- collection_frequency:

  Data collection frequency (Mensal, Anual, etc.)

- publication_frequency:

  Publication frequency

- thematic_classifications:

  List-column of tibbles with thematic classification details (name,
  domain, description). Only if `thematic_classifications = TRUE`.

## Details

This function queries a **different API** from the other `ibge_*`
functions. While
[`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md)
and
[`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
use the Aggregates API (v3), this function uses the Metadata API (v2),
which provides institutional and methodological documentation about
surveys.

## See also

[`ibge_survey_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_periods.md),
[`ibge_survey_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Full catalog
ibge_surveys()

# Flat table without classifications
ibge_surveys(thematic_classifications = FALSE)

# Filter active conjunctural surveys
library(dplyr)
ibge_surveys(thematic_classifications = FALSE) |>
  filter(status == "Ativa", category == "Conjuntural")
} # }
```
