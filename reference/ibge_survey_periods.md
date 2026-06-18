# List periods with metadata for a survey

Retrieves the periods (year/month combinations) for which a given survey
has associated metadata records. Before querying, the survey code is
validated against the IBGE catalog; invalid codes produce a helpful
error with suggestions.

## Usage

``` r
ibge_survey_periods(survey)
```

## Arguments

- survey:

  Character string. The survey code as returned by
  [`ibge_surveys()`](https://strategicprojects.github.io/ibger/reference/ibge_surveys.md)
  (e.g. `"SC"` for Pesquisa Mensal de Serviços, `"CD"` for Censo
  Demográfico). Invalid codes are caught before the request is sent,
  with suggestions for similar codes.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- year:

  Integer year with metadata available.

- month:

  Integer month (1–12) or `NA` for structural (annual or longer)
  surveys.

- order:

  Publication order within the period (0 = most recent).

## See also

[`ibge_surveys()`](https://strategicprojects.github.io/ibger/reference/ibge_surveys.md),
[`ibge_survey_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Conjunctural survey (monthly periods)
ibge_survey_periods("SC")   # Pesquisa Mensal de Serviços

# Structural survey (annual periods)
ibge_survey_periods("CD")   # Censo Demográfico

# Invalid code: helpful error with suggestions
ibge_survey_periods("PMS")
#> Error: Survey code "PMS" not found in the IBGE catalog.
#> i Did you mean one of these?
#>   * SC - Pesquisa Mensal de Serviços
#>   ...
} # }
```
