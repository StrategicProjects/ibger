# Get survey metadata for a specific period

Retrieves detailed institutional and methodological metadata for a
survey in a given reference period. Before querying, both the survey
code and the year/month combination are validated against the IBGE
catalog, so invalid inputs produce clear errors without wasting an API
call.

## Usage

``` r
ibge_survey_metadata(survey, year, month = NULL, order = 0)
```

## Arguments

- survey:

  Character string. The survey code (e.g. `"SC"`, `"CD"`). Must match a
  code from
  [`ibge_surveys()`](https://strategicprojects.github.io/ibger/reference/ibge_surveys.md).

- year:

  Integer. Reference year. Must be a year with available metadata for
  the survey (see
  [`ibge_survey_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_periods.md)).

- month:

  Integer or `NULL`. Reference month (1–12). Required for conjunctural
  (sub-annual) surveys; omit for structural (annual+) surveys. If
  omitted for a conjunctural survey, a warning is issued listing the
  available months.

- order:

  Integer. Publication order within the period (default `0`, the most
  recent).

## Value

A list of class `ibge_survey_metadata` with:

- status:

  Survey status (corrente, concluída, desativada, etc.)

- category:

  conjuntural or estrutural

- type:

  Type of statistical operation

- area:

  Responsible area

- acronym:

  Survey acronym

- start_date:

  Survey start date

- deactivation_date:

  Deactivation date, if applicable

- sidra_url:

  URL to SIDRA data

- concla_url:

  URL to CONCLA (CNAE classifications)

- thematic_classifications:

  Tibble of thematic classifications

- occurrences:

  List of metadata records for the period (structure varies by survey)

## Details

The structure of `occurrences` varies by survey and may include fields
such as objective, data collection method, sample design, geographic
scope, reference period, and more.

## See also

[`ibge_surveys()`](https://strategicprojects.github.io/ibger/reference/ibge_surveys.md),
[`ibge_survey_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_periods.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Structural survey (no month needed)
ibge_survey_metadata("CD", year = 2022)

# Conjunctural survey (specify month)
ibge_survey_metadata("SC", year = 2023, month = 6)

# Inspect methodology fields
meta <- ibge_survey_metadata("CD", year = 2022)
names(meta$occurrences[[1]])

# Invalid code: clear error with suggestions
ibge_survey_metadata("PMS", year = 2024)
#> Error: Survey code "PMS" not found in the IBGE catalog.

# Invalid year: error with available range
ibge_survey_metadata("CD", year = 1800)
#> Error: Year 1800 not available for survey "CD".
#> i Available years: 1940 to 2022 (9 total).
} # }
```
