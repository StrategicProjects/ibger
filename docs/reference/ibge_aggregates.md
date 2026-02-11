# List IBGE aggregates

Retrieves the set of available aggregates (tables), grouped by survey.
Each aggregate corresponds to a SIDRA table.

## Usage

``` r
ibge_aggregates(
  period = NULL,
  subject = NULL,
  classification = NULL,
  periodicity = NULL,
  level = NULL
)
```

## Arguments

- period:

  Period of interest, e.g. `"P5[202001]"` (monthly), `"P10[20201]"`
  (quarterly).

- subject:

  Numeric subject code (e.g. `70` for animal slaughter).

- classification:

  Numeric classification code.

- periodicity:

  Periodicity code: `"P5"` (monthly), `"P10"` (quarterly), `"P13"`
  (annual), etc.

- level:

  Geographic level: `"N1"` (Brazil), `"N2"` (region), `"N3"` (state),
  `"N6"` (municipality), etc.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns: `survey_id`, `survey_name`, `aggregate_id`, `aggregate_name`

## Examples

``` r
if (FALSE) { # \dontrun{
ibge_aggregates()
ibge_aggregates(periodicity = "P5")
ibge_aggregates(level = "N6")
} # }
```
