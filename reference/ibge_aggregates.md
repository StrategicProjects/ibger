# List IBGE aggregates

Retrieves the set of available aggregates (tables), grouped by survey.
Each aggregate corresponds to a SIDRA table. Results are cached in
memory per unique combination of parameters, so repeated calls with the
same filters are instant.

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

  Period of interest, as a periodicity code followed by one or more
  period ids in brackets: `"P5[202001]"` (January 2020, monthly
  aggregates), `"P1[2019,2020]"` (2019 and 2020, annual aggregates).

- subject:

  Numeric subject code (e.g. `70` for animal slaughter). Use
  [`ibge_subjects()`](https://strategicprojects.github.io/ibger/reference/ibge_subjects.md)
  to look up codes.

- classification:

  Numeric classification code (e.g. `12026`).

- periodicity:

  Periodicity code (see Details): `"P1"` (annual), `"P5"` (monthly),
  `"P8"` (semi-annual), `"P9"` (quarterly), `"P13"` (rolling quarter),
  etc.

- level:

  Geographic level: `"N1"` (Brazil), `"N2"` (region), `"N3"` (state),
  `"N6"` (municipality), etc.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns: `survey_id`, `survey_name`, `aggregate_id`, `aggregate_name`

## Details

All filters are optional. Their format is checked before the request:
the IBGE API silently ignores filters it cannot parse (returning the
whole catalog) or answers HTTP 500, so malformed values are rejected
here with an informative error. A well-formed filter that matches no
aggregate returns an empty tibble with a warning.

Periodicity codes used by the API (as observed in the catalog):

|       |                                 |
|-------|---------------------------------|
| Code  | Periodicity                     |
| `P1`  | Annual                          |
| `P5`  | Monthly                         |
| `P7`  | Every three years               |
| `P8`  | Semi-annual                     |
| `P9`  | Quarterly                       |
| `P11` | Every two years                 |
| `P13` | Rolling quarter (PNAD Contínua) |
| `P16` | Every six years                 |

## Examples

``` r
if (FALSE) { # interactive()
ibge_aggregates()
ibge_aggregates(periodicity = "P5")
ibge_aggregates(level = "N6")
ibge_aggregates(subject = 70)
ibge_aggregates(period = "P5[202001]")
}
```
