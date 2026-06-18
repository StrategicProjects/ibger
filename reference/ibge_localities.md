# Localities for an aggregate

Retrieves available localities for an aggregate at one or more
geographic levels. Validates the requested level(s) against the
aggregate metadata before querying.

## Usage

``` r
ibge_localities(aggregate, level = "N6", validate = TRUE)
```

## Arguments

- aggregate:

  Numeric aggregate identifier.

- level:

  Geographic level. Use `"N1"` (Brazil), `"N2"` (region), `"N3"`
  (state), `"N6"` (municipality), `"N7"` (metropolitan area), etc. For
  multiple levels, use a vector: `c("N6", "N7")`.

- validate:

  Logical. If `TRUE` (default), validates level against aggregate
  metadata. Use `FALSE` to skip.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns: `id`, `name`, `level_id`, `level_name`

## Examples

``` r
if (FALSE) { # \dontrun{
ibge_localities(1437, level = "N1")
ibge_localities(1437, level = c("N6", "N7"))
} # }
```
