# Periods for an aggregate

Retrieves available periods for an aggregate.

## Usage

``` r
ibge_periods(aggregate)
```

## Arguments

- aggregate:

  Numeric aggregate identifier.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns: `id`, `literal`, `modification`

## Examples

``` r
if (FALSE) { # \dontrun{
ibge_periods(1705)
} # }
```
