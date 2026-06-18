# Get variable data from an aggregate

Main function of the package. Retrieves variable results from an IBGE
aggregate for the specified localities, periods and classifications.

## Usage

``` r
ibge_variables(
  aggregate,
  variable = NULL,
  periods = -6,
  localities = "BR",
  classification = NULL,
  view = NULL,
  validate = TRUE
)
```

## Arguments

- aggregate:

  Numeric aggregate identifier (SIDRA table).

- variable:

  Variable(s) to retrieve. Can be:

  - `NULL` (default): returns all standard variables

  - Numeric vector: specific IDs, e.g. `c(284, 285)`

  - `"all"`: includes automatically generated percentage variables

- periods:

  Period(s) to query. Can be:

  - Negative integer: last N periods, e.g. `-6` (default)

  - Numeric vector: e.g. `c(201701, 201702, 201703)`

  - String with range: e.g. `"201701-201712"`

- localities:

  Locality(ies) to query. Can be:

  - `"BR"` (default): Brazil

  - Level code: `"N3"` (all states), `"N6"` (all municipalities)

  - Named list for specific localities: `list(N3 = c(33, 35))` (RJ and
    SP states), `list(N6 = c(3550308, 3304557))` (SP and RJ
    municipalities)

- classification:

  Classification(s) to filter results. Named list where names are
  classification IDs and values are category ID vectors. Use `"all"` for
  all categories. E.g. `list("226" = c(4844, 96608), "218" = 4780)`

- view:

  Display mode: `NULL` (default), `"OLAP"` or `"flat"`.

- validate:

  Logical. If `TRUE` (default), validates parameters against aggregate
  metadata before querying. Use `FALSE` to skip.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) in tidy
(long) format with columns: `variable_id`, `variable_name`,
`variable_unit`, classification columns (when present), `locality_id`,
`locality_name`, `locality_level`, `period`, `value`

## Details

Before querying the API, validates all parameters against the aggregate
metadata. If any parameter is invalid, stops with a clear error message
showing the allowed values.

## Examples

``` r
if (FALSE) { # \dontrun{
# IPCA in Brazil
ibge_variables(7060, localities = "BR")

# Specific variables for states
ibge_variables(1705, variable = c(284, 285), localities = "N3")

# Specific municipalities with classification
ibge_variables(
  aggregate      = 1712,
  variable       = 214,
  periods        = -3,
  localities     = list(N6 = c(3550308, 3304557)),
  classification = list("226" = c(4844, 96608))
)
} # }

```
