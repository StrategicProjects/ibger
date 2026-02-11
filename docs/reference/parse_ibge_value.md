# Parse IBGE value column

Converts the character `value` column returned by
[`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md)
to numeric, handling IBGE special value conventions.

## Usage

``` r
parse_ibge_value(x)
```

## Arguments

- x:

  A character or numeric vector of IBGE values.

## Value

A numeric vector where:

- `"-"` becomes `0`

- `".."`, `"..."` and `"X"` become `NA_real_`

- All other values are converted with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html)

## Details

According to IBGE's tabular presentation standards, the value column may
contain special character codes instead of numbers:

|       |                                                        |
|-------|--------------------------------------------------------|
| Code  | Meaning                                                |
| `-`   | Numeric value equal to zero (not from rounding)        |
| `..`  | Not applicable                                         |
| `...` | Data not available                                     |
| `X`   | Suppressed to avoid identifying individual respondents |

## Examples

``` r
parse_ibge_value(c("1.5", "10", "-", "..", "...", "X", NA))
#> [1]  1.5 10.0  0.0   NA   NA   NA   NA
#> [1] 1.5  10.0  0.0   NA    NA    NA    NA

if (FALSE) { # \dontrun{
# Typical usage after ibge_variables()
library(dplyr)

ibge_variables(7060, localities = "BR") |>
  mutate(value = parse_ibge_value(value))
} # }
```
