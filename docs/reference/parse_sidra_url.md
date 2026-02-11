# Parse a SIDRA API URL into ibger parameters

Converts a SIDRA API URL (from the SIDRA Query Builder or sidrar
package) into a human-readable breakdown of its parameters, enriched
with names from the aggregate metadata.

## Usage

``` r
parse_sidra_url(url)
```

## Arguments

- url:

  Character string. A SIDRA API URL, typically starting with
  `https://apisidra.ibge.gov.br/values/`.

## Value

A list of class `sidra_query` with:

- `aggregate`: list with `id` and `name`

- `variables`: tibble with `id` and `name`

- `periods`: character vector of period codes

- `localities`: list of level/locality pairs

- `classifications`: list of classification/category details

- `ibger_call`: string with the equivalent
  [`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md)
  call

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
parse_sidra_url(url)
} # }
```
