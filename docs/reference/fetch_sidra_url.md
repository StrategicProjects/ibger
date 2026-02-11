# Fetch data from a SIDRA API URL

Parses a SIDRA API URL and fetches the data using
[`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md),
returning the same tidy tibble format.

## Usage

``` r
fetch_sidra_url(url, validate = TRUE)
```

## Arguments

- url:

  Character string. A SIDRA API URL, typically starting with
  `https://apisidra.ibge.gov.br/values/`.

- validate:

  Logical. If `TRUE` (default), validates parameters against aggregate
  metadata before querying.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) in tidy
(long) format, same as
[`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md).

## Examples

``` r
if (FALSE) { # \dontrun{
url <- "https://apisidra.ibge.gov.br/values/t/7060/n1/all/v/63/p/last%2012/c315/7169"
fetch_sidra_url(url)

# Pipe-friendly: inspect then fetch
url |> parse_sidra_url()
url |> fetch_sidra_url()
} # }
```
