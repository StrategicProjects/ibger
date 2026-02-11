# Package index

## Retrieve data

Main functions to query the IBGE API.

- [`ibge_variables()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_variables.md)
  : Get variable data from an aggregate
- [`ibge_aggregates()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_aggregates.md)
  : List IBGE aggregates

## Explore metadata

Discover available aggregates, variables, periods, and localities.

- [`ibge_metadata()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_metadata.md)
  : Metadata for an aggregate
- [`ibge_periods()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_periods.md)
  : Periods for an aggregate
- [`ibge_localities()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_localities.md)
  : Localities for an aggregate

## Utilities

Helpers and cache management.

- [`parse_ibge_value()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/parse_ibge_value.md)
  : Parse IBGE value column
- [`ibge_clear_cache()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/ibge_clear_cache.md)
  : Clear metadata cache
- [`fetch_sidra_url()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/fetch_sidra_url.md)
  : Fetch data from a SIDRA API URL
- [`parse_sidra_url()`](https://monitoramento.sepe.pe.gov.br/ibger/reference/parse_sidra_url.md)
  : Parse a SIDRA API URL into ibger parameters
