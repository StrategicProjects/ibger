# Changelog

## ibger 0.1.0

CRAN release: 2026-02-20

- Initial CRAN release.
- Core functions:
  [`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md),
  [`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md),
  [`ibge_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_periods.md),
  [`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md),
  [`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md).
- Pre-flight validation against aggregate metadata for all query
  parameters.
- In-memory metadata cache with
  [`ibge_clear_cache()`](https://strategicprojects.github.io/ibger/reference/ibge_clear_cache.md).
- [`parse_ibge_value()`](https://strategicprojects.github.io/ibger/reference/parse_ibge_value.md)
  utility for converting IBGE special value codes.
- [`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md)
  and
  [`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md)
  for migrating from SIDRA API URLs (e.g. from the Query Builder or
  sidrar package).
- Support for flat view (`view = "flat"`) response parsing.
- Four vignettes: getting started, API concepts, IPCA example, and a
  real-world tutorial on tracking state GDP components.
