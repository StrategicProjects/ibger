# Changelog

## ibger (development version)

- Expanded the test suite to cover all exported functions (~85%
  coverage): pure helpers and parsers are tested directly, and API
  interactions are tested against recorded fixtures with `httptest2` (no
  network needed).
- Added continuous integration: `R CMD check` on Linux/macOS/Windows and
  test coverage reporting via GitHub Actions.
- Added `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `codemeta.json` in
  preparation for rOpenSci submission.

## ibger 0.2.0

- [`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
  gains a `chunk` argument (default `TRUE`): queries whose estimated
  result exceeds the API’s value limit are now transparently split into
  multiple smaller requests (by periods, then by localities) and
  combined into a single tibble, instead of failing with HTTP 500
  ([\#1](https://github.com/StrategicProjects/ibger/issues/1),
  [@danielvartan](https://github.com/danielvartan)). Use `chunk = FALSE`
  to disable, or a positive number for a custom per-request limit. The
  documented API limit is 100,000 values, but empirically requests fail
  above ~50,000, so that is the default.
- Period and locality id lists fetched for chunking are cached per
  session (cleared by
  [`ibge_clear_cache()`](https://strategicprojects.github.io/ibger/reference/ibge_clear_cache.md)).
- Initial test suite (testthat) covering the chunking logic.

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
