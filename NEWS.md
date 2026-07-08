# ibger (development version)

* Expanded the test suite to cover all exported functions (~85% coverage):
  pure helpers and parsers are tested directly, and API interactions are
  tested against recorded fixtures with `httptest2` (no network needed).
* Added continuous integration: `R CMD check` on Linux/macOS/Windows and
  test coverage reporting via GitHub Actions.
* Added `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `codemeta.json` in
  preparation for rOpenSci submission.

# ibger 0.2.0

* `ibge_variables()` gains a `chunk` argument (default `TRUE`): queries whose
  estimated result exceeds the API's value limit are now transparently split
  into multiple smaller requests (by periods, then by localities) and
  combined into a single tibble, instead of failing with HTTP 500
  (#1, @danielvartan). Use `chunk = FALSE` to disable, or a positive number
  for a custom per-request limit. The documented API limit is 100,000 values,
  but empirically requests fail above ~50,000, so that is the default.
* Period and locality id lists fetched for chunking are cached per session
  (cleared by `ibge_clear_cache()`).
* Initial test suite (testthat) covering the chunking logic.

# ibger 0.1.0

* Initial CRAN release.
* Core functions: `ibge_aggregates()`, `ibge_metadata()`, `ibge_periods()`,
  `ibge_localities()`, `ibge_variables()`.
* Pre-flight validation against aggregate metadata for all query parameters.
* In-memory metadata cache with `ibge_clear_cache()`.
* `parse_ibge_value()` utility for converting IBGE special value codes.
* `parse_sidra_url()` and `fetch_sidra_url()` for migrating from SIDRA API
  URLs (e.g. from the Query Builder or sidrar package).
* Support for flat view (`view = "flat"`) response parsing.
* Four vignettes: getting started, API concepts, IPCA example, and a
  real-world tutorial on tracking state GDP components.
