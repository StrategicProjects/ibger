# ibger (development version)

## Changes from the rOpenSci review (ropensci/software-review#787)

* `curl (>= 6.0.0)` is now declared in `Imports`, enforcing at install time
  the version requirement that was previously only documented in the README
  (older curl versions fail with `curl_modify_url is not an exported object`).
* The table of IBGE special value codes (`-`, `..`, `...`, `X`) is now
  documented in a single place — `?parse_ibge_value` — and linked from the
  README and vignettes instead of being repeated.
* The `ipca-example` and `tutorial` vignettes are now precompiled from
  `.Rmd.orig` sources against the live IBGE API (`vignettes/precompile.R`),
  so their output and ggplot2 figures render on the pkgdown site.
* README: added the rOpenSci review badge, linked the API-concepts vignette
  from the quick start, pointed the "Value column" section to
  `parse_ibge_value()`, and rewrote the `ibge_explorer()` section to match
  the tone of the rest of the page.
* Examples no longer use `\dontrun{}`: examples that query the live IBGE
  API (or launch the Shiny explorer) are now guarded with
  `@examplesIf interactive()`, so they render in the docs and run for
  interactive users without hitting the network during checks.
* Internal refactor to address the remaining pkgcheck/goodpractice notes:
  every function is now below the cyclomatic-complexity threshold of 15
  (long functions such as `parse_sidra_url()`, `fetch_sidra_url()`,
  `ibge_metadata()` and `ibge_survey_metadata()` were decomposed into
  focused helpers), duplicated `@param` docs were replaced with
  `@inheritParams`, and the lintr issues flagged in the review (long
  lines, `expect_equal()` vs `expect_identical()`, static regexes without
  `fixed = TRUE`, duplicate `cli` bullet names, `<<-`, `require()` in the
  Shiny app, `setwd()` in `precompile.R`) were cleaned up. No user-facing
  behavior changes.

## Other changes

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
