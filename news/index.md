# Changelog

## ibger (development version)

### Changes from the rOpenSci review (ropensci/software-review#787)

#### Second review ([@ddiannae](https://github.com/ddiannae))

- [`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md)
  /
  [`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md):
  a SIDRA URL without a `/p/` (periods) segment no longer crashes the
  print method and the fetch; the equivalent call falls back to the
  [`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
  default (last 6 periods) (reported by
  [@ddiannae](https://github.com/ddiannae)).
- [`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md):
  a territorial level that is not in the internal lookup table
  (e.g. `n12`) no longer errors with “subscript out of bounds”. Levels
  that the aggregate does not offer now raise a warning at parse time
  listing the available levels, and
  [`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md)
  fails through the regular validation (reported by
  [@ddiannae](https://github.com/ddiannae)).
- [`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md):
  URLs with several territorial levels (e.g. `n1/all/n3/all`) produced
  an “Equivalent ibger call” with an unnamed list that
  [`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
  rejects. The printed call and
  [`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md)
  now share one translation: a named list when every level has specific
  codes, and the API’s pipe syntax (`"N1|N3[33,35]"`) otherwise
  (reported by [@ddiannae](https://github.com/ddiannae)).
- [`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md)
  now checks the format of its five filters before the request. The API
  silently drops filters it cannot parse (returning the whole catalog
  under a success message) and answers HTTP 500 to others,
  e.g. `periodicity = 30`; malformed values now abort with an example of
  the expected format. An empty result is reported with a warning
  instead of a green “0 aggregates found”, and keeps the documented
  columns (reported by [@ddiannae](https://github.com/ddiannae)).
- [`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md):
  the periodicity codes in the documentation were wrong (`P10` and `P58`
  do not exist; `P13` is the rolling quarter, not annual). The
  documented table now lists the codes observed in the catalog: `P1`
  annual, `P5` monthly, `P8` semi-annual, `P9` quarterly, `P13` rolling
  quarter, plus the multi-year codes (reported by
  [@ddiannae](https://github.com/ddiannae)).
- [`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md)
  with several levels queries each level separately and binds the
  results: the API’s own multi-level endpoint returns an empty list as
  soon as one of the levels has no localities for the aggregate. An
  empty result now keeps the documented columns and is reported with a
  warning (reported by [@ddiannae](https://github.com/ddiannae)).
- The “exceeds the 100,000 value limit” hint on HTTP 500 is now only
  shown for data requests
  ([`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md));
  for the other endpoints, where a 500 means the API could not interpret
  the request (non-existent aggregate, invalid filter), the message says
  so (reported by [@ddiannae](https://github.com/ddiannae)).
- Vignettes: `getting-started` and `api-concepts` are now precomputed
  against the live API like the other two (`vignettes/precompile.R`), so
  every printed output is real. This removed a set of wrong hand-written
  outputs — aggregate 1705 (IPCA-15) was described as the quarterly
  animal slaughter table, which is 1092; the
  `ibge_variables(1705, localities = "N3")` example (also in
  [`?ibge_variables`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md))
  errors because 1705 has no state level; `periodicity = "P10"` returned
  nothing — and replaced the advice to split large queries by hand with
  a section on the automatic chunking (`chunk`) in all three vignettes
  that mentioned the limit (reported by
  [@ddiannae](https://github.com/ddiannae); precomputing suggested by
  [@beatrizmilz](https://github.com/beatrizmilz)).
- [`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md)
  example: `ibge_localities(1437, level = c("N6", "N7"))` returned an
  empty tibble (1437 has no metropolitan-area localities); the example
  now uses `c("N2", "N3")` (reported by
  [@ddiannae](https://github.com/ddiannae)).

#### First review ([@allanvc](https://github.com/allanvc))

- Fixed the `bsicons` availability check in
  [`ibge_explorer()`](https://strategicprojects.github.io/ibger/reference/ibge_explorer.md),
  which told users to install `bslib` instead of `bsicons` (reported by
  [@allanvc](https://github.com/allanvc)).

- Fixed the documentation of the `launch.browser` argument of
  [`ibge_explorer()`](https://strategicprojects.github.io/ibger/reference/ibge_explorer.md),
  which described `FALSE` as the default; the default is and remains
  `TRUE` (open in the browser), and `FALSE` opens the app in the RStudio
  Viewer pane (reported by [@allanvc](https://github.com/allanvc)).

- Documented the IBGE API’s server-side latency for large queries in
  [`?ibge_variables`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
  and the README: municipality-level requests can take one to several
  minutes each (suggested by [@allanvc](https://github.com/allanvc)).

- README: replaced the link to IBGE’s terms of use, whose old address
  now returns a 404, with the current “Termo de Uso e Política de
  Privacidade” page (reported by
  [@allanvc](https://github.com/allanvc)).

- Removed the stray `.Rhistory` file from the repository (suggested by
  [@allanvc](https://github.com/allanvc)).

- `curl (>= 6.0.0)` is now declared in `Imports`, enforcing at install
  time the version requirement that was previously only documented in
  the README (older curl versions fail with
  `curl_modify_url is not an exported object`).

- The table of IBGE special value codes (`-`, `..`, `...`, `X`) is now
  documented in a single place —
  [`?parse_ibge_value`](https://strategicprojects.github.io/ibger/reference/parse_ibge_value.md)
  — and linked from the README and vignettes instead of being repeated.

- The `ipca-example` and `tutorial` vignettes are now precompiled from
  `.Rmd.orig` sources against the live IBGE API
  (`vignettes/precompile.R`), so their output and ggplot2 figures render
  on the pkgdown site.

- README: added the rOpenSci review badge, linked the API-concepts
  vignette from the quick start, pointed the “Value column” section to
  [`parse_ibge_value()`](https://strategicprojects.github.io/ibger/reference/parse_ibge_value.md),
  and rewrote the
  [`ibge_explorer()`](https://strategicprojects.github.io/ibger/reference/ibge_explorer.md)
  section to match the tone of the rest of the page.

- Examples no longer use `\dontrun{}`: examples that query the live IBGE
  API (or launch the Shiny explorer) are now guarded with
  `@examplesIf interactive()`, so they render in the docs and run for
  interactive users without hitting the network during checks.

- Internal refactor to address the remaining pkgcheck/goodpractice
  notes: every function is now below the cyclomatic-complexity threshold
  of 15 (long functions such as
  [`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md),
  [`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md),
  [`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
  and
  [`ibge_survey_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_survey_metadata.md)
  were decomposed into focused helpers), duplicated `@param` docs were
  replaced with `@inheritParams`, and the lintr issues flagged in the
  review (long lines, `expect_equal()` vs `expect_identical()`, static
  regexes without `fixed = TRUE`, duplicate `cli` bullet names, `<<-`,
  [`require()`](https://rdrr.io/r/base/library.html) in the Shiny app,
  [`setwd()`](https://rdrr.io/r/base/getwd.html) in `precompile.R`) were
  cleaned up. No user-facing behavior changes.

### Other changes

- Expanded the test suite to cover all exported functions (~85%
  coverage): pure helpers and parsers are tested directly, and API
  interactions are tested against recorded fixtures with `httptest2` (no
  network needed).
- Added continuous integration: `R CMD check` on Linux/macOS/Windows and
  test coverage reporting via GitHub Actions.
- Added `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md` and `codemeta.json` in
  preparation for rOpenSci submission.

## ibger 0.2.0

CRAN release: 2026-07-08

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
