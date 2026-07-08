# Contributing to ibger

Thanks for your interest in improving **ibger**! This document explains how
to propose changes and what we expect from contributions.

## Filing an issue

- **Bugs**: please include a minimal reproducible example (consider
  [reprex](https://reprex.tidyverse.org)), the output of `sessionInfo()`,
  and — for API errors — the full URL shown in the error message.
- **Feature requests**: describe the use case first; API design follows from
  it. Note that the public API mirrors the Python sibling package
  [ibgepy](https://github.com/StrategicProjects/ibgepy), so changes to the
  function surface should make sense for both.

## Pull requests

1. Fork the repo and create a branch from `main`.
2. Make your changes:
   - Follow the existing code style (tidyverse-ish, native pipe `|>`,
     `cli` for all messages/errors with `call = NULL` on `cli_abort()`).
   - Public API is in **English**; internal helpers and IBGE API query
     parameters stay in Portuguese (`periodo`, `localidades`, ...).
   - Document with roxygen2 (markdown enabled) and run
     `devtools::document()` — do not edit `man/*.Rd` or `NAMESPACE` by hand.
3. Add tests. We use testthat (3e). HTTP interactions are tested against
   recorded fixtures with [httptest2](https://enpiar.com/httptest2/) — see
   `tests/testthat/test-api.R`. Please keep new fixtures small (trim large
   JSON payloads to the fields the test needs).
4. Run `devtools::test()` and `devtools::check()` locally; both must pass
   cleanly.
5. Update `NEWS.md` under the development version heading.

## Scope notes

- Requests go through `ibge_request()` in `R/utils.R` (Aggregates API v3)
  or `ibge_metadados_request()` in `R/surveys.R` (Metadata API v2). New
  endpoints should reuse these helpers.
- Large-query splitting lives in `R/chunking.R`. The IBGE API rejects
  requests above ~50,000 values in practice, so anything that changes query
  size estimation should keep that limit in mind.

## Code of conduct

This project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating you
agree to abide by its terms.
