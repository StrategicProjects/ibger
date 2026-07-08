## Submission: ibger 0.2.0

This is a minor release. Main changes (see NEWS.md):

* `ibge_variables()` gains a `chunk` argument (default `TRUE`): queries whose
  estimated result exceeds the IBGE API per-request value limit are now
  transparently split into multiple smaller requests and combined into a
  single tibble, instead of failing with an HTTP 500 error
  (https://github.com/StrategicProjects/ibger/issues/1).
* Added an initial testthat suite. Tests do not require network access; all
  code that reaches the IBGE API is only exercised in examples wrapped in
  \dontrun{}.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: macOS (aarch64-apple-darwin), R 4.6.0
