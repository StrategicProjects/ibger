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
