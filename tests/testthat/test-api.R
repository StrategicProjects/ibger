# Integration tests against recorded API responses (httptest2).
#
# On the first run with network access, `with_mock_dir()` records real
# responses under `tests/testthat/api/`; subsequent runs replay them
# offline, so these tests never hit the network on CI/CRAN.

skip_if_not_installed("httptest2")

# Each test clears the session caches first so requests always go through
# the (mocked) HTTP layer instead of the in-memory cache.
local_clear_caches <- function(env = parent.frame()) {
  suppressMessages(ibge_clear_cache())
  withr::defer(suppressMessages(ibge_clear_cache()), envir = env)
}

test_that("ibge_metadata fetches and parses aggregate metadata", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    meta <- ibge_metadata(1612)
  })

  expect_s3_class(meta, "ibge_metadata")
  expect_equal(as.character(meta$id), "1612")
  expect_gt(nrow(meta$variables), 0)
  expect_true(all(c("id", "name", "unit") %in% names(meta$variables)))
  expect_gt(nrow(meta$classifications), 0)
  expect_s3_class(meta$classifications$categories[[1]], "tbl_df")
  expect_equal(meta$periodicity$frequency, "anual")

  # Second call comes from the cache (no HTTP)
  meta2 <- ibge_metadata(1612)
  expect_identical(meta, meta2)
})

test_that("ibge_periods returns the period table", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    periods <- ibge_periods(1612)
  })

  expect_s3_class(periods, "tbl_df")
  expect_named(periods, c("id", "literal", "modification"))
  expect_gt(nrow(periods), 20)
  expect_true("2023" %in% periods$id)
})

test_that("ibge_localities returns localities for a level", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    locs <- ibge_localities(1612, level = "N3")
  })

  expect_s3_class(locs, "tbl_df")
  expect_named(locs, c("id", "name", "level_id", "level_name"))
  expect_equal(nrow(locs), 27)
  expect_true("Pernambuco" %in% locs$name)
})

test_that("ibge_variables fetches and tidies a small query", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    result <- ibge_variables(1612, variable = 112, periods = -2, localities = "BR")
  })

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$variable_id), "112")
  expect_equal(unique(result$locality_name), "Brasil")
  expect_type(result$value, "character")
  # Values coerce cleanly (numbers or IBGE special codes -> NA)
  expect_type(parse_ibge_value(result$value), "double")
})

test_that("ibge_variables validation rejects a bad level before any data request", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    expect_error(
      ibge_variables(1612, localities = "N99"),
      "not available"
    )
  })
})

test_that("ibge_aggregates lists tables filtered by subject", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    aggs <- ibge_aggregates(subject = 70)
  })

  expect_s3_class(aggs, "tbl_df")
  expect_named(aggs, c("survey_id", "survey_name", "aggregate_id", "aggregate_name"))
  expect_gt(nrow(aggs), 0)

  # Second call with the same filters comes from the cache
  aggs2 <- ibge_aggregates(subject = 70)
  expect_identical(aggs, aggs2)
})

test_that("ibge_surveys returns the survey catalog", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    surveys <- ibge_surveys(thematic_classifications = FALSE)
  })

  expect_s3_class(surveys, "tbl_df")
  expect_true(all(c("id", "name", "status", "category") %in% names(surveys)))
  expect_true("CD" %in% surveys$id)
})

test_that("ibge_survey_periods validates the code and returns periods", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    periods <- ibge_survey_periods("CD")
  })

  expect_s3_class(periods, "tbl_df")
  expect_named(periods, c("year", "month", "order"))
  expect_true(2022 %in% periods$year)
  # Census is structural: months are NA
  expect_true(all(is.na(periods$month)))
})

test_that("ibge_survey_periods suggests near matches for bad codes", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    expect_error(ibge_survey_periods("XX_INVALID"), "not found")
  })
})

test_that("ibge_survey_metadata fetches and structures survey metadata", {
  local_clear_caches()
  httptest2::with_mock_dir("api", {
    meta <- ibge_survey_metadata("CD", year = 2022)
  })

  expect_s3_class(meta, "ibge_survey_metadata")
  expect_equal(meta$acronym, "CD")
  expect_equal(tolower(meta$category), "estrutural")
  expect_s3_class(meta$thematic_classifications, "tbl_df")
  expect_gt(length(meta$occurrences), 0)

  expect_invisible(print(meta))
  out <- cli::cli_fmt(print(meta))
  expect_true(any(grepl("CD", out)))
})

test_that("ibge_survey_metadata validates inputs before any request", {
  expect_error(ibge_survey_metadata("CD", year = "x"), "single integer")
  expect_error(ibge_survey_metadata("CD", year = 2022, month = 13), "between 1 and 12")

  local_clear_caches()
  httptest2::with_mock_dir("api", {
    # Year outside the recorded periods for the survey
    expect_error(ibge_survey_metadata("CD", year = 1800), "not available")
    # Structural survey must not receive a month
    expect_error(
      ibge_survey_metadata("CD", year = 2022, month = 6),
      "does not use monthly periods"
    )
  })
})
