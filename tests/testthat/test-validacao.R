# extract_levels -------------------------------------------------------------

test_that("extract_levels recognizes all locality forms", {
  expect_equal(extract_levels("BR"), "N1")
  expect_equal(extract_levels("br"), "N1")
  expect_equal(extract_levels("N3"), "N3")
  expect_equal(extract_levels(c("N3", "N6")), c("N3", "N6"))
  expect_equal(extract_levels("N3[33,35]|N6[123]"), c("N3", "N6"))
  expect_equal(extract_levels(list(N6 = c(1, 2), N3 = 3)), c("N6", "N3"))
  expect_equal(extract_levels(NULL), character())
})

# extract_numeric_periods -----------------------------------------------------

test_that("extract_numeric_periods expands ranges and skips last-N", {
  expect_equal(extract_numeric_periods(NULL), numeric())
  expect_equal(extract_numeric_periods(-6), numeric())
  expect_equal(extract_numeric_periods("-3"), numeric())
  expect_equal(extract_numeric_periods(c(201701, 201702)), c(201701, 201702))
  expect_equal(extract_numeric_periods("2017-2019"), c(2017, 2018, 2019))
  expect_equal(extract_numeric_periods("201701|201702"), c(201701, 201702))
})

# validate_* against fake metadata --------------------------------------------

test_that("validate_level accepts valid and rejects invalid levels", {
  meta <- make_fake_meta(levels = c("N1", "N3"))
  expect_invisible(validate_level(meta, "N3"))
  expect_error(validate_level(meta, "N6"), "not available")
})

test_that("validate_localities checks requested levels", {
  meta <- make_fake_meta(levels = c("N1", "N3"))
  expect_invisible(validate_localities(meta, "BR"))
  expect_invisible(validate_localities(meta, "N3"))
  expect_invisible(validate_localities(meta, list(N3 = c(33, 35))))
  expect_error(validate_localities(meta, "N6"), "not available")
  expect_error(validate_localities(meta, list(N6 = 3550308)), "not available")
})

test_that("validate_periods checks the metadata range", {
  meta <- make_fake_meta(period_start = "2010", period_end = "2020")
  expect_invisible(validate_periods(meta, -6))
  expect_invisible(validate_periods(meta, c(2015, 2016)))
  expect_error(validate_periods(meta, 2050), "out of range")
  expect_error(validate_periods(meta, "2005-2012"), "out of range")
})

test_that("validate_variables checks ids against metadata", {
  meta <- make_fake_meta()
  expect_invisible(validate_variables(meta, NULL))
  expect_invisible(validate_variables(meta, "all"))
  expect_invisible(validate_variables(meta, c(112, 214)))
  expect_error(validate_variables(meta, 999), "not found")
})

test_that("validate_classifications checks ids and categories", {
  meta <- make_fake_meta()
  expect_invisible(validate_classifications(meta, NULL))
  expect_invisible(validate_classifications(meta, list("81" = c(2688, 2689))))
  expect_invisible(validate_classifications(meta, list("81" = "all")))
  expect_error(validate_classifications(meta, list("999" = 1)), "not found")
  expect_error(validate_classifications(meta, list("81" = 424242)), "not found")
})

test_that("validate_query runs all checks together", {
  meta <- make_fake_meta()
  expect_invisible(validate_query(
    meta,
    localities     = "N3",
    periods        = c(2015, 2016),
    variable       = 112,
    classification = list("81" = 2688)
  ))
  expect_error(
    validate_query(meta, localities = "N9"),
    "not available"
  )
})

# print method ----------------------------------------------------------------

test_that("print.ibge_metadata renders without error", {
  meta <- make_fake_meta()
  expect_invisible(print(meta))
  out <- cli::cli_fmt(print(meta))
  expect_true(any(grepl("Fake aggregate", out)))
  expect_true(any(grepl("Produto das lavouras", out)))
})
