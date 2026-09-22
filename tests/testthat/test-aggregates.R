# ibge_aggregates() filter validation happens before any request, so these
# tests need no fixtures.

test_that("ibge_aggregates rejects malformed filters before any request", {
  expect_error(ibge_aggregates(classification = "AAA"), "classification")
  expect_error(ibge_aggregates(subject = "abate"), "subject")
  expect_error(ibge_aggregates(subject = c(70, 71)), "subject")
  expect_error(ibge_aggregates(periodicity = 15), "periodicity")
  expect_error(ibge_aggregates(periodicity = 30), "periodicity")
  expect_error(ibge_aggregates(periodicity = "monthly"), "periodicity")
  expect_error(ibge_aggregates(level = "state"), "level")
  expect_error(ibge_aggregates(level = 3), "level")
  expect_error(ibge_aggregates(period = "202001"), "period")
  expect_error(ibge_aggregates(period = "P5[2020-01]"), "period")
})

test_that("well-formed filters pass the format check", {
  expect_true(validate_aggregates_filters(
    period = "P5[202001,202002]", subject = 70, classification = "12026",
    periodicity = "P13", level = "N6"
  ))
  expect_true(validate_aggregates_filters(
    period = NULL, subject = NULL, classification = NULL,
    periodicity = NULL, level = NULL
  ))
})

test_that("parse_aggregates returns the promised columns when empty", {
  empty <- parse_aggregates(list())
  expect_s3_class(empty, "tbl_df")
  expect_identical(
    names(empty),
    c("survey_id", "survey_name", "aggregate_id", "aggregate_name")
  )
  expect_identical(nrow(empty), 0L)

  # surveys with no aggregates are skipped, not turned into NA rows
  only_empty <- parse_aggregates(list(list(id = "AB", nome = "x",
                                           agregados = list())))
  expect_identical(nrow(only_empty), 0L)
})

test_that("ibge_aggregates warns on an empty result", {
  local_mocked_bindings(ibge_request = function(...) list())
  ibge_clear_cache()
  expect_message(res <- ibge_aggregates(periodicity = "P59"),
                 "No aggregates found")
  expect_identical(nrow(res), 0L)
  expect_identical(ncol(res), 4L)
})
