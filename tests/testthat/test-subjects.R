test_that("ibge_subjects returns the full built-in table", {
  result <- ibge_subjects()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "name"))
  expect_gt(nrow(result), 200)
  expect_type(result$id, "integer")
})

test_that("ibge_subjects filters by pattern", {
  result <- ibge_subjects("internet")
  expect_gt(nrow(result), 0)
  expect_true(all(grepl("internet", result$name, ignore.case = TRUE)))

  # Case-sensitive match finds nothing for upper-case pattern
  result_cs <- ibge_subjects("INTERNET", ignore_case = FALSE)
  expect_equal(nrow(result_cs), 0)
})

test_that("ibge_subjects supports regular expressions", {
  result <- ibge_subjects("saneamento|esgoto")
  expect_gt(nrow(result), 0)
  expect_true(all(grepl("saneamento|esgoto", result$name, ignore.case = TRUE)))
})
