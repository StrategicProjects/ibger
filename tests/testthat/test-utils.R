# format_classification ---------------------------------------------------

test_that("format_classification builds the API syntax", {
  expect_null(format_classification(NULL))
  expect_equal(format_classification(list("226" = c(4844, 96608))), "226[4844,96608]")
  expect_equal(format_classification(list("218" = 4780)), "218[4780]")
  expect_equal(
    format_classification(list("226" = c(4844, 96608), "218" = 4780)),
    "226[4844,96608]|218[4780]"
  )
  expect_equal(format_classification(list("226" = "all")), "226[all]")
  expect_equal(format_classification(list("226" = "todos")), "226[all]")
})

test_that("format_classification rejects unnamed input", {
  expect_error(format_classification("226"), "named list")
  expect_error(format_classification(list(1, 2)), "named list")
})

# format_localities --------------------------------------------------------

test_that("format_localities accepts all documented forms", {
  expect_equal(format_localities("BR"), "BR")
  expect_equal(format_localities("N3"), "N3")
  expect_equal(format_localities(c("N3", "N6")), "N3|N6")
  expect_equal(format_localities(list(N6 = c(3550308, 3304557))), "N6[3550308,3304557]")
  expect_equal(
    format_localities(list(N3 = c(33, 35), N6 = 5208707)),
    "N3[33,35]|N6[5208707]"
  )
})

test_that("format_localities rejects unnamed lists", {
  expect_error(format_localities(list(1, 2)), "named list")
})

# format_periods ------------------------------------------------------------

test_that("format_periods handles last-N, vectors and ranges", {
  expect_equal(format_periods(NULL), "-6")
  expect_equal(format_periods(-6), "-6")
  expect_equal(format_periods(c(201701, 201702)), "201701|201702")
  expect_equal(format_periods("201701-201712"), "201701-201712")
})

# format_variable -----------------------------------------------------------

test_that("format_variable handles NULL, all and vectors", {
  expect_equal(format_variable(NULL), "allxp")
  expect_equal(format_variable("all"), "all")
  expect_equal(format_variable("todas"), "all")
  expect_equal(format_variable(c(284, 285)), "284|285")
  expect_equal(format_variable(214), "214")
})

# pluck_chr -------------------------------------------------------------------

test_that("pluck_chr walks nested lists safely", {
  x <- list(a = list(b = "value"), n = 42)
  expect_equal(pluck_chr(x, "a", "b"), "value")
  expect_equal(pluck_chr(x, "n"), "42")
  expect_true(is.na(pluck_chr(x, "missing")))
  expect_equal(pluck_chr(x, "missing", .default = "d"), "d")
})

# parse_ibge_value ------------------------------------------------------------

test_that("parse_ibge_value converts IBGE special codes", {
  expect_equal(
    parse_ibge_value(c("1.5", "10", "-", "..", "...", "X", NA)),
    c(1.5, 10, 0, NA, NA, NA, NA)
  )
  # Numeric input is returned untouched
  expect_equal(parse_ibge_value(c(1, 2.5)), c(1, 2.5))
})
