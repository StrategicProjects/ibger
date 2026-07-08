# parse_sidra_url resolves names from metadata; seed the cache with fake
# metadata so no API call is made.

sidra_meta <- function() {
  make_fake_meta(
    id = 5434,
    name = "Pessoas de 14 anos ou mais de idade",
    variables = tibble::tibble(
      id   = "4090",
      name = "Pessoas de 14 anos ou mais de idade",
      unit = "Mil pessoas"
    ),
    classifications = tibble::tibble(
      id   = "888",
      name = "Condicao em relacao a forca de trabalho",
      categories = list(tibble::tibble(
        category_id    = c("47946", "56623"),
        category_name  = c("Forca de trabalho", "Fora da forca de trabalho"),
        category_unit  = NA_character_,
        category_level = c("1", "1")
      ))
    ),
    frequency = "trimestral"
  )
}

test_that("parse_sidra_url decodes a full SIDRA URL", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
  parsed <- parse_sidra_url(url)

  expect_s3_class(parsed, "sidra_query")
  expect_equal(parsed$aggregate$id, "5434")
  expect_equal(parsed$variables$id, "4090")
  expect_equal(parsed$variables$name, "Pessoas de 14 anos ou mais de idade")
  expect_equal(parsed$periods, "last 1")
  expect_equal(parsed$localities[[1]]$level, "N1")
  expect_equal(parsed$localities[[1]]$codes, "all")
  expect_equal(parsed$classifications[["888"]]$name, "Condicao em relacao a forca de trabalho")
  expect_equal(
    parsed$classifications[["888"]]$categories$category_name,
    c("Forca de trabalho", "Fora da forca de trabalho")
  )
})

test_that("parse_sidra_url builds the equivalent ibge_variables() call", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
  call_str <- parse_sidra_url(url)$ibger_call

  expect_match(call_str, "aggregate = 5434", fixed = TRUE)
  expect_match(call_str, "variable = 4090", fixed = TRUE)
  expect_match(call_str, "periods = -1", fixed = TRUE)
  expect_match(call_str, 'localities = "BR"', fixed = TRUE)
  expect_match(call_str, '"888" = c(47946, 56623)', fixed = TRUE)
})

test_that("parse_sidra_url handles specific locality codes and levels", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n3/33,35/v/4090/p/202301"
  parsed <- parse_sidra_url(url)

  expect_equal(parsed$localities[[1]]$level, "N3")
  expect_equal(parsed$localities[[1]]$codes, "33,35")
  expect_match(parsed$ibger_call, "localities = list(N3 = c(33,35))", fixed = TRUE)
  expect_match(parsed$ibger_call, 'periods = "202301"', fixed = TRUE)
})

test_that("parse_sidra_url aborts without an aggregate id", {
  expect_error(
    parse_sidra_url("https://apisidra.ibge.gov.br/values/v/4090"),
    "aggregate ID"
  )
})

test_that("print.sidra_query renders without error", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623"
  parsed <- parse_sidra_url(url)

  expect_invisible(print(parsed))
  out <- cli::cli_fmt(print(parsed))
  expect_true(any(grepl("SIDRA Query", out)))
  expect_true(any(grepl("5434", out)))
})

test_that("fetch_sidra_url translates the URL into ibge_variables() arguments", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  captured <- NULL
  local_mocked_bindings(
    ibge_variables = function(aggregate, variable, periods, localities,
                              classification, validate) {
      captured <<- list(
        aggregate = aggregate, variable = variable, periods = periods,
        localities = localities, classification = classification
      )
      tibble::tibble()
    }
  )

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090/p/last%203/c888/47946"
  fetch_sidra_url(url)

  expect_equal(captured$aggregate, 5434L)
  expect_equal(captured$variable, 4090)
  expect_equal(captured$periods, -3L)
  expect_equal(captured$localities, "BR")
  expect_equal(captured$classification, list("888" = 47946))
})
