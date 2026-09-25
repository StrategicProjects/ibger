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

  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623")
  parsed <- parse_sidra_url(url)

  expect_s3_class(parsed, "sidra_query")
  expect_identical(parsed$aggregate$id, "5434")
  expect_identical(parsed$variables$id, "4090")
  expect_identical(parsed$variables$name, "Pessoas de 14 anos ou mais de idade")
  expect_identical(parsed$periods, "last 1")
  expect_identical(parsed$localities[[1]]$level, "N1")
  expect_identical(parsed$localities[[1]]$codes, "all")
  expect_identical(
    parsed$classifications[["888"]]$name,
    "Condicao em relacao a forca de trabalho"
  )
  expect_identical(
    parsed$classifications[["888"]]$categories$category_name,
    c("Forca de trabalho", "Fora da forca de trabalho")
  )
})

test_that("parse_sidra_url builds the equivalent ibge_variables() call", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623")
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

  expect_identical(parsed$localities[[1]]$level, "N3")
  expect_identical(parsed$localities[[1]]$codes, "33,35")
  expect_match(parsed$ibger_call, "localities = list(N3 = c(33, 35))",
               fixed = TRUE)
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

  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/v/4090/p/last%201/c888/47946,56623")
  parsed <- parse_sidra_url(url)

  expect_invisible(print(parsed))
  out <- cli::cli_fmt(print(parsed))
  expect_true(any(grepl("SIDRA Query", out, fixed = TRUE)))
  expect_true(any(grepl("5434", out, fixed = TRUE)))
})

test_that("fetch_sidra_url translates the URL into ibge_variables()", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    ibge_variables = function(aggregate, variable, periods, localities,
                              classification, validate) {
      captured$args <- list(
        aggregate = aggregate, variable = variable, periods = periods,
        localities = localities, classification = classification
      )
      tibble::tibble()
    }
  )

  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/v/4090/p/last%203/c888/47946")
  fetch_sidra_url(url)

  expect_identical(captured$args$aggregate, 5434L)
  expect_identical(captured$args$variable, 4090)
  expect_identical(captured$args$periods, -3L)
  expect_identical(captured$args$localities, "BR")
  expect_identical(captured$args$classification, list("888" = 47946))
})

# --- Regressions reported in rOpenSci review 2 (@ddiannae) -----------------

test_that("a SIDRA URL without a /p/ segment prints and fetches", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n1/all/v/4090"
  parsed <- parse_sidra_url(url)

  expect_length(parsed$periods, 0)
  expect_no_error(cli::cli_fmt(print(parsed)))
  out <- cli::cli_fmt(print(parsed))
  expect_true(any(grepl("last 6 periods", out, fixed = TRUE)))
  expect_false(grepl("periods =", parsed$ibger_call, fixed = TRUE))

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    ibge_variables = function(aggregate, variable, periods, localities,
                              classification, validate) {
      captured$periods <- periods
      tibble::tibble()
    }
  )
  fetch_sidra_url(url)
  expect_identical(captured$periods, -6)
})

test_that("an unknown territorial level warns instead of crashing", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  url <- "https://apisidra.ibge.gov.br/values/t/5434/n12/all/v/4090"
  expect_warning(parse_sidra_url(url), "N12")
  parsed <- suppressWarnings(parse_sidra_url(url))

  expect_identical(parsed$localities[[1]]$level, "N12")
  expect_identical(parsed$localities[[1]]$level_name, "unknown level")
  expect_no_error(suppressWarnings(cli::cli_fmt(print(parsed))))
})

test_that("a level absent from the aggregate metadata warns", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  # N7 has a known name but sidra_meta() only offers N1, N3 and N6
  url <- "https://apisidra.ibge.gov.br/values/t/5434/n7/all/v/4090"
  expect_warning(parse_sidra_url(url), "Available levels")
})

test_that("multi-level URLs produce a localities argument that is valid", {
  cleanup <- seed_fake_meta(5434, sidra_meta())
  withr::defer(cleanup())

  # two "all" levels: the API pipe syntax as a string
  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/n3/all/v/4090/p/last%201")
  parsed <- parse_sidra_url(url)
  expect_match(parsed$ibger_call, 'localities = "N1|N3"', fixed = TRUE)
  expect_identical(sidra_localities_arg(list(
    list(level = "N1", codes = "all"), list(level = "N3", codes = "all")
  )), "N1|N3")
  expect_identical(format_localities("N1|N3"), "N1|N3")

  # "all" mixed with specific codes
  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n1/all/n3/33,35/v/4090/p/last%201")
  parsed <- parse_sidra_url(url)
  expect_match(parsed$ibger_call, 'localities = "N1|N3[33,35]"',
               fixed = TRUE)

  # only specific codes: a named list
  url <- paste0("https://apisidra.ibge.gov.br/values",
                "/t/5434/n3/33,35/n6/3550308/v/4090/p/last%201")
  parsed <- parse_sidra_url(url)
  expect_match(parsed$ibger_call,
               "localities = list(N3 = c(33, 35), N6 = 3550308)",
               fixed = TRUE)
  expect_identical(
    sidra_localities_arg(list(
      list(level = "N3", codes = "33,35"), list(level = "N6", codes = "3550308")
    )),
    list(N3 = c(33, 35), N6 = 3550308)
  )
})
