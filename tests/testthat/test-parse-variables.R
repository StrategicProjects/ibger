# Hand-built API responses mimicking the Aggregates API shapes.

nested_response <- function() {
  list(
    list(
      id = "112", variavel = "Valor da producao", unidade = "Mil Reais",
      resultados = list(
        list(
          classificacoes = list(
            list(id = "81", categoria = list("0" = "Total"))
          ),
          series = list(
            list(
              localidade = list(
                id = "1", nome = "Brasil",
                nivel = list(id = "N1", nome = "Pais")
              ),
              serie = list("2022" = "1000", "2023" = "1100")
            ),
            list(
              localidade = list(
                id = "26", nome = "Pernambuco",
                nivel = list(id = "N3", nome = "Unidade da Federacao")
              ),
              serie = list("2022" = "50", "2023" = "-")
            )
          )
        )
      )
    )
  )
}

flat_response <- function() {
  list(
    list(NC = "Nivel Territorial (Codigo)", NN = "Nivel Territorial",
         MC = "Unidade de Medida (Codigo)", MN = "Unidade de Medida",
         V = "Valor", D1C = "Brasil (Codigo)", D1N = "Brasil",
         D2C = "Variavel (Codigo)", D2N = "Variavel"),
    list(NC = "1", NN = "Brasil", MC = "33", MN = "Mil Reais",
         V = "1000", D1C = "1", D1N = "Brasil", D2C = "112",
         D2N = "Valor da producao"),
    list(NC = "1", NN = "Brasil", MC = "33", MN = "Mil Reais",
         V = "1100", D1C = "1", D1N = "Brasil", D2C = "112",
         D2N = "Valor da producao")
  )
}

test_that("parse_variables handles empty responses", {
  expect_equal(nrow(parse_variables(list())), 0)
})

test_that("parse_variables_default builds tidy long output", {
  result <- parse_variables(nested_response())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_named(result, c(
    "variable_id", "variable_name", "variable_unit",
    "classification_81",
    "locality_id", "locality_name", "locality_level", "period", "value"
  ))
  expect_equal(unique(result$variable_id), "112")
  expect_equal(unique(result$classification_81), "Total")
  expect_setequal(unique(result$locality_name), c("Brasil", "Pernambuco"))
  expect_setequal(unique(result$period), c("2022", "2023"))
  # Values stay as character (special codes preserved)
  expect_type(result$value, "character")
  expect_true("-" %in% result$value)
})

test_that("parse_variables detects and parses flat view", {
  result <- parse_variables(flat_response())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  # Column names come from the header row labels
  expect_true("Valor" %in% names(result))
  expect_true("Brasil" %in% names(result))
  expect_equal(result[["Valor"]], c("1000", "1100"))
})

test_that("parse_variables_flat with only a header returns empty", {
  expect_equal(nrow(parse_variables_flat(flat_response()[1])), 0)
})

test_that("parse_classifications returns one column per classification", {
  cls <- list(
    list(id = "81", categoria = list("0" = "Total")),
    list(id = "226", categoria = list("4844" = "Arroz"))
  )
  result <- parse_classifications(cls)
  expect_named(result, c("classification_81", "classification_226"))
  expect_equal(result$classification_81, "Total")
  expect_equal(result$classification_226, "Arroz")

  # No classifications -> 1-row, 0-col tibble (bindable)
  empty <- parse_classifications(NULL)
  expect_equal(nrow(empty), 1)
  expect_equal(ncol(empty), 0)
})
