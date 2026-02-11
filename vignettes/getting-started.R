## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("StrategicProjects/ibger")

## ----setup--------------------------------------------------------------------
# library(ibger)

## -----------------------------------------------------------------------------
# # All aggregates
# ibge_aggregates()
# #> ✔ 1420 aggregates found.
# #> # A tibble: 1,420 × 4
# #>   survey_id survey_name          aggregate_id aggregate_name
# #>   <chr>     <chr>                <chr>        <chr>
# #> 1 AB        Abate de animais     1705         Animais abatidos …
# #> 2 AB        Abate de animais     1706         Peso total das ca…
# #> ...
# 
# # Monthly aggregates only
# ibge_aggregates(periodicity = "P5")
# 
# # Aggregates with municipality-level data
# ibge_aggregates(level = "N6")

## -----------------------------------------------------------------------------
# meta <- ibge_metadata(1705)
# meta

## -----------------------------------------------------------------------------
# meta$variables
# #> # A tibble: 2 × 3
# #>   id    name                  unit
# #>   <chr> <chr>                 <chr>
# #> 1 284   Número de informantes Unidades
# #> 2 285   Cabeças abatidas      Cabeças
# 
# meta$classifications
# #> # A tibble: 1 × 3
# #>   id    name                        categories
# #>   <chr> <chr>                       <list>
# #> 1 12529 Tipo de rebanho bovino      <tibble [9 × 4]>
# 
# # Unnest to see every category
# tidyr::unnest(meta$classifications, categories)
# 
# # Geographic levels
# meta$territorial_level
# #> $administrative
# #> [1] "N1" "N2" "N3"
# 
# # Time range
# meta$periodicity
# #> $frequency [1] "trimestral"
# #> $start     [1] "200101"
# #> $end       [1] "202404"

## -----------------------------------------------------------------------------
# ibge_variables(1705, localities = "BR")
# #> ✔ 12 records retrieved.
# #> # A tibble: 12 × 9
# #>   variable_id variable_name      variable_unit classification_12529
# #>   <chr>       <chr>              <chr>         <chr>
# #> 1 284         Número de inform…  Unidades      Total
# #> 2 285         Cabeças abatidas   Cabeças       Total
# #> ...
# #>   locality_id locality_name locality_level period value
# #>   <chr>       <chr>         <chr>          <chr>  <chr>
# #> 1 1           Brasil        Brasil         202303 2584
# #> 2 1           Brasil        Brasil         202303 7802044
# #> ...

## -----------------------------------------------------------------------------
# # Country total
# ibge_variables(1705, localities = "BR")
# 
# # All states
# ibge_variables(8884, localities = "N3")
# 
# # Specific states (RJ = 33, SP = 35)
# ibge_variables(8884, localities = list(N3 = c(33, 35)))
# 
# # Mix levels: metropolitan areas + a specific municipality
# ibge_variables(1705, localities = list(N7 = c(3501, 3301), N6 = 5208707))

## -----------------------------------------------------------------------------
# # Last 6 periods (the default)
# ibge_variables(1705, periods = -6, localities = "BR")
# 
# # Last 12 periods
# ibge_variables(1705, periods = -12, localities = "BR")
# 
# # Specific period codes
# ibge_variables(8884, periods = c(202301, 202302, 202303), localities = "BR")
# 
# # Range (inclusive)
# ibge_variables(8884, periods = "202101-202304", localities = "BR")
# 
# # Range + extra period
# ibge_variables(8884, periods = "202101-202106|202301", localities = "BR")

## -----------------------------------------------------------------------------
# # Single category: pineapple (4844) from product classification (226)
# ibge_variables(
#   aggregate      = 1712,
#   localities     = "BR",
#   classification = list("226" = 4844)
# )
# 
# # Multiple categories
# ibge_variables(
#   aggregate      = 1712,
#   localities     = "BR",
#   classification = list("226" = c(4844, 96608, 96609))
# )
# 
# # Multiple classifications
# ibge_variables(
#   aggregate      = 1712,
#   localities     = "BR",
#   classification = list("226" = c(4844, 96608), "218" = 4780)
# )
# 
# # All categories of a classification (can be large!)
# ibge_variables(
#   aggregate      = 1712,
#   periods        = -1,
#   localities     = "BR",
#   classification = list("226" = "all")
# )

## -----------------------------------------------------------------------------
# # N3 (states) is not available for aggregate 1705
# ibge_variables(1705, localities = "N3")
# #> Error:
# #> ! Geographic level(s) "N3" not available for aggregate 1705.
# #> ℹ Available levels: "N1", "N6", and "N7".
# 
# # Period out of range
# ibge_variables(1705, periods = 199901, localities = "BR")
# #> Error:
# #> ! Period(s) "199901" out of range for aggregate 1705.
# #> ℹ Valid range: "201202" to "202001" (monthly).
# 
# # Non-existent variable
# ibge_variables(1705, variable = 999, localities = "BR")
# #> Error:
# #> 355 - IPCA15 - Variação mensal (%)
# #> 356 - IPCA15 - Variação acumulada no ano (%)
# #> 1120 - IPCA15 - Variação acumulada em 12 meses (%)
# #> 357 - IPCA15 - Peso mensal (%)

## -----------------------------------------------------------------------------
# ibge_clear_cache()

## -----------------------------------------------------------------------------
# ibge_variables(1705, localities = "BR", validate = FALSE)

## -----------------------------------------------------------------------------
# # List all 98 IBGE surveys
# ibge_surveys()
# #> # A tibble: 98 × 8
# #>   id    name                                 status category    ...
# #>   <chr> <chr>                                <chr>  <chr>
# #> 1 AC    Pesquisa Anual da Indústria da Cons… Ativa  Estrutural
# #> 2 AA    Pesquisa Nacional de Saúde do Escol… Ativa  Especial
# #> ...
# 
# # Filter active monthly surveys
# library(dplyr)
# ibge_surveys(thematic_classifications = FALSE) |>
#   filter(status == "Ativa", category == "Conjuntural")
# 
# # Check which periods have metadata for the Censo Demográfico
# ibge_survey_periods("CD")
# #> # A tibble: 9 × 3
# #>    year month order
# #>   <int> <int> <int>
# #> 1  2022    NA     0
# #> 2  2010    NA     0
# #> ...
# 
# # Get full institutional metadata for a specific period
# meta <- ibge_survey_metadata("CD", year = 2022)
# meta
# #> ── CD ──
# #> Status: Ativa
# #> Category: Estrutural
# #> ...
# #> ── Metadata occurrences (1) ──
# #> Use `meta$occurrences` to explore the full metadata.
# 
# # Explore methodology fields
# names(meta$occurrences[[1]])

## -----------------------------------------------------------------------------
# ibge_survey_periods("PMS")
# #> Error: Survey code "PMS" not found in the IBGE catalog.
# #> ℹ Did you mean one of these?
# #>   * SC - Pesquisa Mensal de Serviços
# #>   * MC - Pesquisa Mensal de Comércio
# #>   ...

## -----------------------------------------------------------------------------
# ibge_variables(7060, localities = "BR") |>
#   dplyr::mutate(value = parse_ibge_value(value))

