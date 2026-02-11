## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(ibger)
# 
# # Search for aggregates
# ibge_aggregates()

## -----------------------------------------------------------------------------
# # Only quarterly aggregates
# ibge_aggregates(periodicity = "P10")
# 
# # Aggregates that have state-level data
# ibge_aggregates(level = "N3")

## -----------------------------------------------------------------------------
# meta <- ibge_metadata(1712)
# meta$variables

## -----------------------------------------------------------------------------
# # Two specific variables
# ibge_variables(1712, variable = c(214, 1982), localities = "BR")

## -----------------------------------------------------------------------------
# meta <- ibge_metadata(1712)
# meta$classifications
# 
# # Unnest to see all categories
# tidyr::unnest(meta$classifications, categories)

## -----------------------------------------------------------------------------
# # Default: Total category (aggregated across all products)
# ibge_variables(1712, localities = "BR")
# 
# # Specific products
# ibge_variables(
#   1712,
#   localities     = "BR",
#   classification = list("226" = c(4844, 96608))
# )
# 
# # All products (can be large)
# ibge_variables(
#   1712,
#   periods        = -1,
#   localities     = "BR",
#   classification = list("226" = "all")
# )

## -----------------------------------------------------------------------------
# meta <- ibge_metadata(1705)
# meta$territorial_level
# #> $administrative
# #> [1] "N1" "N2" "N3"

## -----------------------------------------------------------------------------
# # All states
# ibge_variables(1705, localities = "N3")
# 
# # Specific states
# ibge_variables(1705, localities = list(N3 = c(33, 35)))

## -----------------------------------------------------------------------------
# ibge_variables(
#   512,
#   variable   = 216,
#   periods    = -6,
#   localities = "N6[N3[33,35],N2[1]]"
# )

## -----------------------------------------------------------------------------
# meta <- ibge_metadata(7060)
# meta$periodicity
# #> $frequency [1] "mensal"
# #> $start     [1] "202001"
# #> $end       [1] "202512"

## -----------------------------------------------------------------------------
# ibge_periods(7060)

## -----------------------------------------------------------------------------
# # OLAP notation
# ibge_variables(1705, localities = "BR", view = "OLAP")
# 
# # Flat mode (first element is metadata, data starts at second)
# ibge_variables(1705, localities = "BR", view = "flat")

