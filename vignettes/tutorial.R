## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(ibger)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(lubridate)
# library(stringr)

## -----------------------------------------------------------------------------
# # Monthly periods: "202501" -> 2025-01-01
# period_to_monthly <- function(x) ym(x)
# 
# # Quarterly periods: "202501" -> 2025-01-01
# # lubridate::yq() expects "2025.1", so we reformat first
# period_to_quarterly <- function(x) {
#   yr <- substr(x, 1, 4)
#   qt <- as.integer(substr(x, 5, 6))
#   as.Date(paste0(yr, "-", qt * 3 - 2, "-01"))
# }

## -----------------------------------------------------------------------------
# meta_7060 <- ibge_metadata(7060)
# 
# # Find classification categories matching "Plano" (health plan) or "Índice" (index)
# unnest(meta_7060$classifications, categories) |>
#   filter(str_detect(category_name, "Plano|Índice")) |>
#   select(id, category_id, category_name, category_level)
# 
# # Available variables
# meta_7060$variables

## -----------------------------------------------------------------------------
# ipca_health <- ibge_variables(
#   aggregate = 7060,
#   variable = 63,                          # IPCA - Monthly variation
#   periods = -12,
#   classification = list(
#     "315" = c("7169", "7695")             # General index + Health insurance
#   ),
#   localities = "N7[2601]"                 # Recife Metropolitan Area
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_315, locality_name, value)

## -----------------------------------------------------------------------------
# ipca_health |>
#   pivot_wider(
#     id_cols    = c(period, locality_name),
#     names_from = classification_315,
#     values_from = value
#   ) |>
#   arrange(desc(period))

## -----------------------------------------------------------------------------
# ipca_health |>
#   ggplot(aes(period, value, color = classification_315)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "IPCA — Health insurance vs General index",
#     subtitle = "Recife Metropolitan Area, monthly variation (%)",
#     x = NULL, y = "Monthly variation (%)", color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# # Find category ID for "Seguro" (insurance)
# unnest(meta_7060$classifications, categories) |>
#   filter(str_detect(category_name, "Seguro|Índice")) |>
#   select(id, category_id, category_name)

## -----------------------------------------------------------------------------
# ipca_vehicle_ins <- ibge_variables(
#   aggregate = 7060,
#   variable = 63,
#   periods = -12,
#   classification = list("315" = c("7169", "7643")),  # General + Vehicle insurance
#   localities = "N7[2601]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_315, locality_name, value)

## -----------------------------------------------------------------------------
# ipca_vehicle_ins |>
#   ggplot(aes(period, value, color = classification_315)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "IPCA — Vehicle insurance vs General index",
#     subtitle = "Recife Metropolitan Area, monthly variation (%)",
#     x = NULL, y = "Monthly variation (%)", color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# meta_8693 <- ibge_metadata(8693)
# 
# # Browse classifications and categories
# unnest(meta_8693$classifications, categories)
# meta_8693$variables

## -----------------------------------------------------------------------------
# pms_transport <- ibge_variables(
#   aggregate = 8693,
#   variable = 7167,                          # Index number (2022 = 100)
#   periods = -12,
#   classification = list(
#     "11046" = "all",                        # All index types (revenue + volume)
#     "12355" = "106876"                      # Transportation/postal services
#   ),
#   localities = "N3[26]"                     # Pernambuco
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_11046, locality_name, value)

## -----------------------------------------------------------------------------
# pms_transport |>
#   ggplot(aes(period, value, color = classification_11046)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PMS — Index numbers (2022 = 100)",
#     subtitle = "Transportation, storage and postal services (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)", color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# meta_5434 <- ibge_metadata(5434)
# unnest(meta_5434$classifications, categories)
# meta_5434$variables

## -----------------------------------------------------------------------------
# pnad_accommodation <- ibge_variables(
#   aggregate = 5434,
#   variable = 4090,                          # Employed persons (thousands)
#   periods = -12,                            # Last 12 quarters
#   classification = list("888" = "56623"),   # Accommodation and food services
#   localities = "N3[26]"                     # Pernambuco
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_quarterly(period)
#   ) |>
#   select(period, classification_888, locality_name, value)

## -----------------------------------------------------------------------------
# pnad_accommodation |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PNAD Contínua — Employed persons (14+)",
#     subtitle = "Accommodation and food services (Pernambuco, thousands)",
#     x = NULL, y = "Employed (thousands)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# pms_professional <- ibge_variables(
#   aggregate = 8693,
#   variable = 7167,
#   periods = -12,
#   classification = list(
#     "11046" = "all",
#     "12355" = "31399"                       # Professional/administrative services
#   ),
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_11046, locality_name, value)

## -----------------------------------------------------------------------------
# pms_professional |>
#   ggplot(aes(period, value, color = classification_11046)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PMS — Index numbers (2022 = 100)",
#     subtitle = "Professional and administrative services (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)", color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# pnad_domestic <- ibge_variables(
#   aggregate = 5434,
#   variable = 4090,
#   periods = -12,
#   classification = list("888" = "56628"),   # Domestic services
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_quarterly(period)
#   ) |>
#   select(period, classification_888, locality_name, value)

## -----------------------------------------------------------------------------
# pnad_domestic |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PNAD Contínua — Employed persons (14+)",
#     subtitle = "Domestic services (Pernambuco, thousands)",
#     x = NULL, y = "Employed (thousands)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# meta_8888 <- ibge_metadata(8888)
# unnest(meta_8888$classifications, categories)
# meta_8888$variables

## -----------------------------------------------------------------------------
# pim_selected <- ibge_variables(
#   aggregate = 8888,
#   variable = 12606,                         # Index number (2022 = 100)
#   periods = -12,
#   classification = list(
#     "544" = c(129318, 129338)               # Beverages; Motor vehicles
#   ),
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_544, locality_name, value)

## -----------------------------------------------------------------------------
# pim_selected |>
#   ggplot(aes(period, value, color = classification_544)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PIM-PF — Index numbers (2022 = 100)",
#     subtitle = "Beverages and Motor vehicles (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)", color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# meta_8886 <- ibge_metadata(8886)
# meta_8886$variables

## -----------------------------------------------------------------------------
# construction <- ibge_variables(
#   aggregate = 8886,
#   variable = 12606,                         # Index number (2022 = 100)
#   periods = -12,
#   localities = "N1"                         # Brazil
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, locality_name, value)

## -----------------------------------------------------------------------------
# construction |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "Construction — Typical inputs (physical production)",
#     subtitle = "Brazil, index number (2022 = 100)",
#     x = NULL, y = "Index (2022 = 100)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# meta_8884 <- ibge_metadata(8884)
# unnest(meta_8884$classifications, categories)
# meta_8884$variables

## -----------------------------------------------------------------------------
# pmc_vehicles <- ibge_variables(
#   aggregate = 8884,
#   variable = 7169,                          # Index number (2022 = 100)
#   periods = -12,
#   classification = list("11046" = 56738),   # Volume index
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_11046, locality_name, value)

## -----------------------------------------------------------------------------
# pmc_vehicles |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PMC — Sales volume index (2022 = 100)",
#     subtitle = "Vehicles, motorcycles, parts and accessories (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# pmc_construction <- ibge_variables(
#   aggregate = 8757,
#   variable = 7169,
#   periods = -12,
#   classification = list("11046" = 56732),   # Volume — construction materials
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_11046, locality_name, value)

## -----------------------------------------------------------------------------
# pmc_construction |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PMC — Sales volume index (2022 = 100)",
#     subtitle = "Construction materials (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# pmc_retail <- ibge_variables(
#   aggregate = 8880,
#   variable = 7169,
#   periods = -12,
#   classification = list("11046" = 56734),   # Volume — retail trade
#   localities = "N3[26]"
# ) |>
#   mutate(
#     value  = parse_ibge_value(value),
#     period = period_to_monthly(period)
#   ) |>
#   select(period, classification_11046, locality_name, value)

## -----------------------------------------------------------------------------
# pmc_retail |>
#   ggplot(aes(period, value)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "PMC — Sales volume index (2022 = 100)",
#     subtitle = "Retail trade (Pernambuco)",
#     x = NULL, y = "Index (2022 = 100)"
#   ) +
#   theme_minimal()

