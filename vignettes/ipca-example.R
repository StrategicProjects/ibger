## ----include = FALSE----------------------------------------------------------
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
# 
# meta <- ibge_metadata(7060)
# meta

## -----------------------------------------------------------------------------
# # See what variables are available
# meta$variables
# 
# # Peek at the classification categories
# tidyr::unnest(meta$classifications, categories) |>
#   head(20)

## -----------------------------------------------------------------------------
# ipca_br <- ibge_variables(
#   aggregate  = 7060,
#   variable   = 63,
#   periods    = -24,
#   localities = "BR"
# )
# 
# ipca_br

## -----------------------------------------------------------------------------
# ipca_br <- ipca_br |>
#   mutate(
#     value = parse_ibge_value(value),
#     date  = as.Date(paste0(period, "01"), format = "%Y%m%d")
#   )

## -----------------------------------------------------------------------------
# ggplot(ipca_br, aes(date, value)) +
#   geom_col(fill = "#2e86c1", alpha = 0.8) +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   labs(
#     title    = "IPCA — Monthly variation (%)",
#     subtitle = "Brazil, last 24 months",
#     x = NULL, y = "Variation (%)",
#     caption  = "Source: IBGE via ibger"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# ipca_vars <- ibge_variables(
#   aggregate  = 7060,
#   variable   = c(63, 69, 2265),
#   periods    = -12,
#   localities = "BR"
# )
# 
# ipca_vars <- ipca_vars |>
#   mutate(
#     value = parse_ibge_value(value),
#     date  = as.Date(paste0(period, "01"), format = "%Y%m%d")
#   )
# 
# ggplot(ipca_vars, aes(date, value, colour = variable_name)) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   labs(
#     title  = "IPCA — Three measures of variation",
#     x = NULL, y = "%", colour = NULL,
#     caption = "Source: IBGE via ibger"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# cats <- tidyr::unnest(meta$classifications, categories)
# 
# # Level-1 groups (just below the general index)
# cats |> filter(category_level == "1")

## -----------------------------------------------------------------------------
# groups <- ibge_variables(
#   aggregate      = 7060,
#   variable       = 63,
#   periods        = -12,
#   localities     = "BR",
#   classification = list("315" = c(7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786))
# )
# 
# groups <- groups |>
#   mutate(
#     value = parse_ibge_value(value),
#     date  = as.Date(paste0(period, "01"), format = "%Y%m%d")
#   )
# 
# ggplot(groups, aes(date, value, fill = classification_315)) +
#   geom_col(position = "dodge", alpha = 0.85) +
#   labs(
#     title = "IPCA by product group — Monthly variation",
#     x = NULL, y = "%", fill = NULL,
#     caption = "Source: IBGE via ibger"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.text = element_text(size = 7))

## -----------------------------------------------------------------------------
# # Check available metropolitan areas
# metros <- ibge_localities(7060, level = "N7")
# metros

## -----------------------------------------------------------------------------
# ipca_metros <- ibge_variables(
#   aggregate  = 7060,
#   variable   = 2265,
#   periods    = -12,
#   localities = list(N7 = c(3501, 3301, 2901, 4101, 1501))
# )
# 
# ipca_metros <- ipca_metros |>
#   mutate(
#     value = parse_ibge_value(value),
#     date  = as.Date(paste0(period, "01"), format = "%Y%m%d")
#   )
# 
# ggplot(ipca_metros, aes(date, value, colour = locality_name)) +
#   geom_line(linewidth = 0.8) +
#   labs(
#     title   = "IPCA — 12-month cumulative by metro area",
#     x = NULL, y = "%", colour = NULL,
#     caption = "Source: IBGE via ibger"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
# all_metros <- ibge_variables(
#   aggregate      = 7060,
#   variable       = c(63, 69, 2265),
#   periods        = -12,
#   localities     = "N7",
#   classification = list("315" = 7169)
# )
# 
# all_metros <- all_metros |>
#   mutate(value = parse_ibge_value(value)) |>
#   select(variable_name, locality_name, period, value) |>
#   pivot_wider(names_from = variable_name, values_from = value)
# 
# all_metros

## -----------------------------------------------------------------------------
# # Example: query all categories for just 1 period, Brazil only
# full_breakdown <- ibge_variables(
#   aggregate      = 7060,
#   variable       = 63,
#   periods        = -1,
#   localities     = "BR",
#   classification = list("315" = "all")
# )
# 
# nrow(full_breakdown)

## -----------------------------------------------------------------------------
# ibge_variables(7060, localities = "BR") |>
#   mutate(value = parse_ibge_value(value))

## -----------------------------------------------------------------------------
# parse_ibge_value(c("1.5", "10", "-", "..", "...", "X"))
# #> [1] 1.5  10.0  0.0   NA    NA    NA

