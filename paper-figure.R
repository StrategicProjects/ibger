# Generates paper-figure1.png (Figure 1 of paper.md): resident population of
# all 5,570 Brazilian municipalities from the 2022 Demographic Census,
# fetched with a single ibge_variables() call.
library(ibger)
library(ggplot2)

pop <- ibge_variables(4709, variable = 93, periods = 2022, localities = "N6")

muni <- geobr::read_municipality(year = 2022, simplified = TRUE, showProgress = FALSE)

pop$code_muni <- as.numeric(pop$locality_id)
pop$population <- parse_ibge_value(pop$value)
dat <- merge(muni, pop[, c("code_muni", "population")], by = "code_muni")

p <- ggplot(dat) +
  geom_sf(aes(fill = population), color = NA) +
  scale_fill_viridis_c(
    trans = "log10",
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    name = "Resident\npopulation"
  ) +
  theme_void(base_size = 13) +
  theme(legend.position = c(0.2, 0.3))

ggsave("paper-figure1.png", p, width = 7, height = 6.2, dpi = 300, bg = "white")
