
load(here::here("data", "studyData.RData"))

x <- data$incidence |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    Region = dplyr::if_else(region == "overall", "Overall", region) |>
      factor(c("Overall", "England", "Northern Ireland", "Scotland", "Wales")),
    year = clock::get_year(as.Date(incidence_start_date))
  ) |>
  dplyr::filter(
    variable_name == "Outcome",
    analysis_interval == "years",
    denominator_sex == "Female"
  )

# by region
xp <- x |>
  dplyr::filter(
    denominator_age_group == "18 to 49",
    region != "overall"
  )
p1 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = incidence_100000_pys,
    ymin = incidence_100000_pys_95CI_lower,
    ymax = incidence_100000_pys_95CI_upper
  )
) +
  ggplot2::geom_point(colour = "#4C78A8") +
  ggplot2::geom_errorbar(colour = "#4C78A8") +
  ggplot2::geom_ribbon(alpha = 0.2, colour = NA, fill = "#4C78A8") +
  ggplot2::facet_wrap(. ~ Region, ncol = 2) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = "Year", y = "Incidence (per 100,000 py)") +
  ggplot2::scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  ggplot2::coord_cartesian(ylim = c(0, 120))
  
# age groups
xp <- x |>
  dplyr::filter(
    region == "overall"
  ) |>
  dplyr::mutate(
    age = factor(denominator_age_group, c("18 to 49", "18 to 39", "40 to 49"))
  )
p3 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = incidence_100000_pys,
    ymin = incidence_100000_pys_95CI_lower,
    ymax = incidence_100000_pys_95CI_upper,
    colour = age,
    fill = age
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::geom_ribbon(alpha = 0.2, colour = NA) +
  ggplot2::ylim(c(0, 200)) +
  ggplot2::theme(legend.position = "right") +
  ggplot2::labs(x = "Year", y = "Incidence (per 100,000 py)", colour = "Age group", fill = "Age group") +
  ggplot2::scale_colour_manual(values = c(
    "18 to 49" = "#4D4D4D",
    "18 to 39" = "#F8766D",
    "40 to 49" = "#619CFF"
  )) +
  ggplot2::scale_fill_manual(values = c(
    "18 to 49" = "#4D4D4D",
    "18 to 39" = "#F8766D",
    "40 to 49" = "#619CFF"
  )) +
  ggplot2::theme(legend.position = "bottom")

library(ggplot2)
library(patchwork)

layout_plot <- p3 + p1 +
  plot_layout(widths = c(1, 1.4)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave("Figure1.png", plot = layout_plot, height = 4.5, width = 10)
