
load(here::here("data", "studyData.RData"))

x <- data$prevalence |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    prevalence = prevalence * 10000,
    prevalence_95CI_lower = prevalence_95CI_lower * 10000,
    prevalence_95CI_upper = prevalence_95CI_upper * 10000,
  )

# point vs period prevalence
xp <- x |>
  dplyr::filter(
    outcome_cohort_name == "breast_cancer_end",
    region == "overall",
    denominator_age_group == "18 to 49",
    variable_name == "Outcome",
    denominator_sex == "Both"
  ) |>
  dplyr::mutate(year = clock::get_year(as.Date(prevalence_start_date)))
p1 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = prevalence,
    ymin = prevalence_95CI_lower,
    ymax = prevalence_95CI_upper,
    colour = analysis_type
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(color = "Prevalence type", x = "Year", y = "Prevalence per 10,000")

# male vs female
xp <- x |>
  dplyr::filter(
    outcome_cohort_name == "breast_cancer_end",
    region == "overall",
    denominator_age_group == "18 to 49",
    variable_name == "Outcome",
    analysis_type == "point prevalence"
  ) |>
  dplyr::mutate(
    year = clock::get_year(as.Date(prevalence_start_date)),
    sex = factor(denominator_sex, levels = c("Both", "Female", "Male"))
  )
p2 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = prevalence,
    ymin = prevalence_95CI_lower,
    ymax = prevalence_95CI_upper,
    colour = sex
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(color = "Sex", x = "Year", y = "Point-prevalence per 10,000") +
  ggplot2::scale_colour_manual(values = c(
    "Both" = "#4D4D4D",
    "Female" = "#F8766D",
    "Male" = "#619CFF"
  ))

# age groups
xp <- x |>
  dplyr::filter(
    outcome_cohort_name == "breast_cancer_end",
    region == "overall",
    variable_name == "Outcome",
    denominator_sex == "Both",
    analysis_type == "point prevalence"
  ) |>
  dplyr::mutate(
    year = clock::get_year(as.Date(prevalence_start_date)),
    age = factor(denominator_age_group, levels = c("18 to 49", "18 to 39", "40 to 49"))
  )
p3 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = prevalence,
    ymin = prevalence_95CI_lower,
    ymax = prevalence_95CI_upper,
    colour = age
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(color = "Age group", x = "Year", y = "Point-prevalence per 10,000") +
  ggplot2::scale_colour_manual(values = c(
    "18 to 49" = "#4D4D4D",
    "18 to 39" = "#F8766D",
    "40 to 49" = "#619CFF"
  ))

# definitions
xp <- x |>
  dplyr::filter(
    region == "overall",
    denominator_age_group == "18 to 49",
    variable_name == "Outcome",
    denominator_sex == "Both",
    analysis_type == "point prevalence"
  ) |>
  dplyr::mutate(
    year = clock::get_year(as.Date(prevalence_start_date)),
    end = dplyr::case_when(
      outcome_cohort_name == "breast_cancer_end" ~ "Forever",
      outcome_cohort_name == "breast_cancer_1y" ~ "1 year",
      outcome_cohort_name == "breast_cancer_3y" ~ "3 years",
      outcome_cohort_name == "breast_cancer_5y" ~ "5 years"
    ) |>
      factor(levels = c("Forever", "1 year", "3 years", "5 years"))
  )
p4 <- ggplot2::ggplot(
  data = xp, 
  mapping = ggplot2::aes(
    x = year,
    y = prevalence,
    ymin = prevalence_95CI_lower,
    ymax = prevalence_95CI_upper,
    colour = end
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(color = "Prevalence definition", x = "Year", y = "Point-prevalence per 10,000")

library(ggplot2)
library(patchwork)
library(scales)   # for pretty_breaks()

# --- 1) determine a common x-range (automatically try to extract from built plots) ---
get_xrange <- function(p){
  gb <- ggplot_build(p)
  # ggplot2 internals vary across versions; try panel_params then fallback to data
  rng <- NULL
  if (!is.null(gb$layout$panel_params) && length(gb$layout$panel_params) >= 1) {
    rng <- gb$layout$panel_params[[1]]$x.range
  }
  if (is.null(rng)) {
    # fallback: try the x column in the first layer's data
    dd <- gb$data[[1]]
    if (!is.null(dd$x)) rng <- range(dd$x, na.rm = TRUE)
  }
  rng
}

xr <- range(
  get_xrange(p1),
  get_xrange(p2),
  get_xrange(p3),
  get_xrange(p4),
  na.rm = TRUE
)

# If extraction failed, set xr manually:
# xr <- c(0, 100)  

# common breaks
brks <- pretty_breaks(n = 6)(xr)

# apply the same x scale to each plot (keeps y scales independent)
p1x <- p1 + scale_x_continuous(limits = xr, breaks = brks)
p2x <- p2 + scale_x_continuous(limits = xr, breaks = brks)
p3x <- p3 + scale_x_continuous(limits = xr, breaks = brks)
p4x <- p4 + scale_x_continuous(limits = xr, breaks = brks)

# Ensure each plot shows its legend (optionally set position)
p1x <- p1x + theme(legend.position = "right")
p2x <- p2x + theme(legend.position = "right")
p3x <- p3x + theme(legend.position = "right")
p4x <- p4x + theme(legend.position = "right")

# Arrange in 2x2 while keeping each legend
layout_plot <- (p1x + p2x) / (p3x + p4x) + plot_layout(guides = "keep")

ggsave("Figure2.png", plot = layout_plot, height = 7, width = 10)
