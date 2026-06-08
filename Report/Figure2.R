
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
    denominator_sex == "Female"
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
  ggplot2::labs(color = "", x = "Year", y = "Prevalence per 10,000")

# # male vs female
# xp <- x |>
#   dplyr::filter(
#     outcome_cohort_name == "breast_cancer_end",
#     region == "overall",
#     denominator_age_group == "18 to 49",
#     variable_name == "Outcome",
#     analysis_type == "point prevalence"
#   ) |>
#   dplyr::mutate(
#     year = clock::get_year(as.Date(prevalence_start_date)),
#     sex = factor(denominator_sex, levels = c("Both", "Female", "Male"))
#   )
# p2 <- ggplot2::ggplot(
#   data = xp,
#   mapping = ggplot2::aes(
#     x = year,
#     y = prevalence,
#     ymin = prevalence_95CI_lower,
#     ymax = prevalence_95CI_upper,
#     colour = sex
#   )
# ) +
#   ggplot2::geom_point() +
#   ggplot2::geom_errorbar() +
#   ggplot2::theme(legend.position = "top") +
#   ggplot2::labs(color = "Sex", x = "Year", y = "Point-prevalence per 10,000") +
#   ggplot2::scale_colour_manual(values = c(
#     "Both" = "#4D4D4D",
#     "Female" = "#F8766D",
#     "Male" = "#619CFF"
#   ))

# age groups
xp <- x |>
  dplyr::filter(
    outcome_cohort_name == "breast_cancer_end",
    region == "overall",
    variable_name == "Outcome",
    denominator_sex == "Female",
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
  ggplot2::labs(color = "", x = "Year", y = "Point-prevalence per 10,000") +
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
    denominator_sex == "Female",
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
  ggplot2::labs(color = "", x = "Year", y = "Point-prevalence per 10,000")

library(patchwork)

p <- p1+p3+p4

ggsave("Figure2.png", plot = p, height = 4, width = 10, dpi = 300)
