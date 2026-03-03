
load(here::here("data", "studyData.RData"))

x <- data$summarise_large_scale_characteristics |>
  omopgenerics::tidy() |>
  dplyr::filter(cohort_name %in% c("breast_cancer_first_sampled", "breast_cancer_first_matched")) |>
  dplyr::select(!c("type", "analysis", "count")) |>
  dplyr::mutate(percentage = percentage / 100) |>
  tidyr::pivot_wider(names_from = "cohort_name", values_from = "percentage") |>
  dplyr::rename(pc = "breast_cancer_first_sampled", pn = "breast_cancer_first_matched") |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("p"), \(x) dplyr::coalesce(x, 0.0049))) |>
  dplyr::mutate(smd = dplyr::if_else(pc == pn, 0, (pc - pn) / sqrt((pc * (1- pc) + pn * (1 - pn)) / 2)))

x |>
  dplyr::filter(age_group == "overall", region == "overall", sex == "overall", year_group == "overall") |>
  dplyr::filter(abs(smd) > 0.1) |>
  dplyr::arrange(variable_level, -abs(smd)) |>
  dplyr::mutate(
    covariate = paste0(variable_name, " (", concept_id, ")"),
    pc = dplyr::if_else(pc == 0.0049, "<0.5%", paste0(sprintf("%.1f", 100 * pc), "%")),
    pn = dplyr::if_else(pn == 0.0049, "<0.5%", paste0(sprintf("%.1f", 100 * pn), "%")),
    smd = sprintf("%.3f", smd)
  ) |>
  dplyr::select(covariate, variable_level, pc, pn, smd, table_name) |>
  readr::write_csv("lsc.csv")

xp <- x |>
  dplyr::filter(age_group == "overall", region == "overall", sex == "overall", year_group == "overall") |>
  dplyr::mutate(
    colour = dplyr::if_else(abs(smd) > 0.1, "|SMD| > 0.1", "|SMD| ≤ 0.1"),
    domain = dplyr::case_when(
      table_name == "condition_occurrence" ~ "Conditions",
      table_name == "drug_era" ~ "Drugs",
      table_name == "measurement" ~ "Measurements",
      table_name == "observation" ~ "Observations",
      table_name == "procedure_occurrence" ~ "Procedures"
    )
  )

p <- xp |>
  ggplot2::ggplot(mapping = ggplot2::aes(x = pn, y = pc, colour = colour)) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    color = "lightgray",
    linewidth = 0.5
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_grid(variable_level ~ domain) +
  ggplot2::scale_colour_manual(values = c(
    "|SMD| > 0.1" = "#F8766D",
    "|SMD| ≤ 0.1" = "#4D4D4D"
  )) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::labs(y = "Prevalence in Breast Cancer patients (%)", x = "Prevalence in no Breast Cancer patients (%)", colour = "") +
  ggplot2::theme(legend.position = "top") +
  ggplot2::geom_point(data = xp |> dplyr::filter(concept_id == "80767"), colour = "#00BFC4", shape = 21, fill = NA, size = 3, stroke = 2)

ggsave("Figure7.png", plot = p, height = 7, width = 10)
