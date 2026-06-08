
load(here::here("data", "studyData.RData"))

library(omopgenerics)
library(dplyr)
library(gt)
library(tidyr)

surv <- data$survival |>
  filterSettings(result_type == "survival_estimates") |>
  filter(additional_level %in% c("365", "1825", "3650")) |>
  tidy() |>
  filter(sex == "Female", region == "overall", age_group != "overall", year_group != "overall") |>
  mutate(
    survival = sprintf("%.1f (%.1f; %.1f)", 100 * estimate, 100 * estimate_95CI_lower, 100 *estimate_95CI_upper),
    age_group = case_when(
      age_group == "overall" ~ "18–49 (overall)",
      age_group == "18 to 39" ~ "18–39",
      age_group == "40 to 49" ~ "40–49"
    ) |>
      factor(levels = c("18–49 (overall)", "18–39", "40–49")),
    time = case_when(
      time == "365" ~ "One-year Survival (%)",
      time == "1825" ~ "Five-year Survival (%)",
      time == "3650" ~ "Ten-year Survival (%)"
    ),
    year_group = if_else(
      year_group == "overall", "2000 to 2024 (overall)", year_group
    ) |>
      factor(levels = c("2000 to 2024 (overall)", "2000 to 2004", "2005 to 2009", "2010 to 2014", "2015 to 2019", "2020 to 2024"))
  ) |>
  select(year_group, age_group, time, survival) |>
  pivot_wider(values_from = "survival", names_from = "time") |>
  gt(groupname_col = "year_group")

gtsave(surv, "Table2.docx")
