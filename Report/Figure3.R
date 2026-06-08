
load(here::here("data", "studyData.RData"))

library(omopgenerics)
library(dplyr)
library(gt)
library(tidyr)
library(ggplot2)
library(scales)

surv <- data$survival |>
  filterSettings(result_type == "survival_estimates") |>
  filterStrata(
    sex == "Female", 
    region == "overall",
    year_group != "overall"
  ) |>
  tidy() |>
  mutate(
    time = as.numeric(time) / 365.25,
    age_group = case_when(
      age_group == "overall" ~ "18 to 49",
      age_group == "18 to 39" ~ "18 to 39",
      age_group == "40 to 49" ~ "40 to 49"
    ) |>
      factor(levels = c("18 to 49", "18 to 39", "40 to 49")),
    year_group = if_else(
      year_group == "overall", "2000 to 2024 (overall)", year_group
    ) |>
      factor(levels = c("2000 to 2024 (overall)", "2000 to 2004", "2005 to 2009", "2010 to 2014", "2015 to 2019", "2020 to 2024"))
  )

p <- ggplot(data = surv, mapping = aes(x = time, y = estimate, ymin = estimate_95CI_lower, ymax = estimate_95CI_upper, colour = age_group, fill = age_group)) +
  geom_step() +
  facet_grid(. ~ year_group) +
  geom_ribbon(colour = NA, alpha = 0.3) +
  scale_colour_manual(values = c(
    "18 to 49" = "#4D4D4D",
    "18 to 39" = "#F8766D",
    "40 to 49" = "#619CFF"
  )) +
  scale_fill_manual(values = c(
    "18 to 49" = "#4D4D4D",
    "18 to 39" = "#F8766D",
    "40 to 49" = "#619CFF"
  )) +
  scale_y_continuous(labels = label_percent()) +
  labs(color = "", fill = "", x = "Time (years)", y = "Survival probability (%)") +
  theme(legend.position = "top")

ggsave("Figure3.png", plot = p, height = 4, width = 10, dpi = 300)

p <- ggplot(data = surv, mapping = aes(x = time, y = estimate, ymin = estimate_95CI_lower, ymax = estimate_95CI_upper, colour = year_group, fill = year_group)) +
  geom_step() +
  facet_grid(. ~ age_group) +
  geom_ribbon(colour = NA, alpha = 0.3) +
  scale_y_continuous(labels = label_percent()) +
  labs(color = "", fill = "", x = "Time (years)", y = "Survival probability (%)") +
  theme(legend.position = "top")

ggsave("Figure4.png", plot = p, height = 4, width = 10, dpi = 300)
