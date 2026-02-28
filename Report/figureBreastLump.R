
library(omopgenerics)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)

x <- importSummarisedResult(path = here("rawData")) |>
  filterSettings(result_type == "summarise_large_scale_characteristics") |>
  tidy() |>
  filter(
    concept_id == "80767", 
    sex == "Female",
    cohort_name == "breast_cancer_first",
  ) |>
  mutate(
    window = factor(
      x = variable_level,
      levels = c("-inf to -366", "-365 to -31", "-30 to -1")
    ),
    region = factor(
      x = region,
      levels = c("overall", "England", "Northern Ireland", "Scotland", "Wales")
    ),
    year_group = factor(
      x = year_group,
      levels = c("overall", "2000 to 2004", "2005 to 2009", "2010 to 2014", "2015 to 2019", "2020 to 2024")
    ),
    age_group = factor(
      x = age_group,
      levels = c("overall", "18 to 39", "40 to 49")
    )
  ) |>
  select("region", "year_group", "age_group", "window", "percentage")

combs <- expand_grid(
  region = factor(levels(x$region)),
  year_group = factor(levels(x$year_group)),
  age_group = factor(levels(x$age_group)),
  window = factor(levels(x$window))
)
x <- x |>
  full_join(combs, by = c("region", "year_group", "age_group", "window")) |>
  mutate(percentage = coalesce(percentage, 0))

ggplot(
  data = x,
  mapping = aes(x = window, y = percentage, colour = age_group)
) +
  geom_point() +
  geom_line() +
  facet_grid(region ~ year_group)
