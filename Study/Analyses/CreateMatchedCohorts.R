
cdm$breast_cancer_matched <- cdm$breast_cancer_char |>
  select(!c("age_group", "sex", "year_group", "region")) |>
  matchCohorts(
    keepOriginalCohorts = TRUE,
    name = "breast_cancer_matched", 
    cohortId = NULL,
    matchSex = TRUE,
    matchYearOfBirth = TRUE,
    ratio = 1
  ) |>
  addDemographics(
    age = FALSE,
    ageGroup = ageGroups,
    sex = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "breast_cancer_matched"
  ) |>
  mutate(year_group = paste0(
    as.integer(floor(get_year(cohort_start_date) / 5) * 5),
    " to ",
    as.integer(floor(get_year(cohort_start_date) / 5) * 5 + 4)
  )) |>
  addRegion()
