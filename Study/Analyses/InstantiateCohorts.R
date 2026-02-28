## Base cohort
logMessage("Create base cohort")
cdm$breast_cancer_all <- conceptCohort(
  cdm = cdm,
  conceptSet = breastCancer,
  name = "breast_cancer_all",
  exit = "event_start_date"
)

## Add inclusion criteria
logMessage("Apply inclusion criteria")
cdm$breast_cancer_char <- cdm$breast_cancer_all |> 
  requireIsFirstEntry(name = "breast_cancer_char") |>
  exitAtObservationEnd() |>
  requireAge(ageRange = c(18, 49)) |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireInDateRange(dateRange = studyPeriod) |>
  addDemographics(
    age = FALSE,
    ageGroup = ageGroups,
    sex = TRUE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "breast_cancer_char"
  ) |>
  mutate(year_group = paste0(
    as.integer(floor(get_year(cohort_start_date) / 5) * 5),
    " to ",
    as.integer(floor(get_year(cohort_start_date) / 5) * 5 + 4)
  )) |>
  compute(name = "breast_cancer_char")

##Cohort Prevalence
logMessage("Create prevalence cohorts")
cdm$breast_cancer_prev <- cdm$breast_cancer_all |>
  copyCohorts(n = 4, name = "breast_cancer_prev") |>
  renameCohort(newCohortName = c(
    "breast_cancer_end", "breast_cancer_1y", "breast_cancer_3y", 
    "breast_cancer_5y"
  )) |>
  exitAtObservationEnd(cohortId = "breast_cancer_end") |>
  padCohortEnd(days = 365, cohortId = "breast_cancer_1y") |>
  padCohortEnd(days = 3*365, cohortId = "breast_cancer_3y") |>
  padCohortEnd(days = 5*365, cohortId = "breast_cancer_5y")

logMessage("Add region")
cdm$breast_cancer_char <- cdm$breast_cancer_char |>
  renameCohort("breast_cancer_first") |>
  addRegion()
cdm$breast_cancer_all <- cdm$breast_cancer_all |>
  renameCohort("breast_cancer_all") |>
  addRegion()
cdm$breast_cancer_prev <- cdm$breast_cancer_prev |>
  addRegion()
