logMessage("Create death cohort")
cdm$death_cohort_bc <- deathCohort(
  cdm = cdm,
  name = "death_cohort_bc"
)

logMessage("Single event survival")
survival <- estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "breast_cancer_char",
  outcomeCohortTable = "death_cohort_bc",
  followUpDays = 10 * 365,
  strata = combineStrata(c("sex", "age_group", "region", "year_group"))
)
