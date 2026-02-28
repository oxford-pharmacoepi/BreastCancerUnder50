
logMessage("Create denominator")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_cohort",
  cohortDateRange = studyPeriod,
  ageGroup = list(c(18, 49), c(18, 39), c(40, 49)),
  sex = c("Male", "Female", "Both"),
  daysPriorObservation = 365
)

logMessage("Add region to denominator")
cdm$denominator_cohort <- cdm$denominator_cohort |>
  addRegion()

logMessage("Estimate incidence")
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_cohort",  
  outcomeTable = "breast_cancer_all", 
  interval = c("years", "overall"), 
  strata = list("region"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE   
)

logMessage("Estimate point prevalence")
pointPrev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_cohort",
  outcomeTable = "breast_cancer_prev",
  interval = "years",
  strata = list("region")
)

logMessage("Estimate period prevalence")
periodPrev <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_cohort",
  outcomeTable = "breast_cancer_prev",
  interval = "years",
  completeDatabaseIntervals = TRUE,
  fullContribution = TRUE,
  strata = list("region")
)
