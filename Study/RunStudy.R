logFile <- here("Results", "log_{date}_{time}.txt")
createLogFile(logFile = logFile)
studyPeriod <- as.Date(c("2000-01-01", "2024-12-31"))
ageGroups <- list("18 to 39" = c(18, 39), "40 to 49" = c(40, 49))

logMessage("Creating cdm_reference object")
cdm <- cdmFromCon(
  con = con, 
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = dbName
)

logMessage("Source reusable functions")
source(here("Analyses", "functions.R"))

logMessage("Import codelists")
breastCancer <- importCodelist(path = here("Codelists"), type = "csv")
conditions <- importCodelist(path = here("Codelists", "Conditions"), type = "csv")
medications <- importCodelist(path = here("Codelists", "Medications"), type = "csv")

logMessage("Extract snapshot")
snapshot <- summariseOmopSnapshot(cdm = cdm)

logMessage("Creating study cohorts")
source(here("Analyses", "InstantiateCohorts.R"))

logMessage("Export cohort code use")
codeUse <- summariseCohortCodeUse(
  x = breastCancer,
  cdm = cdm, 
  cohortTable = "breast_cancer_all", 
  timing = "entry",
  countBy = c("record", "person"),
  byConcept = TRUE
)

logMessage("Export cohort counts")
cohortCounts <- summariseCohortCount(cdm$breast_cancer_char)

logMessage("Export cohort attrition")
cohortAttrition <- summariseCohortAttrition(cdm$breast_cancer_char)

logMessage("Estimate Incidence and Prevalence")
source(here("Analyses", "IncidencePrevalence.R"))

logMessage("Estimate Survival")
source(here("Analyses", "Survival.R"))

logMessage("Creating matched cohorts")
source(here("Analyses", "CreateMatchedCohorts.R"))

logMessage("Characterise cohorts")
source(here("Analyses", "Characterisation.R"))

##Export Results
exportSummarisedResult(
  snapshot,
  codeUse,
  cohortCounts,
  cohortAttrition,
  inc,
  pointPrev,
  periodPrev,
  survival,
  characteristics,
  lsc,
  minCellCount = minCellCount,
  path = here("Results"),
  fileName = "bcu50_{cdm_name}_{date}.csv"
)
