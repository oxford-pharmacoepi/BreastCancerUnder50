logMessage("Create characterisation medications cohorts")
cdm$medications <- conceptCohort(
  cdm = cdm,
  conceptSet = medications, 
  name = "medications",
  exit = "event_end_date",
  subsetCohort = "breast_cancer_matched"
)

logMessage("Create characterisation conditions cohorts")
cdm$conditions <- conceptCohort(
  cdm = cdm,
  conceptSet = conditions, 
  name = "conditions",
  exit = "event_end_date",
  subsetCohort = "breast_cancer_matched"
)

logMessage("Characterise characteristics")
characteristics <- cdm$breast_cancer_matched |>
  summariseCharacteristics(
    strata = combineStrata(c("sex", "age_group", "region", "year_group")),
    ageGroup = ageGroups,      
    cohortIntersectFlag = list(
      "Conditions prior to index date" = list(
        targetCohortTable = "conditions",
        window = c(-Inf, -1)
      ),
      "Medications the prior year to index date" = list(
        targetCohortTable = "medications",
        window = c(-365, -1)
      )
    )
  )

logMessage("Large Scale Characteristics")
lsc <- cdm$breast_cancer_matched |>
  summariseLargeScaleCharacteristics(
    strata = combineStrata(c("age_group", "region", "year_group", "sex")),
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1)),
    eventInWindow = c("condition_occurrence", "observation", "procedure_occurrence", "measurement"),
    episodeInWindow = "drug_era"
  )
