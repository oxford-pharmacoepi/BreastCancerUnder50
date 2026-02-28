# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  summarise_large_scale_characteristics = list(result_type = "summarise_large_scale_characteristics"),
  incidence = list(result_type = "incidence"),
  incidence_attrition = list(result_type = "incidence_attrition"),
  prevalence = list(result_type = "prevalence"),
  prevalence_attrition = list(result_type = "prevalence_attrition"),
  survival = list(result_type = c("survival_summary", "survival_estimates", "survival_events", "survival_attrition")),
  summarise_log_file = list(result_type = "summarise_log_file")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
