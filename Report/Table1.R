
load(here::here("data", "studyData.RData"))

data$summarise_characteristics |>
  dplyr::mutate(
    estimate_value = dplyr::if_else(
      .data$estimate_type == "date",
      as.character(clock::get_year(as.Date(.data$estimate_value, "%Y-%m-%d"))),
      .data$estimate_value
    ),
    estimate_type = dplyr::if_else(
      .data$estimate_type == "date", "integer", .data$estimate_type
    ),
    variable_name = dplyr::case_when(
      .data$variable_name == "Cohort start date" ~ "Index year",
      .default = .data$variable_name
    )
  ) |>
  dplyr::filter(
    .data$variable_name %in% c(
      "Number subjects", "Index year", "Age", "Age group", "Sex", 
      "Prior observation", "Future observation", "Conditions prior to index date",
      "Medications the prior year to index date"
    ),
    .data$estimate_name %in% c("count", "percentage", "median", "q25", "q75"),
    .data$variable_name != "Conditions prior to index date" | .data$variable_level %in% c(
      "Family history breast cancer", "Breast lump", "Pain in breast", 
      "Obesity diagnosis", "Hypertension", "T 2 d", "Venous thromboembolism",
      "Depressive disorder", "Anxiety", "Anemia", "Uti", "Abdominal pain",
      "Cough", "Chest pain"
    )
  ) |>
  omopgenerics::filterGroup(cohort_name == "breast_cancer_first") |>
  omopgenerics::filterStrata(
    age_group == "overall", region == "overall", year_group == "overall"
  ) |>
  CohortCharacteristics::tableCharacteristics(header = "sex")
