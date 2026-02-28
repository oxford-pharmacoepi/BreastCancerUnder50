addRegion <- function(cohort) {
  name <- tableName(cohort)
  cdm <- cdmReference(cohort)
  
  cohort |>
    left_join(
      cdm$person |>
        select("person_id", "care_site_id") |>
        left_join(
          cdm$care_site |> 
            select("care_site_id", "location_id") |>
            left_join(
              cdm$location |> 
                select("location_id", "location_source_value") |>
                mutate(region = case_when(
                  location_source_value == "Wales" ~ "Wales",
                  location_source_value == "Scotland" ~ "Scotland",
                  location_source_value == "Northern Ireland" ~ "Northern Ireland",
                  .default = "England"
                )) |>
                select("location_id", "region"),
              by = "location_id"
            ) |> 
            select("care_site_id", "region"),
          by = "care_site_id"
        ) |>
        select("person_id", "region"),
      by = c("subject_id" = "person_id")
    ) |>
    mutate(region = coalesce(region, "unknown")) |>
    compute(name = name)
}
