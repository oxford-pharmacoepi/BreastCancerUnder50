
library(omopgenerics)
library(here)
library(CodelistGenerator)

##Getting Code List
breastcancer_codes <- getCandidateCodes(
  cdm =  cdm,
  keywords = c("malignant neoplasm of breast",
               "malignant tumor of breast"),
  exclude = c("melanoma",
              "lymphoma",
              "sarcoma",
              "secondary",
              "metastasis",
              "lymphocytic",
              "benign",
              "hodgkin",
              "neuroendocrine",
              "rhabdomyosarcoma",
              "angiomyosarcoma",
              "fibrosarcoma",
              "leiomyosarcoma",
              "hemangiosarcoma",
              "pseudosarcomatous",
              "carcinosarcoma",
              "leukemia",
              "blastoma",
              "T-cell",
              "atelectasis",
              "plasmacytoma",
              "mesenchymoma",
              "heavy chain disease" ,
              "ectomesenchymoma",
              "myeloproliferative",
              "sezary",
              "lymphoid",
              "epithelioid hemangioendothelioma"),
  domains = c("Condition", "Observation")
) 

##Excluding Codes from Codelist (from diagnostics)
codetoexclude <- c(44807962, 45763684, 4154629, 4116234, 4112853)

breastcancer_cs <- list(breast_cancer = setdiff(breastcancer_codes$concept_id, codetoexclude)) |>
  newCodelist()

exportCodelist(breastcancer_cs, here("Codelists"), type = "csv")

# convert json files to csv
importConceptSetExpression(path = here("Codelists", "Conditions")) |>
  # convert to codelist
  validateConceptSetArgument(cdm = cdm) |>
  # export to codelist
  exportCodelist(path = here("Codelists", "Conditions"), type = "csv")

importConceptSetExpression(path = here("Codelists", "Medications")) |>
  # convert to codelist
  validateConceptSetArgument(cdm = cdm) |>
  # export to codelist
  exportCodelist(path = here("Codelists", "Medications"), type = "csv")
