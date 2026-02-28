###CODE TO RUN

##Libraries
library(DBI)
library(CDMConnector)
library(dplyr)
library(omopgenerics)
library(OmopSketch)
library(CDMConnector)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(here)
library(IncidencePrevalence)
library(PatientProfiles)
library(clock)
library(CohortSurvival)

##Connecting to db

##Populate your connection details, see for examples https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
con <- dbConnect("....")

##db details
cdmSchema <- "..." # schema with the omop tables
writeSchema <- "..." # schema with writing permissions
writePrefix <- "..." # to avoid collision with other runs
dbName <- "..." # name of the database
minCellCount <- 5

source(here("RunStudy.R"))

cli::cli_alert_success("Study finished :)")
