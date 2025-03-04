# Start
start_time <- Sys.time()
outputFolder <-  here::here("Results")

logfile <- file.path( paste0(outputFolder, 
  "/log_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = logfile, append = TRUE)
  cli::cli_inform(paste(Sys.time(), "-", message, "\n"))
}

log_message("Start time recorded. Code version: 1.0.7")

tableName <- c("observation_period", "visit_occurrence", "condition_occurrence", "drug_exposure", "procedure_occurrence", 
               "device_exposure", "measurement" , "observation", "death")
sex <- TRUE # FALSE
ageGroup <- list(c(0,19), c(20, 39),c(40, 59), c(60, 79), c(80, Inf) ) 

dateRange <- as.Date(c("2012-01-01", NA))

# Snapshot
log_message("Getting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm) 

# Population Characteristics
log_message("Getting population characteristics")

cdm <- omopgenerics::bind(
  CohortConstructor::demographicsCohort(cdm, "population_1", sex = "Both"),
  CohortConstructor::demographicsCohort(cdm, "population_2", sex = "Both", ageRange = ageGroup),
  name = "population"
)


set <- omopgenerics::settings(cdm$population) |>
  dplyr::mutate(cohort_name = tolower(dplyr::if_else(
    is.na(.data$age_range), "general_population", paste0("age_group_", .data$age_range)
  ))) |>
  dplyr::select("cohort_definition_id", "cohort_name")

result_populationCharacteristics <- cdm$population |>
  omopgenerics::newCohortTable(cohortSetRef = set, .softValidation = TRUE) |>
  CohortConstructor::trimToDateRange(dateRange = dateRange) |>
  PatientProfiles::addSexQuery() |>
  CohortCharacteristics::summariseCharacteristics(
    strata = list("sex"), 
    estimates = list(
      date = c("min", "q25", "median", "q75", "max"),
      numeric = c("min", "q25", "median", "q75", "max", "mean", "sd", "density"),
      categorical = c("count", "percentage"),
      binary = c("count", "percentage")
    )
  )

omopgenerics::dropSourceTable(cdm = cdm, c("population_1", "population_2", "population"))

# Summarise missing data
log_message("Summarising missing data")
result_missingData <- OmopSketch::summariseMissingData(cdm ,
                                                       omopTableName = tableName, 
                                                       sex = sex, 
                                                       ageGroup = ageGroup, 
                                                       year = TRUE, 
                                                       dateRange = dateRange)
                                       



# Summarise concept counts
log_message("Summarising concept id counts")
result_conceptIdCount <- OmopSketch::summariseConceptIdCounts(cdm, 
                                                              omopTableName = tableName, 
                                                              sex = sex, 
                                                              ageGroup = ageGroup, 
                                                              year = TRUE, 
                                                              dateRange = dateRange)

# Summarise clinical records
log_message("Summarising clinical records")
result_clinicalRecords<- OmopSketch::summariseClinicalRecords(cdm, 
                                                  omopTableName = tableName, 
                                                  sex = sex, 
                                                  ageGroup = ageGroup,
                                                  dateRange = dateRange) 

# Summarize record counts
log_message("Summarising record counts")
result_recordCounts <- OmopSketch::summariseRecordCount(cdm,  tableName,
                                                   sex = sex,
                                                   ageGroup = ageGroup,
                                                   interval = "years",
                                                   dateRange = dateRange)




# Summarize in observation records
log_message("Summarising in observation records and person-days")
result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                           output = c("records","person-days"), 
                                                           interval = "years",
                                                           sex = sex,
                                                           ageGroup = ageGroup,
                                                           dateRange = dateRange) 




# Summarise observation period
log_message("Summarising observation period")
result_observationPeriod <- OmopSketch::summariseObservationPeriod(cdm$observation_period, 
                                                                   sex = sex, 
                                                                   ageGroup = ageGroup, 
                                                                   dateRange = dateRange)

log_message("Summarising missing data - person table")

result_missingDataPerson <- OmopSketch::summariseMissingData(cdm,
                                                             omopTableName = "person")
# Combine results and export
result <- omopgenerics::bind(snapshot, result_populationCharacteristics, result_missingData, result_conceptIdCount, result_clinicalRecords, result_recordCounts, result_inObservation, result_observationPeriod, result_missingDataPerson)
omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, path = outputFolder, fileName = paste0(
  "result_characterisation_", dbName, ".csv"))



# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Close connection
CDMConnector::cdmDisconnect(cdm)
log_message("Database connection closed")

# Zip the results
log_message("Zipping results") 

files_to_zip <- list.files(outputFolder)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, dbName)]

zip::zip(zipfile = file.path(paste0(
  outputFolder, "/results_characterisation_", dbName, ".zip"
)),
files = files_to_zip,
root = outputFolder)

