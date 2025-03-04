start_time <- Sys.time()
outputFolder <-  here::here("Results")

logfile <- file.path( paste0(outputFolder, 
                             "/log_addendum_", dbName, "_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"),".txt"
))

log_message <- function(message) {
  cat(paste(Sys.time(), "-", message, "\n"), file = logfile, append = TRUE)
  cli::cli_inform(paste(Sys.time(), "-", message, "\n"))
}

log_message("Start time recorded")


sex <- TRUE 
ageGroup <- list(c(0,19), c(20, 39),c(40, 59), c(60, 79), c(80, Inf)) 
ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup, ageGroupName = "")[[1]]
dateRange <- as.Date(c("2012-01-01", NA))

log_message("Getting population characteristics")


cdm <- omopgenerics::bind(
  CohortConstructor::demographicsCohort(cdm, "population_1", sex = "Both"),
  CohortConstructor::demographicsCohort(cdm, "population_2", sex = "Both", ageRange = ageGroup),
  name = "population"
)

omopgenerics::dropSourceTable(cdm = cdm, dplyr::starts_with("population_1"))
omopgenerics::dropSourceTable(cdm = cdm, dplyr::starts_with("population_2"))


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

log_message("Summarising in observation records and person-days")

result_inObservation <- OmopSketch::summariseInObservation(cdm$observation_period, 
                                                           output = c("records","person-days"), 
                                                           interval = "years",
                                                           sex = sex,
                                                           ageGroup = ageGroup,
                                                           dateRange = dateRange) 
log_message("Summarising missing data - person table")

result_missingDataPerson <- OmopSketch::summariseMissingData(cdm,
                                                             omopTableName = "person")

result <- omopgenerics::bind(result_populationCharacteristics, result_inObservation, result_missingDataPerson)
omopgenerics::exportSummarisedResult(result, minCellCount = minCellCount, path = outputFolder, fileName = "result_addendum_{cdm_name}.csv")



# Calculate duration and log
dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
log_message(paste("Study code finished. Code ran in", floor(dur / 60), "min and", dur %% 60 %/% 1, "sec"))

# Close connection
CDMConnector::cdmDisconnect(cdm)
log_message("Database connection closed")

# Zip the results
log_message("Zipping results") 

files_to_zip <- list.files(outputFolder)
files_to_zip <- files_to_zip[stringr::str_detect(files_to_zip, "addendum")]

zip::zip(zipfile = file.path(paste0(
  outputFolder, "/results_addendum_", dbName, ".zip"
)),
files = files_to_zip,
root = outputFolder)
