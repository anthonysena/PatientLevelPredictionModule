# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CohortGeneratorModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  inherits(jobContext, 'list')

  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }
  
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  
  rlang::inform("Executing PLP")
  moduleInfo <- getModuleInfo()
  
  # Creating database details
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = jobContext$moduleExecutionSettings$connectionDetails, 
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cdmDatabaseName = jobContext$moduleExecutionSettings$connectionDetailsReference, 
    cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
    outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema, 
    outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
  )
  
  rlang::inform("Creating cohort definition set from job context")
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(sharedResources = jobContext$sharedResources)
  
  # run the models
  PatientLevelPrediction::runMultiplePlp(
    databaseDetails = databaseDetails, 
    modelDesignList = jobContext$settings, 
    cohortDefinitions = cohortDefinitionSet,
    saveDirectory = resultsFolder
  )
  
  # Export the results
  rlang::inform("Export data to csv files")

  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(resultsFolder, "sqlite","databaseFile.sqlite")
  )
    
  PatientLevelPrediction::extractDatabaseToCsv(
    connectionDetails = sqliteConnectionDetails, 
    databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
      resultSchema = 'main', # sqlite settings
      tablePrefix = '', # sqlite settings
      targetDialect = 'sqlite', 
      tempEmulationSchema = NULL
    ), 
    csvFolder = file.path(resultsFolder, 'results'),
    fileAppend = moduleInfo$TablePrefix
  )
  
  # Export the resultsDataModelSpecification.csv
  resultsDataModel <- CohortGenerator::readCsv(file = "resultsDataModelSpecification.csv",
                                               warnOnCaseMismatch = FALSE)
  newTableNames <- paste0(moduleInfo$TablePrefix, resultsDataModel$tableName)
  resultsDataModel$tableName <- newTableNames
  CohortGenerator::writeCsv(
    x = resultsDataModel,
    file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
    warnOnCaseMismatch = FALSE,
    warnOnFileNameCaseMismatch = FALSE,
    warnOnUploadRuleViolations = FALSE
  )  
  
  # Zip the results
  rlang::inform("Zipping csv files")
  DatabaseConnector::createZipFile(
    zipFile = file.path(resultsFolder, 'results.zip'),
    files = file.path(resultsFolder)
  )
}

# Private methods -------------------------
createCohortDefinitionSetFromJobContext <- function(sharedResources) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  for (i in 1:length(sharedResources)) {
    if (which(class(sharedResources[[i]]) %in% "CohortDefinitionSharedResources") > 0) {
      cohortDefinitions <- sharedResources[[i]]$cohortDefinitions
      break;
    }
  }
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- list()
  length(cohortDefinitionSet) <- length(cohortDefinitions)
  for(i in 1:length(cohortDefinitions)){
    cohortDefinitionSet[[i]] <- list(
      id = as.integer(cohortDefinitions[[i]]$cohortId),
      name = cohortDefinitions[[i]]$cohortName,
      expression = cohortDefinitions[[i]]$cohortDefinition,
      cohortName = cohortDefinitions[[i]]$cohortName,
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortDefinition = cohortDefinitions[[i]]$cohortDefinition
    )
  }
  
  return(cohortDefinitionSet)
}