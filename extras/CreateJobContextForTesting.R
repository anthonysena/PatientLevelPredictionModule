# Create a job context for testing purposes
library(Strategus)
library(dplyr)
library(Eunomia)
source("SettingsFunctions.R")

# Generic Helpers ----------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

# Sample Data Helpers ----------------------------
getSampleCohortDefintionSet <- function() {
  sampleCohorts <- CohortGenerator::createEmptyCohortDefinitionSet()
  eunomiaCohorts <- CohortGenerator::readCsv(file = system.file("settings/CohortsToCreate.csv", package = "Eunomia"),
                                             warnOnCaseMismatch = FALSE)
  
  emptyCohortDefinition <- "{\"ConceptSets\":[],\"PrimaryCriteria\":{\"CriteriaList\":[],\"ObservationWindow\":{\"PriorDays\":0,\"PostDays\":0},\"PrimaryCriteriaLimit\":{\"Type\":\"First\"}},\"QualifiedLimit\":{\"Type\":\"First\"},\"ExpressionLimit\":{\"Type\":\"First\"},\"InclusionRules\":[],\"CensoringCriteria\":[],\"CollapseSettings\":{\"CollapseType\":\"ERA\",\"EraPad\":0},\"CensorWindow\":{}}"
  # Grab the first 3 cohorts
  eunomiaCohorts <- eunomiaCohorts[eunomiaCohorts$cohortId <= 3,]
  for (i in 1:nrow(eunomiaCohorts)) {
    cohortId <- eunomiaCohorts$cohortId[i]
    cohortName <- eunomiaCohorts$name[i]
    cohortJson <- emptyCohortDefinition
    sampleCohorts <- rbind(sampleCohorts, data.frame(
      cohortId = cohortId,
      cohortName = cohortName,
      cohortDefinition = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  sampleCohorts <- apply(sampleCohorts, 1, as.list)
  return(sampleCohorts)
}

createCohortSharedResource <- function(cohortDefinitionSet) {
  sharedResource <- list(cohortDefinitions = cohortDefinitionSet)
  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}

# design the study (example below is 2 models with different Ts but same O)
modelDesignList <- list(
  PatientLevelPrediction::createModelDesign(
    targetId = 1, 
    outcomeId = 3, 
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), 
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
      riskWindowStart = 1, 
      riskWindowEnd = 365
      ), 
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = T, 
      useDemographicsAgeGroup = T
      ), 
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), 
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(), 
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(), 
    runCovariateSummary = T
    ),
  
  PatientLevelPrediction::createModelDesign(
    targetId = 2, 
    outcomeId = 3, 
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), 
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
      riskWindowStart = 1, 
      riskWindowEnd = 365
    ), 
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = T, 
      useDemographicsAgeGroup = T
    ), 
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(), 
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(), 
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(), 
    runCovariateSummary = T
  )
  
)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(
    createCohortSharedResource(getSampleCohortDefintionSet())
  ) %>%
  Strategus::addModuleSpecifications(
    createPatientLevelPredictionModuleSpecifications(
      modelDesignList = modelDesignList
    )
  )
executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "dummy",
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
  workFolder = "dummy",
  resultsFolder = "dummy",
  minCellCount = 5
)

# Job Context ----------------------------
module <- "PatientLevelPredictionModule"
moduleIndex <- 1
moduleExecutionSettings <- executionSettings
moduleExecutionSettings$workSubFolder <- "dummy"
moduleExecutionSettings$resultsSubFolder <- "dummy"
moduleExecutionSettings$databaseId <- 123
jobContext <- list(
  sharedResources = analysisSpecifications$sharedResources,
  settings = analysisSpecifications$moduleSpecifications[[moduleIndex]]$settings,
  moduleExecutionSettings = moduleExecutionSettings
)
saveRDS(jobContext, "tests/testJobContext.rds")