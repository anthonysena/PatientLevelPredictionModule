library(testthat)
library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()

workFolder <- tempfile("work")
dir.create(workFolder)
resultsfolder <- tempfile("results")
dir.create(resultsfolder)
jobContext <- readRDS("tests/testJobContext.rds")
jobContext$moduleExecutionSettings$workSubFolder <- workFolder
jobContext$moduleExecutionSettings$resultsSubFolder <- resultsfolder
jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails

test_that("Run module", {
  invisible(Eunomia::createCohorts(connectionDetails = connectionDetails))
  source("Main.R")
  #debugonce(execute)
  execute(jobContext)
  resultsFiles <- list.files(resultsfolder)
  expect_true("plp_cohort_definition.csv" %in% resultsFiles)
})

unlink(workFolder)
unlink(resultsfolder)
unlink(connectionDetails$server())
