# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of InfluenzaHospCohortDiag
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

# Generation additional table 1
#' @export
additionalTable1 <- function(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             oracleTempSchema = oracleTempSchema,
                             outputFolder = outputFolder,
                             databaseId = databaseId,
                             minCellCount = 10){
  start <- Sys.time()
  
  pathToCsv <- system.file("settings", "CohortsToCreateCovid.csv", package = "CovidHospCohortDiag")
  cohortsToCreate <- read.csv(pathToCsv)
  
  exportFolder <- file.path(outputFolder, "diagnosticsExport")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  #Defining concept IDs
  conditionGroupConceptIds <- 
    c(439777,4212540,201820,201254,201826,318800,192671,4030518,193782,
439727,432867,80180,4291005,4281232,197494,380378,81902,434557,440638,
432586,4182210,440383,435783,4279309,134057,381591,313217,
321319,381591,317576,321588,316139,316866,4185932,321052,
440417,4167085,4231363,444247,44784217,314054,438112,
443392,4147164,432571,4044013,
4112853,4180790,40481902,40493428,443388,40493428,200962,
4155297,3169522,255055008,363392002,
4154630,320136,317009,255573,255841,261325,441267,256449,
3185877,4322024,4197819,313459,40396500,4212886,433740,
140168,80809,255891,374919,4074815,201606,
81893,4253901,314963,4028942,40438630,40386324,
194992,443394,254443,134442,432295,192675,200762,76685,4103532,4137275)
    
  
drugGroupConceptIds <- c(21601822,
                           21602796,
                           21601964,
                           21603126,
                           21601782,
                           21604389,
                           21603932,
                           21604889,
                           21603929,
                           21603898,
                           21600663,
                           21601387,
                           21602028,
                           21601664,
                           21601744,
                           21601461,
                           21600046,
                           21603248,
                           21600712,
                           21600713,
                           21603890,
                           21601853,
                           21604254,
                           21604489,
                           21604752,
                           21601254,
                           21603907,
                           21603914,
                           21604686, #ANTIDEPRESSANTS
                           21600960 #ANTITHROMBOTIC AGENTS
  )
  
  drugIngredientConceptIds <- c(1777087,
                                1305058,
                                1101898,
                                964339,
                                40171288,
                                1545958,
                                1539403,
                                1551860,
                                1510813,
                                1549686,
                                1592085,
                                40165636,
                                1503297,
                                1177480,
                                1308842,
                                1317640,
                                40226742,
                                1367500,
                                1347384,
                                1346686,
                                1351557,
                                40235485,
                                1342439,
                                1334456,
                                1331235,
                                1373225,
                                1310756,
                                1308216,
                                1363749,
                                1341927,
                                1340128,
                                1335471
  )

  
  for( i in 1:nrow(cohortsToCreate)){
    cohortName <- cohortsToCreate$name[i]
    cohortId <- cohortsToCreate$cohortId[i]
    
    for(endDay in c(0,-1)){
      tableSpecification <- setTableSpecification(useDemographicsGender = T,
                                                  useDemographicsAge = T,
                                                  useDemographicsAgeGroup = T,
                                                  useDemographicsRace = T,
                                                  useDemographicsEthnicity = T,
                                                  useDemographicsIndexYear = T,
                                                  useDemographicsIndexMonth = T,
                                                  useDemographicsPriorObservationTime = FALSE,
                                                  useDemographicsPostObservationTime = FALSE,
                                                  useDemographicsTimeInCohort = FALSE,
                                                  useDemographicsIndexYearMonth = T,
                                                  conceptIdsConditionOccurrenceAnyTimePrior = c(),
                                                  conceptIdsConditionOccurrenceLongTerm = c(),
                                                  conceptIdsConditionOccurrenceMediumTerm = c(),
                                                  conceptIdsConditionOccurrenceShortTerm = c(),
                                                  conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior = c(),
                                                  conceptIdsConditionOccurrencePrimaryInpatientLongTerm = c(),
                                                  conceptIdsConditionOccurrencePrimaryInpatientMediumTerm = c(),
                                                  conceptIdsConditionOccurrencePrimaryInpatientShortTerm = c(),
                                                  conceptIdsConditionEraAnyTimePrior = c(),
                                                  conceptIdsConditionEraLongTerm = c(),
                                                  conceptIdsConditionEraMediumTerm = c(),
                                                  conceptIdsConditionEraShortTerm = c(),
                                                  conceptIdsConditionEraOverlapping = c(),
                                                  conceptIdsConditionEraStartLongTerm = c(),
                                                  conceptIdsConditionEraStartMediumTerm = c(),
                                                  conceptIdsConditionEraStartShortTerm = c(),
                                                  conceptIdsConditionGroupEraAnyTimePrior = conditionGroupConceptIds,
                                                  conceptIdsConditionGroupEraLongTerm = conditionGroupConceptIds,
                                                  conceptIdsConditionGroupEraMediumTerm = conditionGroupConceptIds,
                                                  conceptIdsConditionGroupEraShortTerm = conditionGroupConceptIds,
                                                  conceptIdsConditionGroupEraOverlapping = c(),
                                                  conceptIdsConditionGroupEraStartLongTerm = c(),
                                                  conceptIdsConditionGroupEraStartMediumTerm = c(),
                                                  conceptIdsConditionGroupEraStartShortTerm = c(),
                                                  conceptIdsDrugExposureAnyTimePrior = c(),
                                                  conceptIdsDrugExposureLongTerm = c(),
                                                  conceptIdsDrugExposureMediumTerm = c(),
                                                  conceptIdsDrugExposureShortTerm = c(),
                                                  conceptIdsDrugEraAnyTimePrior = drugIngredientConceptIds,
                                                  conceptIdsDrugEraLongTerm = drugIngredientConceptIds,
                                                  conceptIdsDrugEraMediumTerm = drugIngredientConceptIds,
                                                  conceptIdsDrugEraShortTerm = drugIngredientConceptIds,
                                                  conceptIdsDrugEraOverlapping = c(),
                                                  conceptIdsDrugEraStartLongTerm = c(),
                                                  conceptIdsDrugEraStartMediumTerm = c(),
                                                  conceptIdsDrugEraStartShortTerm = c(),
                                                  conceptIdsDrugGroupEraAnyTimePrior = drugGroupConceptIds,
                                                  conceptIdsDrugGroupEraLongTerm = drugGroupConceptIds,
                                                  conceptIdsDrugGroupEraMediumTerm = drugGroupConceptIds,
                                                  conceptIdsDrugGroupEraShortTerm = drugGroupConceptIds,
                                                  conceptIdsDrugGroupEraOverlapping = c(),
                                                  conceptIdsDrugGroupEraStartLongTerm = c(),
                                                  conceptIdsDrugGroupEraStartMediumTerm = c(),
                                                  conceptIdsDrugGroupEraStartShortTerm = c(),
                                                  conceptIdsProcedureOccurrenceAnyTimePrior = c(),
                                                  conceptIdsProcedureOccurrenceLongTerm = c(),
                                                  conceptIdsProcedureOccurrenceMediumTerm = c(),
                                                  conceptIdsProcedureOccurrenceShortTerm = c(),
                                                  conceptIdsDeviceExposureAnyTimePrior = c(),
                                                  conceptIdsDeviceExposureLongTerm = c(),
                                                  conceptIdsDeviceExposureMediumTerm = c(),
                                                  conceptIdsDeviceExposureShortTerm = c(),
                                                  conceptIdsMeasurementAnyTimePrior = c(),
                                                  conceptIdsMeasurementLongTerm = c(),
                                                  conceptIdsMeasurementMediumTerm = c(),
                                                  conceptIdsMeasurementShortTerm = c(),
                                                  conceptIdsMeasurementValueAnyTimePrior = c(),
                                                  conceptIdsMeasurementValueLongTerm = c(),
                                                  conceptIdsMeasurementValueMediumTerm = c(),
                                                  conceptIdsMeasurementValueShortTerm = c(),
                                                  conceptIdsMeasurementRangeGroupAnyTimePrior = c(),
                                                  conceptIdsMeasurementRangeGroupLongTerm = c(),
                                                  conceptIdsMeasurementRangeGroupMediumTerm = c(),
                                                  conceptIdsMeasurementRangeGroupShortTerm = c(),
                                                  conceptIdsObservationAnyTimePrior = c(),
                                                  conceptIdsObservationLongTerm = c(),
                                                  conceptIdsObservationMediumTerm = c(),
                                                  conceptIdsObservationShortTerm = c(),
                                                  useCharlsonIndex = T,
                                                  useDcsi = FALSE,
                                                  useChads2 = FALSE,
                                                  useChads2Vasc = FALSE,
                                                  useHfrs = T,#T,
                                                  useDistinctConditionCountLongTerm = FALSE,
                                                  useDistinctConditionCountMediumTerm = FALSE,
                                                  useDistinctConditionCountShortTerm = FALSE,
                                                  useDistinctIngredientCountLongTerm = FALSE,
                                                  useDistinctIngredientCountMediumTerm = FALSE,
                                                  useDistinctIngredientCountShortTerm = FALSE,
                                                  useDistinctProcedureCountLongTerm = FALSE,
                                                  useDistinctProcedureCountMediumTerm = FALSE,
                                                  useDistinctProcedureCountShortTerm = FALSE,
                                                  useDistinctMeasurementCountLongTerm = FALSE,
                                                  useDistinctMeasurementCountMediumTerm = FALSE,
                                                  useDistinctMeasurementCountShortTerm = FALSE,
                                                  useDistinctObservationCountLongTerm = FALSE,
                                                  useDistinctObservationCountMediumTerm = FALSE,
                                                  useDistinctObservationCountShortTerm = FALSE,
                                                  useVisitCountLongTerm = T,
                                                  useVisitCountMediumTerm = T,
                                                  useVisitCountShortTerm = T,
                                                  useVisitConceptCountLongTerm = T,
                                                  useVisitConceptCountMediumTerm = T,
                                                  useVisitConceptCountShortTerm = T,
                                                  longTermStartDays = -365,
                                                  mediumTermStartDays = -180,
                                                  shortTermStartDays = -30,
                                                  endDays = endDay) #you can try diverse time settings
      tryCatch({
        comparativeCharacterization(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTable = cohortTable,
                                    oracleTempSchema = oracleTempSchema,
                                    outputFolder = outputFolder,
                                    minCellCount = minCellCount,
                                    targetCohortId = cohortId,
                                    comparatorCohortId = NULL,#7,
                                    outcomeCohortIds = NULL,
                                    tableSpecification = tableSpecification,
                                    sampleSize = NULL,
                                    output = "one column",
                                    percentDigits = 1, 
                                    valueDigits = 1,
                                    stdDiffDigits = 2,
                                    studyPopulationSetting = NULL,
                                    fileName = file.path(exportFolder, sprintf("base_char_cohortId%s_until%s.csv",cohortId, endDay)))
      },
      error = function(e) {
        ParallelLogger::logTrace(paste0(sprintf("Generating table 1 for cohort ID %s is failed. The error message:", cohortId), e))},
      finally = {
        ParallelLogger::logTrace('Done.')})
      
    }
    
  }
  # Add all to zip file -------------------------------------------------------------------------------
  ParallelLogger::logInfo("Adding results to zip file")
  zipName <- file.path(exportFolder, paste0("Results_", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Generating additional table 1 took",
                                signif(delta, 3),
                                attr(delta, "units")))
  
  
}


#' Comparative Characterization
#'
#' @details
#' This function create the comparative table 1 for target and comparator
#'
#' @export
comparativeCharacterization <- function(connectionDetails,
                                        cdmDatabaseSchema,
                                        cohortDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = "cohort",
                                        oracleTempSchema = NULL,
                                        outputFolder,
                                        minCellCount = 0,
                                        targetCohortId,
                                        comparatorCohortId = NULL,
                                        outcomeCohortIds = NULL,
                                        tableSpecification = setTableSpecification(),
                                        sampleSize = NULL,
                                        output = "one column",
                                        percentDigits = 1, 
                                        valueDigits = 1,
                                        stdDiffDigits = 2,
                                        studyPopulationSetting = NULL,
                                        fileName = NULL){
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
    warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
    dir.create(getOption("fftempdir"), recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  
  if(is.null(outcomeCohortIds)){
    targetCovData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                                           cohortDatabaseSchema = cohortDatabaseSchema,
                                                           cohortTable = cohortTable,
                                                           cohortId = targetCohortId,
                                                           rowIdField = "subject_id",
                                                           covariateSettings = tableSpecification$covariateSetting,
                                                           aggregated = F)
    targetCovAggData <- FeatureExtraction::aggregateCovariates(targetCovData)
    ParallelLogger::logInfo(sprintf("Covariates for target Id %s are extracted", targetCohortId))
    
    if(!is.null(comparatorCohortId)){
      comparatorCovData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 cohortTable = cohortTable,
                                                                 cohortId = comparatorCohortId,
                                                                 rowIdField = "subject_id",
                                                                 covariateSettings = tableSpecification$covariateSetting,
                                                                 aggregated = F)
      comparatorCovAggData <- FeatureExtraction::aggregateCovariates(comparatorCovData)
      ParallelLogger::logInfo(sprintf("Covariates for comparator Id %s are extracted", comparatorCohortId))
      
    }else {
      comparatorCovData <- NULL
      comparatorCovAggData <- NULL
    }
    table1 <- createTable1(targetCovAggData,
                           comparatorCovAggData,
                           specification = tableSpecification$tableSpec,
                           output = "one column",
                           showCounts = TRUE,
                           showPercent = TRUE,
                           percentDigits = 1, 
                           valueDigits = 1,
                           stdDiffDigits = 2)
    if(!is.null(fileName)){
      write.csv(table1,fileName)
      ParallelLogger::logInfo(sprintf("Table 1 is saved in %s", fileName))
    }
    
    
  }else{
    stop("currently, stratification on outcome Id or incidence calculation is not supported")
  }
  
  
}

# restricts to pop and saves/creates mapping
mapCovariates <- function(plpData,
                          population){
  covariates = plpData$covariates
  covariateRef = plpData$covariateRef
  
  # restrict to population stratified by outcomes
  ParallelLogger::logTrace('restricting to population according to the outcome status...')
  idx <- ffbase::ffmatch(x = covariates$rowId, table = ff::as.ff(population$rowId))
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- covariates[idx, ]
  
  covariateData = list(covariates = covariates,
                       covariateRef = covariateRef,
                       timeRef = plpData$timeRef,
                       analysisRef = plpData$analysisRef,
                       metaData = plpData$metaData)
  class (covariateData) = "covariateData"
  return(covariateData)
}

#' Set the setting for the table 1
#'
#' @details
#' This function create the setting for the table 1.
#'
#' @param useDemographicsGender = FALSE
#' @param useDemographicsAge
#' @param useDemographicsAgeGroup
#' @param useDemographicsRace
#' @param useDemographicsEthnicity
#' @param useDemographicsIndexYear
#' @param useDemographicsIndexMonth
#' @param useDemographicsPriorObservationTime
#' @param useDemographicsPostObservationTime
#' @param useDemographicsTimeInCohort
#' @param useDemographicsIndexYearMonth
#' @param conceptIdsConditionOccurrenceAnyTimePrior
#' @param conceptIdsConditionOccurrenceLongTerm
#' @param conceptIdsConditionOccurrenceMediumTerm
#' @param conceptIdsConditionOccurrenceShortTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior
#' @param conceptIdsConditionOccurrencePrimaryInpatientLongTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientMediumTerm
#' @param conceptIdsConditionOccurrencePrimaryInpatientShortTerm
#' @param conceptIdsConditionEraAnyTimePrior
#' @param conceptIdsConditionEraLongTerm
#' @param conceptIdsConditionEraMediumTerm
#' @param conceptIdsConditionEraShortTerm
#' @param conceptIdsConditionEraOverlapping
#' @param conceptIdsConditionEraStartLongTerm
#' @param conceptIdsConditionEraStartMediumTerm
#' @param conceptIdsConditionEraStartShortTerm
#' @param conceptIdsConditionGroupEraAnyTimePrior
#' @param conceptIdsConditionGroupEraLongTerm
#' @param conceptIdsConditionGroupEraMediumTerm
#' @param conceptIdsConditionGroupEraShortTerm
#' @param conceptIdsConditionGroupEraOverlapping
#' @param conceptIdsConditionGroupEraStartLongTerm
#' @param conceptIdsConditionGroupEraStartMediumTerm
#' @param conceptIdsConditionGroupEraStartShortTerm
#' @param conceptIdsDrugExposureAnyTimePrior
#' @param conceptIdsDrugExposureLongTerm
#' @param conceptIdsDrugExposureMediumTerm
#' @param conceptIdsDrugExposureShortTerm
#' @param conceptIdsDrugEraAnyTimePrior
#' @param conceptIdsDrugEraLongTerm
#' @param conceptIdsDrugEraMediumTerm
#' @param conceptIdsDrugEraShortTerm
#' @param conceptIdsDrugEraOverlapping
#' @param conceptIdsDrugEraStartLongTerm
#' @param conceptIdsDrugEraStartMediumTerm
#' @param conceptIdsDrugEraStartShortTerm
#' @param conceptIdsDrugGroupEraAnyTimePrior
#' @param conceptIdsDrugGroupEraLongTerm
#' @param conceptIdsDrugGroupEraMediumTerm
#' @param conceptIdsDrugGroupEraShortTerm
#' @param conceptIdsDrugGroupEraOverlapping
#' @param conceptIdsDrugGroupEraStartLongTerm
#' @param conceptIdsDrugGroupEraStartMediumTerm
#' @param conceptIdsDrugGroupEraStartShortTerm
#' @param conceptIdsProcedureOccurrenceAnyTimePrior
#' @param conceptIdsProcedureOccurrenceLongTerm
#' @param conceptIdsProcedureOccurrenceMediumTerm
#' @param conceptIdsProcedureOccurrenceShortTerm
#' @param conceptIdsDeviceExposureAnyTimePrior
#' @param conceptIdsDeviceExposureLongTerm
#' @param conceptIdsDeviceExposureMediumTerm
#' @param conceptIdsDeviceExposureShortTerm
#' @param conceptIdsMeasurementAnyTimePrior
#' @param conceptIdsMeasurementLongTerm
#' @param conceptIdsMeasurementMediumTerm
#' @param conceptIdsMeasurementShortTerm
#' @param conceptIdsMeasurementValueAnyTimePrior
#' @param conceptIdsMeasurementValueLongTerm
#' @param conceptIdsMeasurementValueMediumTerm
#' @param conceptIdsMeasurementValueShortTerm
#' @param conceptIdsMeasurementRangeGroupAnyTimePrior
#' @param conceptIdsMeasurementRangeGroupLongTerm
#' @param conceptIdsMeasurementRangeGroupMediumTerm
#' @param conceptIdsMeasurementRangeGroupShortTerm
#' @param conceptIdsObservationAnyTimePrior
#' @param conceptIdsObservationLongTerm
#' @param conceptIdsObservationMediumTerm
#' @param conceptIdsObservationShortTerm
#' @param useCharlsonIndex
#' @param useDcsi
#' @param useChads2
#' @param useChads2Vasc
#' @param useHfrs
#' @param useDistinctConditionCountLongTerm
#' @param useDistinctConditionCountMediumTerm
#' @param useDistinctConditionCountShortTerm
#' @param useDistinctIngredientCountLongTerm
#' @param useDistinctIngredientCountMediumTerm
#' @param useDistinctIngredientCountShortTerm
#' @param useDistinctProcedureCountLongTerm
#' @param useDistinctProcedureCountMediumTerm
#' @param useDistinctProcedureCountShortTerm
#' @param useDistinctMeasurementCountLongTerm
#' @param useDistinctMeasurementCountMediumTerm
#' @param useDistinctMeasurementCountShortTerm
#' @param useDistinctObservationCountLongTerm
#' @param useDistinctObservationCountMediumTerm
#' @param useDistinctObservationCountShortTerm
#' @param useVisitCountLongTerm
#' @param useVisitCountMediumTerm
#' @param useVisitCountShortTerm
#' @param useVisitConceptCountLongTerm
#' @param useVisitConceptCountMediumTerm
#' @param useVisitConceptCountShortTerm
#' @param longTermStartDays
#' @param mediumTermStartDays
#' @param shortTermStartDays
#' @param endDays
#'
#' @return
#' An object of type \code{tableSpecification}, to be used in other functions.
#'
#' @export
setTableSpecification <- function(useDemographicsGender = FALSE,
                                  useDemographicsAge = FALSE,
                                  useDemographicsAgeGroup = FALSE,
                                  useDemographicsRace = FALSE,
                                  useDemographicsEthnicity = FALSE,
                                  useDemographicsIndexYear = FALSE,
                                  useDemographicsIndexMonth = FALSE,
                                  useDemographicsPriorObservationTime = FALSE,
                                  useDemographicsPostObservationTime = FALSE,
                                  useDemographicsTimeInCohort = FALSE,
                                  useDemographicsIndexYearMonth = FALSE,
                                  conceptIdsConditionOccurrenceAnyTimePrior = c(),
                                  conceptIdsConditionOccurrenceLongTerm = c(),
                                  conceptIdsConditionOccurrenceMediumTerm = c(),
                                  conceptIdsConditionOccurrenceShortTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientLongTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientMediumTerm = c(),
                                  conceptIdsConditionOccurrencePrimaryInpatientShortTerm = c(),
                                  conceptIdsConditionEraAnyTimePrior = c(),
                                  conceptIdsConditionEraLongTerm = c(),
                                  conceptIdsConditionEraMediumTerm = c(),
                                  conceptIdsConditionEraShortTerm = c(),
                                  conceptIdsConditionEraOverlapping = c(),
                                  conceptIdsConditionEraStartLongTerm = c(),
                                  conceptIdsConditionEraStartMediumTerm = c(),
                                  conceptIdsConditionEraStartShortTerm = c(),
                                  conceptIdsConditionGroupEraAnyTimePrior = c(),
                                  conceptIdsConditionGroupEraLongTerm = c(),
                                  conceptIdsConditionGroupEraMediumTerm = c(),
                                  conceptIdsConditionGroupEraShortTerm = c(),
                                  conceptIdsConditionGroupEraOverlapping = c(),
                                  conceptIdsConditionGroupEraStartLongTerm = c(),
                                  conceptIdsConditionGroupEraStartMediumTerm = c(),
                                  conceptIdsConditionGroupEraStartShortTerm = c(),
                                  conceptIdsDrugExposureAnyTimePrior = c(),
                                  conceptIdsDrugExposureLongTerm = c(),
                                  conceptIdsDrugExposureMediumTerm = c(),
                                  conceptIdsDrugExposureShortTerm = c(),
                                  conceptIdsDrugEraAnyTimePrior = c(),
                                  conceptIdsDrugEraLongTerm = c(),
                                  conceptIdsDrugEraMediumTerm = c(),
                                  conceptIdsDrugEraShortTerm = c(),
                                  conceptIdsDrugEraOverlapping = c(),
                                  conceptIdsDrugEraStartLongTerm = c(),
                                  conceptIdsDrugEraStartMediumTerm = c(),
                                  conceptIdsDrugEraStartShortTerm = c(),
                                  conceptIdsDrugGroupEraAnyTimePrior = c(),
                                  conceptIdsDrugGroupEraLongTerm = c(),
                                  conceptIdsDrugGroupEraMediumTerm = c(),
                                  conceptIdsDrugGroupEraShortTerm = c(),
                                  conceptIdsDrugGroupEraOverlapping = c(),
                                  conceptIdsDrugGroupEraStartLongTerm = c(),
                                  conceptIdsDrugGroupEraStartMediumTerm = c(),
                                  conceptIdsDrugGroupEraStartShortTerm = c(),
                                  conceptIdsProcedureOccurrenceAnyTimePrior = c(),
                                  conceptIdsProcedureOccurrenceLongTerm = c(),
                                  conceptIdsProcedureOccurrenceMediumTerm = c(),
                                  conceptIdsProcedureOccurrenceShortTerm = c(),
                                  conceptIdsDeviceExposureAnyTimePrior = c(),
                                  conceptIdsDeviceExposureLongTerm = c(),
                                  conceptIdsDeviceExposureMediumTerm = c(),
                                  conceptIdsDeviceExposureShortTerm = c(),
                                  conceptIdsMeasurementAnyTimePrior = c(),
                                  conceptIdsMeasurementLongTerm = c(),
                                  conceptIdsMeasurementMediumTerm = c(),
                                  conceptIdsMeasurementShortTerm = c(),
                                  conceptIdsMeasurementValueAnyTimePrior = c(),
                                  conceptIdsMeasurementValueLongTerm = c(),
                                  conceptIdsMeasurementValueMediumTerm = c(),
                                  conceptIdsMeasurementValueShortTerm = c(),
                                  conceptIdsMeasurementRangeGroupAnyTimePrior = c(),
                                  conceptIdsMeasurementRangeGroupLongTerm = c(),
                                  conceptIdsMeasurementRangeGroupMediumTerm = c(),
                                  conceptIdsMeasurementRangeGroupShortTerm = c(),
                                  conceptIdsObservationAnyTimePrior = c(),
                                  conceptIdsObservationLongTerm = c(),
                                  conceptIdsObservationMediumTerm = c(),
                                  conceptIdsObservationShortTerm = c(),
                                  useCharlsonIndex = FALSE,
                                  useDcsi = FALSE,
                                  useChads2 = FALSE,
                                  useChads2Vasc = FALSE,
                                  useHfrs = FALSE,
                                  useDistinctConditionCountLongTerm = FALSE,
                                  useDistinctConditionCountMediumTerm = FALSE,
                                  useDistinctConditionCountShortTerm = FALSE,
                                  useDistinctIngredientCountLongTerm = FALSE,
                                  useDistinctIngredientCountMediumTerm = FALSE,
                                  useDistinctIngredientCountShortTerm = FALSE,
                                  useDistinctProcedureCountLongTerm = FALSE,
                                  useDistinctProcedureCountMediumTerm = FALSE,
                                  useDistinctProcedureCountShortTerm = FALSE,
                                  useDistinctMeasurementCountLongTerm = FALSE,
                                  useDistinctMeasurementCountMediumTerm = FALSE,
                                  useDistinctMeasurementCountShortTerm = FALSE,
                                  useDistinctObservationCountLongTerm = FALSE,
                                  useDistinctObservationCountMediumTerm = FALSE,
                                  useDistinctObservationCountShortTerm = FALSE,
                                  useVisitCountLongTerm = FALSE,
                                  useVisitCountMediumTerm = FALSE,
                                  useVisitCountShortTerm = FALSE,
                                  useVisitConceptCountLongTerm = FALSE,
                                  useVisitConceptCountMediumTerm = FALSE,
                                  useVisitConceptCountShortTerm = FALSE,
                                  longTermStartDays = -365,
                                  mediumTermStartDays = -180,
                                  shortTermStartDays = -30,
                                  endDays = 0){
  
  covariateSetting <- FeatureExtraction::createCovariateSettings(useDemographicsGender = useDemographicsGender,
                                                                 useDemographicsAge = useDemographicsAge,
                                                                 useDemographicsAgeGroup = useDemographicsAgeGroup,
                                                                 useDemographicsRace = useDemographicsRace,
                                                                 useDemographicsEthnicity = useDemographicsEthnicity,
                                                                 useDemographicsIndexYear = useDemographicsIndexYear,
                                                                 useDemographicsIndexMonth = useDemographicsIndexMonth,
                                                                 useDemographicsPriorObservationTime = useDemographicsPriorObservationTime,
                                                                 useDemographicsPostObservationTime = useDemographicsPostObservationTime,
                                                                 useDemographicsTimeInCohort = useDemographicsTimeInCohort,
                                                                 useDemographicsIndexYearMonth = useDemographicsIndexYearMonth,
                                                                 
                                                                 useConditionOccurrenceAnyTimePrior = as.logical(length(conceptIdsConditionOccurrenceAnyTimePrior)),
                                                                 useConditionOccurrenceLongTerm = as.logical(length(conceptIdsConditionOccurrenceLongTerm)),
                                                                 useConditionOccurrenceMediumTerm = as.logical(length(conceptIdsConditionOccurrenceMediumTerm)),
                                                                 useConditionOccurrenceShortTerm = as.logical(length(conceptIdsConditionOccurrenceShortTerm)),
                                                                 useConditionOccurrencePrimaryInpatientAnyTimePrior = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior)),
                                                                 useConditionOccurrencePrimaryInpatientLongTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientLongTerm)),
                                                                 useConditionOccurrencePrimaryInpatientMediumTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientMediumTerm)),
                                                                 useConditionOccurrencePrimaryInpatientShortTerm = as.logical(length(conceptIdsConditionOccurrencePrimaryInpatientShortTerm)),
                                                                 useConditionEraAnyTimePrior = as.logical(length(conceptIdsConditionEraAnyTimePrior)),
                                                                 useConditionEraLongTerm = as.logical(length(conceptIdsConditionEraLongTerm)),
                                                                 useConditionEraMediumTerm = as.logical(length(conceptIdsConditionEraMediumTerm)),
                                                                 useConditionEraShortTerm = as.logical(length(conceptIdsConditionEraShortTerm)),
                                                                 useConditionEraOverlapping = as.logical(length(conceptIdsConditionEraOverlapping)),
                                                                 useConditionEraStartLongTerm = as.logical(length(conceptIdsConditionEraStartLongTerm)),
                                                                 useConditionEraStartMediumTerm = as.logical(length(conceptIdsConditionEraStartMediumTerm)),
                                                                 useConditionEraStartShortTerm = as.logical(length(conceptIdsConditionEraStartShortTerm)),
                                                                 useConditionGroupEraAnyTimePrior = as.logical(length(conceptIdsConditionGroupEraAnyTimePrior)),
                                                                 useConditionGroupEraLongTerm = as.logical(length(conceptIdsConditionGroupEraLongTerm)),
                                                                 useConditionGroupEraMediumTerm = as.logical(length(conceptIdsConditionGroupEraMediumTerm)),
                                                                 useConditionGroupEraShortTerm = as.logical(length(conceptIdsConditionGroupEraShortTerm)),
                                                                 useConditionGroupEraOverlapping = as.logical(length(conceptIdsConditionGroupEraOverlapping)),
                                                                 useConditionGroupEraStartLongTerm = as.logical(length(conceptIdsConditionGroupEraStartLongTerm)),
                                                                 useConditionGroupEraStartMediumTerm = as.logical(length(conceptIdsConditionGroupEraStartMediumTerm)),
                                                                 useConditionGroupEraStartShortTerm = as.logical(length(conceptIdsConditionGroupEraStartShortTerm)),
                                                                 useDrugExposureAnyTimePrior = as.logical(length(conceptIdsDrugExposureAnyTimePrior)),
                                                                 useDrugExposureLongTerm = as.logical(length(conceptIdsDrugExposureLongTerm)),
                                                                 useDrugExposureMediumTerm = as.logical(length(conceptIdsDrugExposureMediumTerm)),
                                                                 useDrugExposureShortTerm = as.logical(length(conceptIdsDrugExposureShortTerm)),
                                                                 useDrugEraAnyTimePrior = as.logical(length(conceptIdsDrugEraAnyTimePrior)),
                                                                 useDrugEraLongTerm = as.logical(length(conceptIdsDrugEraLongTerm)),
                                                                 useDrugEraMediumTerm = as.logical(length(conceptIdsDrugEraMediumTerm)),
                                                                 useDrugEraShortTerm = as.logical(length(conceptIdsDrugEraShortTerm)),
                                                                 useDrugEraOverlapping = as.logical(length(conceptIdsDrugEraOverlapping)),
                                                                 useDrugEraStartLongTerm = as.logical(length(conceptIdsDrugEraStartLongTerm)),
                                                                 useDrugEraStartMediumTerm = as.logical(length(conceptIdsDrugEraStartMediumTerm)),
                                                                 useDrugEraStartShortTerm = as.logical(length(conceptIdsDrugEraStartShortTerm)),
                                                                 useDrugGroupEraAnyTimePrior = as.logical(length(conceptIdsDrugGroupEraAnyTimePrior)),
                                                                 useDrugGroupEraLongTerm = as.logical(length(conceptIdsDrugGroupEraLongTerm)),
                                                                 useDrugGroupEraMediumTerm = as.logical(length(conceptIdsDrugGroupEraMediumTerm)),
                                                                 useDrugGroupEraShortTerm = as.logical(length(conceptIdsDrugGroupEraShortTerm)),
                                                                 useDrugGroupEraOverlapping = as.logical(length(conceptIdsDrugGroupEraOverlapping)),
                                                                 useDrugGroupEraStartLongTerm = as.logical(length(conceptIdsDrugGroupEraStartLongTerm)),
                                                                 useDrugGroupEraStartMediumTerm = as.logical(length(conceptIdsDrugGroupEraStartMediumTerm)),
                                                                 useDrugGroupEraStartShortTerm = as.logical(length(conceptIdsDrugGroupEraStartShortTerm)),
                                                                 useProcedureOccurrenceAnyTimePrior = as.logical(length(conceptIdsProcedureOccurrenceAnyTimePrior)),
                                                                 useProcedureOccurrenceLongTerm = as.logical(length(conceptIdsProcedureOccurrenceLongTerm)),
                                                                 useProcedureOccurrenceMediumTerm = as.logical(length(conceptIdsProcedureOccurrenceMediumTerm)),
                                                                 useProcedureOccurrenceShortTerm = as.logical(length(conceptIdsProcedureOccurrenceShortTerm)),
                                                                 useDeviceExposureAnyTimePrior = as.logical(length(conceptIdsDeviceExposureAnyTimePrior)),
                                                                 useDeviceExposureLongTerm = as.logical(length(conceptIdsDeviceExposureLongTerm)),
                                                                 useDeviceExposureMediumTerm = as.logical(length(conceptIdsDeviceExposureMediumTerm)),
                                                                 useDeviceExposureShortTerm = as.logical(length(conceptIdsDeviceExposureShortTerm)),
                                                                 useMeasurementAnyTimePrior = as.logical(length(conceptIdsMeasurementAnyTimePrior)),
                                                                 useMeasurementLongTerm = as.logical(length(conceptIdsMeasurementLongTerm)),
                                                                 useMeasurementMediumTerm = as.logical(length(conceptIdsMeasurementMediumTerm)),
                                                                 useMeasurementShortTerm = as.logical(length(conceptIdsMeasurementShortTerm)),
                                                                 useMeasurementValueAnyTimePrior = as.logical(length(conceptIdsMeasurementValueAnyTimePrior)),
                                                                 useMeasurementValueLongTerm = as.logical(length(conceptIdsMeasurementValueLongTerm)),
                                                                 useMeasurementValueMediumTerm = as.logical(length(conceptIdsMeasurementValueMediumTerm)),
                                                                 useMeasurementValueShortTerm = as.logical(length(conceptIdsMeasurementValueShortTerm)),
                                                                 useMeasurementRangeGroupAnyTimePrior = as.logical(length(conceptIdsMeasurementRangeGroupAnyTimePrior)),
                                                                 useMeasurementRangeGroupLongTerm = as.logical(length(conceptIdsMeasurementRangeGroupLongTerm)),
                                                                 useMeasurementRangeGroupMediumTerm = as.logical(length(conceptIdsMeasurementRangeGroupMediumTerm)),
                                                                 useMeasurementRangeGroupShortTerm = as.logical(length(conceptIdsMeasurementRangeGroupShortTerm)),
                                                                 useObservationAnyTimePrior = as.logical(length(conceptIdsObservationAnyTimePrior)),
                                                                 useObservationLongTerm = as.logical(length(conceptIdsObservationLongTerm)),
                                                                 useObservationMediumTerm = as.logical(length(conceptIdsObservationMediumTerm)),
                                                                 useObservationShortTerm = as.logical(length(conceptIdsObservationShortTerm)),
                                                                 
                                                                 useCharlsonIndex = useCharlsonIndex,
                                                                 useDcsi = useDcsi,
                                                                 useChads2 = useChads2,
                                                                 useChads2Vasc = useChads2Vasc,
                                                                 useHfrs = useHfrs,
                                                                 useDistinctConditionCountLongTerm = useDistinctConditionCountLongTerm,
                                                                 useDistinctConditionCountMediumTerm = useDistinctConditionCountMediumTerm,
                                                                 useDistinctConditionCountShortTerm = useDistinctConditionCountShortTerm,
                                                                 useDistinctIngredientCountLongTerm = useDistinctIngredientCountLongTerm,
                                                                 useDistinctIngredientCountMediumTerm = useDistinctIngredientCountMediumTerm,
                                                                 useDistinctIngredientCountShortTerm = useDistinctIngredientCountShortTerm,
                                                                 useDistinctProcedureCountLongTerm = useDistinctProcedureCountLongTerm,
                                                                 useDistinctProcedureCountMediumTerm = useDistinctProcedureCountMediumTerm,
                                                                 useDistinctProcedureCountShortTerm = useDistinctProcedureCountShortTerm,
                                                                 useDistinctMeasurementCountLongTerm = useDistinctMeasurementCountLongTerm,
                                                                 useDistinctMeasurementCountMediumTerm = useDistinctMeasurementCountMediumTerm,
                                                                 useDistinctMeasurementCountShortTerm = useDistinctMeasurementCountShortTerm,
                                                                 useDistinctObservationCountLongTerm = useDistinctObservationCountLongTerm,
                                                                 useDistinctObservationCountMediumTerm = useDistinctObservationCountMediumTerm,
                                                                 useDistinctObservationCountShortTerm = useDistinctObservationCountShortTerm,
                                                                 useVisitCountLongTerm = useVisitCountLongTerm,
                                                                 useVisitCountMediumTerm = useVisitCountMediumTerm,
                                                                 useVisitCountShortTerm = useVisitCountShortTerm,
                                                                 useVisitConceptCountLongTerm = useVisitConceptCountLongTerm,
                                                                 useVisitConceptCountMediumTerm = useVisitConceptCountMediumTerm,
                                                                 useVisitConceptCountShortTerm = useVisitConceptCountShortTerm,
                                                                 
                                                                 longTermStartDays = longTermStartDays,
                                                                 mediumTermStartDays = mediumTermStartDays,
                                                                 shortTermStartDays = shortTermStartDays,
                                                                 endDays = endDays)
  
  
  label <- c()
  analysisId <- c()
  covariateIds <- c()
  if(useDemographicsGender){
    label <- c(label, "Gender")
    analysisId <- c(analysisId, 1)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",8532,1),collapse=","))
  }
  if(useDemographicsAge){
    label <- c(label, "Age")
    analysisId <- c(analysisId, 2)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsAgeGroup){
    label <- c(label, "Age Group")
    analysisId <- c(analysisId, 3)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsRace){
    label <- c(label, "Race")
    analysisId <- c(analysisId, 4)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsEthnicity){
    label <- c(label, "Ethnicity")
    analysisId <- c(analysisId, 5)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexYear){
    label <- c(label, "Index year")
    analysisId <- c(analysisId, 6)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexMonth){
    label <- c(label, "Index month")
    analysisId <- c(analysisId, 7)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsPriorObservationTime){
    label <- c(label, "Prior obesrvation time")
    analysisId <- c(analysisId, 8)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsPostObservationTime){
    label <- c(label, "Post observation time")
    analysisId <- c(analysisId, 9)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsTimeInCohort){
    label <- c(label, "Time in cohort")
    analysisId <- c(analysisId, 10)
    covariateIds <- c(covariateIds, "")
  }
  if(useDemographicsIndexYearMonth){
    label <- c(label, "Index year month")
    analysisId <- c(analysisId, 11)
    covariateIds <- c(covariateIds, "")
  }
  if(length(conceptIdsConditionOccurrenceAnyTimePrior)){
    label <- c(label, "ConditionOccurrenceAnyTimePrior")
    analysisId <- c(analysisId, 101)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceAnyTimePrior,101),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceLongTerm)){
    label <- c(label, "ConditionOccurrenceLongTerm")
    analysisId <- c(analysisId, 102)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceLongTerm,102),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceMediumTerm)){
    label <- c(label, "ConditionOccurrenceMediumTerm")
    analysisId <- c(analysisId, 103)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceMediumTerm,103),collapse=","))
  }
  if(length(conceptIdsConditionOccurrenceShortTerm)){
    label <- c(label, "ConditionOccurrenceShortTerm")
    analysisId <- c(analysisId, 104)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrenceShortTerm,104),collapse=","))
  }
  
  if(length(conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientAnyTimePrior")
    analysisId <- c(analysisId, 105)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior,105),collapse=","))
  }
  
  if(length(conceptIdsConditionOccurrencePrimaryInpatientLongTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientLongTerm")
    analysisId <- c(analysisId, 106)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientLongTerm,106),collapse=","))
  }
  if(length(conceptIdsConditionOccurrencePrimaryInpatientMediumTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientMediumTerm")
    analysisId <- c(analysisId, 107)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientMediumTerm,107),collapse=","))
  }
  
  if(length(conceptIdsConditionOccurrencePrimaryInpatientShortTerm)){
    label <- c(label, "ConditionOccurrencePrimaryInpatientShortTerm")
    analysisId <- c(analysisId, 108)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionOccurrencePrimaryInpatientShortTerm,108),collapse=","))
  }
  
  if(length(conceptIdsConditionEraAnyTimePrior)){
    label <- c(label, "ConditionEraAnyTimePrior")
    analysisId <- c(analysisId, 201)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraAnyTimePrior,201),collapse=","))
  }
  
  if(length(conceptIdsConditionEraLongTerm)){
    label <- c(label, "ConditionEraLongTerm")
    analysisId <- c(analysisId, 202)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraLongTerm,202),collapse=","))
  }
  
  if(length(conceptIdsConditionEraMediumTerm)){
    label <- c(label, "ConditionEraMediumTerm")
    analysisId <- c(analysisId, 203)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraMediumTerm,203),collapse=","))
  }
  
  if(length(conceptIdsConditionEraShortTerm)){
    label <- c(label, "ConditionEraShortTerm")
    analysisId <- c(analysisId, 204)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraShortTerm,204),collapse=","))
  }
  if(length(conceptIdsConditionEraOverlapping)){
    label <- c(label, "ConditionEraOverlapping")
    analysisId <- c(analysisId, 205)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraOverlapping,205),collapse=","))
  }
  if(length(conceptIdsConditionEraStartLongTerm)){
    label <- c(label, "ConditionEraStartLongTerm")
    analysisId <- c(analysisId, 206)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartLongTerm,206),collapse=","))
  }
  if(length(conceptIdsConditionEraStartMediumTerm)){
    label <- c(label, "ConditionEraStartMediumTerm")
    analysisId <- c(analysisId, 207)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartMediumTerm,207),collapse=","))
  }
  if(length(conceptIdsConditionEraStartShortTerm)){
    label <- c(label, "ConditionEraStartShortTerm")
    analysisId <- c(analysisId, 208)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionEraStartShortTerm,208),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraAnyTimePrior)){
    label <- c(label, "ConditionGroupEraAnyTimePrior")
    analysisId <- c(analysisId, 209)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraAnyTimePrior,209),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraLongTerm)){
    label <- c(label, "ConditionGroupEraLongTerm")
    analysisId <- c(analysisId, 210)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraLongTerm,210),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraMediumTerm)){
    label <- c(label, "ConditionGroupEraMediumTerm")
    analysisId <- c(analysisId, 211)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraMediumTerm,211),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraShortTerm)){
    label <- c(label, "ConditionGroupEraShortTerm")
    analysisId <- c(analysisId, 212)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraShortTerm,212),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraOverlapping)){
    label <- c(label, "ConditionGroupEraOverlapping")
    analysisId <- c(analysisId, 213)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraOverlapping,213),collapse=","))
  }
  if(length(conceptIdsConditionGroupEraStartLongTerm)){
    label <- c(label, "ConditionGroupEraStartLongTerm")
    analysisId <- c(analysisId, 214)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartLongTerm,214),collapse=","))
  }
  
  if(length(conceptIdsConditionGroupEraStartMediumTerm)){
    label <- c(label, "ConditionGroupEraStartMediumTerm")
    analysisId <- c(analysisId, 215)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartMediumTerm,215),collapse=","))
  }
  
  if(length(conceptIdsConditionGroupEraStartShortTerm)){
    label <- c(label, "ConditionGroupEraStartShortTerm")
    analysisId <- c(analysisId, 216)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsConditionGroupEraStartShortTerm,216),collapse=","))
  }
  
  if(length(conceptIdsDrugExposureAnyTimePrior)){
    label <- c(label, "DrugExposureAnyTimePrior")
    analysisId <- c(analysisId, 301)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureAnyTimePrior,301),collapse=","))
  }
  
  if(length(conceptIdsDrugExposureLongTerm)){
    label <- c(label, "DrugExposureLongTerm")
    analysisId <- c(analysisId, 302)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureLongTerm,302),collapse=","))
  }
  if(length(conceptIdsDrugExposureMediumTerm)){
    label <- c(label, "DrugExposureMediumTerm")
    analysisId <- c(analysisId, 303)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureMediumTerm,303),collapse=","))
  }
  if(length(conceptIdsDrugExposureShortTerm)){
    label <- c(label, "DrugExposureShortTerm")
    analysisId <- c(analysisId, 304)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugExposureShortTerm,304),collapse=","))
  }
  if(length(conceptIdsDrugEraAnyTimePrior)){
    label <- c(label, "DrugEraAnyTimePrior")
    analysisId <- c(analysisId, 401)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraAnyTimePrior,401),collapse=","))
  }
  if(length(conceptIdsDrugEraLongTerm)){
    label <- c(label, "DrugEraLongTerm")
    analysisId <- c(analysisId, 402)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraLongTerm,402),collapse=","))
  }
  if(length(conceptIdsDrugEraMediumTerm)){
    label <- c(label, "DrugEraMediumTerm")
    analysisId <- c(analysisId, 403)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraMediumTerm,403),collapse=","))
  }
  
  if(length(conceptIdsDrugEraShortTerm)){
    label <- c(label, "DrugEraShortTerm")
    analysisId <- c(analysisId, 404)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraShortTerm,404),collapse=","))
  }
  
  if(length(conceptIdsDrugEraOverlapping)){
    label <- c(label, "DrugEraOverlapping")
    analysisId <- c(analysisId, 405)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraOverlapping,405),collapse=","))
  }
  
  if(length(conceptIdsDrugEraStartLongTerm)){
    label <- c(label, "DrugEraStartLongTerm")
    analysisId <- c(analysisId, 406)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartLongTerm,406),collapse=","))
  }
  
  if(length(conceptIdsDrugEraStartMediumTerm)){
    label <- c(label, "DrugEraStartMediumTerm")
    analysisId <- c(analysisId, 407)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartMediumTerm,407),collapse=","))
  }
  
  if(length(conceptIdsDrugEraStartShortTerm)){
    label <- c(label, "DrugEraStartShortTerm")
    analysisId <- c(analysisId, 408)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugEraStartShortTerm,408),collapse=","))
  }
  
  if(length(conceptIdsDrugGroupEraAnyTimePrior)){
    label <- c(label, "DrugGroupEraAnyTimePrior")
    analysisId <- c(analysisId, 409)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraAnyTimePrior,409),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraLongTerm)){
    label <- c(label, "DrugGroupEraLongTerm")
    analysisId <- c(analysisId, 410)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraLongTerm,410),collapse=","))
  }
  
  if(length(conceptIdsDrugGroupEraMediumTerm)){
    label <- c(label, "DrugGroupEraMediumTerm")
    analysisId <- c(analysisId, 411)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraMediumTerm,411),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraShortTerm)){
    label <- c(label, "DrugGroupEraShortTerm")
    analysisId <- c(analysisId, 412)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraShortTerm,412),collapse=","))
  }
  
  if(length(conceptIdsDrugGroupEraOverlapping)){
    label <- c(label, "DrugGroupEraOverlapping")
    analysisId <- c(analysisId, 413)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraOverlapping,413),collapse=","))
  }
  
  if(length(conceptIdsDrugGroupEraStartLongTerm)){
    label <- c(label, "DrugGroupEraStartLongTerm")
    analysisId <- c(analysisId, 414)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartLongTerm,414),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraStartMediumTerm)){
    label <- c(label, "DrugGroupEraStartMediumTerm")
    analysisId <- c(analysisId, 415)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartMediumTerm,415),collapse=","))
  }
  if(length(conceptIdsDrugGroupEraStartShortTerm)){
    label <- c(label, "DrugGroupEraStartShortTerm")
    analysisId <- c(analysisId, 416)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDrugGroupEraStartShortTerm,416),collapse=","))
  }
  
  if(length(conceptIdsProcedureOccurrenceAnyTimePrior)){
    label <- c(label, "ProcedureOccurrenceAnyTimePrior")
    analysisId <- c(analysisId, 501)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceAnyTimePrior,501),collapse=","))
  }
  if(length(conceptIdsProcedureOccurrenceLongTerm)){
    label <- c(label, "ProcedureOccurrenceLongTerm")
    analysisId <- c(analysisId, 502)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceLongTerm,502),collapse=","))
  }
  if(length(conceptIdsProcedureOccurrenceMediumTerm)){
    label <- c(label, "ProcedureOccurrenceMediumTerm")
    analysisId <- c(analysisId, 503)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceMediumTerm,503),collapse=","))
  }
  
  if(length(conceptIdsProcedureOccurrenceShortTerm)){
    label <- c(label, "ProcedureOccurrenceShortTerm")
    analysisId <- c(analysisId, 504)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsProcedureOccurrenceShortTerm,504),collapse=","))
  }
  
  if(length(conceptIdsDeviceExposureAnyTimePrior)){
    label <- c(label, "DeviceExposureAnyTimePrior")
    analysisId <- c(analysisId, 601)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureAnyTimePrior,601),collapse=","))
  }
  if(length(conceptIdsDeviceExposureLongTerm)){
    label <- c(label, "DeviceExposureLongTerm")
    analysisId <- c(analysisId, 602)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureLongTerm,602),collapse=","))
  }
  if(length(conceptIdsDeviceExposureMediumTerm)){
    label <- c(label, "DeviceExposureMediumTerm")
    analysisId <- c(analysisId, 603)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureMediumTerm,603),collapse=","))
  }
  
  if(length(conceptIdsDeviceExposureShortTerm)){
    label <- c(label, "DeviceExposureShortTerm")
    analysisId <- c(analysisId, 604)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsDeviceExposureShortTerm,604),collapse=","))
  }
  
  if(length(conceptIdsMeasurementAnyTimePrior)){
    label <- c(label, "MeasurementAnyTimePrior")
    analysisId <- c(analysisId, 701)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementAnyTimePrior,701),collapse=","))
  }
  
  if(length(conceptIdsMeasurementLongTerm)){
    label <- c(label, "MeasurementLongTerm")
    analysisId <- c(analysisId, 702)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementLongTerm,702),collapse=","))
  }
  if(length(conceptIdsMeasurementMediumTerm)){
    label <- c(label, "MeasurementMediumTerm")
    analysisId <- c(analysisId, 703)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementMediumTerm,703),collapse=","))
  }
  
  if(length(conceptIdsMeasurementShortTerm)){
    label <- c(label, "MeasurementShortTerm")
    analysisId <- c(analysisId, 704)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementShortTerm,704),collapse=","))
  }
  
  if(length(conceptIdsMeasurementValueAnyTimePrior)){
    label <- c(label, "MeasurementValueAnyTimePrior")
    analysisId <- c(analysisId, 705)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueAnyTimePrior,705),collapse=","))
  }
  
  if(length(conceptIdsMeasurementValueLongTerm)){
    label <- c(label, "MeasurementValueLongTerm")
    analysisId <- c(analysisId, 706)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueLongTerm,706),collapse=","))
  }
  if(length(conceptIdsMeasurementValueMediumTerm)){
    label <- c(label, "MeasurementValueMediumTerm")
    analysisId <- c(analysisId, 707)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueMediumTerm,707),collapse=","))
  }
  
  if(length(conceptIdsMeasurementValueShortTerm)){
    label <- c(label, "MeasurementValueShortTerm")
    analysisId <- c(analysisId, 708)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementValueShortTerm,708),collapse=","))
  }
  
  if(length(conceptIdsMeasurementRangeGroupAnyTimePrior)){
    label <- c(label, "MeasurementRangeGroupAnyTimePrior")
    analysisId <- c(analysisId, 709)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupAnyTimePrior,709),collapse=","))
  }
  
  if(length(conceptIdsMeasurementRangeGroupLongTerm)){
    label <- c(label, "MeasurementRangeGroupLongTerm")
    analysisId <- c(analysisId, 710)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupLongTerm,710),collapse=","))
  }
  if(length(conceptIdsMeasurementRangeGroupMediumTerm)){
    label <- c(label, "MeasurementRangeGroupMediumTerm")
    analysisId <- c(analysisId, 711)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupMediumTerm,711),collapse=","))
  }
  
  if(length(conceptIdsMeasurementRangeGroupShortTerm)){
    label <- c(label, "MeasurementRangeGroupShortTerm")
    analysisId <- c(analysisId, 712)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsMeasurementRangeGroupShortTerm,712),collapse=","))
  }
  
  if(length(conceptIdsObservationAnyTimePrior)){
    label <- c(label, "ObservationAnyTimePrior")
    analysisId <- c(analysisId, 801)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationAnyTimePrior,801),collapse=","))
  }
  if(length(conceptIdsObservationLongTerm)){
    label <- c(label, "ObservationLongTerm")
    analysisId <- c(analysisId, 802)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationLongTerm,802),collapse=","))
  }
  if(length(conceptIdsObservationMediumTerm)){
    label <- c(label, "ObservationMediumTerm")
    analysisId <- c(analysisId, 803)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationMediumTerm,803),collapse=","))
  }
  if(length(conceptIdsObservationShortTerm)){
    label <- c(label, "ObservationShortTerm")
    analysisId <- c(analysisId, 804)
    covariateIds <- c(covariateIds, paste0(sprintf("%d%03d",conceptIdsObservationShortTerm,804),collapse=","))
  }
  
  if(useCharlsonIndex){
    label <- c(label, "CharlsonIndex")
    analysisId <- c(analysisId, 901)
    covariateIds <- c(covariateIds, "")
  }
  if(useDcsi){
    label <- c(label, "Dcsi")
    analysisId <- c(analysisId, 902)
    covariateIds <- c(covariateIds, "")
  }
  if(useChads2){
    label <- c(label, "Chads2")
    analysisId <- c(analysisId, 903)
    covariateIds <- c(covariateIds, "")
  }
  if(useChads2Vasc){
    label <- c(label, "Chads2Vasc")
    analysisId <- c(analysisId, 904)
    covariateIds <- c(covariateIds, "")
  }
  if(useHfrs){
    label <- c(label, "Hfrs")
    analysisId <- c(analysisId, 926)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountLongTerm){
    label <- c(label, "DistinctConditionCountLongTerm")
    analysisId <- c(analysisId, 905)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountMediumTerm){
    label <- c(label, "DistinctConditionCountMediumTerm")
    analysisId <- c(analysisId, 906)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctConditionCountShortTerm){
    label <- c(label, "DistinctConditionCountShortTerm")
    analysisId <- c(analysisId, 907)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountLongTerm){
    label <- c(label, "DistinctIngredientCountLongTerm")
    analysisId <- c(analysisId, 908)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountMediumTerm){
    label <- c(label, "DistinctIngredientCountMediumTerm")
    analysisId <- c(analysisId, 909)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctIngredientCountShortTerm){
    label <- c(label, "DistinctIngredientCountShortTerm")
    analysisId <- c(analysisId, 910)
    covariateIds <- c(covariateIds, "")
  }
  
  if(useDistinctProcedureCountLongTerm){
    label <- c(label, "DistinctProcedureCountLongTerm")
    analysisId <- c(analysisId, 911)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctProcedureCountMediumTerm){
    label <- c(label, "DistinctProcedureCountMediumTerm")
    analysisId <- c(analysisId, 912)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctProcedureCountShortTerm){
    label <- c(label, "DistinctProcedureCountShortTerm")
    analysisId <- c(analysisId, 913)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountLongTerm){
    label <- c(label, "DistinctMeasurementCountLongTerm")
    analysisId <- c(analysisId, 914)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountMediumTerm){
    label <- c(label, "DistinctMeasurementCountMediumTerm")
    analysisId <- c(analysisId, 915)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctMeasurementCountShortTerm){
    label <- c(label, "DistinctMeasurementCountShortTerm")
    analysisId <- c(analysisId, 916)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountLongTerm){
    label <- c(label, "DistinctObservationCountLongTerm")
    analysisId <- c(analysisId, 917)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountMediumTerm){
    label <- c(label, "DistinctObservationCountMediumTerm")
    analysisId <- c(analysisId, 918)
    covariateIds <- c(covariateIds, "")
  }
  if(useDistinctObservationCountShortTerm){
    label <- c(label, "DistinctObservationCountShortTerm")
    analysisId <- c(analysisId, 919)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountLongTerm){
    label <- c(label, "VisitCountLongTerm")
    analysisId <- c(analysisId, 920)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountMediumTerm){
    label <- c(label, "VisitCountMediumTerm")
    analysisId <- c(analysisId, 921)
    covariateIds <- c(covariateIds, "")
  }
  if(useVisitCountShortTerm){
    label <- c(label, "VisitCountShortTerm")
    analysisId <- c(analysisId, 922)
    covariateIds <- c(covariateIds, "")
  }
  
  if(useVisitConceptCountLongTerm){
    label <- c(label, "VisitConceptCountLongTerm")
    analysisId <- c(analysisId, 923)
    covariateIds <- c(covariateIds, "")
  }
  
  if(useVisitConceptCountMediumTerm){
    label <- c(label, "VisitConceptCountMediumTerm")
    analysisId <- c(analysisId, 924)
    covariateIds <- c(covariateIds, "")
  }
  
  if(useVisitConceptCountShortTerm){
    label <- c(label, "VisitConceptCountShortTerm")
    analysisId <- c(analysisId, 925)
    covariateIds <- c(covariateIds, "")
  }
  
  tableSpec <- data.frame(label = label,
                          analysisId = analysisId,
                          covariateIds = covariateIds,
                          stringsAsFactors = F)
  result <- list(tableSpec = tableSpec, covariateSetting = covariateSetting)
  class(result) <- "tableSpecification"
  
  return(result)
}

#' Create a table 1
#'
#' @description
#' Creates a formatted table of cohort characteristics, to be included in publications or reports.
#' Allows for creating a table describing a single cohort, or a table comparing two cohorts.
#'
#' @param covariateData1   The covariate data of the cohort to be included in the table.
#' @param covariateData2   The covariate data of the cohort to also be included, when comparing two
#'                         cohorts.
#' @param specifications   Specifications of which covariates to display, and how.
#' @param output           The output format for the table. Options are \code{output = "two columns"},
#'                         \code{output = "one column"}, or \code{output = "list"}.
#' @param showCounts       Show the number of cohort entries having the binary covariate?
#' @param showPercent      Show the percentage of cohort entries having the binary covariate?
#' @param percentDigits    Number of digits to be used for percentages.
#' @param stdDiffDigits    Number of digits to be used for the standardized differences.
#' @param valueDigits    Number of digits to be used for the values of continuous variables.
#' 
#' @return
#' A data frame, or, when \code{output = "list"} a list of two data frames.
#'
#' @export
createTable1 <- function(covariateData1,
                         covariateData2 = NULL,
                         specifications = getDefaultTable1Specifications(),
                         output = "two columns",
                         showCounts = FALSE,
                         showPercent = TRUE,
                         percentDigits = 1,
                         valueDigits = 1,
                         stdDiffDigits = 2) {
  comparison <- !is.null(covariateData2)
  if (!is(covariateData1, "covariateData")) {
    stop("covariateData1 is not of type 'covariateData'")
  }
  if (comparison && !is(covariateData2, "covariateData")) {
    stop("covariateData2 is not of type 'covariateData'")
  }
  if (is.null(covariateData1$covariatesContinuous) && is.null(covariateData1$covariates$averageValue)) {
    stop("Covariate1 data is not aggregated")
  }
  if (comparison && is.null(covariateData2$covariatesContinuous) && is.null(covariateData2$covariates$averageValue)) {
    stop("Covariate2 data is not aggregated")
  }
  if (!showCounts && !showPercent) {
    stop("Must show counts or percent, or both")
  }
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  formatCount <- function(x) {
    result <- format(round(x), justify = "right", big.mark = ",")
    result <- gsub("NA", "", result)
    result <- gsub(" ", " ", result)
    return(result)
  }
  
  formatPercent <- function(x) {
    result <- format(round(100*x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", " ", result)
    return(result)
  }
  
  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", " ", result)
    return(result)
  }
  
  formatValue <- function(x) {
    return(format(round(x, valueDigits), nsmall = valueDigits))
  }
  
  if (is.null(covariateData1$covariates)) {
    covariates <- NULL
  } else {
    covariates <- as.data.frame(ff::as.ram(covariateData1$covariates[, c("covariateId", "sumValue", "averageValue")]))
    colnames(covariates) <- c("covariateId", "count1", "percent1")
    covariates$count1 <- formatCount(covariates$count1)
    covariates$percent1 <- formatPercent(covariates$percent1)
  }
  if (is.null(covariateData1$covariatesContinuous)) {
    covariatesContinuous <- NULL
  } else {
    covariatesContinuous <- as.data.frame(ff::as.ram(covariateData1$covariatesContinuous[, c("covariateId",
                                                                                             "averageValue",
                                                                                             "standardDeviation",
                                                                                             "minValue",
                                                                                             "p25Value",
                                                                                             "medianValue",
                                                                                             "p75Value",
                                                                                             "maxValue")]))
    colnames(covariatesContinuous) <- c("covariateId",
                                        "averageValue1",
                                        "standardDeviation1",
                                        "minValue1",
                                        "p25Value1",
                                        "medianValue1",
                                        "p75Value1",
                                        "maxValue1")
    covariatesContinuous$averageValue1 <- formatValue(covariatesContinuous$averageValue1)
    covariatesContinuous$standardDeviation1 <- formatValue(covariatesContinuous$standardDeviation1)
    covariatesContinuous$minValue1 <- formatValue(covariatesContinuous$minValue1)
    covariatesContinuous$p25Value1 <- formatValue(covariatesContinuous$p25Value1)
    covariatesContinuous$medianValue1 <- formatValue(covariatesContinuous$medianValue1)
    covariatesContinuous$p75Value1 <- formatValue(covariatesContinuous$p75Value1)
    covariatesContinuous$maxValue1 <- formatValue(covariatesContinuous$maxValue1)
  }
  
  covariateRef <- ff::as.ram(covariateData1$covariateRef)
  analysisRef <- ff::as.ram(covariateData1$analysisRef)
  if (comparison) {
    stdDiff <- FeatureExtraction::computeStandardizedDifference(covariateData1, covariateData2)
    if (!is.null(covariateData1$covariates) && !is.null(covariateData2$covariates)) {
      tempCovariates <- ff::as.ram(covariateData2$covariates[, c("covariateId", "sumValue", "averageValue")])
      colnames(tempCovariates) <- c("covariateId", "count2", "percent2")
      tempCovariates$count2 <- formatCount(tempCovariates$count2)
      tempCovariates$percent2 <- formatPercent(tempCovariates$percent2)
      covariates <- merge(covariates, tempCovariates, all = TRUE)
      covariates$count1[is.na(covariates$count1)] <- " 0"
      covariates$count2[is.na(covariates$count2)] <- " 0"
      covariates$percent1[is.na(covariates$percent1)] <- " 0"
      covariates$percent2[is.na(covariates$percent2)] <- " 0"
      covariates <- merge(covariates, stdDiff[, c("covariateId", "stdDiff")])
      covariates$stdDiff <- formatStdDiff(covariates$stdDiff)
    }
    if (!is.null(covariatesContinuous)) {
      tempCovariates <- as.data.frame(ff::as.ram(covariateData2$covariatesContinuous[, c("covariateId",
                                                                                         "averageValue",
                                                                                         "standardDeviation",
                                                                                         "minValue",
                                                                                         "p25Value",
                                                                                         "medianValue",
                                                                                         "p75Value",
                                                                                         "maxValue")]))
      colnames(tempCovariates) <- c("covariateId",
                                    "averageValue2",
                                    "standardDeviation2",
                                    "minValue2",
                                    "p25Value2",
                                    "medianValue2",
                                    "p75Value2",
                                    "maxValue2")
      tempCovariates$averageValue2 <- formatValue(tempCovariates$averageValue2)
      tempCovariates$standardDeviation2 <- formatValue(tempCovariates$standardDeviation2)
      tempCovariates$minValue2 <- formatValue(tempCovariates$minValue2)
      tempCovariates$p25Value2 <- formatValue(tempCovariates$p25Value2)
      tempCovariates$medianValue2 <- formatValue(tempCovariates$medianValue2)
      tempCovariates$p75Value2 <- formatValue(tempCovariates$p75Value2)
      tempCovariates$maxValue2 <- formatValue(tempCovariates$maxValue2)
      covariatesContinuous <- merge(covariatesContinuous, tempCovariates, all = TRUE)
      covariatesContinuous$averageValue1[is.na(covariatesContinuous$averageValue1)] <- "  "
      covariatesContinuous$standardDeviation1[is.na(covariatesContinuous$standardDeviation1)] <- "  "
      covariatesContinuous$minValue1[is.na(covariatesContinuous$minValue1)] <- "  "
      covariatesContinuous$p25Value1[is.na(covariatesContinuous$p25Value1)] <- "  "
      covariatesContinuous$medianValue1[is.na(covariatesContinuous$medianValue1)] <- "  "
      covariatesContinuous$p75Value1[is.na(covariatesContinuous$p75Value1)] <- "  "
      covariatesContinuous$maxValue1[is.na(covariatesContinuous$maxValue1)] <- "  "
      covariatesContinuous$averageValue2[is.na(covariatesContinuous$averageValue2)] <- "  "
      covariatesContinuous$standardDeviation2[is.na(covariatesContinuous$standardDeviation2)] <- "  "
      covariatesContinuous$minValue2[is.na(covariatesContinuous$minValue2)] <- "  "
      covariatesContinuous$p25Value2[is.na(covariatesContinuous$p25Value2)] <- "  "
      covariatesContinuous$medianValue2[is.na(covariatesContinuous$medianValue2)] <- "  "
      covariatesContinuous$p75Value2[is.na(covariatesContinuous$p75Value2)] <- "  "
      covariatesContinuous$maxValue2[is.na(covariatesContinuous$maxValue2)] <- "  "
      covariatesContinuous <- merge(covariatesContinuous, stdDiff[, c("covariateId", "stdDiff")])
      covariatesContinuous$stdDiff <- formatStdDiff(covariatesContinuous$stdDiff)
    }
    idx <- !ffbase::`%in%`(covariateData2$covariateRef$covariateId,
                           covariateData1$covariateRef$covariateId)
    if (ffbase::any.ff(idx)) {
      covariateRef <- rbind(covariateRef, ff::as.ram(covariateData2$covariateRef[idx, ]))
    }
  } else {
    covariates$count2 <- " 0"
    covariates$percent2 <- " 0"
    covariates$stdDiff <- " 0"
    covariatesContinuous$averageValue2 <- "  "
    covariatesContinuous$standardDeviation2 <- "  "
    covariatesContinuous$minValue2 <- "  "
    covariatesContinuous$p25Value2 <- "  "
    covariatesContinuous$medianValue2 <- "  "
    covariatesContinuous$p75Value2 <- "  "
    covariatesContinuous$maxValue2 <- "  "
    covariatesContinuous$stdDiff <- "  "
  }
  
  binaryTable <- data.frame()
  continuousTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      binaryTable <- rbind(binaryTable,
                           data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- analysisRef$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        isBinary <- analysisRef$isBinary[idx]
        covariateIds <- NULL
        if (isBinary == "Y") {
          # Binary
          if (specifications$covariateIds[i] == "") {
            idx <- covariateRef$analysisId == specifications$analysisId[i]
          } else {
            covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ",")[[1]])
            idx <- covariateRef$covariateId %in% covariateIds
          }
          if (any(idx)) {
            covariateRefSubset <- covariateRef[idx, ]
            covariatesSubset <- merge(covariates, covariateRefSubset)
            if (is.null(covariateIds)) {
              covariatesSubset <- covariatesSubset[order(covariatesSubset$covariateId), ]
            } else {
              covariatesSubset <- merge(covariatesSubset, data.frame(covariateId = covariateIds,
                                                                     rn = 1:length(covariateIds)))
              covariatesSubset <- covariatesSubset[order(covariatesSubset$rn,
                                                         covariatesSubset$covariateId), ]
            }
            covariatesSubset$covariateName <- fixCase(gsub("^.*: ",
                                                           "",
                                                           covariatesSubset$covariateName))
            if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
              binaryTable <- rbind(binaryTable, data.frame(Characteristic = specifications$label[i],
                                                           count1 = "",
                                                           percent1 = "",
                                                           count2 = "",
                                                           percent2 = "",
                                                           stdDiff = "",
                                                           stringsAsFactors = FALSE))
              binaryTable <- rbind(binaryTable,
                                   data.frame(Characteristic = paste0("  ", covariatesSubset$covariateName),
                                              count1 = covariatesSubset$count1,
                                              percent1 = covariatesSubset$percent1,
                                              count2 = covariatesSubset$count2,
                                              percent2 = covariatesSubset$percent2,
                                              stdDiff = covariatesSubset$stdDiff,
                                              stringsAsFactors = FALSE))
            } else {
              binaryTable <- rbind(binaryTable, data.frame(Characteristic = specifications$label[i],
                                                           count1 = covariatesSubset$count1,
                                                           percent1 = covariatesSubset$percent1,
                                                           count2 = covariatesSubset$count2,
                                                           percent2 = covariatesSubset$percent2,
                                                           stdDiff = covariatesSubset$stdDiff,
                                                           stringsAsFactors = FALSE))
            }
          }
        } else {
          # Not binary
          if (specifications$covariateIds[i] == "") {
            idx <- covariateRef$analysisId == specifications$analysisId[i]
          } else {
            covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ",")[[1]])
            idx <- covariateRef$covariateId %in% covariateIds
          }
          if (any(idx)) {
            covariateRefSubset <- covariateRef[idx, ]
            covariatesSubset <- covariatesContinuous[covariatesContinuous$covariateId %in% covariateRefSubset$covariateId, ]
            covariatesSubset <- merge(covariatesSubset, covariateRefSubset)
            if (is.null(covariateIds)) {
              covariatesSubset <- covariatesSubset[order(covariatesSubset$covariateId), ]
            } else {
              covariatesSubset <- merge(covariatesSubset, data.frame(covariateId = covariateIds,
                                                                     rn = 1:length(covariateIds)))
              covariatesSubset <- covariatesSubset[order(covariatesSubset$rn,
                                                         covariatesSubset$covariateId), ]
            }
            covariatesSubset$covariateName <- fixCase(gsub("^.*: ",
                                                           "",
                                                           covariatesSubset$covariateName))
            if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
              continuousTable <- rbind(continuousTable,
                                       data.frame(Characteristic = specifications$label[i],
                                                  value1 = "",
                                                  value2 = "",
                                                  stdDiff = "",
                                                  stringsAsFactors = FALSE))
              for (j in 1:nrow(covariatesSubset)) {
                continuousTable <- rbind(continuousTable,
                                         data.frame(Characteristic = paste0("  ", covariatesSubset$covariateName[j]),
                                                    value1 = "",
                                                    value2 = "",
                                                    stdDiff = "",
                                                    stringsAsFactors = FALSE))
                continuousTable <- rbind(continuousTable, data.frame(Characteristic = c("    Mean",
                                                                                        "    Std. deviation",
                                                                                        "    Minimum",
                                                                                        "    25th percentile",
                                                                                        "    Median",
                                                                                        "    75th percentile",
                                                                                        "    Maximum"),
                                                                     value1 = c(covariatesSubset$averageValue1[j],
                                                                                covariatesSubset$standardDeviation1[j],
                                                                                covariatesSubset$minValue1[j],
                                                                                covariatesSubset$p25Value1[j],
                                                                                covariatesSubset$medianValue1[j],
                                                                                covariatesSubset$p75Value1[j],
                                                                                covariatesSubset$maxValue1[j]),
                                                                     value2 = c(covariatesSubset$averageValue2[j],
                                                                                covariatesSubset$standardDeviation2[j],
                                                                                covariatesSubset$minValue2[j],
                                                                                covariatesSubset$p25Value2[j],
                                                                                covariatesSubset$medianValue2[j],
                                                                                covariatesSubset$p75Value2[j],
                                                                                covariatesSubset$maxValue2[j]),
                                                                     stdDiff = c(covariatesSubset$stdDiff[j],
                                                                                 "  ",
                                                                                 "  ",
                                                                                 "  ",
                                                                                 "  ",
                                                                                 "  ",
                                                                                 "  "),
                                                                     stringsAsFactors = FALSE))
                
              }
            } else {
              continuousTable <- rbind(continuousTable,
                                       data.frame(Characteristic = specifications$label[i],
                                                  value1 = "",
                                                  value2 = "",
                                                  stdDiff = "",
                                                  stringsAsFactors = FALSE))
              continuousTable <- rbind(continuousTable, data.frame(Characteristic = c("    Mean",
                                                                                      "    Std. deviation",
                                                                                      "    Minimum",
                                                                                      "    25th percentile",
                                                                                      "    Median",
                                                                                      "    75th percentile",
                                                                                      "    Maximum"),
                                                                   value1 = c(covariatesSubset$averageValue1,
                                                                              covariatesSubset$standardDeviation1,
                                                                              covariatesSubset$minValue1,
                                                                              covariatesSubset$p25Value1,
                                                                              covariatesSubset$medianValue1,
                                                                              covariatesSubset$p75Value1,
                                                                              covariatesSubset$maxValue1),
                                                                   value2 = c(covariatesSubset$averageValue2,
                                                                              covariatesSubset$standardDeviation2,
                                                                              covariatesSubset$minValue2,
                                                                              covariatesSubset$p25Value2,
                                                                              covariatesSubset$medianValue2,
                                                                              covariatesSubset$p75Value2,
                                                                              covariatesSubset$maxValue2),
                                                                   stdDiff = c(covariatesSubset$stdDiff,
                                                                               "  ",
                                                                               "  ",
                                                                               "  ",
                                                                               "  ",
                                                                               "  ",
                                                                               "  "),
                                                                   stringsAsFactors = FALSE))
            }
          }
        }
      }
    }
  }
  if (nrow(continuousTable) != 0) {
    if (showCounts && showPercent) {
      if (comparison) {
        continuousTable$dummy1 <- ""
        continuousTable$dummy2 <- ""
        continuousTable <- continuousTable[, c(1, 5, 2, 6, 3, 4)]
        colnames(continuousTable) <- c("Characteristic", "", "Value", "", "Value", "Std.Diff")
      } else {
        continuousTable$dummy <- ""
        continuousTable <- continuousTable[, c(1,3,2)]
        colnames(continuousTable) <- c("Characteristic", "", "Value")
      }
    } else {
      if (comparison) {
        colnames(continuousTable) <- c("Characteristic", "Value", "Value", "Std.Diff")
      }  else {
        continuousTable$value2 <- NULL
        continuousTable$stdDiff <- NULL
        colnames(continuousTable) <- c("Characteristic", "Value")
      }
    }
  }
  
  if (nrow(binaryTable) != 0) {
    if (comparison) {
      colnames(binaryTable) <- c("Characteristic",
                                 "Count",
                                 paste0("% (n = ",
                                        formatCount(covariateData1$metaData$populationSize),
                                        ")"),
                                 "Count",
                                 paste0("% (n = ",
                                        formatCount(covariateData2$metaData$populationSize),
                                        ")"),
                                 "Std.Diff")
      if (!showCounts) {
        binaryTable[, 4] <- NULL
        binaryTable[, 2] <- NULL
      }
      if (!showPercent) {
        binaryTable[, 5] <- NULL
        binaryTable[, 3] <- NULL
      }
    } else {
      binaryTable$count2 <- NULL
      binaryTable$percent2 <- NULL
      binaryTable$stdDiff <- NULL
      colnames(binaryTable) <- c("Characteristic",
                                 "Count",
                                 paste0("% (n = ",
                                        formatCount(covariateData1$metaData$populationSize),
                                        ")"))
      if (!showCounts) {
        binaryTable[, 2] <- NULL
      }
      if (!showPercent) {
        binaryTable[, 3] <- NULL
      }
    }
  }
  
  if (output == "two columns") {
    if (nrow(binaryTable) > nrow(continuousTable)) {
      if (nrow(continuousTable) > 0) {
        rowsPerColumn <- ceiling((nrow(binaryTable) + nrow(continuousTable) + 2)/2)
        column1 <- binaryTable[1:rowsPerColumn, ]
        ct <- continuousTable
        colnames(ct) <- colnames(binaryTable)
        column2 <- rbind(binaryTable[(rowsPerColumn + 1):nrow(binaryTable),
                                     ],
                         rep("", ncol(binaryTable)),
                         colnames(continuousTable),
                         ct)
      } else {
        rowsPerColumn <- ceiling((nrow(binaryTable) + nrow(continuousTable))/2)
        column1 <- binaryTable[1:rowsPerColumn, ]
        column2 <- binaryTable[(rowsPerColumn + 1):nrow(binaryTable), ]
      }
      if (nrow(column1) > nrow(column2)) {
        column2 <- rbind(column2, rep("", ncol(binaryTable)))
      }
      result <- cbind(column1, column2)
    } else {
      stop("Don't know what to do when there are more rows in the table of continuous covariates than there are in the table of binary covariates.")
    }
  } else if (output == "one column") {
    ct <- continuousTable
    colnames(ct) <- colnames(binaryTable)
    result <- rbind(binaryTable,
                    rep("", ncol(binaryTable)),
                    colnames(continuousTable),
                    ct)
  } else {
    result <- list(part1 = binaryTable, part2 = continuousTable)
  }
  return(result)
}