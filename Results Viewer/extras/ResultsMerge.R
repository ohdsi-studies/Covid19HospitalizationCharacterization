# The root directory contains 2 sub folders: influenza and covid
# to hold the results from those respective packages. The cohort
# file contains the cohort.csv to insert into each set of results
rootDirectory <- "E:/covidCharacterizationResults/input"
outputDirectory <- "E:/covidCharacterizationResults/output"
cohortFile <- "E:/covidCharacterizationResults/input/cohort.csv"
cohortIdsToKeep <- c( 1,2,5,6,9,10,105:108)
analysisIdsToKeep <- c(1:3,6:11,209:216,409:416,901)

# The database_id used when running the analysis didn't
# match up to the ones used in the publication so fixing
# that through this mapping
databaseIdMap <- data.frame(rawName = character(),
                            mappedName = character())
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "CUIMC",
                                  mappedName = "CUIMC"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "DCMC_COVID19",
                                  mappedName = "DCMC"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "HIRA",
                                  mappedName = "HIRA"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "DEID",
                                  mappedName = "STARR-OMOP"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "VA-OMOP",
                                  mappedName = "VA OMOP"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "HM",
                                  mappedName = "HM"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "SIDIAP",
                                  mappedName = "SIDIAP"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "RED",
                                  mappedName = "Tufts CLARET"))
databaseIdMap <- rbind(databaseIdMap,
                       data.frame(rawName = "UCHealth_OMOP",
                                  mappedName = "HDC-UColorado"))

if (!dir.exists(outputDirectory)) {
  dir.create(outputDirectory)
}

# Unzip all of the files and keep track of where each one is stored by database ---------------
processZipFiles <- function(folder, analysis) {
  dfFileList <- data.frame(databaseId = character(),
                           analysis = character(),
                           tempDirectory = character())
  zipFiles <- list.files(folder, pattern = ".*\\.zip$")
  for (i in 1:length(zipFiles)) {
    ParallelLogger::logInfo(paste0("   -- ", zipFiles[i]))
    tempFolder <- tempfile()
    dir.create(tempFolder)
    unzip(file.path(folder, zipFiles[i]), exdir = tempFolder)
    csvFiles <- list.files(tempFolder, pattern = ".csv")
    if (length(csvFiles[csvFiles == "database.csv"])) {
      databaseInfo <- read.csv(file.path(tempFolder, "database.csv"))
      databaseId <- as.character(databaseInfo$database_id)
    } else {
      warning(paste0("No database.csv file found for ", zipFiles[i]))
    }
    dfFileList <- rbind(dfFileList,
                        data.frame(databaseId = databaseId,
                                   analysis = analysis,
                                   tempDirectory = tempFolder))
  }
  return(dfFileList)
}

ParallelLogger::logInfo("Unzipping influenza results")
dfInfluenzaFileList <- processZipFiles(file.path(rootDirectory, "influenza"), "influenza")
ParallelLogger::logInfo("Unzipping covid results")
dfCovidFileList <- processZipFiles(file.path(rootDirectory, "covid"), "covid")
dfFullFileList <- rbind(dfInfluenzaFileList, dfCovidFileList)

getAnalysisIdFromCovariateId <- function(covariateId) {
  analysisId <- substr(covariateId, nchar(covariateId)-2, nchar(covariateId))
  return(as.integer(analysisId))
}

isStudyCovariate <- function(covariateId) {
  analysisId <- getAnalysisIdFromCovariateId(covariateId)
  return(!is.na(match(analysisId, analysisIdsToKeep)))
}

subsetToCohortsAndAnalysesToKeep <- function(data) {
  if (any(names(data) == 'cohort_id')) {
    data <- data[data$cohort_id %in% cohortIdsToKeep, ]
  }
  if (any(names(data) == 'covariate_id')) {
    data <- data[isStudyCovariate(data$covariate_id) == TRUE, ]
  }
  return(data)
}

remapCohortId <- function(filePath) {
  if (file.exists(filePath)) {
    data <- read.csv(filePath)
    data$cohort_id[data$cohort_id == 5] <- 105
    data$cohort_id[data$cohort_id == 6] <- 106
    readr::write_csv(data, filePath)
  } else {
    warning(paste("File:", filePath, "not found"))
  }
}

covidCohortMapping <- function(folder) {
  # Find the files that require a new mapping of the cohort IDs
  # Specifically cohort_id == 5 -> 105, cohort_id == 6 -> 106
  ParallelLogger::logInfo("  -- Checking covid cohort mapping")
  cohorts <- read.csv(file.path(folder, "cohort.csv"))
  cohortIds <- cohorts$cohort_id
  if (length(cohortIds[cohortIds %in% c(5,6)])) {
    ParallelLogger::logInfo("  -- Remapping cohort_id 5,6")
    remapCohortId(file.path(folder, "cohort_count.csv"))
    remapCohortId(file.path(folder, "covariate_value.csv"))
    remapCohortId(file.path(folder, "inclusion_rule_stats.csv"))
  }
}

remapDatabaseId <- function(file, mappedDatabaseName) {
  if (file.exists(file)) {
    data <- read.csv(file)
    if (any(names(data) == 'database_id')) {
      data$database_id <- mappedDatabaseName
      if (any(names(data) == 'database_name')) {
        data$database_name <- mappedDatabaseName
      }
      readr::write_csv(data, file);
    }
  }
}

remapDatabaseIdAndCopy <- function(from, to, mappedDatabaseName) {
  remapDatabaseId(from, mappedDatabaseName)
  file.copy(from = from,
            to = to)
}

combine <- function(files, resultsFile, mappedDatabaseName) {
  filesExist <- lapply(files, file.exists)
  if (length(filesExist[filesExist == FALSE]) == 0) {
    for (i in 1:length(files)) {
      remapDatabaseId(files[i], mappedDatabaseName)
    }
    combinedData <- do.call("rbind",lapply(files,FUN=function(files){ read.csv(files)}))
    combinedData <- subsetToCohortsAndAnalysesToKeep(combinedData)
    return(combinedData)
  } else {
    return(NULL)
  }
}

concatFiles <- function(files, resultsFile, mappedDatabaseName) {
  ParallelLogger::logInfo(paste0("  -- Combining result to ", resultsFile))
  combinedData <- combine(files, resultsFile, mappedDatabaseName)
  if (!is.null(combinedData)) {
    readr::write_csv(combinedData, resultsFile)
  }
}

distinctFiles <- function(files, resultsFile, mappedDatabaseName) {
  ParallelLogger::logInfo(paste0("  -- Combining distinct results to ", resultsFile))
  combinedData <- combine(files, resultsFile, mappedDatabaseName)
  combinedData <- subsetToCohortsAndAnalysesToKeep(combinedData)
  if (!is.null(combinedData)) {
    readr::write_csv(unique(combinedData), resultsFile)
  }
}

# Iterate through the folder to consolidate the results -------
filesToCopy <- list("database.csv")
resultsFilesToCombine <- list("cohort_count.csv", "covariate_value.csv", "inclusion_rule_stats.csv")
resultsFilesToDistinct <- list("covariate.csv")
databaseList <- unique(dfFullFileList$databaseId)
ParallelLogger::logInfo("Combining influenza and covid results")
for (i in 1:length(databaseList)) {
  curDatabaseId <- as.character(databaseList[i])
  # Get the proper database name from the map
  mappedDatabaseName <- databaseIdMap[databaseIdMap$rawName == curDatabaseId, ]$mappedName[1]
  foldersForDatabase <- dfFullFileList[dfFullFileList$databaseId == curDatabaseId,]
  foldersForDatabase <- foldersForDatabase[order(foldersForDatabase$analysis, decreasing = TRUE),]
  print(paste0("Processing: ", nrow(foldersForDatabase), " folders for ", curDatabaseId))
  # Remap the cohort ids for the covid analysis
  if (length(foldersForDatabase$analysis[foldersForDatabase$analysis == "covid"]) == 1) {
    covidCohortMapping(foldersForDatabase$tempDirectory[foldersForDatabase$analysis == "covid"])
  }
  # If we have both covid + influenza results, combine the files
  # in a 3rd temp directory
  tempFolder <- tempfile()
  dir.create(tempFolder)
  if (nrow(foldersForDatabase) == 2) {
    # Copy over the DB file from the covid directory
    covidFolder <- foldersForDatabase$tempDirectory[foldersForDatabase$analysis == "covid"]
    # Someday, do.call instead of this approach
    for (j in 1:length(filesToCopy)) {
      remapDatabaseIdAndCopy(from = file.path(covidFolder, filesToCopy[j]),
                             to = tempFolder,
                             mappedDatabaseName = mappedDatabaseName)
    }
    for (j in 1:length(resultsFilesToDistinct)) {
      distinctFiles(files = file.path(foldersForDatabase$tempDirectory, resultsFilesToDistinct[j]),
                    resultsFile = file.path(tempFolder, resultsFilesToDistinct[j]),
                    mappedDatabaseName = mappedDatabaseName)
    }
    for (j in 1:length(resultsFilesToCombine)) {
      concatFiles(files = file.path(foldersForDatabase$tempDirectory, resultsFilesToCombine[j]),
                  resultsFile = file.path(tempFolder, resultsFilesToCombine[j]),
                  mappedDatabaseName = mappedDatabaseName)
    }
  } else {
    resultsFolder <- foldersForDatabase$tempDirectory[1]
    allFilesToCopy <- c(filesToCopy, resultsFilesToCombine, resultsFilesToDistinct)
    for (j in 1:length(allFilesToCopy)) {
      remapDatabaseIdAndCopy(from = file.path(resultsFolder, allFilesToCopy[j]),
                             to = tempFolder,
                             mappedDatabaseName = mappedDatabaseName)
    }
  }
  # Copy in the cohort file
  file.copy(from = cohortFile, to = tempFolder)

  # Zip the final set of files
  files <- list.files(tempFolder, pattern = ".*\\.csv$")
  zipFileName <- paste0(mappedDatabaseName, "_results.zip")
  #zipPath <- file.path(tempFolder, zipFileName)
  DatabaseConnector::createZipFile(zipFile = zipFileName, files = file.path(tempFolder, files), rootFolder = tempFolder)
  file.copy(from = zipFileName, to = outputDirectory, overwrite = TRUE)
  file.remove(zipFileName)
  unlink(tempFolder, recursive = TRUE)
}

# Cleanup the temp folders ---------------------------------------
unlink(dfFullFileList$tempDirectory, recursive = TRUE)

CohortDiagnostics::preMergeDiagnosticsFiles("E:/covidCharacterizationResults/output")
