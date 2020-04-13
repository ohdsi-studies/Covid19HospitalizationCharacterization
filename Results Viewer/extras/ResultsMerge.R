# The root directory contains 2 sub folders: influenza and covid 
# to hold the results from those respective packages
rootDirectory <- "E:/covidCharacterizationResults/input"
outputDirectory <- "E:/covidCharacterizationResults/output"

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
  if (length(cohortIds[cohortIds >= 5])) {
    ParallelLogger::logInfo("  -- Remapping cohort_id 5,6")
    # cohort.csv
    cohorts$cohort_id[cohorts$cohort_id == 5] <- 105
    cohorts$cohort_id[cohorts$cohort_id == 6] <- 106
    readr::write_csv(cohorts, file.path(folder, "cohort.csv"))
    remapCohortId(file.path(folder, "cohort_count.csv"))
    remapCohortId(file.path(folder, "covariate_value.csv"))
    remapCohortId(file.path(folder, "inclusion_rule_stats.csv"))
  }
}

mergeFiles <- function(filesToMerge, resultsFile) {
  filesToMergeExist <- lapply(filesToMerge, file.exists)
  if (length(filesToMergeExist[filesToMergeExist == FALSE]) == 0) {
    ParallelLogger::logInfo(paste0("  -- Merging result to ", resultsFile))
    mergedData <- do.call("rbind",lapply(filesToMerge,FUN=function(files){ read.csv(files)}))
    readr::write_csv(mergedData, resultsFile)
  }
}

# Iterate through the folder to consolidate the results -------
filesToCopy <- list("database.csv", "covariate.csv")
resultsFilesToKeep <- list("cohort.csv", "cohort_count.csv", "covariate_value.csv", "inclusion_rule_stats.csv")
databaseList <- unique(dfFullFileList$databaseId)
ParallelLogger::logInfo("Combining influenza and covid results")
for (i in 1:length(databaseList)) {
  curDatabaseId <- databaseList[i]
  foldersForDatabase <- dfFullFileList[dfFullFileList$databaseId == curDatabaseId,]
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
    for (j in 1:length(filesToCopy)) {
      file.copy(from = file.path(covidFolder, filesToCopy[j]),
                to = tempFolder)
    }
    # Someday, lapply instead of this approach
    for (j in 1:length(resultsFilesToKeep)) {
      mergeFiles(filesToMerge = file.path(foldersForDatabase$tempDirectory, resultsFilesToKeep[j]), 
                 resultsFile = file.path(tempFolder, resultsFilesToKeep[j]))
    }
  } else {
    resultsFolder <- foldersForDatabase$tempDirectory[1]
    for (j in 1:length(filesToCopy)) {
      file.copy(from = file.path(resultsFolder, filesToCopy[j]),
                to = tempFolder)
    }
    for (j in 1:length(resultsFilesToKeep)) {
      file.copy(from = file.path(resultsFolder, resultsFilesToKeep[j]), 
                to = tempFolder)
    }
  }
  # Zip the final set of files
  files <- list.files(tempFolder, pattern = ".*\\.csv$")
  zipFileName <- paste0(curDatabaseId, "_results.zip")
  #zipPath <- file.path(tempFolder, zipFileName)
  DatabaseConnector::createZipFile(zipFile = zipFileName, files = file.path(tempFolder, files), rootFolder = tempFolder)
  file.copy(from = zipFileName, to = outputDirectory, overwrite = TRUE)
  file.remove(zipFileName)
  unlink(tempFolder, recursive = TRUE)
}

# Cleanup the temp folders ---------------------------------------
unlink(dfFullFileList$tempDirectory, recursive = TRUE)

