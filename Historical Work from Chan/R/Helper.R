# restricts to pop and saves/creates mapping
MapCovariates <- function(plpData,
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

# create TCO set
createTcos <- function(connectionDetails,
                       cdmDatabaseSchema,
                       cohortDatabaseSchema,
                       cohortTable,
                       oracleTempSchema,
                       minCellCount) {

    ParallelLogger::logInfo("Counting cohorts")
    sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                             "CovidCharacter",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             work_database_schema = cohortDatabaseSchema,
                                             study_cohort_table = cohortTable)

    conn <- DatabaseConnector::connect(connectionDetails)
    counts <- DatabaseConnector::querySql(conn, sql)
    colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
    counts <- addCohortNames(counts)
    DatabaseConnector::disconnect(conn)

    pathToCsv <- system.file("settings", "TcosOfInterest.csv", package = "CovidCharacter")
    tcosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    tcos <- unique(tcosOfInterest[, c("targetId", "comparatorId","outcomeIds")])

    #subsetting tcs to only those have counts more than pre-specified minimum cohort counts in the database
    tcos <- tcos [(tcos$targetId %in% counts$cohortDefinitionId[counts$cohortCount >= minCellCount]) &
                      ( (is.na(tcos$comparatorId)) | (tcos$comparatorId %in% counts$cohortDefinitionId[counts$cohortCount >= minCellCount]) ),]

    for (i in 1:nrow(tcos)){
        if(i == 1)tcosDf <- data.frame()
        targetId <- tcos$targetId[i]
        comparatorId <- tcos$comparatorId[i]
        comparatorId <- ifelse(comparatorId=="",NA,comparatorId)
        outcomeIds <- tcos$outcomeIds[i]
        if(is.na(outcomeIds)|is.null(outcomeIds)|outcomeIds==""){
            outcomeIds <- NA
        }else{
            outcomeIds <- as.numeric(strsplit(outcomeIds, split = ";")[[1]])
        }
        tcosDf <- rbind(tcosDf, expand.grid(list(targetId=targetId,
                                                 comparatorId=comparatorId,
                                                 outcomeIds=outcomeIds)))
    }
    tcosList <- split(tcosDf, seq(nrow(tcosDf)))

    return(tcosList)
}
