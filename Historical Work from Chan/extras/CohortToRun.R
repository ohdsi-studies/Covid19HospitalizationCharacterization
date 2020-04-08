library(CovidCharacter)
# add the cdm database schema with the data
cdmDatabaseSchema <- ''
# add the work database schema this requires read/write privileges
cohortDatabaseSchema <- ''

# the name of the table that will be created in cohortDatabaseSchema to hold the cohorts
cohortTable <- 'covid_character'

# the location to save the prediction models results to:
outputFolder <- ''

databaseId <- ''

# add connection details:
options(fftempdir = '/temp')

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "your dbms",
                                                                server = Sys.getenv("server"),
                                                                user = Sys.getenv("user"),
                                                                password = Sys.getenv("password"),
                                                                port = Sys.getenv("port"))


execute(connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = NULL,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseId,
        databaseDescription = databaseId,
        createCohorts = T,
        comparativeCharacteristics = T,
        StratificationOnOutcome = F, #This function is not working now
        packageResults = T,
        maxCores = 4,
        minCellCount= 5)
