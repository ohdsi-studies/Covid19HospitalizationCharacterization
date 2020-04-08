# # Make sure to install all dependencies (not needed if already done):
# install.packages("SqlRender")
# install.packages("DatabaseConnector")
# install.packages("ggplot2")
# install.packages("ParallelLogger")
# install.packages("readr")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("RJSONIO")
# install.packages("devtools")
# devtools::install_github("OHDSI/ROhdsiWebApi")
# devtools::install_github("OHDSI/FeatureExtraction")
# devtools::install_github("OHDSI/CohortDiagnostics")
# --------------------------------------------
# If using Google BiQuery, install the "develop" branch
# devtools::install_github("OHDSI/FeatureExtraction", ref="develop")
# devtools::install_github("OHDSI/CohortDiagnostics", ref="develop")

# Load the package
library(CovidHospCohortDiag)

# Optional: specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "C:/FFtemp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# Details for connecting to the server:
# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
#                                                                 server = Sys.getenv("PDW_SERVER"),
#                                                                 user = NULL,
#                                                                 password = NULL,
#                                                                 port = Sys.getenv("PDW_PORT"))

# For Oracle: define a schema that can be used to emulate temp tables:
# oracleTempSchema <- NULL

# Details specific to the database:
# outputFolder <- "s:/CovidHospCohortDiag/mdcd"
# cdmDatabaseSchema <- "cdm_ibm_mdcd_v1023.dbo"
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "mschuemi_skeleton_mdcd"
# databaseId <- "MDCD"
# databaseName <- "Truven Health MarketScan® Multi-State Medicaid Database"
# databaseDescription <- "Truven Health MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data. [For further information link to RWE site for Truven MDCD."

# Use this to run the cohorttDiagnostics. The results will be stored in the diagnosticsExport subfolder of the outputFolder. This can be shared between sites.
runCohortDiagnostics(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortTable = cohortTable,
                     oracleTempSchema = oracleTempSchema,
                     outputFolder = outputFolder,
                     databaseId = databaseId,
                     databaseName = databaseName,
                     databaseDescription = databaseDescription,
                     createCohorts = TRUE,
                     runInclusionStatistics = FALSE,
                     runIncludedSourceConcepts = FALSE,
                     runOrphanConcepts = FALSE,
                     runTimeDistributions = FALSE,
                     runBreakdownIndexEvents = FALSE,
                     runIncidenceRates = FALSE,
                     runCohortOverlap = FALSE,
                     runCohortCharacterization = TRUE,
                     minCellCount = 10)

# To view your results: 
CohortDiagnostics::launchDiagnosticsExplorer(file.path(outputFolder, "diagnosticsExport"))


# To generate additional tables.
additionalTable1(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 cohortTable = cohortTable,
                 oracleTempSchema = oracleTempSchema,
                 outputFolder = outputFolder,
                 databaseId = databaseId,
                 minCellCount = 10)

