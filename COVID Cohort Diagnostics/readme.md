Charactericising Hospitalized COVID-19 Patients (CovidHospCohortDiag)
==============================


- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- Incorporation of [OMOP Vocabulary release v20200331](https://github.com/OHDSI/Vocabulary-v5.0/releases) in your local ETL and following [OHDSI Community Guidance for Mapping](https://github.com/OHDSI/Covid-19/wiki/Release)
- R version 3.5.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

See [here](https://ohdsi.github.io/MethodsLibrary/rSetup.html) for instructions on how to set up the R environment on Windows.

How to run
==========

1. In `R`, use the following code to install the dependencies:

	```r
	install.packages("readr")
	install.packages("tibble")
	install.packages("dplyr")
	install.packages("RJSONIO")
	install.packages("ggplot2")
	install.packages("devtools")
	library(devtools)
	install_github("ohdsi/ParallelLogger", ref = "v1.1.1")
	install_github("ohdsi/SqlRender", ref = "v1.6.3")
	install_github("ohdsi/DatabaseConnector", ref = "v2.4.1")
	install_github("ohdsi/OhdsiSharing", ref = "v0.1.3")
	install_github("ohdsi/FeatureExtraction", ref = "v2.2.5")
	install_github("ohdsi/CohortDiagnostics")
	install_github("ohdsi/EmpiricalCalibration", ref = "v2.0.0")
	install_github("ohdsi/MethodEvaluation", ref = "v1.1.0")
 	```
  
 If using Google BiQuery, install the "develop" branch
 ```r
 devtools::install_github("OHDSI/FeatureExtraction", ref="develop")
 devtools::install_github("OHDSI/CohortDiagnostics", ref="develop")
```

If you experience problems on Windows where rJava can't find Java, one solution may be to add `args = "--no-multiarch"` to each `install_github` call, for example:
	
	```r
	install_github("ohdsi/SqlRender", args = "--no-multiarch")
	```

Alternatively, ensure that you have installed only the 64-bit versions of R and Java, as described in [the Book of OHDSI](https://ohdsi.github.io/TheBookOfOhdsi/OhdsiAnalyticsTools.html#installR)
	
2. In `R`, use the following `devtools` command to install the CovidHospCohortDiag package:

	```r
	devtools::install_github("ohdsi-studies/Covid19HospitalizationCharacterization/")
	```
	
3. Once installed, you can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:

	```r
	# Load the package
	library(InfluenzaHospCohortDiag)
	
	# Optional: specify where the temporary files (used by the ff package) will be created:
	options(fftempdir = "C:/FFtemp")
	
	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores()
	
	# Details for connecting to the server:
	connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
	                                                                 server = Sys.getenv("PDW_SERVER"),
	                                                                 user = NULL,
	                                                                 password = NULL,
	                                                                 port = Sys.getenv("PDW_PORT"))
	
	# For Oracle: define a schema that can be used to emulate temp tables:
	oracleTempSchema <- NULL
	
	# Details specific to the database:
     outputFolder <- "s:/CovidHospCohortDiag/mdcd"
     cdmDatabaseSchema <- "cdm_ibm_mdcd_v1023.dbo"
     cohortDatabaseSchema <- "scratch.dbo"
     cohortTable <- "mschuemi_skeleton_mdcd"
     databaseId <- "MDCD"
     databaseName <- "Truven Health MarketScan® Multi-State Medicaid Database"
     databaseDescription <- "Truven Health MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data. [For further information link to RWE site for Truven MDCD."
     
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
     	```  

*Note: If you experience an issue while running the code, please report this via the ```Issues``` tab above. Where possible, please include a snippet of the error message and any relevant information about your R environment / data layer (e.g. SQL Server, RedShift, Google BigQuery). Issues will be debugged and code will be updated accordingly.*

4. Upload the file ```export/Results<DatabaseId>.zip``` in the output folder to the study coordinator:

	```r
	submitResults("export/Results<DatabaseId>.zip", key = "<key>", secret = "<secret>")
	```
	
	Where ```key``` and ```secret``` are the credentials provided to you personally by the study coordinator.
		
5. To view the results, use the Shiny app:

	```r
	# To view your results: 
	CohortDiagnostics::launchDiagnosticsExplorer(file.path(outputFolder, "diagnosticsExport"))
	```
  
License
=======
The CovidHospCohortDiag package is licensed under Apache License 2.0

Development
===========
CovidHospCohortDiag was developed in R Studio.

### Development status
Beta release - potential for issues in new environments. Please report.
