Clinical Characterization of Patients Hospitalized with COVID-19 and Influenza
=============

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available"> 

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **OHDSI, Study-a-thon, COVID-19**
- Study lead: **Edward Burn, Seng Chan You**
- Study lead forums tag: **[EdBurn](https://forums.ohdsi.org/u/edburn), [SCYou](https://forums.ohdsi.org/u/SCYou)**
- Study start date: **March 16, 2020**
- Study end date: **April 9, 2020**
- Protocol: **[Word Doc](https://github.com/ohdsi-studies/Covid19HospitalizationCharacterization/blob/master/documents/Protocol_COVID-19%20Hospitalization%20Characterisation_V1.1.docx)**
- Publications: **[MedRxiv](https://www.medrxiv.org/content/10.1101/2020.04.22.20074336v1)**
- Results explorer: **[Shiny app](http://evidence.ohdsi.org:3838/Covid19CharacterizationHospitalization/)**

A study to describe the baseline characteristics of suspected cases of the COVID-19 or confirmed cases of the COVID-19 and influenza patients from 2014-2019 and 2009-2010 (H1N1 pandemic).

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- Incorporation of [OMOP Vocabulary release v20200331](https://github.com/OHDSI/Vocabulary-v5.0/releases) in your local ETL and following [OHDSI Community Guidance for Mapping](https://github.com/OHDSI/Covid-19/wiki/Release)
- R version 3.5.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

See [here](https://ohdsi.github.io/MethodsLibrary/rSetup.html) for instructions on how to set up the R environment on Windows.

How to run
==========
1. This study is currently comprised of two cohort diagnostics packages. The first, in ["/COVID Cohort Diagnostics"](https://github.com/ohdsi-studies/Covid19HospitalizationCharacterization/tree/master/COVID%20Cohort%20Diagnostics), is for cohorts of individuals with COVID-19. The second, in ["/Influenza Cohort Diagnostic"](https://github.com/ohdsi-studies/Covid19HospitalizationCharacterization/tree/master/Influenza%20Cohort%20Diagnostics), is for cohorts of individuals with influenza in previous years.  

2. You will ultimately build two separate libraries for these analysis: CovidHospCohortDiag (the Covid Characterization study) and InfluenzaHospCohortDiag (the Influena Characterization study). Each Analysis folder has specific instructions on how to install the study library.
 
 *Note: If you encounter errors in devtools pulling the study packages, you may find it easier to download the repo zip locally and uploading it through your RStudio console. Instructions to upload packages are provided in [The Book of OHDSI](https://ohdsi.github.io/TheBookOfOhdsi/PopulationLevelEstimation.html#running-the-study-package)*

3. When completed, the output will exist as a .ZIP file in the `export` directory in the `output` folder location. This file contains the results to submit to the study lead. To do so, please use the function below.  You must supply the directory location to where you have saved the `<study key name>.dat` file to the `privateKeyFileName` argument. You must contact the [study coordinator](mailto:kristin.kostka@iqvia.com) to receive the required private key.

  ```r
	keyFileName <- "<directory location of where you saved the study key name.dat>"
	userName <- "study-data-site-covid19"
	OhdsiSharing::sftpUploadFile(privateKeyFileName = keyFileName,
                             userName = userName,
                             remoteFolder = "Covid19Characterization/<COVID cohort diagnostics OR Influenza cohort diagnostics>",
                             fileName = "<directory location of outputFolder/export>")
  ```
  
  *Note: You will need to remove the <> and replace with information from your environment. For the remoteFolder, you will choose one: COVID cohort diagnostics OR Influenza cohort diagnostics based on which study package you ran.*
  
  If you are unable to utilize the `OhdsiSharing` package, you may utilize a SFTP client of your choosing (e.g. FileZilla) and upload through that tool. If you have questions, contact the [study coordinator](mailto:kristin.kostka@iqvia.com).


License
=======

The  CovidHospCohortDiag (the Covid Characterization study) and InfluenzaHospCohortDiag (the Influena Characterization study) packages are licensed under Apache License 2.0
