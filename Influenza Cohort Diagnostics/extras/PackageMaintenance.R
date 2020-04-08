# Copyright 2019 Observational Health Data Sciences and Informatics
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

# Format and check code ---------------------------------------------------
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("InfluenzaHospCohortDiag")
OhdsiRTools::updateCopyrightYearFolder()

# Create manual -----------------------------------------------------------
shell("rm extras/InfluenzaHospCohortDiag.pdf")
shell("R CMD Rd2pdf ./ --output=extras/InfluenzaHospCohortDiag.pdf")


# Insert cohort definitions from ATLAS into package -----------------------
ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "inst/settings/CohortsToCreateInfluenza.csv",
                                                 baseUrl = "http://10.80.192.24:8080/WebAPI",
                                                 insertTableSql = TRUE,
                                                 insertCohortCreationR = TRUE,
                                                 generateStats = TRUE,
                                                 packageName = "InfluenzaHospCohortDiag")


# Store environment in which the study was executed -----------------------
OhdsiRTools::insertEnvironmentSnapshotInPackage("InfluenzaHospCohortDiag")
