# Code for performing the evidence synthesis across sites. This does not need
# to be executed by each site. Instead, this code will be run only by the study
# coordinating center, after all results have been uploaded to the results database.
# install.packages("EvidenceSynthesis")
# install.packages("Strategus")
# remotes::install_github("ohdsi/ResultModelManager")

# Start of inputs --------------------------------------------------------------
resultsDatabaseConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  port = 5432,
  server = paste(keyring::key_get("fqAaStudy"), keyring::key_get("fqAaStudyDatabase"), sep = "/"),
  user = keyring::key_get("fqAaStudyUser"),
  password = keyring::key_get("fqAaStudyPassword")
)
resultsDatabaseSchema = keyring::key_get("fqAaStudySchema")
outputLocation <- "D:/SosFqAa"
# End of inputs ----------------------------------------------------------------

# connection <- DatabaseConnector::connect(resultsDatabaseConnectionDetails)

# Create Strategus analysis specifications -------------------------------------
library(Strategus)
library(dplyr)
source("https://raw.githubusercontent.com/ohdsi/EvidenceSynthesisModule/v0.2.1/SettingsFunctions.R")
evidenceSynthesisSourceCm <- createEvidenceSynthesisSource(sourceMethod = "CohortMethod",
                                                           likelihoodApproximation = "adaptive grid")
metaAnalysisCm <- createBayesianMetaAnalysis(evidenceSynthesisAnalysisId = 1,
                                             alpha = 0.05,
                                             evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
                                             evidenceSynthesisSource = evidenceSynthesisSourceCm)
evidenceSynthesisSourceSccs <- createEvidenceSynthesisSource(sourceMethod = "SelfControlledCaseSeries",
                                                             likelihoodApproximation = "adaptive grid")
metaAnalysisSccs <- createBayesianMetaAnalysis(evidenceSynthesisAnalysisId = 2,
                                               alpha = 0.05,
                                               evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
                                               evidenceSynthesisSource = evidenceSynthesisSourceSccs)
evidenceSynthesisAnalysisList <- list(metaAnalysisCm, metaAnalysisSccs)
evidenceSynthesisAnalysisSpecifications <- createEvidenceSynthesisModuleSpecifications(evidenceSynthesisAnalysisList)
analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addModuleSpecifications(evidenceSynthesisAnalysisSpecifications) 

# Create Strategus execution settings ------------------------------------------
library(Strategus)
storeConnectionDetails(connectionDetails = resultsDatabaseConnectionDetails,
                       connectionDetailsReference = "fqAaResultsConnectionDetailsRef")
executionSettings <- createResultsExecutionSettings(
  resultsConnectionDetailsReference = "fqAaResultsConnectionDetailsRef",
  resultsDatabaseSchema = resultsDatabaseSchema,
  workFolder = file.path(outputLocation, "work"),
  resultsFolder = file.path(outputLocation, "results"),
  minCellCount = 5
)

# Run Strategus ----------------------------------------------------------------
library(Strategus)
execute(analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings)

# Upload evidence synthesis results to database --------------------------------
library(dplyr)
connection <- DatabaseConnector::connect(resultsDatabaseConnectionDetails)
# # Backup old tables:
# backupFolder <- "d:/temp/resultsBackup"
# dir.create(backupFolder)
# tables <- DatabaseConnector::getTableNames(connection, resultsDatabaseSchema)
# for (table in tables) {
#   message(sprintf("Backing up table '%s.%s'", resultsDatabaseSchema, table)) 
#   data <- DatabaseConnector::dbReadTable(connection, table, databaseSchema = resultsDatabaseSchema) 
#   saveRDS(data, file.path(backupFolder, sprintf("%s.rds", table)))
# }

# Drop tables (if exist)
resultsFolder <- file.path(outputLocation, "results", "EvidenceSynthesisModule_1")
rdmsFile <- file.path(resultsFolder, "resultsDataModelSpecification.csv")
specification <- readr::read_csv(file = rdmsFile, show_col_types = FALSE) %>%
  SqlRender::snakeCaseToCamelCaseNames()
tableNames <- unique(specification$tableName)
sql <- paste(sprintf("DROP TABLE IF EXISTS @database_schema.%s;", tableNames), collapse = "\n")
sql <- SqlRender::render(
  sql = sql,
  database_schema = resultsDatabaseSchema
)
DatabaseConnector::executeSql(connection, sql)

# Create tables
resultsFolder <- file.path(outputLocation, "results", "EvidenceSynthesisModule_1")
rdmsFile <- file.path(resultsFolder, "resultsDataModelSpecification.csv")
specification <- readr::read_csv(file = rdmsFile, show_col_types = FALSE) %>%
  SqlRender::snakeCaseToCamelCaseNames()
sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
sql <- SqlRender::render(
  sql = sql,
  database_schema = resultsDatabaseSchema
)
DatabaseConnector::executeSql(connection, sql)

# Upload results
ResultModelManager::uploadResults(
  connection = connection,
  schema = resultsDatabaseSchema,
  resultsFolder = resultsFolder,
  purgeSiteDataBeforeUploading = F,
  specifications = specification
)

# Grand read access to read-only account
sql <- "GRANT SELECT ON ALL TABLES IN SCHEMA @schema TO @user;"
DatabaseConnector::renderTranslateExecuteSql(
  connection = connection,
  sql = sql,
  schema = resultsDatabaseSchema,
  user = Sys.getenv('quinoloneaadbUser')
)

# Martijn's OCD: change cohort names to something readable
# Severe OCD: Create backup
# DatabaseConnector::renderTranslateExecuteSql(
#   connection = connection,
#   sql = "SELECT * INTO @schema.cgcd_backup FROM @schema.cg_cohort_definition;",
#   schema = resultsDatabaseSchema)
cgCohortDefinition <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection,
  sql = "SELECT * FROM @schema.cg_cohort_definition;",
  schema = resultsDatabaseSchema,
  snakeCaseToCamelCase = TRUE)
cgCohortDefinition$cohortName <- gsub("\\[SOS .*\\] ", "", cgCohortDefinition$cohortName)
cgCohortDefinition$cohortName <- gsub("trimethoprim", "Trimethoprim", cgCohortDefinition$cohortName)
cgCohortDefinition$cohortName <- gsub(" - .*$", "", cgCohortDefinition$cohortName)
cgCohortDefinition$cohortName <- gsub("urinary tract infection", "UTI", cgCohortDefinition$cohortName)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = resultsDatabaseSchema,
  tableName = "cg_cohort_definition",
  data = cgCohortDefinition,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE
)
DatabaseConnector::disconnect(connection)

# Use local SQLite and Shiny app -----------------------------------------------
library(dplyr)
localConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = "d:/temp/fq.sqlite"
)
localDatabaseSchema <- "main"

localConnection <- DatabaseConnector::connect(localConnectionDetails)

# Copy existing tables from Shiny DB
connection <- DatabaseConnector::connect(resultsDatabaseConnectionDetails)
tables <- DatabaseConnector::getTableNames(connection, resultsDatabaseSchema)
tables <- tables[grepl("cg_", tables) | 
                   grepl("cm_", tables) |
                   grepl("sccs_", tables) |
                   tables == "database_meta_data"]
for (table in tables) {
  message(sprintf("Copying table %s", table))
  data <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @schema.@table;",
    schema = resultsDatabaseSchema,
    table = table)
  DatabaseConnector::insertTable(
    connection = localConnection,
    databaseSchema = localDatabaseSchema,
    tableName = table,
    data = data,
    dropTableIfExists = TRUE,
    createTable = TRUE)
}

# Create tables
resultsFolder <- file.path(outputLocation, "results", "EvidenceSynthesisModule_1")
rdmsFile <- file.path(resultsFolder, "resultsDataModelSpecification.csv")
specification <- readr::read_csv(file = rdmsFile, show_col_types = FALSE) %>%
  SqlRender::snakeCaseToCamelCaseNames()
sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
sql <- SqlRender::render(
  sql = sql,
  database_schema = localDatabaseSchema
)
DatabaseConnector::executeSql(localConnection, sql)

# Upload results
ResultModelManager::uploadResults(
  connection = localConnection,
  schema = localDatabaseSchema,
  resultsFolder = resultsFolder,
  purgeSiteDataBeforeUploading = F,
  specifications = specification
)

DatabaseConnector::disconnect(localConnection)

# Create Shiny app and launch
library(ShinyAppBuilder)
resultDatabaseDetails <- list(
  dbms = resultsDatabaseConnectionDetails$dbms,
  tablePrefix = 'cg_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = localDatabaseSchema,
  databaseTable = 'DATABASE_META_DATA'
)
cohortGeneratorModule <- createDefaultCohortGeneratorConfig(
  resultDatabaseDetails = resultDatabaseDetails,
  useKeyring = FALSE
)


# Specify cohort method module
resultDatabaseDetails <- list(
  dbms = localConnectionDetails$dbms,
  tablePrefix = 'cm_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = localDatabaseSchema,
  databaseTable = 'DATABASE_META_DATA'
)
cohortMethodModule <- createDefaultEstimationConfig(
  resultDatabaseDetails = resultDatabaseDetails,
  useKeyring = FALSE
)

# Specify SCCS module
resultDatabaseDetails <- list(
  dbms = localConnectionDetails$dbms,
  tablePrefix = 'sccs_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = localDatabaseSchema,
  databaseTable = 'DATABASE_META_DATA'
)
sccsModule <- createDefaultSCCSConfig(
  resultDatabaseDetails = resultDatabaseDetails,
  useKeyring = FALSE
)

# Specify evidence synthesis module
resultDatabaseDetails <- list(
  dbms = localConnectionDetails$dbms,
  tablePrefix = 'es_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = localDatabaseSchema,
  databaseTable = 'DATABASE_META_DATA'
)
metaModule <- ShinyAppBuilder::createModuleConfig( 
  moduleIcon = "object-group",#"meta",
  moduleId = 'EvidenceSynthesis',
  tabName = 'Meta',
  shinyModulePackage = "OhdsiShinyModules",
  moduleUiFunction = "evidenceSynthesisViewer",
  moduleServerFunction = "evidenceSynthesisServer",
  moduleDatabaseConnectionKeyUsername = 'es',
  moduleInfoBoxFile = "evidenceSynthesisHelperFile()",
  resultDatabaseDetails = list(
    tablePrefix = 'es_',
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    sccsTablePrefix = 'sccs_',
    schema = localDatabaseSchema,
    databaseMetaData = 'DATABASE_META_DATA'
  ),
  useKeyring = F
)


# Combine module specifications
shinyAppConfig <- initializeModuleConfig() %>%
  addModuleConfig(aboutModule) %>%
  addModuleConfig(cohortGeneratorModule) %>%
  addModuleConfig(cohortMethodModule) %>%
  addModuleConfig(sccsModule) %>%
  addModuleConfig(metaModule)

connectionHandler <- ResultModelManager::ConnectionHandler$new(localConnectionDetails)
ShinyAppBuilder::viewShiny(shinyAppConfig, connectionHandler)
connectionHandler$closeConnection()
