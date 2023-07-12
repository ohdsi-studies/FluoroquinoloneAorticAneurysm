# Copied from https://github.com/OHDSI/OhdsiShinyModules/blob/main/R/evidence-synth-main.R
# because I was in a hurry

library(EmpiricalCalibration)

getESTargetIds <- function(
    connectionHandler,
    mySchema, 
    cmTablePrefix,
    cgTablePrefix
){
  
  sql <- "select distinct
  c1.cohort_name as target,
  r.target_id
  
  from 
   @my_schema.@cm_table_prefixresult as r
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix
  ) 
  
  output <- as.list(result$targetId)
  names(output) <- result$target
  
  return(output)
  
}

getESOutcomeIds <- function(
    connectionHandler,
    mySchema, 
    cmTablePrefix,
    cgTablePrefix
) {
  sql <- "select distinct
  c1.cohort_name as outcome,
  r.outcome_id
  
  from 
   @my_schema.@cm_table_prefixresult as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.outcome_id
   
  where 
  tco.outcome_of_interest = 1
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix
  ) 
  
  output <- as.list(result$outcomeId)
  names(output) <- result$outcome
  
  return(output)
  
} 


getCMEstimation <- function(
    connectionHandler,
    mySchema,
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    databaseMetaData = 'database_meta_data',
    targetId,
    outcomeId
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  db.cdm_source_abbreviation as database, r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @my_schema.@cm_table_prefixresult as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @my_schema.@cm_table_prefixdiagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id and 
   r.database_id = unblind.database_id
   
   inner join
   @my_schema.@database_meta_data as db
   on db.database_id = r.database_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @my_schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    database_meta_data = databaseMetaData,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(result)
}

getMetaEstimation <- function(
    connectionHandler,
    mySchema,
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    esTablePrefix = 'es_',
    targetId,
    outcomeId
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  ev.evidence_synthesis_description as database,
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @my_schema.@es_table_prefixcm_result as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @my_schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @my_schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @my_schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix,
    es_table_prefix = esTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(unique(result))
}

getMetaEstimation <- function(
    connectionHandler,
    mySchema,
    cmTablePrefix = 'cm_',
    cgTablePrefix = 'cg_',
    esTablePrefix = 'es_',
    targetId,
    outcomeId
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "select 
  c1.cohort_name as target,
  c2.cohort_name as comparator,
  c3.cohort_name as outcome,
  r.target_id, r.comparator_id, r.outcome_id, r.analysis_id, 
  a.description,
  ev.evidence_synthesis_description as database,
  r.calibrated_rr, 
  r.calibrated_ci_95_lb, r.calibrated_ci_95_ub, r.calibrated_p,
  r.calibrated_log_rr, r.calibrated_se_log_rr
  
  from 
   @my_schema.@es_table_prefixcm_result as r
   inner join 
   @my_schema.@cm_table_prefixtarget_comparator_outcome as tco
   on 
   r.target_id = tco.target_id and 
   r.comparator_id = tco.comparator_id and 
   r.outcome_id = tco.outcome_id
   
   inner join
   
   @my_schema.@es_table_prefixcm_diagnostics_summary as unblind
   on
   r.analysis_id = unblind.analysis_id and 
   r.target_id = unblind.target_id and 
   r.comparator_id = unblind.comparator_id and 
   r.outcome_id = unblind.outcome_id 
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c1
   on c1.cohort_definition_id = r.target_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c2
   on c2.cohort_definition_id = r.comparator_id
   
   inner join
   @my_schema.@cg_table_prefixcohort_definition as c3
   on c3.cohort_definition_id = r.outcome_id
   
   inner join
   @my_schema.@cm_table_prefixanalysis as a
   on a.analysis_id = r.analysis_id
   
   inner join
   @my_schema.@es_table_prefixanalysis as ev
   on ev.evidence_synthesis_analysis_id = r.evidence_synthesis_analysis_id
   
   where 
   r.calibrated_rr != 0 and
   tco.outcome_of_interest = 1 and
   unblind.unblind = 1 and
   r.target_id = @target_id and
   r.outcome_id = @outcome_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    my_schema = mySchema,
    cm_table_prefix = cmTablePrefix,
    cg_table_prefix = cgTablePrefix,
    es_table_prefix = esTablePrefix,
    outcome_id = outcomeId,
    target_id = targetId
  ) %>%
    dplyr::mutate(
      calibratedP = ifelse(
        .data$calibratedRr < 1, 
        computeTraditionalP(
          logRr = .data$calibratedLogRr, 
          seLogRr = .data$calibratedSeLogRr, 
          twoSided = FALSE, 
          upper = TRUE
        ),
        .data$calibratedP / 2)
    )
  
  return(unique(result))
}

