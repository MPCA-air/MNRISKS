#' get_closed_facilities
#'
#' Closed facilities from TEMPO 
#' @param year The facility focus program year. Default value is 2022.
#' @keywords permit terminated Minnesota
#' @export
#' @examples
#' # All receptors
#' closed_facilities <- get_closed_facilities()
#' 
#' #For non-default year
#' closed_facilities <- get_closed_facilities(year = 2025)
# 

get_closed_facilities <- function(year = 2022) {
  
require(RODBC)
require(tidyverse)

credentials <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/credentials.csv", stringsAsFactors = FALSE)

start_year = 2013

d_cnx <- odbcConnect("deltaw", uid = credentials$delta_user, pwd = credentials$delta_pwd, believeNRows = FALSE)

terminated_facilities  <- sqlQuery(d_cnx, "SELECT WH_TEMPO.AI_METADATA_MF.MASTER_AI_ID,
WH_TEMPO.AI_METADATA_MF.MASTER_AI_NAME,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.TASK_DESC,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.REFERENCE_TASK_ID,
WH_TEMPO.MV_DOCUMENT_METADATA.ACTIVITY_TYPE_DESC AS Activity,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.COMPLETED_DATE,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.CUSTOM_TASK_DESC,
WH_TEMPO.MV_DOCUMENT_METADATA.ACTIVITY_YEAR,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.ACTIVITY_ID,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.ACTIVITY_CATEGORY_CODE,
WH_TEMPO.MV_DOC_ACTIVITY_TASK.STATUS_DESC,
WH_TEMPO.MV_DOCUMENT_METADATA.PROGRAM_CODE
FROM WH_TEMPO.MV_DOCUMENT_METADATA
INNER JOIN WH_TEMPO.AI_METADATA_MF
ON WH_TEMPO.MV_DOCUMENT_METADATA.MASTER_AI_ID = WH_TEMPO.AI_METADATA_MF.MASTER_AI_ID
INNER JOIN WH_TEMPO.MV_DOC_ACTIVITY_TASK
ON WH_TEMPO.MV_DOCUMENT_METADATA.MASTER_AI_ID = WH_TEMPO.MV_DOC_ACTIVITY_TASK.MASTER_AI_ID
AND WH_TEMPO.MV_DOCUMENT_METADATA.ACTIVITY_ID = WH_TEMPO.MV_DOC_ACTIVITY_TASK.ACTIVITY_ID
WHERE WH_TEMPO.MV_DOCUMENT_METADATA.ACTIVITY_TYPE_DESC = 'Permit Termination'", stringsAsFactors = F) 


terminated_facilities <- terminated_facilities %>%
  filter(between(ACTIVITY_YEAR, start_year, year),
         TASK_DESC == "Approve Permit for Termination",
         PROGRAM_CODE == "AQ") %>%
  rename_all(tolower) %>%
  unique()

# Get CEDR Source IDs ---------------------------------------------#
agency_source_id <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Results/update_point_source_tool/info_tables/Facility TEMPO IDs_&_CEDR_IDs.csv") %>% filter(program_code == "AQ") %>% select(ai_id, mn_id) %>% unique()

terminated_facilities <- left_join(terminated_facilities, agency_source_id, by = c("master_ai_id" = "ai_id")) %>%
  select(master_ai_id, mn_id) %>%
  unique()

return(terminated_facilities)
}