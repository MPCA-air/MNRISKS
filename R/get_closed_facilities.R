#' get_closed_facilities
#'
#' Closed facilities from TEMPO 
#' @param start_year The start year to look for permit terminations. Default = 2010.
#' @param end_year The end year to look for terminations. Default = Sys.Date().
#' @keywords permit terminated Minnesota
#' @export
#' @examples
#' # All receptors
#' closed_facilities <- get_closed_facilities()
#' 
#' #For non-default year
#' closed_facilities <- get_closed_facilities(year = 2025)
# 

get_closed_facilities <- function(start_year = 2013, 
                                  end_year = as.numeric(substring(Sys.Date(),1,4))) {
  
  
credentials <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/credentials.csv", stringsAsFactors = FALSE)

d_cnx <- RODBC::odbcConnect("deltaw", uid = credentials$delta_user, pwd = credentials$delta_pwd, believeNRows = FALSE)

terminated_facilities  <- RODBC::sqlQuery(d_cnx, "SELECT WH_TEMPO.AI_METADATA_MF.MASTER_AI_ID,
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
WHERE WH_TEMPO.MV_DOCUMENT_METADATA.ACTIVITY_TYPE_DESC = 'Permit Termination'", 
                                          stringsAsFactors = F) 


terminated_facilities <- terminated_facilities %>%
                    dplyr::filter(between(ACTIVITY_YEAR, start_year, year),
                                  TASK_DESC == "Approve Permit for Termination",
                                  PROGRAM_CODE == "AQ") %>%
                    dplyr::rename_all(tolower) %>%
                    unique()

# Get CEDR Source IDs ---------------------------------------------#
agency_source_id <- readr::read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Results/update_point_source_tool/info_tables/Facility TEMPO IDs_&_CEDR_IDs.csv") %>% 
      dplyr::filter(program_code == "AQ") %>% 
      dplyr::select(ai_id, mn_id) %>% 
      unique()

terminated_facilities <- dplyr::left_join(terminated_facilities, agency_source_id, by = c("master_ai_id" = "ai_id")) %>%
      dplyr::select(master_ai_id, mn_id) %>%
      unique()

return(terminated_facilities)
}