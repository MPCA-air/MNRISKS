#' get_facility_results
#'
#' MNRISKS modeled Resident Risks and air concentrations for facilities
#' @param year The MNRISKS results year. Default value is 2017.
#' @keywords facility risks mnrisks
#' @export
#' @examples


get_facility_risks <- function(source_id = NULL, year = 2017) {
  
  # Load facility risks data frame
  e <- new.env()
  
  #Include path
  path = "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/results/_lakes_output/processed_results/facility/rds/"
  
  facility_df <- readRDS(paste0(path, source_id, ".rds"))
  
  
}

