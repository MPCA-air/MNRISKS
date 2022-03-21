#' get_copc_groups
#'
#' MNRISKS pollutants / COPCs (chemicals of potential concern) groups 
#' @param year The MNRISKS results year. Default value is 2017.
#' @keywords copc pollutant mnrisks
#' @export
#' @examples
#' # COPC groups for 2017
#' copc_groups <- get_copc_groups(year = 2017)
#' 
# 

get_copc_groups <- function(year = 2017) {
 
  e <- new.env()
  
  # Load COPC data frame
  copc_groups <- get(data(list = paste0("copc_groups_", year), envir = e), envir = e)

  return(copc_groups)
  
}
