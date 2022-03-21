#' get_copcs
#'
#' MNRISKS pollutants / COPCs (chemicals of potential concern) and parameters 
#' @param year The MNRISKS results year. Default value is 2017.
#' @keywords copc pollutant mnrisks
#' @export
#' @examples
#' # COPC parameters for 2017
#' copcs <- get_copcs(year = 2017)
#' 
# 

get_copcs <- function(year = 2017) {
 
  # Load COPC data frame
  e <- new.env()
  
  copc <- get(data(list = paste0("copc_", year), envir = e), envir = e)
  
  return(copc)
  
}

#' @export
get_copc <- get_copcs
