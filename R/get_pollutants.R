#' get_pollutants
#'
#' MNRISKS pollutants / COPCs (chemicals of potential concern) and parameters 
#' @param year The MNRISKS results year. Default value is 2014.
#' @keywords copc pollutant mnrisks
#' @export
#' @examples
#' # Pollutant parameters for 2014
#' pollutants <- get_pollutants(year = 2014)
#' 
# 

get_pollutants <- function(year = 2014) {
 
  return(get_copcs(year = year))
  
}
