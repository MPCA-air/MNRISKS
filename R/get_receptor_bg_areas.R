#' get_receptor_bg_areas
#'
#' The modeled MNRISKS receptor area coverage for each block group
#' @param bg_id Block group IDs to include. Default includes all.
#' @param year The MNRISKS results year. Default is 2017.
#' @keywords receptors mnrisks
#' @export
#' @examples
#' # All receptors
#' rec_fractions <- get_receptor_bg_areas()
#' 
#' #For selected block groups
#' rec_fractions <- get_receptor_bg_areas(bg_id = c(271090017012))
# 

get_receptor_bg_areas <- function(bg_id = NULL, year = 2017) {
  
  e <- new.env()
  
  # Load receptor fraction data frame
  recept_df <- get(data(list = paste0("receptor_bg_areas_", year), envir = e), envir = e)
  
  if(!is.null(bg_id)) recept_df <- subset(recept_df, geoid %in% bg_id)
  
  return(recept_df)
  
}
