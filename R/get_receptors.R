#' get_receptors
#'
#' MNRISKS modeled receptors 
#' @param bg_id Block group IDs to include. Default includes all.
#' @param year The MNRISKS results year. Default value is 2017.
#' @keywords receptors mnrisks
#' @export
#' @examples
#' # All receptors
#' points <- get_receptors()
#' 
#' #For selected block groups
#' points <- get_receptors(bg_id = c(271090017012))
# 

get_receptors <- function(bg_id = NULL, year = 2017) {
  
  # Load receptors data frame
  e <- new.env()
  
  #recept_df <- readRDS(paste0("data/receptors_", year, ".rdata"))
  recept_df <- get(data(list = paste0("receptors_", year), envir = e), envir = e)
  
  if(!is.null(bg_id)) recept_df <- subset(recept_df, mpca_bg_geoid %in% bg_id)
  
  return(recept_df)
  
}
