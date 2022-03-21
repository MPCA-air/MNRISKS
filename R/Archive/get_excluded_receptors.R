#' get_receptors
#'
#' MNRISKS 2014 modeled receptors 
#' @param bg_id Block group IDs to include. Default includes all.
#' @keywords receptors mnrisks
#' @export
#' @examples
#' # All excluded receptors
#' points <- get_excluded_receptors()
#' 
#' #For selected block groups
#' points <- get_excluded_receptors(bg_id = c(271090017012))
# 

get_excluded_receptors <- function(bg_id = NULL, year = 2014) {
 
  #e <- new.env()
  
  # Load exclude receptors data frame
  #recept_excl_df <- readRDS("data/exclude_receptors_2014.rdata")
  recept_df <- get(data(list = paste0("exclude_receptors_", year))) # , envir = e))
  
  if(!is.null(bg_id)) recept_excl_df <- subset(recept_excl_df, bg_geoid %in% bg_id)
  
  return(recept_excl_df)
  
}
