#' receptors
#'
#' MNRISKS 2011 modeled receptors 
#' @param bg_id Block group IDs to include. Default includes all.
#' @keywords receptors mnrisks
#' @export
#' @examples
#' # All receptors
#' points <- get_receptors()
#' 
#' #For selected block groups
#' points <- receptors(bg_id = c(271090017012))
# 

receptors <- function(bg_id = NULL) {
  
  # Load receptors data frame
  recept_df <- get(data("receptors", envir = environment()))
  
  if(!is.null(bg_id)) recept_df <- subset(recept_df, geoid %in% bg_id)
  
  return(recept_df)
  
}
