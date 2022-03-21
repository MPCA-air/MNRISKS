#' get_census
#'
#' Load Census block group population data for Minnesota.
#' @param geoid c(x,y,z) a vector of block group IDs to return
#' @param year The Census year. Currently only 2010 available. 
#' @keywords blockgroup block census
#' @export
#' @examples
#' # All block groups
#' bgs <- get_census()
#' 
#' # A single block group
#' bgs <- get_census(geoid = c(271090017024))
# 
# 

get_census <- function(geoid = NULL, year = 2010) {
  
  e <- new.env()
  
  # Load block group data
  bgs <- get(data(list = paste0("bg_census_", year), envir = e), envir = e)
  
  # Filter block groups to given ID num's  
  if(!is.null(geoid)) {
    
    bgs <- subset(bgs, GEOID %in% geoid)
    
  } 
  
  return(bgs)
  
}
