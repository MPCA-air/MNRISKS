#' get_blockgroups
#'
#' Load Census block group data for Minnesota.
#' @param geoid c(x,y,z) a vector of block group IDs to return
#' @keywords blockgroups block group census
#' @export
#' @examples
#' # All block groups
#' bgs <- get_blockgroups()
#' 
#' # A single block group
#' bgs <- get_blockgroups(geoid = c(271090017024))
# 
# 

get_blockgroups <- function(geoid = NULL) {
  
  # Load block group data
  bgs <- get(data("bg_census", envir = environment()))
  
  # Filter block groups to given ID #'s  
  if(!is.null(geoid)) {
    
    bgs <- subset(bgs, GEOID %in% geoid)
    
  } 
  
  return(bgs)
  
}
