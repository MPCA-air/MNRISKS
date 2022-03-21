#' get_bg_shapes
#'
#' Load Census block group shapefile for Minnesota.
#' @param geoid c(271090017024, y, z) a vector of block group IDs to return
#' @keywords census mn blockgroups "block group"
#' @export
#' @examples
#' # All block groups
#' bgs <- get_bg_shapes()
#' 
#' # A single block group
#' bgs <- get_bg_shapes(geoid = c(271090017024))
# 
# 

get_bg_shapes <- function(geoid = NULL) {
  
  e <- new.env()
  
  # Load shapefile
  bgs <- get(data(list = "bg_shapes", envir = e), envir = e)
  
  # Filter block groups to given ID #'s  
  if(!is.null(geoid)) {
    
    bgs <- subset(bgs, GEOID %in% as.numeric(geoid))
    
  } 
  
  return(bgs)
  
}
