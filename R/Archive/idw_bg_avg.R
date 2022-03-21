#' idw_bg_avg
#'
#' Find the IDW spatial block group average for the given receptors using inverse distance weighting
#' @param data Data frame with lat / long coordinates and values for averaging.
#' @keywords spatial average block group receptors mnrisks
#' @export
#' @examples
#' # For all block groups
#' bg_avg <- idw_bg_avg(data   = df, 
#'                      value  = resident_cancer,
#'                      lat    = "lat", 
#'                      long   = "long",
#'                      bg_id  = NULL)
# 
# 
idw_bg_avg <- function(data   = df, 
                       value  = "resident_cancer",
                       lat    = "Lat", 
                       long   = "Long",
                       bg_id  = NULL,
                       metro  = FALSE) {
  
  # Create shapefile from data frame
  names(data)[grepl(lat, names(data))]   <- "lat"
  names(data)[grepl(long, names(data))]  <- "long"
  names(data)[grepl(value, names(data))] <- "value"
  
  sp::coordinates(data) <- ~long + lat
  
  # Load MN grid for IDW
  new_grid <- get(data("mn_grid", envir = environment()))
  
  # Filter to Metro counties for Metro grid
  if(metro) {
    
    metro_cnty <- get(data("bg_census_2014", envir = environment()))
    
    metro_cnty <- filter(metro_cnty, metro == 1)
    
    new_grid   <- subset(new_grid, FIPS %in% metro_cnty$FIPS)
    
  }
  
  # Points per block group
  point_count  <- new_grid %>% group_by(GEOID) %>% summarize(count = n())
  
  print(paste0("Minimum points per block group: ",  min(point_count$count, na.rm = T)))
  
  # Use inverse distance weigting to interpolate values
  idw1 <- gstat::idw(formula   = value ~ 1, 
                     locations = downSp, 
                     newdata   = new_grid, 
                     idp       = 5)
  
  
  return(bg_avg)
  
}
