#' idw_receptors
#' 
#' Inverse distance weighting for MNRISKS receptors
#' @param column Column name containing the point values to interpolate.
#' @param bg_id Block group IDs to include. Default includes all.
#' @keywords spatial grid polygons voronoi
#' @export
#' @examples
#' # For all block groups
#' idw_receptors()
#' 
#' For selected block groups
#' idw_receptors(column = "cancer_risk", 
#'               bg_id = c(271090017012))
#' 
# 
# 

idw_receptors <- function(column = "cancer_risk",
                          bg_id  = NULL) {
  
  library(tigris)
  library(rgdal)
  library(gstat)
  library(raster)
  library(dplyr)
  library(sp)
  
  # Load county shapefiles
  counties <- counties(state = 'MN', cb = TRUE, resolution = '5m')
  #counties20 <- counties(state = 'MN', cb = TRUE, resolution = '20m')
  
  # Save projection info
  county_proj <- proj4string(counties)
  
  coordinates(counties)[1:5]
  
  
  # Load block groups
  bgs <- block_groups(state = 'MN', cb = F) # Detailed file
  #bgs2 <- block_groups(state = 'MN', cb = T)
  #bgs <- mnrisks2011::bg_shape()
  
  bg_ids <- bgs$GEOID
  
  bg_neighbors <- gTouches(bgs, byid = T)
  
  proj4string(bgs)  <- county_proj
  
  
  # Load receptors
  receptors <- mnrisks2011::receptors()
  
  receptors <- data.frame(receptors, stringsAsFactors = F)
  
  coordinates(receptors) <- ~Long + Lat
  proj4string(receptors) <- county_proj
  

  # Test for a sinlge blockgroup
  bg_id <- bgs$GEOID[1]
  
  # Missing block group: 271090017024
  i <- grep(271090017024, bgs$GEOID)
  
  bg_id <- bgs$GEOID[i]
  
  print(bg_id)
  
  # Filter to selected block groups and neighbors
  bg_sub <- subset(bgs, GEOID %in% c(bg_id, bg_ids[bg_neighbors[i, ]]))
  
  plot(bg_sub, col = "steelblue")
  
  bg_recepts <- subset(receptors, GEOID %in% c(bg_id, bg_ids[bg_neighbors[i, ]]))
  
  points(bg_recepts$Lat ~ bg_recepts$Long, col = "black")
  
  
  # Create 30m grid
  #bg_grid    <- spTransform(bg_sub, TA)
  
  bg_rast <- raster(bg_sub, res = 10000)
  
  #plot(bg_rast, add =T)
  
  res(bg_rast) <- 0.002  # 3 meters, if the CRS's units are in km
  
  bg_grid <- as(bg_rast, 'SpatialGrid')
  
  plot(bg_grid, add = T)
  
  # IDW using `gstat`
  library(gstat)
  
  gs  <- gstat(formula = Receptor~1, locations = recept_sub)
  
  v <- variogram(gs, width=20)
  
  head(v)
  plot(v)
  
  fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))
  fve

  plot(variogramLine(fve, 400), type='l')
  
  points(v[ , 2:3], pch = 20, col = 'red')
  
  idw <- idw(formula = gs, locations = recept_sub, newdata = bg_grid, idp = 4)
  
  ## [inverse distance weighted interpolation]
  idwr <- mask(idw, vr)
  
  plot(idwr)
  
}

  ##
  