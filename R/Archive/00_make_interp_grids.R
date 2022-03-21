#' make_interp_grids
#' 
#' Create Minnesota interpolation grids
#' @param grid_space Spacing between grid points.
#' @keywords spatial grid receptors 
#' @export
#' @examples
#' # For all counties
#' make_interp_grids()
#' 
# 
# 

make_interp_grids <- function(grid_space = 0.00135) {
  
library(tigris)
library(sp)
library(maptools)
library(dplyr)

# Load county shapefiles
county_shape <- counties(state = 'MN', cb = TRUE, resolution = '5m')

#counties20 <- counties(state = 'MN', cb = TRUE, resolution = '20m')


# Save projection info
county_proj <- proj4string(county_shape)
#proj4string(county_shape) <- CRS("+init=epsg:4326")

coordinates(county_shape)[1:5]


# Create grid function
make_grid <- function(shapefile = NULL) {
  
  #shape <- data.frame(coordinates(shapefile), stringsAsFactors = F)
  
  shape <- data.frame(shapefile@polygons[[1]]@Polygons[[1]]@coords)
  
  names(shape) <- c("Long", "Lat")
  
  # Find county boundaries
  xmn <- min(shape$Long)
  xmx <- max(shape$Long)
  ymn <- min(shape$Lat)
  ymx <- max(shape$Lat)
  
  # Create interpolation grid
  #grid_space <- 0.00135
  
  #if(in_metro) grid_space <- 0.00185
  
  grd1 <- expand.grid(x = seq(from = xmn - 0.01, to = xmx + 0.01, by = grid_space * 1.01), 
                      y = seq(from = ymn - 0.01, to = ymx + 0.01, by = grid_space * 0.94))
    
  grd1 <- group_by(grd1, row_number()) %>%
          mutate(ran_x = runif(1),
                 ran_y = runif(1),
                 x = x + grid_space * (0.30 - 0.60*runif(1)),
                 y = y + grid_space * (0.30 - 0.60*runif(1)))
  
   #               x = x + ifelse(ran_x < 0.33, -grid_space * 0.15, 
   #                      ifelse(ran_x < 0.66, grid_space * 0.15, 0)),
   #              y = y + ifelse(ran_y < 0.33, -grid_space * 0.15, 
   #                    ifelse(ran_y < 0.66, grid_space * 0.15, 0)))
  
  grd1 <- grd1[ , 1:2]
  
  #plot(grd1$x[1:10] ~  grd1$y[1:10])
  #plot(grd1$x[1:200] ~ grd1$y[1:200])
  

  # Drop points outside county boundary
  #row.names(grd1) <- 1:nrow(grd1)
  
  coordinates(grd1) <- ~x + y
  
  #gridded(grd1)     <- TRUE
  
  proj4string(grd1) <- county_proj
  
  grd1$GEOID        <- sp::over(grd1, shapefile)$GEOID
  
  grd1 <- subset(grd1, !is.na(GEOID))
  
  # Plot coverage
  #print(plot(grd1$y ~ grd1$x, col = "steelblue"))
 
  sample = sample(1:length(grd1), 5000)
  
  print(plot(shapefile, col = "blue"))
  #print(points(a[sample, "y"] ~ a[sample, "x"], col = "red", pch = 16))
  #print(plot(grd1[sample, ], add = T))
  print(points(shape$Lat ~ shape$Long, col = "red", pch = 16))
 
  #Sys.sleep(0.1)
  
  return(SpatialPoints(grd1))
}

  
# Create grid for each county 
state_grid <- make_grid(shapefile = subset(county_shape, NAME == county_shape$NAME[1]))

  
for(county in county_shape$NAME[-1]) {
  
  print(county)
  
  cnty_grid  <- make_grid(shapefile = subset(county_shape, NAME == county))
  
  state_grid  <- spRbind(cnty_grid, state_grid)
  
 # state_grid <- SpatialPointsDataFrame(temp_grid, data = bind_rows(idw3@data, idw4@data), match.ID = F) 
}

# Clean
rm(cnty_grid)
rm(grd1)

row.names(state_grid) <- 1:length(state_grid)

# Test blockgroup coverage
bgs <- mnrisks2014::bg_shape()

#state_grid = grd1

proj4string(bgs)        <- county_proj
proj4string(state_grid) <- county_proj

state_grid$GEOID  <- sp::over(state_grid, bgs)$GEOID

df <- state_grid@data %>% group_by(GEOID) %>% summarize(count = n())

bgs$GEOID[!bgs$GEOID %in% df$GEOID]

plot(subset(bgs, GEOID == 270531091002))
plot(state_grid, add = T)
plot(subset(bgs, GEOID == 270270205001))
plot(state_grid, add = T)
plot(subset(bgs, GEOID == 270531056001))
plot(state_grid, add = T)
plot(subset(bgs, GEOID == 270530078011))
plot(state_grid, add = T)
plot(subset(bgs, GEOID == 271617904004), col = "blue")
plot(state_grid, add = T)
plot(subset(bgs, GEOID == 271450116001), col = "blue")
plot(state_grid, add = T)

# Block group with no receptors
plot(subset(bgs, GEOID == 271090017024))
plot(state_grid, add = T)


points(receptors$Lat ~ receptors$Long, col = "red", pch = 16)

# Label county
state_grid$FIPS  <- sp::over(state_grid, county_shape)$GEOID

# Save
save(state_grid, file = "data/mn_grid.rdata")

}

##
