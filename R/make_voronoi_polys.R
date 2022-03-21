#' make_voronoi_polys
#' 
#' Create Minnesota voronoi grid from receptors
#' @keywords spatial grid polygons voronoi
#' @export
#' @examples
#' \dontrun{
#' # For all counties
#' make_voronoi_polys()
#' }
#' 
# 
# 
make_voronoi_polys <- function() {
  
  library(sp)
  library(rgdal)
  library(dplyr)
  library(sf)
  library(tigris)

  
  # Create boundary box polygon
  bbox_polygon <- function(x) {
    bb <- sf::st_bbox(x)
    
    p <- matrix(
      c(bb["xmin"], bb["ymin"], 
        bb["xmin"], bb["ymax"],
        bb["xmax"], bb["ymax"], 
        bb["xmax"], bb["ymin"], 
        bb["xmin"], bb["ymin"]),
      ncol = 2, byrow = T
    )
    
    sf::st_polygon(list(p))
  }
  
  # Load county shapefiles
  counties <- counties(state = 'MN', cb = TRUE, resolution = '5m')
  #counties20 <- counties(state = 'MN', cb = TRUE, resolution = '20m')
  
  # Convert to sf
  counties <- st_as_sf(counties)
  
  county_epsg <- st_crs(counties)$epsg
  county_proj <- st_crs(counties)$proj4string
  county_crs  <- st_crs(counties)

  # Load block groups
  bgs <- block_groups(state = 'MN', cb = T, year = 2015) # Detailed file
  #bgs2 <- block_groups(state = 'MN', cb = T)
  #bgs  <- mnrisks2011::bg_shape()
  
  bg_ids <- bgs$GEOID
  
 
  # Convert to sf
  bgs <- st_as_sf(bgs)
  
  bgs_crs <- st_crs(bgs)
  
  
  # Find block group neighbors
  bg_neighbors <- st_touches(bgs)
  
  # Load receptors
  receptors <- get_receptors()
 
  # For missing block group: 271090017024
  bg <- bgs$GEOID[1]
  
  i <- grep(271090017024, bgs$GEOID)
  
  # Fewest receptors
  #i <- grep(270990006001, bgs$GEOID)
  i <- grep(270531093001, bgs$GEOID)
  i <- grep(271617902003, bgs$GEOID)
  i <- grep(270717905003, bgs$GEOID)
  i <- grep(270531039002, bgs$GEOID)

  

  
  # Blank table for receptor fractions
  receptor_area_fractions <- data_frame()
  
  # Loop 1 block group at a time
  for(i in 1:length(bg_ids)) {
    
    bg <- bgs$GEOID[i]
    
    print(i)
    print(bg)
    
    # Create boundary for selected block group that includes neighboring block groups
    neighbors <- unlist(c(bg_neighbors[[i]], bg_neighbors[bg_neighbors[[i]]]))
    
    bg_sub <- subset(bgs, GEOID %in% c(bg, bg_ids[neighbors])) 
 
    
    # Filter receptors to selected block group and neighbors
    sub_recepts <- subset(receptors, geoid %in% bg_sub$GEOID)
    
    rec_nums    <- sub_recepts$receptor
    
    bg_recepts  <- sub_recepts[ , c("long", "lat")] %>%
                   as.matrix() %>%
                   st_multipoint() %>%
                   st_sfc() %>%
                   st_set_crs(4269)
    
    bg_recepts  <- st_sf(data_frame(receptor = rec_nums, 
                                    geom = st_cast(bg_recepts, to = "POINT")))

    
    bg_recepts_utm <- sub_recepts[ , c("utm_x", "utm_y")] %>%
                      as.matrix() %>%
                      st_multipoint() %>%
                      st_sfc() %>%
                      st_set_crs(26915)    
 
    
    # Create voronoi polygons
    v_polys <- st_voronoi(bg_recepts_utm) %>% st_cast()
    
    if(F) { plot(v_polys, col = 0) }
    
    
    # Switch projection
    #v_polys <- v_polys %>% st_transform(4269)
    
    
    # Add receptor number labels
    bg_recepts <- st_sf(data_frame(receptor = rec_nums, 
                                   geom = st_cast(bg_recepts_utm, to = "POINT")))
    
    v_recs <- v_polys %>% st_contains(bg_recepts)
    
    v_polys <- st_sf(data_frame(receptor = rec_nums[unlist(v_recs)], 
                                geom     = v_polys))
    
    if(F){
    for(x in 1:5){
      plot(st_geometry(v_polys)[i], col = "steelblue")
      
      plot(st_geometry(subset(bg_recepts, receptor == v_polys$receptor[i])), pch = 19, col = "orange", cex = 2, add = T)
    
    }
      }
    
    # Find boundary box
    bg_poly <- subset(bg_sub, GEOID %in% bg)
    
    #box <- st_sfc(bbox_polygon(bg_poly)) %>% st_set_crs(4269) %>% st_transform(26915) %>% st_cast()
    
    bg_poly <- bg_poly %>% st_set_crs(4269) %>% st_transform(26915) %>% st_cast()
    
    #v_polys <- st_intersection(st_cast(v_polys), box)
    v_poly <- st_intersection(st_cast(v_polys), bg_poly) %>% st_cast()
    
    
    # Plot visuals
    if(F) {
    plot(st_geometry(st_transform(bg_sub, 26915)), col = "steelblue")
    
    plot(st_geometry(bg_poly), col = 0, add = T)
    
    plot(v_poly, col = NA, add = T)
    
    plot(bg_recepts, col = "orange", pch = 19, cex = 0.4, add = T)
    
    }
    
    # Get receptor numbers for polygons
    v_recs    <- subset(bg_recepts, receptor %in% v_poly$receptor) %>% st_geometry()
    
    v_recs_df <- subset(receptors, receptor %in% v_poly$receptor)
    
    # Clear plot window
    plot(1:5, type = "n")
    
    flush.console()
    
    
    # Plot visual check
    plot(v_recs, col = "orange", pch = 19, cex = .8)
    
    text(v_recs_df$utm_x * (1 - .000002), 
         v_recs_df$utm_y * (1 + .000004), 
         labels = v_recs_df$receptor, 
         cex    = .6)
    
    plot(st_geometry(v_poly), col = NA, add =T)
    
    title(bg)
    
    flush.console()
    
    Sys.sleep(0.01)
    
    # Assign polygon areas
    v_poly$area <- st_area(v_poly) 
    
    v_poly$area_frx <- v_poly$area / sum(v_poly$area, na.rm = T) 
    
    # Collapse data frame to one row per block group
    v_summary <- tibble(geoid     = bg,
                        receptor = v_poly$receptor,
                        area_wt  = v_poly$area_frx)
    
    
    receptor_area_fractions <- bind_rows(v_summary, receptor_area_fractions)
    
}
  

  
  names(receptor_area_fractions) <- c("geoid", "receptor", "area_wt")
  
  # Round
  receptor_bg_areas_rounded <- receptor_area_fractions %>% 
                             rowwise() %>% 
                             mutate(area_wt = round(area_wt, 6))
  
  # Check every block group has at least one receptor
  coverage_check <- receptor_bg_areas_rounded %>% 
                    group_by(geoid) %>% 
                    summarize(count = n())
  
  range(coverage_check$count)
  
  # Check sums
  area_check <- receptor_bg_areas_rounded %>% 
                group_by(geoid) %>% 
                summarize(sum_wt = sum(area_wt, na.rm = T))
  
  range(area_check$sum_wt)
  
  # Save
  save(receptor_bg_areas_rounded, file = "data/receptor_bg_areas_rounded.rdata")
  
}

##
