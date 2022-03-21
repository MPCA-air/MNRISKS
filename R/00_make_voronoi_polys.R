#' make_voronoi_polys
#' 
#' Create Minnesota voronoi grid from MNRISK receptors
#' @param year The MNRISKS results year. Default value is 2017.
#' @keywords spatial grid polygons voronoi
#' @export
#' @examples
#' \dontrun{
#' # For all counties
#' make_voronoi_polys()
#' }
#' 
# 
make_voronoi_polys <- function(year = 2017) {
  
  library(sp)
  library(rgdal)
  library(dplyr)
  library(sf)
  library(leaflet)
  library(janitor)
  library(dplyr)
  library(stringr)
  library(magrittr)

  
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
  
  # Load block groups
  # cb = FALSE: Detailed polygons, +2 BGs
  # cb = TRUE: simpler polygons, -2 BGs, but better L. Superior  boundaries
  bgs <- tigris::block_groups(state = 'MN', year = year, cb = TRUE) 
  
  # Convert to sf
  bgs <- st_as_sf(bgs)
  
  bgs_crs <- st_crs(bgs)
  
  bgs <- st_transform(bgs, crs = 4326)
  
  bgs_crs <- st_crs(bgs)
  
  bgs <- clean_names(bgs)
  
  bg_ids <- bgs$geoid
  
  
  # Check for extra modeled blockgroups
  ## Lake Superior
  simple_bg_shapes <- get_simple_bg_shapes() %>% filter(str_starts(geoid, "27"))
  
  simple_bg_shapes <- filter(simple_bg_shapes, !geoid %in% bgs$geoid)
  
  if (F) {
  # Plot extra BGs
  leaflet(simple_bg_shapes) %>% 
    addProviderTiles(providers$CartoDB.Positron)  %>% 
    addPolygons()
  
  # Plot trick BGs: 271370132001
  leaflet(bgs[3319,]) %>% 
    addProviderTiles(providers$CartoDB.Positron)  %>% 
    addPolygons()
}
  # Find blockgroup neighbors
  bg_neighbors <- st_touches(bgs)
  
  # Load receptors
  receptors <- get_receptors() %>%
               rename(receptor = lakes_rec_id,
                      geoid    = mpca_bg_geoid)
 
  # Transform to sf UTM
  recepts_utm <- st_as_sf(as_tibble(receptors), 
                          coords = c("mpca_x_utm", "mpca_y_utm"),
                          crs = 26915)
  
  #plot(recepts_utm[,3])
  
  # Blank table for receptor fractions
  receptor_area_fractions <- tibble()
  
  i <- 434 ##270314801002
  i <- 993 ##grep("271554601002", bgs$geoid)
  
  # Loop thru 1 block group at a time
  for(i in 1:length(bg_ids)) {
    
    bg <- bgs$geoid[i]
    
    print(i)
    print(bg)
    
    # Create boundary for selected block group that includes neighboring block groups
    # And add the neighbors of neighboring blockgroups
    neighbors <- unlist(c(bg_neighbors[[i]], bg_neighbors[bg_neighbors[[i]]]))
    
    bg_sub <- subset(bgs, geoid %in% c(bg, bg_ids[neighbors])) 
 
    plot(bg_sub[ , 6])
    
    # Check for receptors within 350 meters
    bg_poly <- bgs[i, ]
    
    bg_poly <- bg_poly %>% st_transform(26915) %>% st_cast()
    
    buffer <- st_buffer(bg_poly %>% select(geoid), 350) %>%
              st_join(recepts_utm %>% select(receptor)) 
    
    #plot(filter(recepts_utm, receptor %in% buffer$receptor)[,1])
    
   # Filter receptors to selected block group, neighbors, and nearby receptors within buffer
    #sub_recepts <- 
    bg_recepts <- subset(recepts_utm, 
                         geoid %in% bg_sub$geoid | 
                         receptor %in% buffer$receptor)
    
    rec_nums <- bg_recepts$receptor

    if (FALSE) {
      bg_recepts <-  sub_recepts[ , c("long", "lat")] %>%
                          as.matrix() %>%
                          st_multipoint() %>%
                          st_sfc() %>%
                          st_set_crs(4326) %>% 
                          st_transform(26915)  
    }
    
    # Create voronoi polygons
    v_polys <- st_voronoi(bg_recepts %>% st_union) %>% st_cast()
    
    # Add receptor number labels
    v_recs <- v_polys %>% st_contains(bg_recepts)
    
    v_polys <- st_sf(tibble(receptor = rec_nums[unlist(v_recs)], 
                            geom     = v_polys))
    
    plot(v_polys)
    
    # Find boundary box
    v_poly <- st_intersection(st_cast(st_make_valid(v_polys)), bg_poly, 1) %>%
              st_cast()
    
    plot(v_poly[ , 1])
    
    # Get receptor numbers for polygons
    v_recs <- subset(bg_recepts, receptor %in% v_poly$receptor) %>% 
              st_geometry()
    
    v_recs_df <- subset(receptors, receptor %in% v_poly$receptor)
    
    # Assign polygon areas
    v_poly$area <- st_area(v_poly) 
    
    v_poly$area_frx <- v_poly$area / sum(v_poly$area, na.rm = T) 
    
    # Collapse data frame to one row per block group
    v_summary <- tibble(geoid    = bg,
                        receptor = v_poly$receptor,
                        area_wt  = as.numeric(v_poly$area_frx))
    
    # Check Sums
    print(paste0("Area total = ", sum(v_summary$area_wt)))
    
    # Combine all geoid area fractions
    receptor_area_fractions <- bind_rows(v_summary, receptor_area_fractions)
    
    # Plot receptors around BG
    if (FALSE) {
      qpal <- colorQuantile("Greens", (filter(receptors, receptor %in% v_summary$receptor) %>% left_join(select(v_summary, - geoid)))$area_wt, n = 6)
      
      leaflet(filter(bgs, geoid == bg)) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolygons() %>%
        addCircleMarkers(data = filter(receptors, receptor %in% v_summary$receptor) %>% left_join(select(v_summary, - geoid)), 
                         lng = ~long, lat = ~lat, color = ~qpal(area_wt), stroke = F, fillOpacity = 0.7, label = ~as.character(round(area_wt, 5)))
    }
    
}
  
  names(receptor_area_fractions) <- c("geoid", "receptor", "area_wt")
  
  # Round fractions
  receptor_bg_areas_rounded <- receptor_area_fractions %>% 
                               rowwise() %>% 
                               mutate(area_wt = round(area_wt, 11))
  
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
  
  # Check number of receptors per BG
  rec_check <- receptor_bg_areas_rounded %>% 
               group_by(geoid) %>% 
               count()
  
  summary(rec_check)
  
  # Save
  receptor_bg_areas_2017 <- receptor_bg_areas_rounded
  
  usethis::use_data(receptor_bg_areas_2017, overwrite = TRUE)
  #save(receptor_bg_areas_rounded, file = paste0("data/receptor_bg_areas_", year, ".rdata"))
}

##
