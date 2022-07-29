# This script downloads American Community Survey (ACS) results for MN

if (TRUE) {
  
  library(mnrisks)
  library(tidyverse)
  library(sf)
  library(leaflet)
  library(tidycensus)
  library(tigris)
  
  # Previous years data
  year <- 2020
  
  # Load block group data
  load(paste0("data/bg_census_", year, ".rda"))
              
  # ACS data requires a Census key
  # Visit: http://api.census.gov/data/key_signup.html
  census_api_key("2406e6fa13ae3981943a5c0fa5798cf8b6a9602a", overwrite = TRUE) #, install = T)
  

  # View all ACS variables
  acs_variables <- load_variables(2020, "acs5", cache = TRUE)
  
  # Download 5-yr population estimates
  pops <- get_acs(geography = "block group", 
                  state     = "MN", 
                  variables = "B01003_001", 
                  survey    = "acs5") %>%
          rename(population     = estimate,
                 population_moe = moe) %>%
          select(-variable, -NAME)
  
  # Download household median income
  med_inc <- get_acs(geography = "block group", 
                     state     = "MN", 
                     variables = "B19013_001", 
                     survey    = "acs5") %>%
             rename(income_median     = estimate,
                    income_median_moe = moe) %>%
             select(-variable, -NAME)
  
  
  # Download below 150% poverty line
  povtot <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_001", #B06012_004", 
                    survey    = "acs5") %>%
            rename(total = estimate)
  
  abv0 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_002", #B06012_004", 
                    survey    = "acs5") %>%
          rename(abv0 = estimate)
  
  
  abv50 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_003", #B06012_004", 
                    survey    = "acs5") %>%
          rename(abv50 = estimate)
  
  abv100 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_004", #B06012_004", 
                    survey    = "acs5") %>%
            rename(abv100 = estimate)
  
  abv125 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_005", #B06012_004", 
                    survey    = "acs5") %>%
            rename(abv125 = estimate)
  
  abv150 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_006", #B06012_004", 
                    survey    = "acs5") %>%
            rename(abv150 = estimate)
  
  abv185 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_007", #B06012_004", 
                    survey    = "acs5") %>%
            rename(abv185 = estimate)
  
  abv200 <- get_acs(geography = "block group", 
                    state     = "MN", 
                    variables = "C17002_008", #B06012_004", 
                    survey    = "acs5") %>%
            rename(abv200 = estimate)
  
  pov150 <- left_join(abv150, select(abv185, GEOID, abv185), by = "GEOID") %>% 
            left_join(select(abv200, GEOID, abv200), by = "GEOID") %>%
            left_join(select(povtot, GEOID, total), by = "GEOID") %>%
            mutate(pct_below_150pct_poverty = round(100 * (1 - (abv150 + abv185 + abv200) / total), 2)) %>%
            select(-c(abv150, abv185, abv200, total, variable, moe, NAME))
  
  below150 <- left_join(abv0, select(abv50, GEOID, abv50), by = "GEOID") %>% 
              left_join(select(abv100, GEOID, abv100), by = "GEOID") %>%
              left_join(select(abv125, GEOID, abv125), by = "GEOID") %>%
              left_join(select(povtot, GEOID, total), by = "GEOID") %>%
              mutate(pct_below_150pct_povertyline = round(100 * (abv0 + abv50 + abv100 + abv125) / total, 2)) %>%
              select(-c(abv0, abv50, abv100, abv125, total, variable, moe, NAME))
  
  pov150 <- left_join(pov150, below150, by = "GEOID")
            
  
  # Join income indicators
  bg_census_2017 <- left_join(pops, med_inc, by = "GEOID") %>%
                    left_join(below150, by = "GEOID") %>%
                    janitor::clean_names() %>%
                    mutate(geoid = as.numeric(geoid))
  
  # Get geography
  bg_shapes_2020 <- block_groups(state = 27, year = 2020) %>%
    rename_all(tolower) %>%
    select(countyfp, aland, awater, geoid, geometry) %>%
    mutate(geoid = as.numeric(geoid),
           area  = sum(awater, aland, na.rm = T),
           countyfp = paste0("27", countyfp),
           metro = ifelse(countyfp %in% c("27053", "27123", "27003", "27019", "27037", "27163", "27139"), "Metro", "Outstate")) %>%
    rename(county_fips = countyfp) %>%
    select(-c(awater, aland))
  
  # Join geography
  bg_census_2017 <- left_join(bg_census_2017, 
                              bg_shapes_2020) %>%
                    select(geoid, area, metro, county_fips, 
                           population, population_moe, 
                           income_median, income_median_moe,
                           pct_below_150pct_povertyline)
  
  #------------------------------------------------------------------------#
  # Join EJ status by tract (no buffer?)
  ## X-drive @ X:\Agency_Files\EJ\GIS\geospatial_update\2016
  ## Online GeoCommons @ https://gisdata.mn.gov/dataset/env-ej-mpca-census
  ## Download: ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_pca/env_ej_mpca_census/shp_env_ej_mpca_census.zip
  
  # Load BG shapes
  # Load block groups
  # cb = FALSE: Detailed file, +2 BGs
  # cb = TRUE: simpler polygons, -2 BGs, but better L. Superior  boundaries
  #bg_shapes <- tigris::block_groups(state = 'MN', year = 2017, cb = FALSE) 
  
  # Convert to sf
  bg_shapes <- st_as_sf(bg_shapes)
  
  bgs_crs   <- st_crs(bg_shapes)
  
  bg_shapes <- st_transform(bg_shapes, crs = 26915)
  
  # Load MPCA EJ status
  zip_path <- "C:/Users/dkvale/Documents/Downloads"
  
  unzip(paste0(zip_path,"/shp_env_ej_mpca_census.zip"), exdir = zip_path)
  
  ej_shapes <- st_read(paste0(zip_path, "/ej_mpca_census.shp"), stringsAsFactors = F) 
  
  ej_status <- ej_shapes %>%
               st_set_geometry(NULL) %>%
               select(geoid, countyfp, status185x, statuspoc, tot) %>%
               rename(total_population = tot)
  
  # Find blockgroup intersection with tribal areas
  tribe_status <- st_read(paste0(zip_path, "/census_tribal_areas.shp"), stringsAsFactors = F) %>%
                  st_transform(crs = 26915)
  
  st_crs(tribe_status)
  
  # Tribal intersection
  trb_intersect <- st_intersection(bg_shapes, tribe_status) %>%
                   st_set_geometry(NULL) %>%
                   select(GEOID) %>%
                   unique() %>%
                   mutate(status_tribe = T)
 

  bg_shapes <- left_join(bg_shapes, trb_intersect)
  
  # EJ join
  bg_shapes <- mutate(bg_shapes, tract_id = substr(GEOID, 1, 11)) %>%
               left_join(ej_status, by = c("tract_id" = "geoid"))
  
  bg_shapes <-  bg_shapes %>%
                mutate(content = glue::glue("{status_tribe}; {statuspoc}; {status185x}"))  
  
  # Check for overlap
  qpal <- colorQuantile("Blues", coalesce(as.numeric(bg_shapes$status_tribe), 0), n = 1)
  
  leaflet(filter(st_transform(bg_shapes, '+proj=longlat +datum=WGS84'), 
                 status_tribe | statuspoc == "YES" | status185x == "YES")) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(color     = ~ifelse(statuspoc == "YES" | status185x == "YES", "blue", "green"), 
                popup     = ~content) %>%
    addPolygons(data    = st_transform(tribe_status, '+proj=longlat +datum=WGS84'), 
                color   = "orange", 
                stroke  = F, 
                opacity = 0.8) %>%
    addPolygons(data    = st_transform(filter(ej_shapes, statuspoc == "YES" | status185x == "YES" ), '+proj=longlat +datum=WGS84'), 
                color   = "brown", 
                stroke  = F, 
                opacity = 0.85)
  
  # Convert to data.frame
  ej_status <- st_set_geometry(bg_shapes, NULL) %>%
               mutate(geoid               = as.numeric(GEOID),
                      status_tribe        = coalesce(status_tribe, FALSE),
                      status_poc          = statuspoc == "YES",
                      status_185x_poverty = status185x == "YES") %>%
               select(geoid, status_tribe, status_poc, status_185x_poverty)
  
  bg_census_2014 <- left_join(bg_census_2014, ej_status)
  
  # Save to package
  usethis::use_data(bg_census_2014, overwrite = TRUE)
  
}

##
