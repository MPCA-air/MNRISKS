
if(FALSE) {

library(tidyverse)

mpca_rec <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/2014_mnrisks_development/5. Sent to Lakes/Receptors/receptors_mnrisks_2014_v2.csv") %>%
            mutate(bg_geoid       = as.character(bg_geoid),
                   mpca_id_update = mpca_id + 1,
                   mpca_id_update = ifelse(mpca_id_update > 88437, mpca_id_update + 4, mpca_id_update)) 
            #filter(!county %in% c("Canada", "NorthDakota", "SouthDakota", "Iowa", "Wisconsin"))

e <- new.env()

lakes_rec <- get(data("lakes_receptors_2014", envir = e), envir = e) %>% 
             select(-county, -geoid, -mpca_rec_id)


# nr_rec <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/testing_2014/testing_results/Nonroad/Statewide-NONROAD_SPECIFIC-all_pollutants.csv") %>%
#           dplyr::select(`Block Group`, County, `X / Y [m]`, Receptor) %>%
#           unique()
# 
# area_rec <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/testing_2014/testing_results/Area/Statewide-AREA_SPECIFIC-all_pollutants2.csv") %>%
#             dplyr::select(`Block Group`, County, `X / Y [m]`, Receptor) %>%
#             unique()
# 
# 
# all_source_rec <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/testing_2014/testing_results/Statewide-allsources_allpollutants-resrisks2.csv") %>%
#                   dplyr::select(`Block Group`, County, `X / Y [m]`, Receptor) %>%
#                   unique()
# 
# lakes_rec <- bind_rows(nr_rec, area_rec, all_source_rec) %>% unique()
# 
# lakes_rec <- lakes_rec %>%
#               separate(`X / Y [m]`, into = c("utm_x", "utm_y"), sep = "/") %>%
#               mutate(`Block Group` = gsub("-", "", `Block Group`),
#                      utm_x = as.numeric(utm_x),
#                      utm_y = as.numeric(utm_y))


all_rec <- left_join(mpca_rec, lakes_rec, by = c("mpca_id_update" = "receptor")) %>%
           rowwise() %>%
           mutate(#county_same = identical(county, county),
                  x_same  = abs(round(as.numeric(long.x), 4) - as.numeric(long.y)) < 1.1E-04,
                  y_same  = abs(round(as.numeric(lat.x), 4) - as.numeric(lat.y))   < 1.1E-04)
                  #bkgrp_same  = as.numeric(bg_geoid) - as.numeric(`Block Group`))

## Lakes receptor numbers are off by 1 here is a join with 1 subtracted from each receptor
write_csv(all_rec, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Resources and Tools/lakes_mnrisks_receptors/lakes_mpca_receptors.csv")

#county_same <- filter(all_rec, county_same == "TRUE")
#nrow(all_rec) - nrow(county_same)
#[1] 280

#county_no_match <- filter(all_rec, county_same == "FALSE")
#unique(county_no_match$county)
## Of the receptors with matched counties, only two are mismatched in location and block group

no_match_receptors <- all_rec %>%
                      filter(!x_same | !y_same)

## The above results in zero receptors that do not match in geography
## We omit the non-matching receptors and use the receptor table saved below
## We keep the original MPCA assigned counties and blockgroups because they were
## assigned using the detailed geography and not the simplified Block groups for modeling
match_receptors <- all_rec %>%
                   filter(x_same, y_same) %>%
                   dplyr::select(mpca_id_update, mpca_id, county, bg_geoid, long.y, lat.y, x_utm, y_utm) %>%
                   rename(mpca_rec_id   = mpca_id, 
                          mpca_bg_geoid = bg_geoid, 
                          mpca_x_utm    = x_utm, 
                          mpca_y_utm    = y_utm, 
                          long          = long.y, 
                          lat           = lat.y, 
                          lakes_rec_id  = mpca_id_update)

write_csv(match_receptors, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Resources and Tools/lakes_mnrisks_receptors/lakes_mpca_receptors_2014.csv")

# no_match_receptors <- all_rec %>%
#                       filter(county_same == "FALSE"|!between(utm_x_same, -5, 5)|!between(utm_y_same, -5, 5)|!between(bkgrp_same, -0.9, 0.9)) %>%
#                       dplyr::select(mpca_id, county, bg_geoid, x_utm, y_utm, long, lat, mpca_id_update, County, `Block Group`, utm_x, utm_y) %>%
#                       rename(mpca_rec_id   = mpca_id, 
#                              mpca_bg_geoid = bg_geoid, 
#                              mpca_x_utm    = x_utm, 
#                              mpca_y_utm    = y_utm, 
#                              mpca_long     = long, 
#                              mpca_lat      = lat, 
#                              exclude_lakes_receptor =  mpca_id_update,
#                              lakes_bg_geoid         = `Block Group`,
#                              lakes_utm_x   = utm_x, 
#                              lakes_utm_y   = utm_y)

#write_csv(no_match_receptors, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Resources and Tools/lakes_mnrisks_receptors/exclude_lakes_receptors_2014.csv")

#saveRDS(no_match_receptors, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/Packages/mnrisk/mnrisks2014/data/exclude_receptors.rdata")
}