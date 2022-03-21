

if(FALSE) {
  
library(tidyverse)

  
receptors <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/Resources and Tools/lakes_mnrisks_receptors/lakes_mpca_receptors_2014.csv")

receptors <- receptors %>%
             rename(receptor = mpca_rec_lakes_match,
                    geoid    = lakes_bg_geoid,
                    utm_x    = lakes_utm_x,
                    utm_y    = lakes_utm_y,
                    long     = mpca_long,
                    lat      = mpca_lat) %>%
             filter(!county %in% c("NorthDakota", "SouthDakota", "Iowa", "Wisconsin", "Canada"))

saveRDS(receptors,"X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/Packages/mnrisk/mnrisks2014/data/receptors.rdata" )

}