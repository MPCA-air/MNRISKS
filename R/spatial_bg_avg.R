#' spatial_bg_avg
#'
#' Find the spatial block group average, weighting each receptor by its spatial coverage of the block group.
#' @param data Data frame containing modeling results for averaging.
#' @param result_cols Column names containing the modeling concentration/risk results.
#' @param receptor_col Receptor column name with receptor ID numbers.
#' @param pollutant_col Pollutant colulmn name for grouping results.
#' @param counties 5 digit County FIPs codes to return results for, e.g. 27053, 27137. Default returns only the counties included in the data receptor column. "All" will return all MN Counties.
#' @param bg_geoids Census block group geoid numbers to include in results. Default returns all block groups.
#' @param year The MNRISKS results year. Default value is 2017.
#' @param signif_digits Number of significant figures to keep in calculated results.
#' @keywords spatial average block group receptors mnrisks
#' @export
#' @examples
#' \dontrun{
#' # For all block groups
#' bg_avg <- spatial_bg_avg(values    = df$resident_cancer, 
#'                          receptors = df$receptor,
#'                          bg_geoids = df$geoid)
#' }
# 

spatial_bg_avg <- function(data          = NULL,
                           result_cols   = c("resident_cancer", "resident_hazard",
                                             "inhalation_cancer", "inhalation_hazard", 
                                             "acute_hazard", 
                                             "annual_concentration", "acute_concentration"),
                           receptor_col  = "receptor",
                           pollutant_col = "pollutant",
                           year          = 2017,
                           counties      = NULL,
                           bg_geoids     = NULL,
                           signif_digits = 3) {
  
  if (is.null(data) | !"data.frame" %in% class(data)) stop("Incorrect data passed to function. Set `data` argument to the 'data.frame' containing the modeling results.")
  
  if (sum(!result_cols %in% names(data)) > 0) stop(paste("The result_col [", result_cols, "] was not found in the data."))
  
  if (!receptor_col %in% names(data)) stop(paste("The receptor_col [", receptor_col, "] was not found in the data."))
  
  if (!pollutant_col %in% names(data)) data$pollutant <- NA
  
  #-- Load receptor area fractions for each block group 
  #rec_frx <- get_receptor_bg_areas(year = year) %>% ungroup()
  load("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/results/_lakes_output/adjust_correction_tbls/receptor_bg_areas_2017.rdata")
  
  receptor_bg_areas_2017 <- receptor_bg_areas_rounded
  
  rec_frx <- receptor_bg_areas_2017
  
  ##recs <- get_receptors(year = year) %>% 
  #        dplyr::select(mpca_rec_id, mpca_bg_geoid) %>%
   #       dplyr::rename(receptor = mpca_rec_id,
    #                    geoid = mpca_bg_geoid)
  
  recs <- select(receptor_bg_areas_2017, receptor, geoid)
    
  #-- Update names
  data <- dplyr::rename(data,
                        receptor   = {{receptor_col}},
                        pollutant  = {{pollutant_col}})
  
  
  #-- Join geoid column if missing
  if (!"geoid" %in% names(data)) {
    
    data <- dplyr::left_join(data, recs)
    
  } else {
    data$geoid <- as.character(data$geoid)
  }
  
  #-- Filter to selected Counties
  if (!is.null(counties) && toupper(counties) == "ALL") {
    
  } else {
    
    if (is.null(counties)) {
      
      rec_frx <- subset(rec_frx, substr(geoid, 1, 5) %in% unique(substr(data$geoid, 1, 5)))
      
    } else {
      
      rec_frx <- subset(rec_frx, substr(geoid, 1, 5) %in% counties)
      
    }
    
  }
    
  
  #-- Filter to selected block groups
  if (!is.null(bg_geoids)) {
    
    #-- Remove dash from block groups
    bg_geoids <- gsub("-", "", bg_geoids)
    
    rec_frx <- subset(rec_frx, geoid %in% bg_geoids)
    
  }
  

  #-- Replace NA pollutant with "All"
  data <- data %>% dplyr::mutate(pollutant = as.character(pollutant),
                                 pollutant = tidyr::replace_na(pollutant, "All"))
  
  
  all_bg_avg <- tibble()
  
  # Loop through for each result column
  for (column in result_cols) {
    
    if (!column %in% names(data)) next()
      
    print(paste0("Calculating average [", column, "]"))
    
    #-- Create data frame for joining
    sub_data <- dplyr::rename(data, mean_value = {{column}})
    
    sub_data <- dplyr::select(sub_data, mean_value, receptor, pollutant)
    
    rec_frx_all <- dplyr::tibble()
    
    #-- Join area fractions to concentration data frame
    for (i in unique(sub_data$pollutant)) {
      
      print(i)
      
      temp_df <- dplyr::left_join(rec_frx, 
                                  dplyr::filter(sub_data, pollutant == i), 
                                  by = "receptor")
      
      #-- Set mean value at missing receptors to zero
      temp_df$mean_value <- ifelse(is.na(temp_df$mean_value), 0, temp_df$mean_value)
      
      #-- Set pollutant name
      temp_df$pollutant <- i
      
      rec_frx_all <- dplyr::bind_rows(temp_df, rec_frx_all)
    }
    
  #-- Set area weight of missing receptors to zero
  #rec_frx$area_wt <- ifelse(is.na(rec_frx$mean_value), 0, rec_frx$area_wt)
  
  #-- Calculate weighted block group averages using area fractions
  bg_avg <- rec_frx_all %>% 
            dplyr::group_by(geoid, pollutant) %>% 
            dplyr::summarise(sum_of_area_wts = sum(area_wt, na.rm = TRUE), 
                             mean_value      = sum(area_wt * mean_value, na.rm = TRUE) / sum_of_area_wts,
                             mean_value      = signif(mean_value, signif_digits)) %>% 
            dplyr::ungroup()
    
  # Rename results column
  names(bg_avg)[4] <- paste0(column, "_avg")
  
  # Combine all results
  if (nrow(all_bg_avg) < 1) {
    all_bg_avg <- select(bg_avg, -sum_of_area_wts)
  } else {
    all_bg_avg <- left_join(select(bg_avg, -sum_of_area_wts), 
                            all_bg_avg,
                            by = c("geoid", "pollutant"))
  }
  
  }
  
  #-- If results_only is True, return mean_value column as vector
  #if(results_only) return(bg_avg$mean_value)
  
  # Otherwise return data frame
  return(all_bg_avg)
  
}
