#' read_rsk
#'
#' Read MNRISKS modeled .Rsk result file
#' @param file Path to .Rsk file or .zip file
#' @param scenarios Column names to assign to result columns. Default will assign generic numbers (e.g. "scenario_1")
#' @param year The MNRISKS results year. Default value is 2017.
#' @param filter_word Will only keep results with the entered text, such as "Benzene" or "A_Boilers". The default = NULL will keep all results.
#' @param aggregate_facs TRUE/FALSE. Will group_by pollutant and SCC to sum risks/concentrations. Default = FALSE
#' @keywords rsk results mnrisks
#' @export
#' @examples 
#' \dontrun{
#' # Read in a single .rsk file
#' acrolein <- read_rsk("acrolein_results.rsk", year = 2017)
#' 
#' # Read in a single Zipped .rsk file
#' boilers <- read_rsk("area_results.zip", year = 2017, filter_word = "Boilers")
#' }
# 

read_rsk <- function (file, 
                      scenarios = NULL, 
                      year = 2017,
                      filter_word = NULL,
                      aggregate_facs = FALSE) {
  
  options(dplyr.summarise.inform = FALSE)
  
  if (is.null(file)) error("Please provide a file path.")
  
  if (grepl("[.]zip", file)) file <- unz(file, unzip(file, list = T)[[1]])
  
  if (!is.null(filter_word)) {
    
    df <- data.table::fread(cmd = paste0(substr(file, 1, 1), ": & ",
                                         'findstr /c:"', filter_word, '" "', 
                                         gsub("/", "\\\\", file),
                                         '"'), 
                            sep = ",", 
                            header = F,
                            fill = T,
                            colClasses = list(character = 7))
    
  } else {
    
    df <- data.table::fread(file, sep = ",", header = F, fill = T)
    df <- df %>% as_tibble() %>% dplyr::filter(nchar(V1) > 14) 
    df <- unlist(df, use.names = F) %>% paste("\n", collapse = "")
    df <- data.table::fread(df, sep = ",", header = F, fill = T, colClasses = list(character = 7))
  }
  
  # Only process if data found
  if (nrow(df) > 0) {
    
    df <- df[, -c(2, 3, 6, 9, 10, 13)] # Drop unused columns
    
    names(df)[1:7] <- c("receptor", "long", "lat", 
                        "cas", "pollutant", "facility", "scc")
    
    names(df)[8:ncol(df)] <- paste("scenario", 1:(ncol(df) - 7), sep = "_")
    
    df <- subset(df, !is.na(receptor))
    
    # Sum facility or area blockgroup specific risks
    if (aggregate_facs) {
      
      df$facility <- NA
      
      df <- dtplyr::lazy_dt(df) %>%
        dplyr::group_by(receptor, long, lat, cas, pollutant, facility, scc) %>%
        dplyr::summarize(dplyr::across(dplyr::contains("scenario_"), sum, na.rm = T), ) %>%
        dplyr::as_tibble()
    }
    
    # Assign user entered names
    if (!is.null(scenarios)) {
      if (length(scenarios) < (ncol(df) - 7)) {
        scenarios <- paste(scenarios, 1:(ncol(df) - 7), sep = "_")
      }
      names(df)[8:ncol(df)] <- scenarios[1:(ncol(df) - 7)]
    }
    
    df$lat  <- round(as.numeric(df$lat), 4)
    df$long <- round(as.numeric(df$long), 4)
    df$receptor <- as.numeric(df$receptor)
    
    # Attach MPCA receptor info
    receptors <- mnrisks::get_receptors(year = year)
    df <- dplyr::left_join(df, receptors[, c("lakes_rec_id", 
                                             "mpca_rec_id", "county", "mpca_bg_geoid")], 
                           by = c(receptor = "lakes_rec_id"))
    
    # Clean output
    df <- dplyr::rename(df, geoid = mpca_bg_geoid)
    df <- dplyr::select(df, c(1, (ncol(df) - 2), (ncol(df) - 1), ncol(df), 2:(ncol(df) - 3)))
    df$cas <- gsub("^\\s+|\\s+$", "", df$cas)
  } else {
    df <- tibble::tibble()
  }
  
  gc()
  
  return(tibble::as_tibble(df))
}
