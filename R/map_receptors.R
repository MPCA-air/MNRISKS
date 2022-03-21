#' map_receptors
#'
#' Map the average block group mnrisks results with leaflet
#' @param data The dataframe or sf object containing the modeling results for mapping.
#' @param result_col Column name containing the modeling concentration/risk results.
#' @param receptor_col Column name containing the receptor IDs.
#' @param colors Color palette to use for block group fill, ex. "viridis", "inferno", "Blues", "Greens"
#' @param reverse_colors Flip the color palette order
#' @param size Receptor or circle radius. Default = 5
#' @param signif_digits Number of significant figures to show in map labels
#' @param silent Return map as object. Default = FALSE
#' @keywords Map receptors mnrisks leaflet
#' @export
#' @examples
#' \dontrun{
#' map_receptors(data = mnrisk_results,
#'               result_col = "cancer_risk",
#'               colors = "inferno")
#' }               
# 

map_receptors <- function(data        = NULL,
                          result_col  = "cancer_risk",
                          receptor_col = "receptor",
                          colors      = "viridis",
                          reverse_colors = TRUE,
                          size = 5,
                          signif_digits = 3,
                          silent = FALSE) {
  
  
  # Test inputs ----
  if (is.null(data) | !"data.frame" %in% class(data)) stop("Incorrect data passed to function. Set `data` argument to the 'data.frame' or 'sf' object containing the modeling results.")
  
  if (!tolower(result_col) %in% tolower(names(data))) stop(paste("The result_col [", result_col, "] was not found in the data."))
  
  if (!tolower(receptor_col) %in% tolower(names(data))) stop(paste("The receptor_col [", receptor_col, "] was not found in the data."))
  
  # Set column names ----
  names(data)[grep(tolower(result_col), tolower(names(data)))] <- "result"
  names(data)[grep(tolower(receptor_col), tolower(names(data)))] <- "receptor"
  
  # Create color palette ----
  max_result <- max(data$result, na.rm = T)
  
  if (max_result < 1E-03) { 
    
    data$result <- data$result / 1E-05
    
    max_result <- max(data$result, na.rm = T)
    
    title_adj <- " per 100,000"
    
  } else {
    
    title_adj <- ""
    
  }

  
  pal <- leaflet::colorNumeric(palette = colors, domain = data$result, reverse = reverse_colors)
  #pal <- leaflet::colorNumeric("viridis", quantile(data$result, c(seq(0,0.9,0.1),0.95,0.97,1)), reverse = T)
  
  
  # Create labels ----
  names(data) <- tolower(names(data))
  
  data$label <- paste0("<h1 style='font-size: 2.4em; text-align: center; color: grey; margin-top: 0;'>", 
                       signif(data$avg_result, signif_digits), title_adj, "</h1>",
                       '<p style="text-align: center; font-size:14px; margin-top: -22px; margin-bottom: 0;">', gsub("_", " ", result_col), "</p>",
                       "<hr>",
                       "<h2 style='text-align: center; font-weight: 600;'>Receptor #", data$receptor, "</h2>"
                       )

  # Map the map ----
  if (silent) {
    m <- leaflet::leaflet(data %>% subset(!is.na(result))) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addCircleMarkers(color   = "darkgrey",
                                opacity = 0.3,
                                fillColor = ~pal(result),
                                fillOpacity = ~0.15*result/max_result + 0.3, # Scale the opacity up to 0.45 for highest risks
                                radius  = ~2*result/max_result + size,
                                label = ~lapply(label, htmltools::HTML),
                                stroke = T, 
                                weight = 1) %>%
      leaflet::addLegend("bottomright", 
                         pal = pal, 
                         values = ~result, 
                         title = paste0(gsub("_", " ", result_col), title_adj),
                         opacity = 0.7)
      return(m)
  } else {
  
  leaflet::leaflet(data %>% subset(!is.na(result))) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addCircleMarkers(color   = "darkgrey",
                     opacity = 0.3,
                     fillColor = ~pal(result),
                     fillOpacity = ~0.15*result/max_result + 0.3, # Scale the opacity up to 0.45 for highest risks
                     radius  = ~2*result/max_result + size,
                     label = ~lapply(label, htmltools::HTML),
                     stroke = T, 
                     weight = 1) %>%
    leaflet::addLegend("bottomright", 
              pal = pal, 
              values = ~result, 
              title = paste0(result_col, title_adj),
              opacity = 0.7)
}
  
}
