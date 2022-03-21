#' map_bgs
#'
#' Map the average block group mnrisks results with leaflet
#' @param data The dataframe or sf object containing the modeling results for mapping.
#' @param result_col Column name containing the modeling concentration/risk results.
#' @param geoid_col Block group GEOID colulmn name for mapping results.
#' @param colors Color palette to use for block group fill, ex. "viridis", "inferno", "Blues", "Greens"
#' @param reverse_colors Flip the color palette order
#' @param signif_digits Number of significant figures to show in map labels.
#' @param silent Return map as object. Default = FALSE
#' @keywords Map average blockgroup mnrisks leaflet
#' @export
#' @examples
#' \dontrun{
#' # For all block groups
#' map_bgs(data = mnrisk_results,
#'         result_col = "avg_cancer_risk", 
#'         colors = "Blues")
#' }
# 

map_bgs <- function(data        = NULL,
                    result_col  = "avg_risk",
                    geoid_col   = "geoid",
                    colors      = "viridis",
                    reverse_colors = TRUE,
                    signif_digits  = 3,
                    silent = FALSE) {
  
  library(sf)
  
  if (is.null(data) | !"data.frame" %in% class(data)) stop("Incorrect data passed to function. Set `data` argument to the 'data.frame' or 'sf' object containing the modeling results.")
  
  if (!result_col %in% names(data)) stop(paste("The result_col [", result_col, "] was not found in the data."))
  
  if (!geoid_col %in% names(data)) stop(paste("The geoid_col [", geoid_col, "] was not found in the data."))
  
  
  
  names(data)[grep(result_col, names(data))] <- "avg_result"
  names(data)[grep(geoid_col, names(data))] <- "geoid"
  
  names(data) <- tolower(names(data))
  
  # Create color palette ----
  max_result <- max(data$avg_result, na.rm = T)
  
  if (max_result < 1E-03) { 
    
    data$avg_result <- data$avg_result / 1E-05
    
    max_result <- max(data$avg_result, na.rm = T)
    
    title_adj <- " per 100,000"
    
  } else {
    
    title_adj <- ""
    
  }
  
  
  pal <- leaflet::colorNumeric(palette = colors, domain = data$avg_result, reverse = reverse_colors)
  #pal <- leaflet::colorNumeric("viridis", quantile(data$avg_result, c(seq(0,0.9,0.1),0.95,0.97,1)), reverse = T)
  
  if (!'sf' %in% class(data)) {
    
    bgs <- get_bg_shapes()
    
    names(bgs) <- tolower(names(bgs))
    
    bgs <- dplyr::left_join(bgs, data, by = c("geoid" = tolower(geoid_col)), all.x = T)
    
  } else {
    
    bgs <- data
    
  }
  
  
  bgs$label <- paste0("<h1 style='font-size: 2.4em; text-align: center; color: grey; margin-top: 0;'>", 
                      signif(bgs$avg_result, signif_digits), title_adj, "</h1>",
                      '<p style="text-align: center; font-size:14px; margin-top: -22px; margin-bottom: 0;">', gsub("_", " ", result_col), "</p>",
                      "<hr>",
                      "<h3 style='text-align: center; font-weight: 600;'>Block group ", bgs$geoid, "</h3>"
                      )

  if (silent) {
  m <- leaflet::leaflet(bgs %>% subset(!is.na(avg_result))) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(fillColor  = ~pal(avg_result),
                color = ~pal(avg_result),
                fillOpacity = ~0.45*avg_result/max_result + 0.35, # Scale the opacity up to 0.8 for highest risks
                weight  = 1, 
                stroke = T,
                smoothFactor = 0.5,
                opacity = ~0.4*avg_result/max_result + 0.2, # Scale the border opacity up to 0.7 for highest risks
                label = ~lapply(label, htmltools::HTML),
                popup = ~lapply(label, htmltools::HTML)) %>%
    leaflet::addLegend("bottomright", 
              pal = pal, 
              values = ~avg_result,
              title = paste0(gsub("_", " ", result_col), title_adj),
              opacity = 0.7) 
    
    return(m)
  } else {
    
    leaflet::leaflet(bgs %>% subset(!is.na(avg_result))) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(fillColor  = ~pal(avg_result),
                           color = ~pal(avg_result),
                           fillOpacity = ~0.45*avg_result/max_result + 0.35, # Scale the opacity up to 0.8 for highest risks
                           weight  = 1, 
                           stroke = T,
                           smoothFactor = 0.5,
                           opacity = ~0.4*avg_result/max_result + 0.2, # Scale the border opacity up to 0.7 for highest risks
                           label = ~lapply(label, htmltools::HTML),
                           popup = ~lapply(label, htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", 
                         pal = pal, 
                         values = ~avg_result,
                         title = paste0(result_col, title_adj),
                         opacity = 0.7) 
  }

}
