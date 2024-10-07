#' Create domestic placement maps for MESM & MEDS (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_dom_placement_data' or 'meds_dom_placement_data' 
#'
#' @return
#' @export
#'
#' @examples
domesticPlacement_map <- function(input, data) {

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #......................create color palette......................
  bins <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, Inf)
  pal <- colorBin("YlGn", domain = data$count, bins = bins)
  
  #.......................render leaflet map.......................
  leaflet::renderLeaflet({

    leaflet(data$geometry) |> 
      addProviderTiles(providers$Esri.WorldTerrain,
                       options = providerTileOptions(maxZoom = 100)) |>
      setView(lng = -117, lat = 37, zoom = 3) |>
      addPolygons(data = data$geometry, 
                  weight = 1.5,
                  color = "black",
                  fillColor = pal(data$count), 
                  fillOpacity = 1,
                  label = paste0(data$state)) |> 
      leaflet.extras::addResetMapButton()

  }) # END renderLeaflet

} # END fxn
