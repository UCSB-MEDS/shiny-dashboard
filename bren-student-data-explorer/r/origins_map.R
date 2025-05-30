#' Create student origins map 
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
origins_map <- function(input) {
  
  #..............define bins & color palette for map...............
  bins <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, Inf)
  pal <- colorBin("YlGn", domain = ug_geoms$total, bins = bins)

  #.......................render leaflet map.......................
  leaflet::renderLeaflet({
    
    leaflet(ug_geoms$geometry) |> 
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(maxZoom = 100)) |> 
      setView(lng = -10, lat = 32, zoom = 1) |> 
      addPolygons(data = ug_geoms$geometry,
                  weight = 1,
                  color = "black",
                  fillColor = pal(ug_geoms$total),
                  fillOpacity = 1,
                  label = paste0(ug_geoms$ug1_location, " (",
                                 ug_geoms$total, ")")) |> 
      leaflet.extras::addResetMapButton()
    
  })
  
}
