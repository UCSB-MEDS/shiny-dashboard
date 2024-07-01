origins_map <- function(input) {
  
  bins <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, Inf)
  pal <- colorBin("YlGn", domain = ug_geoms$total, bins = bins)

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
                  # popup = paste(ug_geoms$ug1_location, "<br>", 
                  #               "Total students: ", ug_geoms$total)) |> 
      leaflet.extras::addResetMapButton()
    
  })
  
  # # render tmap ----
  # tmap::renderTmap({
  #   
  #   tmap_mode("view")
  #   
  #   tm_shape(ug_geoms) +
  #     tm_tiles(leaflet::providers$CartoDB.PositronNoLabels) +
  #     tm_fill(col = "total", title = "Number of students",
  #     palette = "YlGn", style = "jenks", n = 6,
  #     popup.vars = c("Total students: " = "total")
  #     ) +
  #     # tm_polygons(
  #     #   col = "total",
  #     #   style = "jenks",
  #     #   n = 4,
  #     #   palette = "YlGn",
  #     #   popup.vars = c("Number of students: " = "total"),
  #     #   legend.show = FALSE
  #     # ) +
  #     tm_view(set.view = c(lon = -10, lat = 32, zoom = 1)) 
  #   
  # }) 
  
}
