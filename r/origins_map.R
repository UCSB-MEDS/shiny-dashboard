origins_map <- function(input) {
  
  # render tmap ----
  tmap::renderTmap({
    
    tmap_mode("view")
    
    tm_shape(ug_geoms) +
      tm_fill(col = "total", title = "Number of students",
        palette = "YlGn", style = "jenks", n = 6,
        popup.vars = c("Total students: " = "total")) +
      tm_view(set.view = c(lon = -10, lat = 32, zoom = 1)) 
    
  }) 
  
}