
# may abstract this more so it can be used for both MESM & MEDS #

domesticPlacement_map <- function(input) {
  
  # wrangle data for domestic placement map ----
  mesm_domestic <- mesm_map %>% 
    filter(work_location_country == "United States") %>% 
    select(-c(lat, long)) %>% 
    left_join(us_state_geoms, by = c("work_location_state" = "state_abbrev")) %>% 
    select(-c(fips, work_location_state)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326)
  
  mesm_domestic_stats <- mesm_domestic %>% 
    group_by(state) %>% 
    summarize(count = n()) %>% 
    mutate(state = paste0(state, " (", count, ")"))
  
  # render tmap ----
  tmap::renderTmap({ 
    
    tmap_mode("view")
    
    tm_shape(mesm_domestic_stats) +
      tm_tiles(leaflet::providers$CartoDB.PositronNoLabels) +
      tm_polygons(
        col = "count",
        style = "jenks",
        n = 4,
        palette = "YlGn",
        popup.vars = c("Number of alumni: " = "count"),
        legend.show = FALSE
      ) +
      tm_view(set.view = c(lon = -117, lat = 37, zoom = 3))
    
  }) 
  
}