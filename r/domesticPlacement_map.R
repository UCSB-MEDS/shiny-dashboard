#' Title
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return
#' @export
#'
#' @examples
domesticPlacement_map <- function(input, data) {
  
  # wrangle data (clean up place names) ----
  map <- data %>% 
    select(c(
      employer_account_name,
      work_location_city,
      class_year,
      work_location_state,
      work_location_country,
      lat,long
    )) 

  # wrangle data for domestic placement map ----
  domestic <- map %>% 
    filter(work_location_country == "United States") %>% 
    select(-c(lat,long)) %>%
    left_join(us_state_geoms, by = c("work_location_state" = "state_abbrev")) %>% 
    select(-c(fips, work_location_state)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326)
  
  domestic_stats <- domestic %>% 
    group_by(state) %>% 
    summarize(count = n()) %>%
    mutate(state = paste0(state, " (", count, ")"))
  
  # render tmap ----
  tmap::renderTmap({ 
    
    tmap_mode("view")
    
    tm_shape(domestic_stats) +
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
