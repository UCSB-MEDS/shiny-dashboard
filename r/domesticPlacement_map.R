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
    # no location available for Remote workers 
    filter(work_location_city != "Remote") |> 
    # found NA for position with `work_location_state` listed as `Washington D.C.` -- need to convert to `DC` so that joining dfs actually assigns a geometry to that position
    mutate(work_location_state = str_replace(work_location_state, 
                                             pattern = "Washington D.C.",
                                             replace = "DC"),
           work_location_city = str_replace(work_location_city,
                                            pattern = "Washington D.C.",
                                            replace = "District of Columnbia")) |> 
    left_join(us_state_geoms, by = c("work_location_state" = "state_abbrev")) %>% 
    select(-c(fips, work_location_state)) %>% 
    st_as_sf() #|> 
    #st_transform(crs = 4326)
  
  domestic_stats <- domestic %>% 
    group_by(state) %>% 
    summarize(count = n()) %>%
    mutate(state = paste0(state, " (", count, ")"))
  
  # SC NOTE 2024-06-30: tried converting tmap to leaflet, but getting error; giving up on this for now but would like to transition this over to leaflet at some point
  # bins <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, Inf)
  # pal <- colorBin("YlGn", domain = domestic_stats$count, bins = bins)
  # 
  # leaflet::renderLeaflet({
  # 
  #   leaflet(domestic_stats$geometry) |>
  #     addProviderTiles(providers$Esri.WorldTerrain,
  #                      options = providerTileOptions(maxZoom = 100)) |>
  #     setView(lng = -117, lat = 37, zoom = 3) |>
  #     addPolygons(domestic_stats$geometry,
  #                 weight = 1.5,
  #                 color = "black",
  #                 fillColor = pal(domestic_stats$count),
  #                 fillOpacity = 1,
  #                 label = paste0(domestic_stats$state, " (",
  #                                domestic_stats$count, " alumni)")) |>
  #     leaflet.extras::addResetMapButton()
  # 
  # })
  
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
      tm_view(set.view = c(lon = -118, lat = 37, zoom = 3))

  })

}
