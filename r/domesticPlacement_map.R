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
      lat, long
    )) # %>% 
    # # standardize state values; SC NOTE 2023-02-03 figure out a better way to do this (NOTE: this code is repeated in geographicComparison_plot())
    # mutate(work_location_state = case_when(
    #   work_location_state %in% ca_names ~ "CA",
    #   work_location_state == "Maryland" ~ "MD",
    #   work_location_state == "Washington" ~ "WA",
    #   work_location_state == "District of Columbia" ~ "DC",
    #   work_location_state == "Indiana" ~ "IN",
    #   work_location_state == "Mt" ~ "MT",
    #   work_location_state == "Hawaii" ~ "HI",
    #   # Note(HD): Don't need this because we replace with Seoul below
    #   #work_location_state == "N/A" ~ NA_character_, 
    #   work_location_state == "michoacan" ~ "Michoacan",
    #   # reassign NA values to correct state values
    #   work_location_city == "Washington DC" ~ "DC",
    #   work_location_city == "Oxnard" ~ "CA",
    #   work_location_city == "Santa Cruz" ~ "CA",
    #   work_location_city == "Fort Collins" ~ "CO",
    #   work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
    #   work_location_city == "Amsterdam" ~ "North Holland",
    #   work_location_city == "Seoul" ~ "Seoul",
    #   TRUE ~ work_location_state
    # )) %>% 
    # # standardize united states values
    # mutate(work_location_country = case_when(
    #   work_location_country %in% us_names ~ "United States",
    #   # reassign NA values to correct country values
    #   work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
    #   work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
    #   employer_account_name == "Cruz Foam" ~ "United States",
    #   employer_account_name == "United Water Conservation District" ~ "United States",
    #   TRUE ~ work_location_country
    # )) %>% 
    # # add latitude
    # mutate(lat = case_when(
    #   work_location_state == "Ontario" ~ 51.2538,
    #   work_location_state == "Galapagos" ~ -0.9538,
    #   work_location_state == "Tahiti" ~ -17.6509,
    #   work_location_state == "Michoacan" ~ 19.5665,
    #   work_location_state == "North Holland" ~ 52.5206,
    #   work_location_state == "Seoul" ~ 37.532600,
    #   TRUE ~ NA_real_
    # )) %>% 
    # # add longitude
    # mutate(long = case_when(
    #   work_location_state == "Ontario" ~ -85.3232,
    #   work_location_state == "Galapagos" ~ -90.9656,
    #   work_location_state == "Tahiti" ~ -149.4260,
    #   work_location_state == "Michoacan" ~ -101.7068,
    #   work_location_state == "North Holland" ~ 4.7885,
    #   work_location_state == "Seoul" ~ 127.024612,
    #   TRUE ~ NA_real_
    # ))
  
  
  # wrangle data for domestic placement map ----
  domestic <- map %>% 
    filter(work_location_country == "United States") %>% 
    select(-c(lat, long)) %>% 
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
