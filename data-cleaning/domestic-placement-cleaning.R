
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              import packages                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(sf)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                import data                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mesm_placement <- readRDS(here::here("data", "mesm_placement_cleaned.rds")) |> filter(!class_year %in% c(2019, 2020))
meds_placement <- readRDS("data/meds_placement_cleaned.rds")
us_state_geoms <- readRDS(here::here("data", "us_state_geoms.rds"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    wrangle MESM domestic placement data                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# wrangle data (clean up place names) ----
map_data <- mesm_placement %>% 
  select(c(
    employer_account_name,
    work_location_city,
    class_year,
    work_location_state,
    work_location_country,
    lat,long
  )) 

# wrangle data for domestic placement map ----
domestic <- map_data %>% 
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
  st_as_sf() |> 
  st_transform(crs = 4326)

domestic_stats <- domestic %>% 
  group_by(state) %>% 
  summarize(count = n()) %>%
  mutate(state = paste0(state, " (", count, ")")) |> 
  mutate(count = as.numeric(count))

#..........................write to file.........................
write_rds(domestic_stats, here::here("data", "mesm_domestic_placement_data.rds"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    wrangle MEDS domesitc placement data                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# wrangle data (clean up place names) ----
map_data <- meds_placement %>% 
  select(c(
    employer_account_name,
    work_location_city,
    class_year,
    work_location_state,
    work_location_country,
    lat,long
  )) 

# wrangle data for domestic placement map ----
domestic <- map_data %>% 
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
  st_as_sf() |> 
  st_transform(crs = 4326)

domestic_stats <- domestic %>% 
  group_by(state) %>% 
  summarize(count = n()) %>%
  mutate(state = paste0(state, " (", count, ")")) |> 
  mutate(count = as.numeric(count))

#..........................write to file.........................
write_rds(domestic_stats, here::here("data", "meds_domestic_placement_data.rds"))
