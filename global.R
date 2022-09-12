# PACKAGES ----
library(tidyverse)
library(rsconnect)
library(bslib)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # update boxes
library(shinyjs)
library(lubridate)
library(tmap)
library(sf)
library(plotly)
library(shinycssloaders) # loading icon
library(fontawesome) # icons
library(giscoR) # get world country polygons


# IMPORT DATA ----
admissions <- readRDS("data/admissions.rds")
enrolled <- readRDS("data/enrolled.rds")
ipeds <- readRDS("data/ipeds.rds")
diversity_stats <- readRDS("data/diversity_stats.rds")
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")
mesm_placement <- readRDS("data/placement_data.rds")
mesm_status <- readRDS("data/status_data.rds")

# SOURCE FUNCTIONS ----
source("r/age_plot.R")
source("r/race_plot.R")
source("r/background-distribution.R")
source("r/urm_trends_plot.R")

# STYLING ----
mesm_color <- "#6D7D33" 
meds_color <- "#047C91"
phd_color <- "#005AA3"
all_programs_color <- "#09847a"

# VARIABLES ----
curr_year <- 2022

# DATA FRAMES ----
# program sizes 2017-curr_year
program_size <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1")) %>% 
  group_by(ay_year,
           objective1) %>% 
  summarize(size = n())

# 5 year total number of students per program
tot_5yr <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(size = n())

# total number of students in each year NOT broken down by program
total_students_yr <- enrolled %>% 
  group_by(ay_year) %>% 
  summarize(size = n())

# program sizes + tot mesm responsdees 
placement_size <- mesm_placement %>% 
  select(mesm_class_year) %>% 
  group_by(mesm_class_year) %>% 
  summarize(mesm_responses = n()) %>% 
  mutate(program_size = case_when(
    mesm_class_year == 2021 ~ 83,
    mesm_class_year == 2020 ~ 92,
    mesm_class_year == 2019 ~ 93
  ))

# program sizes + tot mesm responsdees
status_size <- mesm_status %>% 
  select(mesm_class_year) %>% 
  group_by(mesm_class_year) %>% 
  summarize(mesm_responses = n()) %>% 
  mutate(program_size = case_when(
    mesm_class_year == 2021 ~ 83,
    mesm_class_year == 2020 ~ 92,
    mesm_class_year == 2019 ~ 93
  ))

# MESM MAP
# ca / out of state / international
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA")

mesm_map <- mesm_placement %>% 
  select(c(
    employer_account_name,
    work_location_city,
    mesm_class_year,
    work_location_state,
    work_location_country
  )) %>% 
  # standardize state values
  mutate(work_location_state = case_when(
    work_location_state %in% ca_names ~ "CA",
    work_location_state == "Maryland" ~ "MD",
    work_location_state == "Washington" ~ "WA",
    work_location_state == "District of Columbia" ~ "DC",
    # Note(HD): Don't need this because we replace with Seoul below
    #work_location_state == "N/A" ~ NA_character_, 
    work_location_state == "michoacan" ~ "Michoacan",
    # reassign NA values to correct state values
    work_location_city == "Washington DC" ~ "DC",
    work_location_city == "Oxnard" ~ "CA",
    work_location_city == "Santa Cruz" ~ "CA",
    work_location_city == "Fort Collins" ~ "CO",
    work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
    work_location_city == "Amsterdam" ~ "North Holland",
    work_location_city == "Seoul" ~ "Seoul",
    TRUE ~ work_location_state
  )) %>% 
  # standardize united states values
  mutate(work_location_country = case_when(
    work_location_country %in% us_names ~ "United States",
    # reassign NA values to correct country values
    work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
    work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
    employer_account_name == "Cruz Foam" ~ "United States",
    employer_account_name == "United Water Conservation District" ~ "United States",
    TRUE ~ work_location_country
  )) %>% 
  # add latitude
  mutate(lat = case_when(
    work_location_state == "Ontario" ~ 51.2538,
    work_location_state == "Galapagos" ~ -0.9538,
    work_location_state == "Tahiti" ~ -17.6509,
    work_location_state == "Michoacan" ~ 19.5665,
    work_location_state == "North Holland" ~ 52.5206,
    work_location_state == "Seoul" ~ 37.532600,
    TRUE ~ NA_real_
  )) %>% 
  # add longitude
  mutate(long = case_when(
    work_location_state == "Ontario" ~ -85.3232,
    work_location_state == "Galapagos" ~ -90.9656,
    work_location_state == "Tahiti" ~ -149.4260,
    work_location_state == "Michoacan" ~ -101.7068,
    work_location_state == "North Holland" ~ 4.7885,
    work_location_state == "Seoul" ~ 127.024612,
    TRUE ~ NA_real_
  ))
