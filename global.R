#### global ####

# PACKAGES ----
library(tidyverse)
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
library(naniar)

# IMPORT DATA ----
admissions <- readRDS("data/admissions.rds")
enrolled <- readRDS("data/enrolled.rds")
ipeds <- readRDS("data/ipeds.rds")
diversity_stats <- readRDS("data/diversity_stats.rds")
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")
mesm_placement <- readRDS("data/placement_data.rds")
mesm_status <- readRDS("data/status_data.rds")

# STYLING ----
# SC NOTE: updated colors to match those fo the hex stickers
phd_color <- "#78A540" #6D7D33" 
meds_color <- "#027D92" #"#047C91"
mesm_color <- "#003660" #005AA3"
all_programs_color <- "#09847a"

# VARIABLES ----
curr_year <- 2022 # MANUALLY UPDATE ANNUALLY

# DATA FRAMES ----
# program sizes + tot mesm responsdees 
# used in geographicComparison_plot(), jobSource(), sectorTrends(), salary_plot(), salarySpecialization_plot(), salaryBySector_plot()
placement_size <- mesm_placement %>% 
  select(mesm_class_year) %>% 
  group_by(mesm_class_year) %>% 
  summarize(mesm_responses = n()) %>% 
  mutate(program_size = case_when(
    mesm_class_year == 2021 ~ 83,
    mesm_class_year == 2020 ~ 92,
    mesm_class_year == 2019 ~ 93
  ))