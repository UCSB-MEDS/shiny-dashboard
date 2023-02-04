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
mesm_placement <- readRDS("data/mesm_placement.rds")
meds_placement <- readRDS("data/meds_placement.rds")
mesm_status <- readRDS("data/status_data.rds")

# STYLING ----
# SC NOTE: updated colors to match those fo the hex stickers
phd_color <- "#78A540" #6D7D33" 
meds_color <- "#027D92" #"#047C91"
mesm_color <- "#003660" #005AA3"
all_programs_color <- "#09847a"

# VARIABLES ----
# current year (MANUALLY UPDATE ANNUALLY)
curr_year <- 2022 

# set name spelling options for US & CA
# used in domesticPlacement_map(), geographicComparison_plot()
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

# DATA FRAMES ----
# program sizes + total respondants 
# used in geographicComparison_plot(), jobSource(), sectorTrends(), salary_plot(), salarySpecialization_plot(), salaryBySector_plot()
mesm_placement_size <- mesm_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>% # was 'mesm_responses'
  mutate(program_size = case_when(
    class_year == 2021 ~ 83,
    class_year == 2020 ~ 92,
    class_year == 2019 ~ 93
  ))

meds_placement_size <- meds_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2022 ~ 25
  ))

# program sizes 2017-curr_year
# used in programSize_valueBox(), sex_plot()
program_size <- enrolled %>%
  select(c("ay_year", "application_id", "objective1")) %>%
  group_by(ay_year, objective1) %>%
  summarize(size = n())

# total number of students in each year NOT broken down by program
# used in urmTrends_plot(), ipedsTrends_plot()
total_students_yr <- enrolled %>% 
  group_by(ay_year) %>% 
  summarize(size = n())

# 5 year total number of students per program
# used in ipedsCategories_plot(), ipedsBackgrounds_plot()
tot_5yr <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(size = n())