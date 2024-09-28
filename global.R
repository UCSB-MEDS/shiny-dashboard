# ---- SC REMINDERS (2023-02): THINGS TO CHECK/UPDATE AS NEW DATA ARE ADDED: ----
# update year_radioButtons() with new year choices as career data are added
# update curr_year vars, below 
# once MEDS hits 3 years, use salary_plot() & salaryBySector_plot() fxns for generating plots (until then, add 2023 to meds_salary_plot() & meds_salaryBySector_plot())
# update class sizes by year in placementStatus_plot()
# update num_years for MEDS in sectorSatisfaction_plot()
# assess/adjust as needed the x-axis ticks in urmTrends_plot() -- updated during Feb 2023 because 2022 data for PhDs not showing up as originally coded (no URM PhDs in 2022)
# update plot title (year ranges) in age_plot() whenever years are added/removed
# DATA CLEANING UPDATES SHOULD BE MADE TO FILES IN `/data_cleaning`
  # TODO: MOVE ug1_name & ug1_location in internationalUniversities_table() to data-cleaning files

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                load packages                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(bslib)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) 
library(shinyjs)
library(lubridate)
library(tmap)
library(sf)
library(plotly)
library(shinycssloaders)
library(fontawesome) 
library(naniar)
library(leaflet)
# library(leaflet.extras)
# library(memoise)

#.......................enable bookmarking.......................
# enableBookmarking(store = "url") # SC NOTE 2024-09-25: I dont think this is necessary anymore?

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                import data                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#........................Demographics Data.......................
# only visualize 5 most recent years ----
# old years are filtered out during the data cleaning process; see `cleaning-wrangling-NEW.qmd` in the `admissions-data` repo) 
admissions <- readRDS("data/admissions.rds") 
enrolled <- readRDS("data/enrolled.rds") 
ipeds <- readRDS("data/ipeds.rds") 
diversity_stats <- readRDS("data/diversity_stats.rds")

#......................Career Outcomes Data......................
# only visualize 3 most recent years ----
# use `mesm_placement_cleaning.R` & `meds_placement_cleaning.R` to generate `*_placemente_cleaned.rds` files
mesm_placement <- readRDS("data/mesm_placement_cleaned.rds") |> filter(!class_year %in% c(2019, 2020)) 
mesm_status <- readRDS("data/Sam-latest-update-sep24/mesm_status_2019_2023.rds") |> filter(!class_year %in% c(2019, 2020)) 
meds_placement <- readRDS("data/meds_placement_cleaned.rds")
meds_status <- readRDS("data/Sam-latest-update-sep24/meds_status_2022_2023.rds") 

#..................Spatial Geometries (for maps).................
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")

#............Wrangled Data for Domestic Placement Maps...........
# Maps too slow to load otherwise ----
# use `domestic_placement_cleaning.R` to generate `*_domestic_placement_data.rds` files
mesm_dom_placement_data <- readRDS("data/mesm_domestic_placement_data.rds") 
meds_dom_placement_data <- readRDS("data/meds_domestic_placement_data.rds") 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               Source Scripts                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# don't need since shiny v1.5 will automatically source any script in /r, BUT necessary for deploying on Bren server ----
file_path <- "r"
source_scripts = list.files(path = file_path, pattern = "*.R")
map(paste0(file_path, "/", source_scripts), source)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Define Colors                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

phd_color <- "#78A540" 
meds_color <- "#027D92"
mesm_color <- "#003660" 
all_programs_color <- "#09847a"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Define Variables                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..................set current admissions year...................
# admissions data for entering class of 202* used in demographics tab ----
curr_admission_year <- 2024 

#..............set most recent graduated class year..............
# career data for graduating classes of 202* used in career tab ----
curr_grad_year <- 2023

#............current year for MEDS employment status.............
# based on enrollment year which is one year prior to graduation ----
meds_employmentStatus_curr_year <- curr_grad_year - 1 

#............current year for MESM employment status.............
# based on enrollment year which is two years prior to garduation ----
mesm_employmentStatus_curr_year <- curr_grad_year - 2 

#..............set name spelling options for US & CA.............
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Data Frames                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....MESM program sizes + tot respondents (intial placement).....
mesm_placement_size <- mesm_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>% 
  mutate(program_size = case_when(
    class_year == 2023 ~ 80, 
    class_year == 2022 ~ 92,
    class_year == 2021 ~ 93, 
    class_year == 2020 ~ 77 
  ))

#....MEDS program sizes + tot respondents (intial placement).....
meds_placement_size <- meds_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31,
    class_year == 2022 ~ 25
  ))

#......MESM program sizes + tot respondents (active status)......
mesm_status_size <- mesm_status %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 80,
    class_year == 2022 ~ 92,
    class_year == 2021 ~ 93, 
    class_year == 2020 ~ 77 
  ))

#......MEDS program sizes + tot respondents (active status)......
meds_status_size <- meds_status %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31, 
    class_year == 2022 ~ 25 
  ))

#..........................program sizes.........................
program_size <- enrolled %>%
  select(c("app_submission_year", "application_id", "objective1")) %>%
  group_by(app_submission_year, objective1) %>%
  summarize(size = n())

#...............total number students in each year...............
# not broken down by program ----
total_students_yr <- enrolled %>% 
  group_by(app_submission_year) %>% 
  summarize(size = n())

#............5yr total number of students per program............
tot_5yr <- enrolled %>% 
  select(c("app_submission_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(size = n())

#........5yr total number of students across all programs........
totStudents_allPrograms_5yr <- sum(tot_5yr$size)

