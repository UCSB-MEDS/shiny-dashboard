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
##                                                                            --
##------------------------------- LOAD PACKAGES---------------------------------
##                                                                            --
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- IMPORT DATA-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            career outcomes data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# dashboard only visualizes 3 most recent years, so filter for just years of interest

#................initial placement data (cleaned)................
mesm_placement <- readRDS("data/mesm_placement_cleaned.rds") |> filter(year %in% c(2022:2024)) 
meds_placement <- readRDS("data/meds_placement_cleaned.rds") |> filter(year %in% c(2022:2024)) 

#............active placement (status) data (cleaned)............
mesm_status <- readRDS("data/mesm_status_2019_2024.rds") |> filter(class_year %in% c(2022:2024)) 
meds_status <- readRDS("data/meds_status_2022_2024.rds") |> filter(class_year %in% c(2022:2024)) 

#............Wrangled Data for Domestic Placement Maps...........
mesm_dom_placement_data <- readRDS("data/mesm_domestic_placement_data.rds") 
meds_dom_placement_data <- readRDS("data/meds_domestic_placement_data.rds") 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              demographics data                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# dashboard only visualizes 5 most recent years, fo filter for just years of interest
# old years are filtered out during the data cleaning process; see `cleaning-wrangling-NEW.qmd` in the `admissions-data` repo) 
admissions <- readRDS("data/admissions.rds") 
enrolled <- readRDS("data/enrolled.rds") 
ipeds <- readRDS("data/ipeds.rds") 
diversity_stats <- readRDS("data/diversity_stats.rds")

#..................Spatial Geometries (for maps).................
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------- SOURCE SCRIPTS (NECESSARY FOR DEPLOYING ON BREN SERVER-------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# shiny v1.5 will automatically source any script in /r, BUT necessary for deploying on Bren server 

file_path <- "r"
source_scripts = list.files(path = file_path, pattern = "*.R")
map(paste0(file_path, "/", source_scripts), source)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- DEFINE COLORS USED THROUGHOUT DASHBOARD---------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

phd_color <- "#78A540" 
meds_color <- "#027D92"
mesm_color <- "#003660" 
all_programs_color <- "#09847a"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------- DEFINE VARIABLES USED THROUGHOUT DASHBOARD-------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..............set most recent graduated class year..............
# career data for graduating classes of 202* used in career tab 
curr_grad_year <- 2024

#............current year for MEDS employment status.............
# based on enrollment year which is one year prior to graduation 
meds_employmentStatus_curr_year <- curr_grad_year - 1 

#............current year for MESM employment status.............
# based on enrollment year which is two years prior to garduation 
mesm_employmentStatus_curr_year <- curr_grad_year - 2 

#..................set current admissions year...................
# admissions data for entering class of 202* used in demographics tab 
curr_admission_year <- 2024 

#..............set name spelling options for US & CA.............
# get rid of this?
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- CREATE DFS THAT ARE USED THROUGHOUT DASHBOARD-----------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # for testing purposes only; keep commented out when not in use
# mesm_placement <- readRDS("bren-student-data-explorer/data/mesm_placement_cleaned.rds") 
# meds_placement <- readRDS("bren-student-data-explorer/data/meds_placement_cleaned.rds") 

#....MESM program sizes + tot respondents (initial placement).....
# for calculating MESM career exit survey response rate
mesm_placement_size <- mesm_placement |> 
  select(year) |>
  group_by(year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    year  == 2024 ~ 100, # enrolled 2022; graduated 2024 # UPDATE
    year == 2023 ~ 80, # enrolled 2021; graduated 2023
    year == 2022 ~ 92, # enrolled 2020; graduated 2022
    year == 2021 ~ 93, # enrolled 2019; graduated 2021
    year == 2020 ~ 77 # enrolled 2018; graduated 2020
  ))

#....MEDS program sizes + tot respondents (initial placement).....
# for calculating MEDS career exit survey response rate
meds_placement_size <- meds_placement |>
  select(class_year) |>
  group_by(class_year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    class_year == 2024 ~ 100, # enrolled 2023; graduated 2024 # UPDATE
    class_year == 2023 ~ 31, # enrolled 2022; graduated 2023
    class_year == 2022 ~ 25 # enrolled 2021; graduated 2022
  ))

#......MESM program sizes + tot respondents (active status)......
mesm_status_size <- mesm_status |>
  select(class_year) |>
  group_by(class_year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    class_year == 2024 ~ 100, # enrolled 2022; graduated 2024 # UPDATE
    class_year == 2023 ~ 80, # enrolled 2021; graduated 2023
    class_year == 2022 ~ 92, # enrolled 2020; graduated 2022
    class_year == 2021 ~ 93, # enrolled 2019; graduated 2021
    class_year == 2020 ~ 77 # enrolled 2018; graduated 2020
  ))

#......MEDS program sizes + tot respondents (active status)......
meds_status_size <- meds_status |>
  select(class_year) |>
  group_by(class_year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    class_year == 2024 ~ 100, # enrolled 2023; graduated 2024 # UPDATE
    class_year == 2023 ~ 31, # enrolled 2022; graduated 2023
    class_year == 2022 ~ 25 # enrolled 2021; graduated 2022
  ))

#..........................program sizes.........................
program_size <- enrolled |>
  select(c("app_submission_year", "application_id", "objective1")) |>
  group_by(app_submission_year, objective1) |>
  summarize(size = n())

#...............total number students in each year...............
# not broken down by program ----
total_students_yr <- enrolled |>
  group_by(app_submission_year) |> 
  summarize(size = n())

#............5yr total number of students per program............
tot_5yr <- enrolled |>
  select(c("app_submission_year",
           "application_id",
           "objective1",
           "dob")) |>
  group_by(objective1) |>
  summarize(size = n())

#........5yr total number of students across all programs........
totStudents_allPrograms_5yr <- sum(tot_5yr$size)

