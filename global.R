#### global ####

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

#............................packages............................
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

#..........................import data...........................

# DEMOGRAPHICS DATA (5 most recent years -- old years are filtered out during the data cleaning process; see `cleaning-wrangling-NEW.qmd` in the `admissions-data` repo) ----
admissions <- readRDS("data/admissions.rds") 
enrolled <- readRDS("data/enrolled.rds") 
ipeds <- readRDS("data/ipeds.rds") 
diversity_stats <- readRDS("data/diversity_stats.rds")

# CAREER OUTCOMES DATA (keep 3 most recent years) ----
mesm_placement <- readRDS("data/mesm_placement_cleaned.rds") |> filter(!class_year %in% c(2019, 2020)) # need to run `mesm_placement_cleaning.R` to get this .rds file 
mesm_status <- readRDS("data/mesm_status_19-23.rds") |> filter(!class_year %in% c(2019, 2020)) 
meds_placement <- readRDS("data/meds_placement_cleaned.rds") # need to run `meds_placement_cleaning.R` to get this .rds file 
meds_status <- readRDS("data/meds_status_22-23.rds") 

# GEOMETRIES FOR MAPS ----
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")

# WRANGLED DATA FOR DOMESTIC PLACEMENT MAPS (slow to load otherwise) ----
mesm_dom_placement_data <- readRDS("data/mesm_domestic_placement_data.rds") # need to run `domestic_placement_cleaning.R` to get this .rds file
meds_dom_placement_data <- readRDS("data/meds_domestic_placement_data.rds") # need to run `domestic_placement_cleaning.R` to get this .rds file

#.........................source scripts.........................
# (don't need since shiny v1.5 will automatically source any script in /r, but necessary for deploying on Bren server) ----
file_path <- "r"
source_scripts = list.files(path = file_path, pattern = "*.R")
map(paste0(file_path, "/", source_scripts), source)

#............................styling.............................
phd_color <- "#78A540" # was "#6D7D33" 
meds_color <- "#027D92" # was "#047C91"
mesm_color <- "#003660" # was "#005AA3"
all_programs_color <- "#09847a"

#............................variables...........................
curr_admission_year <- 2024 # current admissions year (used in programSize_valueBox() subtitle) | ADMISSIONS DATA FOR ENTERING CLASSES OF 2024 USED IN DEMOGRAPHICS TAB
curr_grad_year <- 2023 # recent graduated class year (used in programSize_valueBox() class size calculation) | CAREER DATA FOR GRADUATING CLASSES OF 2023 USED IN CAREER TAB
meds_employmentStatus_curr_year <- curr_grad_year - 1 # current year for meds employment status, based on enrollment year which is one year prior to graduation (used in employmentStatus_stat_valueBox())
mesm_employmentStatus_curr_year <- curr_grad_year - 2 # current year for mesm employment status, based on enrollment year, which is two years prior to graduation (used in employmentStatus_stat_valueBox())

# set name spelling options for US & CA (domesticPlacement_map(), geographicComparison_plot())
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

#..........................data frames...........................
# program sizes + total respondents to initial placement survey
# used in geographicComparison_plot(), jobSource(), sectorTrends(), salary_plot(), salarySpecialization_plot(), salaryBySector_plot()
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

meds_placement_size <- meds_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31,
    class_year == 2022 ~ 25
  ))

# program sizes + tot respondents to active status survey
# used in placementStatus_plot()
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

meds_status_size <- meds_status %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31, # SC NOTE 2023-02-10: won't get these data until winter 2024, but adding now for when we do
    class_year == 2022 ~ 25 
  ))

# program sizes (5 most recent years)
# used in programSize_valueBox(), sex_plot()
program_size <- enrolled %>%
  select(c("app_submission_year", "application_id", "objective1")) %>%
  group_by(app_submission_year, objective1) %>%
  summarize(size = n())

# total number of students in each year NOT broken down by program
# used in urmTrends_plot(), ipedsTrends_plot()
total_students_yr <- enrolled %>% 
  group_by(app_submission_year) %>% 
  summarize(size = n())

# 5 year total number of students per program
# used in ipedsCategories_plot(), ipedsBackgrounds_plot()
tot_5yr <- enrolled %>% 
  select(c("app_submission_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(size = n())

# 5 year total number of students across all programs 
# used in ipedsCategories_plot()
totStudents_allPrograms_5yr <- sum(tot_5yr$size)

