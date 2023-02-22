#### global ####

# ---- SC REMINDERS (2023-02): THINGS TO CHECK/UPDATE AS NEW DATA ARE ADDED: ----
# update year_radioButtons() with new year choices as career data are added
# update curr_year, below 
# once MEDS hits 3 years, use salary_plot() & salaryBySector_plot() fxns for generating plots (until then, add 2023 to meds_salary_plot() & meds_salaryBySector_plot())
# update class sizes by year in placementStatus_plot()
# update num_years for MEDS in sectorSatisfaction_plot()
# assess/adjust as needed the x-axis ticks in urmTrends_plot() -- updated during Feb 2023 because 2022 data for PhDs not showing up as originally coded (no URM PhDs in 2022)
# update plot title (year ranges) in age_plot() whenever years are added/removed
# DATA CLEANING UPDATES SHOULD BE MADE TO FILES IN `/data_cleaning`
  # TODO: MOVE ug1_name & ug1_location in internationalUniversities_table() to data-cleaning files

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
admissions <- readRDS("data/admissions.rds") |> filter(!ay_year %in% c(2017)) # years removed to maintain 5-year avg
enrolled <- readRDS("data/enrolled_cleaned.rds") |> filter(!ay_year %in% c(2017)) # years removed to maintain 5-year avg
ipeds <- readRDS("data/ipeds.rds") |> filter(!ay_year %in% c(2017)) # years removed to maintain 5-year avg
diversity_stats <- readRDS("data/diversity_stats.rds")
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")
mesm_placement <- readRDS("data/mesm_placement_cleaned.rds") |> filter(!class_year %in% c(2019)) # years removed to maintain 3 years of data;  SC NOTE 2022-02-16: moved data cleaning from within some fxns and also incorporated updates to incorrect data, as requested by KB; see `data/mesm_placement_cleaned.R`
mesm_status <- readRDS("data/mesm_status.rds") |> filter(!class_year %in% c(2019)) # years removed to maintain 3 years of data
meds_placement <- readRDS("data/meds_placement_cleaned.rds") # SC NOTE 2022-02-16: moved data cleaning from within some fxns and also incorporated updates to incorrect data, as requested by KB; see `data/meds_placement_cleaned.R`
meds_status <- readRDS("data/meds_status.rds") 

# SOURCE SCRIPTS (don't need since shiny v1.5 will automatically source any script in /r, but necessary for deploying on Bren server) ----
file_path <- "r"
source_scripts = list.files(path = file_path, pattern = "*.R")
map(paste0(file_path, "/", source_scripts), source)

# STYLING ----
# SC NOTE 2023-02-08: updated colors to match those of the hex stickers
phd_color <- "#78A540" # was "#6D7D33" 
meds_color <- "#027D92" # was "#047C91"
mesm_color <- "#003660" # was "#005AA3"
all_programs_color <- "#09847a"

# VARIABLES ----
curr_year <- 2022 # recent graduated class year (used in programSize_valueBox() class size alculation)
curr_admission_year <- 2023 # current admissions year (used in programSize_valueBox() subtitle)
employmentStatus_curr_year <- 2021 # current year for employment status, based on enrollment year (used in employmentStatus_stat_valueBox())

# class sizes (SC NOTE 2022-02-13: for reference only; vars not used anywhere in code); ay_year = year applied/enrolled
#### MESM (graduate 2 years after enrollment year) ####
# gradClass_mesm2019 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2017) |> count() |> pull() # 85; enrolled ay_year 2017
gradClass_mesm2020 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2018) |> count() |> pull() # 77; enrolled ay_year 2018
gradClass_mesm2021 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2019) |> count() |> pull() # 93; enrolled ay_year 2019
gradClass_mesm2022 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2020) |> count() |> pull() # 92; enrolled ay_year 2020
gradClass_mesm2023 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2021) |> count() |> pull() # 83; enrolled ay_year 2021 
gradClass_mesm2024 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2022) |> count() |> pull() # 73; enrolled ay_year 2022
#### MEDS (graduate 1 year after enrollment year) ####
gradClass_meds2022 <- enrolled |> filter(objective1 == "MEDS" & ay_year == 2021) |> count() |> pull() # 25; enrolled ay_year 2021
gradClass_meds2023 <- enrolled |> filter(objective1 == "MEDS" & ay_year == 2022) |> count() |> pull() # 31; enrolled ay_year 2022

# set name spelling options for US & CA
# used in domesticPlacement_map(), geographicComparison_plot()
us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

# DATA FRAMES ----
# program sizes + total respondents to initial placement survey
# used in geographicComparison_plot(), jobSource(), sectorTrends(), salary_plot(), salarySpecialization_plot(), salaryBySector_plot()
# SC NOTE 2023-02-10: I think these class sizes were incorrect before; updated below and left original class sizes as comments
mesm_placement_size <- mesm_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>% 
  mutate(program_size = case_when(
    class_year == 2022 ~ 92,
    class_year == 2021 ~ 93, # was 83
    class_year == 2020 ~ 77 # was 92
    #class_year == 2019 ~ 85 # was 93; REMOVED AT FEB 2023 UPDATE
  ))

meds_placement_size <- meds_placement %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31, # SC NOTE 2023-02-08: don't have data for this yet, just adding so it's here 
    class_year == 2022 ~ 25
  ))

# program sizes + tot respondents to active status survey
# used in placementStatus_plot()
# SC NOTE 2023-02-10: I think these class sizes were incorrect before; updated below and left original class sizes as comments
mesm_status_size <- mesm_status %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2022 ~ 92,
    class_year == 2021 ~ 93, # was 82
    class_year == 2020 ~ 77 # was 92
    #class_year == 2019 ~ 85 # was 93; REMOVED AT FEB 2023 UPDATE
  ))

meds_status_size <- meds_status %>%
  select(class_year) %>%
  group_by(class_year) %>%
  summarize(responses = n()) %>%
  mutate(program_size = case_when(
    class_year == 2023 ~ 31, # SC NOTE 2023-02-10: won't get these data until winter 2024, but adding now for when we do
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

# 5 year (currently 2017 - 2022) total number of students per program
# used in ipedsCategories_plot(), ipedsBackgrounds_plot()
tot_5yr <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(size = n())

# 5 year (currently 2017 - 2022) total number of students across all programs 
# used in ipedsCategories_plot()
totStudents_allPrograms_5yr <- sum(tot_5yr$size)
