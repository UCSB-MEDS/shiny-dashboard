
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

# # for testing purposes only; keep commented out when not in use
# mesm_placement <- readRDS("bren-student-data-explorer/data/mesm_placement_cleaned.rds")
# meds_placement <- readRDS("bren-student-data-explorer/data/meds_placement_cleaned.rds")
# mesm_status <- readRDS("bren-student-data-explorer/data/mesm_status_cleaned.rds")
# meds_status <- readRDS("bren-student-data-explorer/data/meds_status_cleaned.rds")
# mesm_dom_placement_data <- readRDS("bren-student-data-explorer/data/mesm_domestic_placement_data.rds")
# meds_dom_placement_data <- readRDS("bren-student-data-explorer/data/meds_domestic_placement_data.rds")
# admissions <- readRDS("bren-student-data-explorer/data/admissions.rds")
# enrolled <- readRDS("bren-student-data-explorer/data/enrolled.rds")
# ipeds <- readRDS("bren-student-data-explorer/data/ipeds.rds")
# diversity_stats <- readRDS("bren-student-data-explorer/data/diversity_stats.rds")
# ug_geoms <- readRDS("bren-student-data-explorer/data/ug_geoms.rds")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            career outcomes data                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#................initial placement data (cleaned)................
mesm_placement <- readRDS("data/mesm_placement_cleaned.rds")
meds_placement <- readRDS("data/meds_placement_cleaned.rds") 

#............active placement (status) data (cleaned)............
mesm_status <- readRDS("data/mesm_status_cleaned.rds") 
meds_status <- readRDS("data/meds_status_cleaned.rds")

#............Wrangled Data for Domestic Placement Maps...........
mesm_dom_placement_data <- readRDS("data/mesm_domestic_placement_data.rds") 
meds_dom_placement_data <- readRDS("data/meds_domestic_placement_data.rds") 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              demographics data                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

admissions <- readRDS("data/admissions.rds") 
enrolled <- readRDS("data/enrolled.rds") 
ipeds <- readRDS("data/ipeds.rds") 
diversity_stats <- readRDS("data/diversity_stats.rds")
ug_geoms <- readRDS("data/ug_geoms.rds")

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    set current admissions year (that we have admissions / app data for)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# using `enrolled` here, but should be the same for `admissions`

curr_admission_year <- max(enrolled$admission_year) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##         set current grad class year (that we have career data for)       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# using `meds_placement` here, but should be the same for `mesm_placement`, `mesm_status`, `meds_status`

#..................get current grad class year...................
curr_grad_year <- as.numeric(max(meds_placement$year)) 

#..........use ^ to create 3 year vars for career years..........
car_year1 <- curr_grad_year - 2
car_year2 <- curr_grad_year - 1
car_year3 <- curr_grad_year

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                set current year for MEDS employment status               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# based on enrollment year, which is one year prior to graduation 

meds_employmentStatus_curr_year <- curr_grad_year - 1 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                set current year for MESM employment status               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# based on enrollment year, which is two years prior to graduation 

mesm_employmentStatus_curr_year <- curr_grad_year - 2 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         get individual class sizes                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..................first for all classes / years.................
tot_enrolled <- enrolled |> group_by(admission_year, program, sir) |> summarize(enrolled = n())

#..................then for last 3 MESM cohorts..................
mesm1 <- tot_enrolled |> filter(program == "MESM", admission_year == curr_admission_year - 2) |> pull(enrolled) 
mesm2 <- tot_enrolled |> filter(program == "MESM", admission_year == curr_admission_year - 3) |> pull(enrolled) 
mesm3 <- tot_enrolled |> filter(program == "MESM", admission_year == curr_admission_year - 4) |> pull(enrolled) 

#..................and for last 3 MEDS cohorts...................
meds1 <- tot_enrolled |> filter(program == "MEDS", admission_year == curr_admission_year - 1) |> pull(enrolled) 
meds2 <- tot_enrolled |> filter(program == "MEDS", admission_year == curr_admission_year - 2) |> pull(enrolled) 
meds3 <- tot_enrolled |> filter(program == "MEDS", admission_year == curr_admission_year - 3) |> pull(enrolled) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- CREATE DFS THAT ARE USED THROUGHOUT DASHBOARD-----------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            MEDS / MESM class sizes & career survey respondents           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#....MESM program sizes + tot respondents (initial placement).....
# for calculating MESM career exit survey response rate
# `year` in `mesm_placement` == graduation year (admission year = graduation year - 2)
mesm_placement_size <- mesm_placement |> 
  select(year) |>
  group_by(year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    year == curr_grad_year ~ mesm1, 
    year == curr_grad_year - 1 ~ mesm2,
    year == curr_grad_year - 2 ~ mesm3 
  ))

#....MEDS program sizes + tot respondents (initial placement).....
# for calculating MEDS career exit survey response rate
# `year` in `meds_placement` == graduation year (admission year = graduation year - 1)
meds_placement_size <- meds_placement |>
  select(year) |>
  group_by(year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    year == curr_grad_year ~ meds1, 
    year == curr_grad_year - 1 ~ meds2, 
    year == curr_grad_year - 2 ~ meds3 
  ))

#......MESM program sizes + tot respondents (active status)......
mesm_status_size <- mesm_status |>
  select(year) |>
  group_by(year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    year == curr_grad_year ~ mesm1, 
    year == curr_grad_year - 1 ~ mesm2,
    year == curr_grad_year - 2 ~ mesm3
  ))

#......MEDS program sizes + tot respondents (active status)......
meds_status_size <- meds_status |>
  select(year) |>
  group_by(year) |>
  summarize(responses = n()) |>
  mutate(program_size = case_when(
    year == curr_grad_year ~ meds1, 
    year == curr_grad_year - 1 ~ meds2, 
    year == curr_grad_year - 2 ~ meds3 
  ))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        MEDS / MESM / PhD class sizes                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................program sizes.........................
program_size <- enrolled |>
  select(c(admission_year, application_id, program)) |>
  group_by(admission_year, program) |>
  summarize(size = n())

#...............total number students in each year...............
total_students_yr <- enrolled |>
  group_by(admission_year) |> 
  summarize(size = n())

#............5yr total number of students per program............
tot_5yr <- enrolled |>
  select(c(admission_year, application_id, program)) |>
  group_by(program) |>
  summarize(size = n())

#........5yr total number of students across all programs........
totStudents_allPrograms_5yr <- sum(tot_5yr$size)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Misc. non-reactive dfs                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#................wrangle sex by year and program.................
sex_program_time <- enrolled |> 
  select(c("admission_year", "application_id", "gender", "program")) |>  
  group_by(admission_year, program, gender) |> 
  summarize(count = n())

#................join sex data with program sizes................
sex_stats_time <- left_join(sex_program_time, program_size,
                            by = c("admission_year", "program")) |> 
  mutate(percent = round((count / size) * 100)) |> 
  mutate(gender = factor(gender, levels = c("F", "M", "U"),
                         labels = c("Female", "Male", "Undeclared")))

#................wrangle age by program and year.................
age_program_groups <- enrolled |> 
  select(c(admission_year, application_id, program, dob)) |> 
  mutate(dob_year = year(dob)) |> 
  mutate(age = admission_year - dob_year) |> 
  mutate(age_group = case_when(age >= 20 & age <= 22 ~ "20-22",
                               age >= 23 & age <= 24 ~ "23-24",
                               age >= 25 & age <= 29 ~ "25-29",
                               age >= 30 & age <= 34 ~ "30-34",
                               age >= 35 & age <= 39 ~ "35-39",
                               age >= 40 & age <= 49 ~ "40-49",
                               age >= 50 ~ "50+")) |> 
  group_by(program, age_group) |> 
  summarize(count = n())

#...........wrangle residency data by program and year...........
residency_stats <- enrolled |> 
  select(c(admission_year, application_id, program, citizenship_country, residency_country, 
           birth_country, california_resident, residency_status, ca_high_school, visa)) |> 
  group_by(admission_year, program, residency_status) |>
  summarize(count = n()) |>
  left_join(program_size, by = c("admission_year", "program")) |>
  mutate(percent = round((count / size) * 100, 1)) |>
  mutate(residency_status = fct_relevel(residency_status, "CA Resident", "Non-CA Resident", "International", "Unknown"))

#..............wrangle international UG universities.............
intl_unis <- enrolled |> 
  select(ug1_name, ug1_location) |> 
  filter(!ug1_location %in% c(state.name, "District of Columbia")) |> 
  group_by(ug1_location, ug1_name) |> 
  summarize(count = n())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- DEFINE UI TEXT ELEMENTS-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#footer_date_text <- paste0("Last updated: ", format(Sys.Date(), format = "%B %Y"))
# career_years_text <- paste0("All information presented here draw from initial job placement data collected on the graduating classes of ", curr_grad_year-2, "-", curr_grad_year, ".", "<br>",
#                             "test")
