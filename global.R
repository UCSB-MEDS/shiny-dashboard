# PACKAGES ----
library(tidyverse)
library(rsconnect)
library(bslib)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(lubridate)
library(leaflet)
library(tmap)
library(sf)
library(plotly)
library(treemap)
library(d3treeR) # interactive tree map
library(shinycssloaders) # loading icon

# IMPORT DATA ----
admissions <- readRDS("data/admissions.rds")
enrolled <- readRDS("data/enrolled.rds")
mesmP <- readRDS("data/mesmP_all_clean.rds")
mesmS <- readRDS("data/mesmS_all_clean.rds")
diversity_stats <- readRDS("data/diversity_stats.rds")
ug_geoms <- readRDS("data/ug_geoms.rds")
us_state_geoms <- readRDS("data/us_state_geoms.rds")
ipeds <- readRDS("data/ipeds.rds")

# SOURCE FUNCTIONS ----
source("r/age_plot.R")
source("r/race_plot.R")

# STYLING ----
mesm_color <- "#6D7D33"
meds_color <- "#047C91"
phd_color <- "#005AA3"

# REUSABLE DFS
# program sizes 2016-2021
program_size <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1")) %>% 
  group_by(ay_year,
           objective1) %>% 
  summarize(program_size = n())

# total number of students per program from 2016-2021
tot_5yr <- enrolled %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(tot = n())
