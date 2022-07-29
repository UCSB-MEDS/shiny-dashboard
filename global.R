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
library(sf)
library(plotly)
library(treemap)
library(d3treeR) # interactive tree map
library(shinycssloaders) # loading icon

# IMPORT DATA ----
admissions <- readRDS("data/admissions.rds")
bren_apps <- readRDS("data/bren_apps.rds")
mesmP <- readRDS("data/mesmP_all_clean.rds")
mesmS <- readRDS("data/mesmS_all_clean.rds")
diversity <- readRDS("data/diversity_all.rds")
origins_df <- readRDS("data/origins_df.rds")
df_state_geometries_us <- readRDS("data/df_state_geometries_us.rds")

# SOURCE FUNCTIONS ----

# REUSABLE DFS
prog_cohort_tot_all <- bren_apps %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>%
  group_by(objective1, ay_year) %>%
  summarize(cohort_tot = n())

