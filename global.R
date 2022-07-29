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
library(d3treeR)

# IMPORT DATA ----
apps_clean <- readRDS("data/apps_all_clean.rds")
bren_apps <- readRDS("data/bren_apps.rds")
mesmP <- readRDS("data/mesmP_all_clean.rds")
mesmS <- readRDS("data/mesmS_all_clean.rds")
diversity <- readRDS("data/diversity_all.rds")

# SOURCE FUNCTIONS ----

# REUSABLE DFS
prog_cohort_tot_all <- bren_apps %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>%
  group_by(objective1, ay_year) %>%
  summarize(cohort_tot = n())
