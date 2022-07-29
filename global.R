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

# get state geometries using tigris
df_state_geometries_us <- tigris::states(year = 2018) %>%
  select(GEOID, STUSPS, NAME, geometry) %>% 
  rename(fips = GEOID,
         state_abbrev = STUSPS,
         state = NAME) %>% 
  rmapshaper::ms_simplify(keep = 0.005, keep_shapes = TRUE)

# get international country geometries
world <- st_read(system.file("shapes/world.gpkg", package="spData"))
