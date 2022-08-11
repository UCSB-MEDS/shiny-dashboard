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
bren_apps <- readRDS("data/bren_apps.rds")
mesmP <- readRDS("data/mesmP_all_clean.rds")
mesmS <- readRDS("data/mesmS_all_clean.rds")
diversity <- readRDS("data/diversity_all.rds") # 2016-2021
#origins_df <- readRDS("data/origins_df.rds")
origins_geom <- readRDS("data/origins_geom.rds")
df_state_geometries_us <- readRDS("data/df_state_geometries_us.rds")

# SOURCE FUNCTIONS ----
source("r/age_plot.R")
source("r/race_plot.R")

# STYLING ----
mesm_color <- "#6D7D33"
meds_color <- "#047C91"
phd_color <- "#005AA3"

# REUSABLE DFS
prog_cohort_tot_all <- bren_apps %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>%
  group_by(objective1, ay_year) %>%
  summarize(cohort_tot = n())

# total number of students per program from 2016-2021
tot_5yr <- bren_apps %>% 
  select(c("ay_year",
           "application_id",
           "objective1",
           "dob")) %>% 
  group_by(objective1) %>%
  summarize(tot = n())

# background / ethnicity ipeds def
background_ipeds <- bren_apps %>% 
  select(ay_year,
         objective1,
         background,
         category,
         hispanic_latino) %>% 
  # replace NULL string with NA
  naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
  mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
  # assign demographic using ipeds definition
  mutate(category_ipeds = case_when(
    str_detect(category, ";") == TRUE ~ "Two or more races",
    str_detect(category, "American Indian / Alaska Native") == TRUE & hispanic_latino == FALSE ~ "American Indian or Alaska Native",
    str_detect(category, "Asian / Asian American") == TRUE & hispanic_latino == FALSE ~ "Asian",
    str_detect(category, "African American / Black") == TRUE & hispanic_latino == FALSE ~ "Black or African American",
    str_detect(category, "Native Hawaiian / other Pacific Islander") == TRUE & hispanic_latino == FALSE ~ "Native Hawaiian or Other Pacific Islander",
    str_detect(category, "White / Caucasian") == TRUE & hispanic_latino %in% c(FALSE, NA) ~ "White",
    hispanic_latino == TRUE ~ "Hispanic or Latino",
    is.na(category) == TRUE ~ "Unknown race and ethnicity"
  )) %>% 
  mutate(category_ipeds = factor(category_ipeds, levels = c(
    "American Indian or Alaska Native",
    "Asian",
    "Black or African American",
    "Native Hawaiian or Other Pacific Islander",
    "White",
    "Hispanic or Latino",
    "Two or more races",
    "Unknown race and ethnicity"
  )))