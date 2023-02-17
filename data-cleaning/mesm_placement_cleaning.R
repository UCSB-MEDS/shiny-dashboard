# SC NOTE 2023-02-16: applying same cleaning workflow to both mesm_placement & meds_placement so I don't miss anything

library(tidyverse)

us_names <- c("USA", "US", "Usa")
ca_names <- c("Ca", "CALIFORNIA", "California")

mesm_placement_cleaned <- readRDS("data/mesm_placement.rds") |> 
  
  # ---- DATA ENTERED INCORRECTLY BY STUDENTS ----

  # SC NOTE 2023-02-16: updated as requested by KB (data entered incorrectly by student; KB & team may fix this on their end in the future)
  mutate(work_location_state = case_when(
    work_location_state == "United States" & full_name == "Mariano Viz" ~ "CA",
    TRUE ~ work_location_state
  )) |> 
  # SC NOTE 2023-02-16: updated as requested by KB (data entered incorrectly by student; KB & team may fix this on their end in the future)
  mutate(work_location_country = case_when(
    work_location_country == "Remote" & full_name == "Karla Paulina Garibay Garcia" ~ "Remote (International Location)",
    work_location_country == "california" & full_name == "Mariano Viz" ~ "United States",
    work_location_country == "Remote" & full_name == "Elmera Azadpour" ~ "United States",
    TRUE ~ work_location_country
  )) |> 
  
  # ---- COMPANY NAME CHANGE ----

  mutate(employer_account_name = case_when(
    # SC NOTE 2023-02-16: updated as requested by KB
    employer_account_name == "ViacomCBS" ~ "Paramount (Formerly ViacomCSB)",
    # SC NOTE 2023-02-16: HD code taken from initialEmployers_table.R
    employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
    employer_account_name == "Clean, Renewable and Environmental Opportunities (CREO)CREO" ~ "Clean, Renewable and Environmental Opportunities (CREO)",
    employer_account_name == "Environmental Incentives. LLC" ~ "Environmental Incentives, LLC",
    TRUE ~ employer_account_name
  )) |> 
  
  # ---- INCORRECT EMPLOYMENT_TYPE ----

  # SC NOTE 2023--02-16: updated as requested by KB
  mutate(employment_type = case_when(
    employment_type == "Full-Time Job" & full_name == "Katelyn Elizabeth Toigo" ~ "Internship",
    TRUE ~ employment_type
  )) |> 
  
  # ---- FIX STATE ABBREVIATIONS & STANDARDIZE STATE VALUES ----

  # SC NOTE 2023-02-16: HD code taken from geographicComparison_plot.R
  mutate(work_location_state = case_when(
    work_location_state %in% ca_names ~ "CA",
    work_location_state == "Maryland" ~ "MD",
    work_location_state == "Washington" ~ "WA",
    work_location_state == "District of Columbia" ~ "DC",
    work_location_state == "Indiana" ~ "IN",
    work_location_state == "Mt" ~ "MT",
    work_location_state == "Hawaii" ~ "HI",
    work_location_state == "N/A" ~ NA_character_,
    work_location_state == "michoacan" ~ "Michoacan",
    # specifically assign correct work location state
    work_location_city == "Washington DC" ~ "DC",
    work_location_city == "Oxnard" ~ "CA",
    work_location_city == "Santa Cruz" ~ "CA",
    work_location_city == "Fort Collins" ~ "CO",
    work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
    TRUE ~ work_location_state
  )) |> 
  
  # ---- STANDARDIZE UNITED STATES VALUES & ASSIGN CORRECT COUNTRY VALUES ----

  # SC NOTE 2023-02-16: HD code taken from geographicComparison_plot.R
  mutate(work_location_country = case_when(
    work_location_country %in% us_names ~ "United States",
    # specifically assign correct country values
    work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
    work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
    TRUE ~ work_location_country
  )) |> 
  
  # ---- ASSIGN CA / OUT OF STATE / INTERNATIONAL ----

  # SC NOTE 2023-02-16: HD code taken from geographicComparison_plot.R
  mutate(location = case_when(
    work_location_state == "CA" ~ "Domestic (California)",
    work_location_state != "CA" & work_location_country == "United States" ~ "Domestic (Out of State)",
    work_location_country != "United States" ~ "International"
  )) |> 
  
  # ---- CREATE SECTOR_TYPE COLUMN ----

  # SC NOTE 2023-02-16: taken from salaryBySector_plot.R & meds_salaryBySector_plot.R
  mutate(sector_type = case_when(
    employer_sector %in% c("Consulting", "Corporate") ~ "Private",
    employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
    employer_sector %in% c("Foreign Government", "Other") ~ "Other",
    TRUE ~ employer_sector
  )) 

# saveRDS(mesm_placement_cleaned, "data/mesm_placement_cleaned.rds")
