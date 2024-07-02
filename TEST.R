
# import enrolled data ----
enrolledOLD <- readRDS(here::here("data", "enrolled_2022.rds"))
enrolledNEW <- readRDS(here::here("data", "enrolled.rds"))

# filter for just overlapping years (2019 - 2022), then get application ids from old and new enrolled data ----
enrolled_old_wrangled <- enrolledOLD |> 
  rename(app_submission_year = ay_year) |> 
  rename(racial_categories = category) |> 
  filter(!app_submission_year %in% c(2017, 2018)) |> 
  mutate(application_id = as.numeric(application_id)) |> 
  select(application_id)

enrolled_new_wrangled <- enrolledNEW |> 
  filter(app_submission_year != 2023) |> 
  mutate(application_id = as.numeric(application_id)) |> 
  mutate(california_resident = case_when(
    california_resident == "Yes" ~ TRUE,
    california_resident == "No" ~ FALSE
  )) |> 
  mutate(ca_high_school = case_when(
    ca_high_school == "Yes" ~ TRUE,
    ca_high_school == "No" ~ FALSE
  )) |> 
  select(application_id) 

# compare them -----
discrepancy_enrolled <- anti_join(enrolled_old_wrangled, enrolled_new_wrangled) |> 
  pull()

discrepancy_enrolled <- as.vector(discrepancy_enrolled)

# get df of students who appeared in old data but not newest data ----
enrolled_diff <- enrolled2022 |> 
  rename(app_submission_year = ay_year) |> 
  rename(racial_categories = category) |> 
  filter(application_id %in% discrepancy_enrolled)

#### three MESMs (172159 176607 179182; application year 2022) who do not appear in the latest demographics data #### 
# looked up each of these application_ids in the original raw data for THIS year's (June/July 2024) update (see `apps_clean` df in the `admissions-data` repo)
# 172159 was absent
# 176607 & 179182 were both present, but SIRed "no" -- it seems that they may have accidentally been included in the `enrolled` data in the last update
# This leads me to believe that the latest stats are correct


# ipeds2022 <- readRDS(here::here("data", "ipeds_2022.rds"))
# ipeds2023 <- readRDS(here::here("data", "ipeds.rds"))
# 
# 
# ipeds_old_filtered <- ipeds2022 %>%
#   rename(app_submission_year = ay_year) |> 
#   group_by(app_submission_year, objective1, category_ipeds) %>%
#   summarize(count = n()) %>%
#   left_join(program_size, by = c("app_submission_year", "objective1")) %>%
#   mutate(percent = round((count / size) * 100, 1)) %>%
#   rename(count22 = count, size22 = size, perc22 = percent) |> 
#   filter(objective1 == "MESM") |> 
#   filter(app_submission_year == 2022)
# 
# ipeds_new_filtered <- ipeds2023 %>%
#   group_by(app_submission_year, objective1, category_ipeds) %>%
#   summarize(count = n()) %>%
#   left_join(program_size, by = c("app_submission_year", "objective1")) %>%
#   mutate(percent = round((count / size) * 100, 1)) %>%
#   rename(count23 = count, size23 = size, perc23 = percent) |> 
#   filter(objective1 == "MESM") |> 
#   filter(app_submission_year == 2022)
# 
# 
# mesm_ipeds_22_23 <- full_join(ipeds_old_filtered, ipeds_new_filtered)
# 
# # discrepancy between MESM 2022 Unknown race and ethnicity ----
# mesm_old <- ipeds2022 |> 
#   rename(app_submission_year = ay_year) |> 
#   filter(objective1 == "MESM",
#          app_submission_year == 2022,
#          category_ipeds == "Unknown race and ethnicity")
# mesm_new <- ipeds2023 |> 
#   filter(objective1 == "MESM",
#          app_submission_year == 2022,
#          category_ipeds == "Unknown race and ethnicity")
# 
# 
# ipedsOLD_match <- ipeds2022 |> 
#   rename(app_submission_year = ay_year) |> 
#   filter(!app_submission_year %in% c(2017, 2018))
# ipedsNEW_match <- ipeds2023 |> 
#   filter(!app_submission_year %in% c(2023))
# 
# discrepancy <- anti_join(ipedsOLD_match, ipedsNEW_match)

# ------------------------------------------------------

