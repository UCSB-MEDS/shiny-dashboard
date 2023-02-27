# class sizes (SC NOTE 2022-02-13: for reference only; vars not used anywhere in code); ay_year = year applied/enrolled

librrary(tidyverse)
enrolled <- readRDS("data/enrolled_cleaned.rds")

#### MESM (graduate 2 years after enrollment year) ####
gradClass_mesm2019 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2017) |> count() |> pull() # 85; enrolled ay_year 2017
gradClass_mesm2020 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2018) |> count() |> pull() # 77; enrolled ay_year 2018
gradClass_mesm2021 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2019) |> count() |> pull() # 93; enrolled ay_year 2019
gradClass_mesm2022 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2020) |> count() |> pull() # 92; enrolled ay_year 2020
gradClass_mesm2023 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2021) |> count() |> pull() # 83; enrolled ay_year 2021 
gradClass_mesm2024 <- enrolled |> filter(objective1 == "MESM" & ay_year == 2022) |> count() |> pull() # 73; enrolled ay_year 2022

#### MEDS (graduate 1 year after enrollment year) ####
gradClass_meds2022 <- enrolled |> filter(objective1 == "MEDS" & ay_year == 2021) |> count() |> pull() # 25; enrolled ay_year 2021
gradClass_meds2023 <- enrolled |> filter(objective1 == "MEDS" & ay_year == 2022) |> count() |> pull() # 31; enrolled ay_year 2022
