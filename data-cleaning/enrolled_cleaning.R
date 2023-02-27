
library(tidyverse)

enrolled_cleaned <- readRDS("data/enrolled.rds") |> 
  
  # filter out student who dropped out Summer 2021
  filter(application_id != 160140)

# saveRDS(enrolled_cleaned, "data/enrolled_cleaned.rds")
