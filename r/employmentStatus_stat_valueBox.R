
# may abstract this more so it can be used for both MESM & MEDS #

employmentStatus_stat_valueBox <- function(input) {
  
  # wrangle data for employment status valueBox stat ----
  status_stat <- mesm_status %>% 
    select(mesm_class_year,
           member_status) %>% 
    
    # assign placement status label
    mutate(status = case_when(
      member_status %in% c("FT Career", "FT Temporary Career", "PT Temporary Career", "FT Career-Sponsored", "PT Career", "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off", "Searching") ~ "Searching or Time Off",
      member_status %in% c("FT New Business", "FT Eco-E") ~ "New Business",
      member_status %in% c("Internship/Fellowship", "Continuing Internship", "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
      TRUE ~ member_status
    )) %>% 
    
    # assign placed vs not placed
    mutate(placed = case_when(
      status == "Searching or Time Off" ~ "Not Placed",
      TRUE ~ "Placed"
    )) %>% 
    
    # calculate totals
    group_by(placed) %>% 
    summarize(count = n()) %>% 
    
    # calculate percentage; used tot mesm_responses (79+74+82)
    mutate(percent = round((count / 235) * 100)) %>% 
    filter(placed == "Placed")
  
  # render employment stat valueBox ----
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = "of graduates were employed 6 months after graduation",
      value = paste0(status_stat$percent, "%"),
      icon = icon("house"),
      color = "green"
    ) 
    
  }) # END render employment % valueBox
  
}
