#' employmentStatus_stat_valueBox
#'
#' @param input input
#' @param data df; either 'mesm_status' or 'meds_status' (see global.R)
#'
#' @return a valueBox object
#' @export
#'
#' @examples
employmentStatus_stat_valueBox <- function(input, data) {
  
  # calculate total responses (length of df) ----
  total_responses <- length(data$last_name)
  
  # wrangle data for employment status valueBox stat ----
  status_stat <- data %>% 
    select(class_year,
           member_status) %>% 
    
    # assign placement status label
    mutate(status = case_when(
      member_status %in% c("FT Career", "FT Temporary Career", "PT Temporary Career", "FT Career-Sponsored", "PT Career", "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off", "Searching", "Time-Off") ~ "Searching or Time Off",
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
    
    # calculate percentage
    mutate(percent = round((count / total_responses) * 100)) %>% 
    filter(placed == "Placed")
  
  # render employment stat valueBox ----
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = "of graduates were employed 6 months after graduation",
      value = paste0(status_stat$percent, "%"),
      icon = icon("handshake"),
      color = "green"
    ) 
    
  }) # END render employment % valueBox
  
}


## ORIGINAL FXN
# employmentStatus_stat_valueBox <- function(input) {
#   
#   # wrangle data for employment status valueBox stat ----
#   status_stat <- mesm_status %>% 
#     select(mesm_class_year,
#            member_status) %>% 
#     
#     # assign placement status label
#     mutate(status = case_when(
#       member_status %in% c("FT Career", "FT Temporary Career", "PT Temporary Career", "FT Career-Sponsored", "PT Career", "FT Career-Sponsored") ~ "Career",
#       member_status %in% c("Time Off", "Searching") ~ "Searching or Time Off",
#       member_status %in% c("FT New Business", "FT Eco-E") ~ "New Business",
#       member_status %in% c("Internship/Fellowship", "Continuing Internship", "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
#       TRUE ~ member_status
#     )) %>% 
#     
#     # assign placed vs not placed
#     mutate(placed = case_when(
#       status == "Searching or Time Off" ~ "Not Placed",
#       TRUE ~ "Placed"
#     )) %>% 
#     
#     # calculate totals
#     group_by(placed) %>% 
#     summarize(count = n()) %>% 
#     
#     # calculate percentage; used tot mesm_responses (79+74+82)
#     mutate(percent = round((count / 235) * 100)) %>% 
#     filter(placed == "Placed")
#   
#   # render employment stat valueBox ----
#   renderValueBox({
#     
#     shinydashboard::valueBox(
#       subtitle = "of graduates were employed 6 months after graduation",
#       value = paste0(status_stat$percent, "%"),
#       icon = icon("house"),
#       color = "green"
#     ) 
#     
#   }) # END render employment % valueBox
#   
# }
