#' employmentStatus_stat_valueBox
#'
#' @param input input
#' @param data df; either 'mesm_status' or 'meds_status' (see global.R)
#' @param program_acroynm str; either "MESM" or "MEDS"
#'
#' @return a valueBox object
#' @export
#'
#' @examples
employmentStatus_stat_valueBox <- function(input, data, program_acronym) {
  
  # SC NOTE 2022-02-22: originally calculated stat based on total responses to survey, but really should use total class size ----
  # total_responses <- length(data$last_name)
  
  # SC NOTE 2022-02-22: NEED TO UPDATE SO THAT MESM VALUE BOX DRAWS FROM CORRECT YEAR
  # determine which year (see global.R) to use based on program_acronym supplied ----
  if (program_acronym == "MESM") {
    
    # total_num graduates in curr_year ----
    total_grads_curr_year <- enrolled |> 
      filter(objective1 == program_acronym) |> 
      filter(ay_year == mesm_employmentStatus_curr_year) |> nrow()
    
  } else if (program_acronym == "MEDS") {
    
    # total_num graduates in curr_year ----
    total_grads_curr_year <- enrolled |> 
      filter(objective1 == program_acronym) |> 
      filter(ay_year == meds_employmentStatus_curr_year) |> nrow()
    
  }
  
  # wrangle data for employment status valueBox stat ----
  status_stat <- data %>% 
    select(class_year,
           member_status) %>% 
    
    # filter for most recent grad class year (curr_year)
    filter(class_year == curr_grad_year) |> 
    
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
    mutate(percent = round((count / total_grads_curr_year) * 100)) %>% 
    filter(placed == "Placed")
  
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = paste0("of graduates from the class of ", curr_grad_year, " were employed 6 months after graduation"),
      value = paste0(status_stat$percent, "%"),
      icon = icon("handshake"),
      color = "green"
    ) 
    
  }) # END render employment % valueBox
  
}
