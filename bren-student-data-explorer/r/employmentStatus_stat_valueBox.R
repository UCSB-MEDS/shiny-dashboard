#' Generates (green) valuebox for 6-month employment status (Career tab) 
#'
#' @param input input
#' @param data df; either 'mesm_status' or 'meds_status' 
#' @param program_acroynm str; either "MESM" or "MEDS"
#'
#' @return a valueBox object
#' @export
#'
#' @examples
employmentStatus_stat_valueBox <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               data wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 
  
  #..........wrangle data for employment status valueBox...........
  status_data <- data |> 
    select(year, member_status) |> 
    filter(year == curr_grad_year) |> 
    mutate(status = case_when(
      member_status %in% c("FT Career", 
                           "FT Temporary Career", 
                           "PT Temporary Career", 
                           "FT Career-Sponsored", 
                           "PT Career", 
                           "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off", "Time-Off") ~ "Time Off",
      member_status %in% c("FT New Business", "FT Eco-E") ~ "New Business",
      member_status %in% c("Internship/Fellowship", 
                           "Continuing Internship", 
                           "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
      TRUE ~ member_status
    )) |> 
    mutate(placed = case_when(
      status == "Searching" ~ "Not Placed",
      status == "Time Off" ~ "Time Off",
      TRUE ~ "Placed"
    )) 
  
  #........................calculate totals........................
  # NOTE: not counting people that said they were taking time off ----
  status_stat <-  status_data |> 
    group_by(placed) |>  
    summarize(count = n()) |> 
    mutate(percent = round((count / nrow(status_data |> filter (placed != "Time Off" ))) * 100)) |> 
    filter(placed == "Placed")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................render employment status valueBox...............
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = paste0("of job-seeking survey respondents from the class of ", 
                        curr_grad_year, 
                        " were employed within 6 months after graduation"),
      value = paste0(status_stat$percent, "%"),
      icon = icon("handshake"),
      color = "green"
    ) 
    
  }) # END render employment % valueBox
  
} # END fxn
