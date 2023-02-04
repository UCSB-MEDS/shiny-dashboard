#' brenNet_stat_valueBox
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return
#' @export
#'
#' @examples
brenNet_stat_valueBox <- function(input, data) {
  
  # calculate total responses ----
  total_responses <- length(data$job_source)
  
  # wrangle data for brenNet stat valueBox ----
  brenNet <- data %>% 
    group_by(job_source) %>% 
    summarize(count = n()) %>% 
    mutate(percent = round((count / total_responses) * 100)) %>% 
    filter(job_source == "Bren School Network")
  
  # pull stat value
  brenNet_stat <- brenNet[[1,3]]
  
  # render brenNet stat valueBox ----
  renderValueBox({
    
    valueBox(
      "of graduates found their jobs through the Bren School Network",
      value = paste0(brenNet_stat, "%"),
      icon = icon("briefcase"),
      color = "blue"
    ) 
  }) 
  
}

## ORIGINAL FUNCTION ----
# brenNet_stat_valueBox <- function(input) {
#   
#   # wrangle data for brenNet stat valueBox ----
#   mesm_brenNet <- mesm_placement %>% 
#     group_by(job_source) %>% 
#     summarize(count = n()) %>% 
#     # calculate percentages; total responses (196)
#     mutate(percent = round((count / 196) * 100)) %>% 
#     filter(job_source == "Bren School Network")
#   
#   # render brenNet stat valueBox ----
#   renderValueBox({
#     
#     valueBox(
#       "of graduates found their jobs through the Bren School Network",
#       value = paste0(mesm_brenNet$percent, "%"),
#       icon = icon("briefcase"),
#       color = "blue"
#     ) 
#   }) 
#   
# }