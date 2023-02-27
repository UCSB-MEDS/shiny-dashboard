#' brenNet_stat_valueBox
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return valueBox object
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
      paste0("of survey respondents from the graduating class of ", curr_grad_year, " found their jobs through the Bren School Network"),
      value = paste0(brenNet_stat, "%"),
      icon = icon("briefcase"),
      color = "blue"
    ) 
  }) 
  
}
