#' initPlacementSatisfaction_stat_valueBox 
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return valueBox object
#' @export
#'
#' @examples
initPlacementSatisfaction_stat_valueBox <- function(input, data) {
  
  # calculate total responses ----
  total_responses <- length(data$placement_satisfaction)
 
  # wrangle data for initial placement satisfaction valueBox stat ----
  satisfaction_stat <- data %>% 
    group_by(placement_satisfaction) %>% 
    summarize(count = n()) %>% 
    mutate(percent = round((count / total_responses) * 100)) %>% 
    filter(placement_satisfaction %in% c("Satisfied", "Very Satisfied"))
  
  # isolate just satisfied percent
  satisfied_num <- satisfaction_stat %>% filter(placement_satisfaction == "Satisfied") %>% 
    select(percent)
  
  # isolate just very satisfied percent
  verySatisfied_num <- satisfaction_stat %>% filter(placement_satisfaction == "Very Satisfied") %>% 
    select(percent)
  
  # add percentages together for valueBox
  total_satisfied <- satisfied_num$percent + verySatisfied_num$percent
  
  # render initial placement satisfaction stat valueBox ----
  renderValueBox({
    
    valueBox("of graduates ranked being “satisfied” or “very satisfied” with their initial job placement",
             value = paste0(total_satisfied, "%"),
             icon = icon("heart"),
             color = "light-blue"
    ) 
    
  }) 
   
}
