
# may abstract this more so it can be used for both MESM & MEDS #

brenNet_stat_valueBox <- function(input) {
  
  # wrangle data for brenNet stat valueBox ----
  mesm_brenNet <- mesm_placement %>% 
    group_by(job_source) %>% 
    summarize(count = n()) %>% 
    # calculate percentages; total responses (196)
    mutate(percent = round((count / 196) * 100)) %>% 
    filter(job_source == "Bren School Network")
  
  # render brenNet stat valueBox ----
  renderValueBox({
    
    valueBox(
      "of graduates found their jobs through the Bren School Network",
      value = paste0(mesm_brenNet$percent, "%"),
      icon = icon("briefcase"),
      color = "blue"
    ) 
  }) 
  
}