#' Generates (light blue) valuebox for initial job placement satisfaction (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return valueBox object
#' @export
#'
#' @examples
initPlacementSatisfaction_stat_valueBox <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #.............filter data for only current grad year.............
  data_curr_year <- data |> 
    filter(year == curr_grad_year)
  
  #..............calculate total number of responses...............
  total_responses <- length(data_curr_year$placement_satisfaction)
  
  #..................get sat + very sat percentage.................
  satisfaction_stat <- data_curr_year |> 
    group_by(placement_satisfaction) |> 
    summarize(count = n()) |> 
    mutate(percent = round((count / total_responses) * 100)) |> 
    filter(placement_satisfaction %in% c("Satisfied", "Very Satisfied")) |> 
    summarise(
      sat_vsat_count = sum(count),
      sat_vat_perc = sum(percent)
    ) |> 
    pull(sat_vat_perc)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #......render initial placement satisfacetion stat valueBox......
  renderValueBox({
    
    valueBox(paste0("of survey respondents from the class of ", 
                    curr_grad_year, 
                    " selected being “satisfied” or “very satisfied” with their initial job placement"),
             value = paste0(satisfaction_stat, "%"),
             icon = icon("heart"),
             color = "light-blue"
             
    ) # END valueBox
    
  }) # END renderValueBox
  
} # END fxn
