#' Generates (dark blue) valuebox for brenNet stat (Career tab) 
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return valueBox object
#' @export
#'
#' @examples
brenNet_stat_valueBox <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               data wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................filter data for current grad year...............
  data_curr_year <- data |> 
    filter(year == curr_grad_year)
  
  #....................calculate total responses...................
  total_responses <- length(data_curr_year$job_source)
  
  #..............wrangle data for breNet stat valueBox.............
  brenNet <- data_curr_year |>  
    group_by(job_source) |>  
    summarize(count = n()) |> 
    mutate(percent = round((count / total_responses) * 100)) |> 
    filter(job_source %in% c("Bren School Network", "Personal/Professional Contact")) |> 
    summarize(total_percent = sum(percent))
  
  #........................pull stat value.........................
  brenNet_stat <- brenNet |> 
    pull() 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..................render brenNet stat valueBox..................
  renderValueBox({
    
    valueBox(
      
      paste0("of survey respondents from the class of ", 
             curr_grad_year, 
             " found their jobs through the Bren School Network and/or personal professional contacts"),
      value = paste0(brenNet_stat, "%"),
      icon = icon("briefcase"),
      color = "blue"
      
    ) # END valueBox
    
  }) # END renderValueBox 
  
} # END fxn

