#' programSize_valueBox
#'
#' @param input 
#' @param program_acronym chr str; "MEDS", "MESM", or "PhD"
#' @param color color of valueBox
#'
#' @return valueBox object
#' @export
#'
#' @examples

programSize_valueBox <- function(input, program_acronym, color) {
  
  # program size for current year; SC NOTE 2023-03-06: removed this from global.R
  program_size_curr_year <- program_size %>% filter(ay_year == curr_year)
  
  # filter for individual program (see 'program_size_curr_year' in 'global.R') ----
  program_size_display <- program_size_curr_year |> filter(objective1 == program_acronym)
  
  # render valueBox ----
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = paste0(program_acronym, " students in the ", curr_year, " cohort"),
      value = program_size_display$size,
      icon = icon("users", lib = "font-awesome"),
      color = color)
    
  })
  
}

