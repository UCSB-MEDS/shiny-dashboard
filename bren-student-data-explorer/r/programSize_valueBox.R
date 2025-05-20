#' Generates program size valueboxs (Demographics tab) 
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
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..................program size for current year.................
  program_size_curr_year <- program_size |> 
    filter(admission_year == curr_admission_year) 
  
  #.......................filter for program.......................
  program_size_display <- program_size_curr_year |> 
    filter(program == program_acronym)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..................render program size valueBox..................
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = paste0(program_acronym, " students in the ", curr_admission_year, " incoming cohort"), 
      value = program_size_display$size,
      icon = icon("users", lib = "font-awesome"),
      color = color
      ) # END valueBox
    
  }) # END renderValueBox
  
} # END fxn

