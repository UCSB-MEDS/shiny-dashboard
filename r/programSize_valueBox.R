#' programSize_valueBox
#'
#' @param input 
#' @param program_acronym character; "MEDS", "MESM", or "PhD"
#' @param color color of valueBox
#'
#' @return valueBox
#' @export
#'
#' @examples

programSize_valueBox <- function(input, program_acronym, color) {
  
  # wrangle data (see 'program_size_curr_year' in 'global.R') ----
  program_size <- program_size_curr_year |> filter(objective1 == program_acronym)
  
  # render valueBox ----
  renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = paste0(program_acronym, " students in the ", curr_year, " cohort"),
      value = program_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = color)
  })
  
}







# #....................MEDS cohort size valueBox...................
# 
# MEDS_programSize_valueBox <- function(input) {
#   
#   # wrangle data for MEDS size valueBox ----
#   meds_size <- program_size_curr_year %>% filter(objective1 == "MEDS")
#   
#   # render MEDS size valueBox ----
#   renderValueBox({
#     
#     shinydashboard::valueBox(
#       subtitle = "MEDS students in incoming 2022 cohort",
#       value = meds_size$size,
#       icon = icon("users", lib = "font-awesome"),
#       color = "light-blue")
#   }) 
#   
# }
# 
# 
# #....................MESM cohort size valueBox...................
# 
# MESM_programSize_valueBox <- function(input) {
#   
#   # wrangle data for MESM size valueBox ----
#   mesm_size <- program_size_curr_year %>% filter(objective1 == "MESM")
#   
#   # render MESM size valueBox ----
#   renderValueBox({
#     
#     valueBox(
#       subtitle = "MESM students in incoming 2022 cohort",
#       value = mesm_size$size,
#       icon = icon("users", lib = "font-awesome"),
#       color = "blue"
#     )
#     
#   })
#   
# }
# 
# 
# #....................PhD cohort size valueBox....................
# 
# PhD_programSize_valueBox <- function(input) {
#   
#   # wrangle data for PhD size valueBox ----
#   phd_size <- program_size_curr_year %>% filter(objective1 == "PhD")
#   
#   # render PhD size valueBox ----
#   renderValueBox({
#     
#     valueBox(
#       subtitle = "PhD students in incoming 2022 cohort",
#       value = phd_size$size,
#       icon = icon("users", lib = "font-awesome"),
#       color = "green"
#     )
#     
#   })
# 
# }

