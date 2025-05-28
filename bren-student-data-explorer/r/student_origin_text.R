student_origin_text <- function() {
  
  renderUI({
    
    tags$em(paste0("Over the last five years, Bren has welcomed students from institutions spanning ", 
                   unique_us_states_unis, " U.S. states and ", unique_countries_unis, " countries worldwide."))
    
  })
  
}