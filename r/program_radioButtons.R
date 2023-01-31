program_radioButtons <- function(inputId, selected, include_all = FALSE) { 
  
  choices <- c("MEDS", "MESM", "PhD")
  
  if (include_all) {
    
    choices <- append(choices, "All Programs")
    
  }
    
  radioButtons(inputId = inputId, label = NULL,
               choices = choices,
               selected = selected,
               inline = TRUE) 
  
}
