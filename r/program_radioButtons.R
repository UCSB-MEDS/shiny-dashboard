#' Creates program selection radioButton widget
#'
#' @param inputId inputId
#' @param selected the initial selected value; must be one of the following: MEDS, MESM PhD, or All Programs
#' @param include_all boolean; if FALSE, choices = MEDS, MESM, PhD; if TRUE, choices = MEDS, MESM, PhD, All Programs
#'
#' @return radioButtons widget for selecting Bren graduate program
#' @export
#'
#' @examples program_radioButtons(inputId = myId, selected = "MEDS", include_all = FALSE)
#' 

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
