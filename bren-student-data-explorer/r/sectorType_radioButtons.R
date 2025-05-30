#' Creates sector selection radioButtons input
#'
#' @param inputId inputId
#' @param selected the initial selected value; must be one of the following: Consulting, Corporate, Eco_entrepreneurship/New Business, Federal Government, Foreign Government, Local Government, Non-Profit, Research/Education, State Government
#' @return
#' @export
#'
#' @examples

sectorType_radioButtons <- function(inputId, selected) {
  
  radioButtons(inputId = inputId, label = NULL,
               choices = c("Consulting", 
                           "Corporate", 
                           "Eco-Entrepreneurship/New Business",
                           "Federal Government", 
                           "Foreign Government", 
                           "Local Government", 
                           "Non-Profit", 
                           "Research/Education", 
                           "State Government"),
               selected = selected,
               inline = TRUE)
  
}