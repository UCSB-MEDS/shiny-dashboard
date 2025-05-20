#' Creates year selector radioButtons widget
#'
#' @param inputId inputId
#'
#' @return radioButtons widget with year choices
#' @export
#'
#' @examples
year_radioButtons <- function(inputId, selected) {
  
  radioButtons(inputId = inputId, label = NULL,
               choices = c(car_year1, car_year2, car_year3, "All Years"),
               selected = "All Years",
               inline = TRUE)
  
}