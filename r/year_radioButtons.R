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
               choices = c(2021, 2022, 2023, "All Years"),
               selected = "All Years",
               inline = TRUE)
  
}