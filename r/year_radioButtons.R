#' year_radioButtons
#'
#' @param inputId inputId
#'
#' @return radioButtons widget with year choices
#' @export
#'
#' @examples
year_radioButtons <- function(inputId, selected) {
  
  radioButtons(inputId = inputId, label = NULL,
               choices = c(2019, 2020, 2021, "All Years"),
               selected = "All Years",
               inline = TRUE)
  
}