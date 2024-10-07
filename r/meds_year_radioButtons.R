#' Creates MEDS year selection radioButtons widget; ONCE WE HAVE 3 YEARS OF MEDS CAREER DATA, SWITCH TO USING `year_radioButtons()`
#'
#' @param inputId inputId
#'
#' @return radioButtons widget with year choices
#' @export
#'
#' @examples
meds_year_radioButtons <- function(inputId, selected) {
  
  radioButtons(inputId = inputId, label = NULL,
               choices = c(2022, 2023, "All Years"),
               selected = "All Years",
               inline = TRUE)
  
}