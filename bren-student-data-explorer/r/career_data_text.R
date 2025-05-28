#' Generates text for the "About the <program_acronym> Career Data" box, including an updated range of years for which data are available
#'
#' @param program_acronym 
#'
#' @returns
#' @export
#'
#' @examples
career_data_text <- function(program_acronym){
  
  renderUI({
    HTML(paste0(
      "The information presented here is based on initial job placement data for the ", program_acronym, " graduating classes of ",
      curr_grad_year - 2, "-", curr_grad_year,
      ", collected by <a href='https://bren.ucsb.edu/career-services' target='_blank'>Bren’s Career Services team</a>.", "<br><br>",
      "<em>Please note that data represent alumni employment six months after graduation, rather than current employment.</em>"
    ))
  })
  
}