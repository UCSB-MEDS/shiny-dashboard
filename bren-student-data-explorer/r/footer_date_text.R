#' Generates footer text indicating the date of the last application update for the "About this Dashboard" page
#'
#' @returns
#' @export
#'
#' @examples
footer_date_text <- function(){
  
  renderUI({
    tags$em(paste0("Last updated: ", format(Sys.Date(), format = "%B %Y")))  
  })
  
}