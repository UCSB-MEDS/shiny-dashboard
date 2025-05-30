#' Creates interactive table displaying total number of students from each unique international undergraduate university
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
internationalUniversities_table <- function(input) {
  
  #......................render DT datatable.......................
  DT::renderDataTable({
    
    DT::datatable(data = intl_unis, colnames = c("Country", "University", "# of alumni"),
                  caption = "International universities attended by students prior to coming to Bren",
                  class = "cell-border stripe", rownames = FALSE,
                  options = list(pageLength = 12, dom = 'Bftipr')) 
    
  }) 
  
}




