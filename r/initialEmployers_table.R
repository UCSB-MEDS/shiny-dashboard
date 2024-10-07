#' Creates interactive table displaying initial employer names, sector, and total number of alumni (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement'
#'
#' @return
#' @export
#'
#' @examples
initialEmployers_table <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #............wrangle data for initial employer table.............
  employer <- data |> 
    select(c(employer_account_name,
             employer_sector)) |> 
    mutate(employer_account_name = str_replace_all(employer_account_name, "Formerly", "formerly")) |> 
    group_by(employer_account_name,
             employer_sector) |> 
    summarize(freq = n())
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #...............render initial employers datatable...............
  DT::renderDataTable({
    
    DT::datatable(data = employer, 
                  colnames = c("Employer", "Sector", "# of Alumni"),
                  class = "cell-border stripe", 
                  rownames = FALSE,
                  options = list(pageLength = 8, dom = 'Bftipr') 
                  
    ) # END datatable
    
  }) # END renderDataTable
  
} # END fxn
