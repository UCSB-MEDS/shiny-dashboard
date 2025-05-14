#' Creates interactive table displaying international placements (Career tab) 
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' 
#'
#' @return
#' @export
#'
#' @examples
internationalPlacement_table <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............wrangle international placement data..............
  international_tbl <- data |> 
    filter(!work_location_country %in% c("US", "Usa", "USA", "United States"),
           !is.na(work_location_country)) |> 
    group_by(employer_account_name, employer_sector, work_location_country) |> 
    summarize(count = n())
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # render DT datatable ----
  DT::renderDataTable({
    
    DT::datatable(data = international_tbl, 
                  colnames = c("Employer", "Sector", "Location", "# of alumni"),
                  class = "cell-border stripe", 
                  rownames = FALSE,
                  options = list(pageLength = 9, dom = 'Btipr') 
    ) # END datatable
    
  }) # END renderDataTable
  
} # END fxn