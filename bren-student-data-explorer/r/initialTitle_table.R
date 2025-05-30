#' Creates interactive table displaying total number of alumni with unique job titles (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement'
#'
#' @return
#' @export
#'
#' @examples
initialTitle_table <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # wrangle data for initial employer & sector table ----
  title <- data |> 
    select(c(job_title)) |> 
    group_by(job_title) |> 
    summarize(freq = n()) |> 
    arrange(desc(freq))
  
  # render DT datatable ----
  DT::renderDataTable({
    
    DT::datatable(data = title, colnames = c("Job Title", "# of Alumni"),
                  class = "cell-border stripe", rownames = FALSE,
                  options = list(pageLength = 8, dom = 'Bftipr') 
                  
    ) # END datatable
    
  }) # END renderDataTable 
  
} # END fxn
