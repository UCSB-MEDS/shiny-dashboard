#' Title
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#'
#' @return
#' @export
#'
#' @examples
internationalPlacement_table <- function(input, data) {
  
  # wrangle international placement data ----
  international_tbl <- data %>% 
    filter(!work_location_country %in% c("US", "Usa", "USA", "United States"),
           !is.na(work_location_country)) %>% 
    mutate(employer_account_name = case_when(
      employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
      TRUE ~ employer_account_name
    )) %>% 
    group_by(employer_account_name,
             employer_sector,
             work_location_country) %>% 
    summarize(count = n())
  
  # render DT datatable ----
  DT::renderDataTable({
    
    DT::datatable(data = international_tbl, colnames = c("Employer", "Sector", "Location", "# of alumni"),
      # caption = htmltools::tags$caption(style = "caption-side: top; text-align: left",
      #                                   htmltools::em("Bren MESM Alumni Employers and Sectors since 2019")),
      class = "cell-border stripe", rownames = FALSE,
      options = list(pageLength = 9, dom = 'Btipr') 
    )
    
  }) 
  
}