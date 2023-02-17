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
    # # SC NOTE 2023-02-16: updated based on request by KB (data entered incorrectly by student; KB & team may fix this on their end in the future)
    # mutate(work_location_state = case_when(
    #   work_location_state == "United States" & full_name == "Mariano Viz" ~ "CA",
    #   TRUE ~ work_location_state
    # )) |> 
    # # SC NOTE 2023-02-16: updated based on request by KB (data entered incorrectly by student; KB & team may fix this on their end in the future)
    # mutate(work_location_country = case_when(
    #   work_location_country == "Remote" & full_name == "Karla Paulina Garibay Garcia" ~ "Remote (International Location)",
    #   work_location_country == "california" & full_name == "Mariano Viz" ~ "United States",
    #   work_location_country == "Remote" & full_name == "Elmera Azadpour" ~ "United States",
    #   TRUE ~ work_location_country
    # )) |> 
    filter(!work_location_country %in% c("US", "Usa", "USA", "United States"),
           !is.na(work_location_country)) %>% 
    # mutate(employer_account_name = case_when(
    #   employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
    #   TRUE ~ employer_account_name
    # )) %>% 
    group_by(employer_account_name, employer_sector, work_location_country) %>% 
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