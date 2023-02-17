#' initialEmployers_table
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement'
#'
#' @return
#' @export
#'
#' @examples
initialEmployers_table <- function(input, data) {
  
  # wrangle data for initial employer & sector table ----
  employer <- data %>% 
    select(c(employer_account_name,
             employer_sector)) %>%
    # # NOTE: these may not apply to both MESM & MEDS data, but should affect the use of this function
    # mutate(employer_account_name = case_when(
    #   employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
    #   employer_account_name == "Clean, Renewable and Environmental Opportunities (CREO)CREO" ~ "Clean, Renewable and Environmental Opportunities (CREO)",
    #   employer_account_name == "Environmental Incentives. LLC" ~ "Environmental Incentives, LLC",
    #   TRUE ~ employer_account_name
    # )) %>% 
    mutate(employer_account_name = str_replace_all(employer_account_name, "Formerly", "formerly")) %>% 
    group_by(employer_account_name,
             employer_sector) %>% 
    summarize(freq = n())
  
  # render DT datatable ----
  DT::renderDataTable({
    
    DT::datatable(data = employer, colnames = c("Employer", "Sector", "# of alumni"),
      class = "cell-border stripe", rownames = FALSE,
      options = list(pageLength = 8, dom = 'Bftipr') 
    ) 
    
  }) 
  
}
