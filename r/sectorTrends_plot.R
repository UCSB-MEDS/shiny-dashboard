#' sectorTrends_plot
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
sectorTrends_plot <- function(input, data, program_acronym) {
  
  # determine which *_placement_size df (see global.R) to use based on program_acronym supplied ----
  if (program_acronym == "MESM") {
    
    placement_size <- mesm_placement_size
    
  } else if (program_acronym == "MEDS") {
    
    placement_size <- meds_placement_size
    
  }
  
  # data wrangling for sector trends plot ----
  sector <- data %>% 
    select(c(class_year, employer_sector)) %>% 
    mutate(sector_type = case_when(
      employer_sector %in% c("Consulting", "Corporate") ~ "Private",
      employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
      employer_sector %in% c("Foreign Government", "Other") ~ "Other",
      TRUE ~ employer_sector
    )) %>% 
    mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
    group_by(class_year, sector_type) %>% 
    summarize(count = n()) %>%
    filter(!is.na(sector_type))
  
  sector_time <- sector %>% 
    left_join(placement_size, by = "class_year") %>% 
    mutate(percent = round((count / responses) * 100, 1))
  
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot 
    sector_time_gg <- ggplot(data = sector_time, aes(x = as.factor(class_year), y = percent, fill = sector_type,
                                 text = paste0(sector_type, " (", percent, "%", ")", "\n", "Number of respondents: ", responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = paste0(program_acronym ," Alumni Placement by Sector"),
           x = NULL, y = "Percent of Respondents", fill = NULL) +
      scale_fill_manual(values = c("Private" = "#003660", "Public" = "#047c91", "Non-Profit" = "#dcd6cc", "Other" = "#9cbebe")) # ucsb navy, ucsb aqua, ucsb clay, ucsb mist
    
    # convert to plotly
    plotly::ggplotly(sector_time_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.1, x = 0.1)) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}

