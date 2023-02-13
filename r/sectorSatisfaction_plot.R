#' sectorSatisfaction_plot
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return a renderPlotly object
#' @export
#'
#' @examples
sectorSatisfaction_plot <- function(input, data, program_acronym) {

  # data wrangling for sector satisfaction plot ----
  # total number of alumni in each sector (for all years)
  sector_totals <- data %>% 
    group_by(employer_sector) %>% 
    summarize(sector_count = n())
  
  # create reactive df ----
  sector_satisfaction <- reactive({
    
    # determine which inputId to use based on program_acronym supplied ----
    if (program_acronym == "MESM") {

      radioButton_sectorInput <- input$mesm_sector_types
      
    } else if (program_acronym == "MEDS") {

      radioButton_sectorInput <- input$meds_sector_types
      
    }
    
    data %>% 
      select(c(employer_sector, placement_satisfaction)) %>%  
      group_by(employer_sector, placement_satisfaction) %>% 
      summarize(count = n()) %>% 
      # consulting (3), corporate (2), local gov (1)
      drop_na() %>% 
      left_join(sector_totals, by = "employer_sector") %>% 
      # calculate percent by satisfaction count / total # of alumni working in that sector
      mutate(percent = round((count / sector_count) * 100, 1)) %>% 
      mutate(placement_satisfaction = factor(placement_satisfaction, levels = c("Very Satisfied","Satisfied", "Somewhat Satisfied", "Unsatisfied"),
                                             labels = c("Very Satisfied", "Satisfied", "Somewhat Satisfied", "Unsatisfied"))) %>% 
      # change other to Eco-E/New Business
      mutate(employer_sector = case_when(
        employer_sector == "Other" ~ "Eco-Entrepreneurship/New Business",
        TRUE ~ employer_sector
      )) %>% 
      # reactive filter
      filter(employer_sector %in% radioButton_sectorInput)
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # determine which number of years and inputId to use based on program_acronym supplied ----
    if (program_acronym == "MESM") {
    
      num_years <- 3
      radioButton_sectorInput <- input$mesm_sector_types
      
    } else if (program_acronym == "MEDS") {

      num_years <- 1 # SC NOTE 2023-02-04: needs to be updated as we add additional years
      radioButton_sectorInput <- input$meds_sector_types
      
    }
    
    # validation message prints when there are no alumni from chosen sector ----
    validate(need(nrow(sector_satisfaction()) > 0, paste0("We have not yet had any ", program_acronym, " alumni accept positions in this sector for their initial placements. Check back next year!")))
    
    # create ggplot
    sector_satisfaction_gg <- ggplot(data = sector_satisfaction(), aes(x = placement_satisfaction, y = percent, fill = reorder(placement_satisfaction, percent),
                                         text = paste0(placement_satisfaction, " (", percent, "%", ")", "\n", "Number of respondents: ", sector_count)))+
      geom_bar(position = "dodge", stat = "identity", width = 0.9) +
      coord_flip(ylim = c(0, 100)) +
      scale_x_discrete(limits = rev(levels(sector_satisfaction()$placement_satisfaction))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none") +
      labs(title = paste0(program_acronym, " Placement Satisfaction in ", radioButton_sectorInput, "\n", "(Over ", num_years, " Year(s))"),
           x = NULL, y = "Percent of Respondents", fill = NULL) +
      # color brewer 4-class PuBu
      scale_fill_manual(values = c("Very Satisfied" = "#0570b0", "Satisfied" = "#74a9cf", "Somewhat Satisfied" = "#bdc9e1", "Unsatisfied" = "#f1eef6"))
    
    # convert to plotly
    plotly::ggplotly(sector_satisfaction_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15))) %>% 
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}
