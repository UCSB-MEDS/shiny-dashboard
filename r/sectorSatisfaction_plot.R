
# may abstract this more so it can be used for both MESM & MEDS #

sectorSatisfaction_plot <- function(input) {
  
  # data wrangling for sector satisfaction plot ----
  # total number of alumni in each sector (2019-2021)
  sector_totals <- mesm_placement %>% 
    group_by(employer_sector) %>% 
    summarize(sector_count = n())
  
  # create reactive df ----
  sector_satisfaction <- reactive({
    mesm_placement %>% 
      select(c(employer_sector,placement_satisfaction)) %>%  
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
      filter(employer_sector %in% input$sector_types)
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot
    sector_satisfaction_gg <- ggplot(data = sector_satisfaction(), aes(x = placement_satisfaction, y = percent, fill = reorder(placement_satisfaction, percent),
                                         text = paste0(placement_satisfaction, " (", percent, "%", ")", "\n", "Number of respondents: ", sector_count)))+
      geom_bar(position = "dodge", stat = "identity") +
      coord_flip(ylim = c(0, 100)) +
      scale_x_discrete(limits = rev(levels(sector_satisfaction()$placement_satisfaction))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none") +
      labs(title = paste0("MESM Placement Satisfaction in ", input$sector_types, "\n", "(Over 3 Years)"),
           x = NULL, y = "Percent of Respondents", fill = NULL) +
      # color brewer 4-class PuBu
      scale_fill_manual(values = c("Very Satisfied" = "#0570b0", "Satisfied" = "#74a9cf", "Somewhat Satisfied" = "#bdc9e1", "Unsatisfied" = "#f1eef6"))
    
    # convert to plotly
    plotly::ggplotly(sector_satisfaction_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15))) %>% 
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}