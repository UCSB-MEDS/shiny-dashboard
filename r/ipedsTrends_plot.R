ipedsTrends_plot <- function(input) {
  
  # wrangle data & create reactive df ----
  category_ipeds_stats_time <- reactive({
    
    if (input$race_trends == "All Programs") {
      ipeds %>% 
        group_by(app_submission_year, category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(total_students_yr, by = "app_submission_year") %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
      ipeds %>% 
        group_by(app_submission_year, objective1, category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(program_size, by = c("app_submission_year", "objective1")) %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$race_trends)
      
    } # END else statement
    
  })
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot
    race_trends_gg <- ggplot(data = category_ipeds_stats_time(),
                             aes(x = app_submission_year, y = percent, fill = reorder(category_ipeds, percent),
                                 text = paste0(category_ipeds, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_continuous(breaks = seq(max(category_ipeds_stats_time()$app_submission_year),
                                      min(category_ipeds_stats_time()$app_submission_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(values = c(
        "American Indian or Alaska Native" = "#003660", # ucsb navy
        "Asian" = "#047c91", # ucsb aqua
        "Black or African American" = "#dcd6cc", # ucsb clay
        "Hispanic or Latino" = "#6d7d33", # ucsb moss
        "Native Hawaiian or Other Pacific Islander" = "#9cbebe", # ucsb mist
        "White" = "#dce1e5", # ucsb light grey
        "Two or more races" = "#79a540", # bren leaf green
        "Unknown race and ethnicity" = "#09847a" # ucsb sea green
      )) +
      labs(title = paste0("IPEDS Race / Category Trends", " (", input$race_trends, ")"),
           y = NULL, x = NULL, fill = NULL)
    
    # convert to plotly
    plotly::ggplotly(race_trends_gg, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", y = -0.1)) %>% 
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  })  
  
}