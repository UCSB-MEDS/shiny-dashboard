admissions_plot <- function(input) {
  
  # reactive stacked df (2017 - curr_year)
  admissions_stacked_all <- reactive({
    
    admissions %>% 
      select(c(app_submission_year, objective1, Enrolled, Applied, Admitted)) %>% 
      filter(objective1 == input$admit_stats_all) %>% 
      pivot_longer(cols = c(Enrolled, Applied, Admitted),
                   names_to = "admin_tots", values_to = "counts") %>% 
      mutate(admin_tots = factor(admin_tots, levels = c("Applied", "Admitted", "Enrolled")))
    
  }) 
  
  # avg acceptance rate reactive df ----
  admissions_rate <- reactive({
    
    admissions %>% 
      group_by(objective1) %>%
      summarize(mean = round(mean(admit_rate_pct), 1)) %>% 
      filter(objective1 == input$admit_stats_all)
    
  }) 
  
  # render plotly ----
  renderPlotly({
    
    # create ggplot (2017 - curr_year admissions stacked)
    admissions_all_plot <- ggplot(data = admissions_stacked_all(), aes(x = app_submission_year, y = counts, fill = reorder(admin_tots, counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Applied"),
               stat = "identity", aes(text = paste0("Applied: ", counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Admitted"),
               stat = "identity", width = 0.75, aes(text = paste0("Admitted: ", counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Enrolled"),
               stat = "identity", width = 0.6, aes(text = paste0("Enrolled: ", counts))) +
      scale_x_continuous(breaks = seq(min(admissions_stacked_all()$app_submission_year),
                                      max(admissions_stacked_all()$app_submission_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c("Applied" = "#dcd6cc", "Admitted" = "#9cbebe", "Enrolled" = "#003660")) +
      labs(title = paste0(input$admit_stats_all, " Admissions", "\n", "<i>", "<sup>", "Average acceptance rate: ", 
                          admissions_rate()$mean, "%", "</i>", "</sup>"),
           x = NULL, y = NULL, fill = NULL)
    
    # convert to plotly (2017 - curr_year admissions) 
    plotly::ggplotly(admissions_all_plot, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}