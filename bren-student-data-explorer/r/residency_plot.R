residency_plot <- function(input) {
  
  #..........................render plotly.........................
  renderPlotly({
    
    # build ggplot ----
    residency_all <- ggplot(data = residency_stats,
                            aes(x = admission_year, y = percent, fill = reorder(residency_status, percent),
                                text = paste0(residency_status, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(residency_stats$admission_year),
                                      max(residency_stats$admission_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = "Residency distribution trends by degree program",
           x = NULL, y = NULL, fill = NULL) +
      scale_fill_manual(values = c("CA Resident" = "#9cbebe", "Non-CA Resident" = "#003660", "International" = "#dcd6cc", "Unknown" = "gray40")) +
      facet_wrap(~program, ncol = 1) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) 
    
    # convert to plotly----
    plotly::ggplotly(residency_all, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
  
}




