sex_plot <- function(input) {
  
  # wrangle data & create reactive df for sex plot ----
  sex_program_time <- enrolled %>% 
    select(c("ay_year", "application_id", "gender", "objective1")) %>% 
    group_by(ay_year, objective1, gender) %>% 
    summarize(count = n())
  
  sex_stats_time <- left_join(sex_program_time, program_size,
                              by = c("ay_year", "objective1")) %>% 
    mutate(percent = round((count / size) * 100)) %>% 
    mutate(gender = factor(gender, levels = c("F", "M", "U"),
                           labels = c("Female", "Male", "Undeclared")))
  
  # render plotly ----
  renderPlotly({
    
    # create ggplot (group bar chart over time) 
    sex_all <- ggplot(data = sex_stats_time,
                      aes(x = ay_year, y = percent, fill = reorder(gender, percent),
                          text = paste0(gender, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(sex_stats_time$ay_year),
                                      max(sex_stats_time$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Sex diversity and distribution trends by degree program",
           x = NULL, y = NULL, fill = NULL) +
      scale_fill_manual(values = c("Female" = "#9cbebe", "Male" = "#003660", "Undeclared" = "#dcd6cc")) +
      facet_wrap(~objective1, ncol = 1)
    
    # convert to plotly
    plotly::ggplotly(sex_all, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}


