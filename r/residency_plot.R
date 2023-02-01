residency_plot <- function(input) {
  
  # wrangle data & create reactive df for residency plot ----
  residency_stats <- enrolled %>% 
    select(c("ay_year", "application_id", "objective1", "citizenship_country",
             "residency_country", "birth_country", "california_resident", "ca_high_school", "visa")) %>% 
    mutate(california_resident = unlist(california_resident)) %>% 
    # residency status
    mutate(residency = case_when(
      # ca resident
      california_resident == TRUE & visa %in% c(NA, "DACA/AB540", "None: DACA Recipient", "Permanent Resident", "Undocumented Status") ~ "ca resident",
      # non ca resident
      california_resident == FALSE & visa %in% c(NA, "Permanent Resident", "Undocumented Status") ~ "non resident",
      # international
      visa %in% c("F-1 Student", "J-1", "Family of H,H1,H2,H3") ~ "international"
    )) %>% 
    group_by(ay_year, objective1, residency) %>% 
    summarize(count = n()) %>%
    left_join(program_size, by = c("ay_year", "objective1")) %>% 
    mutate(percent = round((count / size) * 100)) %>% 
    mutate(residency = factor(residency, levels = c("ca resident", "non resident", "international"),
                              labels = c("CA Resident", "Nonresident", "International"))) 
  
  # render plotly ----
  renderPlotly({
    
    # create ggplot
    residency_all <- ggplot(data = residency_stats,
                            aes(x = ay_year, y = percent, fill = reorder(residency, percent),
                                text = paste0(residency, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(residency_stats$ay_year),
                                      max(residency_stats$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Residency distribution trends by degree program",
           x = NULL, y = NULL, fill = NULL) +
      scale_fill_manual(values = c("CA Resident" = "#9cbebe", "Nonresident" = "#003660", "International" = "#dcd6cc"),) +
      facet_wrap(~objective1, ncol = 1)
    
    # convert to plotly
    plotly::ggplotly(residency_all, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })
  
}




