
# may abstract this more so it can be used for both MESM & MEDS #

salary_plot <- function(input) {
  
  # wrangle data/create reactive df for salary plot ----
  salary <- reactive({
    
    if (input$compensation_year == "All Years") {
      
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>%
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>% 
        # 3 year Median
        mutate(Median = median(estimated_annual_compensation_us)) %>% 
        mutate(Low = min(estimated_annual_compensation_us)) %>% 
        mutate(High = max(estimated_annual_compensation_us)) %>% 
        select(-estimated_annual_compensation_us) %>% 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") %>% 
        mutate(mesm_responses = 196)
      
    } # END if statement
    
    else {
      
      # chose 2019, 2020, 2021
      mesm_placement %>% 
        select(mesm_class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>% 
        # filter for year 
        filter(mesm_class_year %in% input$compensation_year) %>% 
        # 3 year Median
        mutate(Median = median(estimated_annual_compensation_us)) %>% 
        mutate(Low = min(estimated_annual_compensation_us)) %>% 
        mutate(High = max(estimated_annual_compensation_us)) %>% 
        select(-estimated_annual_compensation_us) %>% 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") %>% 
        left_join(placement_size, by = "mesm_class_year")
      
    } # END else statement
    
  }) 
  
  # render plotly ----
  renderPlotly({
    
    # create ggplot
    salary_gg <- ggplot(data = salary(), aes(x = reorder(range, values), y = values, fill = range,
                            text = paste0(range, ": ", "$", round(values, 2), "\n", "Number of respondents: ", mesm_responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0, 100000, 25000)) +
      scale_fill_manual(
        values = c("Low" = "#dcd6cc", "Median" = "#047c91", "High" = "#003660")) +
      labs(title = paste0("MESM Alumni Low, Median, and High Salary Compensation", "\n", "(", input$compensation_year, ")"), 
           x = NULL, y = "Dollars ($)", ill = NULL)
    
    # conver to plotly
    plotly::ggplotly(salary_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}