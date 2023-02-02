
# may abstract this more so it can be used for both MESM & MEDS #

salarySpecialization_plot <- function(input) {
  
  # data wrangling/create reactive df for salary specialization plot ----
  salary_special <- reactive({
    
    if (input$compSpecialization_year == "All Years") {
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) %>% 
        mutate(mesm_responses = 196)
      
    } # END if statement
    
    else {
      # chose 2019, 2020, 2021
      mesm_placement %>% 
        select(mesm_class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        # filter for year
        filter(mesm_class_year == input$compSpecialization_year) %>% 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(mesm_class_year, mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) %>%
        left_join(placement_size, by = "mesm_class_year")
    } # END else statement
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot
    salary_special_gg <- ggplot(data = salary_special(), aes(x = mesm_program_enrollment_specializations, y = values,fill = reorder(range, values),
                                                             text = paste0(mesm_program_enrollment_specializations, "\n", range, ": ", "$", values, "\n", "Number of respondents: ", mesm_responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0, 100000, 25000)) +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 25)
      ) +
      scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc" )) + # ucsb navy, ucsb aqua, ucsb clay
      labs(title = paste0("Salary Compensation by MESM Specialization"),
           x = NULL, y = "Dollars ($)", fill = NULL)
    
    # conver to plotly
    plotly::ggplotly(salary_special_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
  
}
  