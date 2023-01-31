
# may abstract this more so it can be used for both MESM & MEDS #

salaryBySector_plot <- function(input) {
  
  # wrangle data/create reactive df for salary by sector plot ----
  salary_sector <- reactive({
    
    if (input$compSector_year == "All Years") {
      mesm_placement %>% 
        select(mesm_class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # assign private, public, and other
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>%
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        group_by(sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>%
        mutate(mesm_responses = 196)
      
    }  # END if statement
    
    else {
      
      mesm_placement %>% 
        select(mesm_class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # assign private, public, and other
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        # filter for year
        filter(mesm_class_year == input$compSector_year) %>%
        group_by(mesm_class_year, sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>%
        left_join(placement_size, by = "mesm_class_year")
      
    } # END else statement
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot
    comp_sector <- ggplot(data = salary_sector(), aes(x = sector_type, y = values,fill = reorder(range, values),
                                                      text = paste0(sector_type, "\n", range, ": ", "$", values, "\n", "Number of respondents: ", mesm_responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0, 100000, 25000)) +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 25)) +
      scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc")) + # ucsb navy, ucsb aqua, ucsb clay
      labs(title = paste0("MESM Alumni Salary Compensation by Sector"),
           x = NULL, y = "Dollars ($)", fill = NULL)
    
    # convert to plotly
    plotly::ggplotly(comp_sector, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}
