#' meds_salary_plot
#'
#' @param data MUST be 'meds_placement' df
#'
#' @return
#' @export
#'
#' @examples
#' ## SC NOTE 2023-02-04: use this fxn to create static plot for MEDS salary since only 1 year of data (no need for radioButtons)
meds_salary_plot <- function(input, data) {
  
  # wrangle data/create reactive df for salary plot ----
  salary <- reactive({  
  
    radioButton_yearInput <- input$meds_salary_year
    placement_size <- meds_placement_size
    response_num <- sum(placement_size$responses)
  
  # if "All Years" selected
  if (radioButton_yearInput == "All Years") { 
    
    data %>% 
      select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
      # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
      filter(employment_type == "Full-Time Job") %>% 
      # remove $0 compensation (5 tot)
      filter(estimated_annual_compensation_us != 0) %>%
      # remove stipend compensation_frequency
      filter(compensation_frequency != "Stipend") %>% 
      # 2 year Median
      mutate(Median = median(estimated_annual_compensation_us)) %>% 
      mutate(Low = min(estimated_annual_compensation_us)) %>% 
      mutate(High = max(estimated_annual_compensation_us)) %>% 
      select(-estimated_annual_compensation_us) %>% 
      pivot_longer(cols = c(Low, High, Median),
                   names_to = "range", values_to = "values") %>% 
      mutate(responses = response_num)
    
  } # END if statement
  
  # if any single year is selected
  else {
    
  data %>% 
      select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
      # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
      filter(employment_type == "Full-Time Job") %>% 
      # remove $0 compensation (5 tot)
      filter(estimated_annual_compensation_us != 0) %>% 
      # remove stipend compensation_frequency
      filter(compensation_frequency != "Stipend") %>% 
      # filter for year 
      filter(class_year %in% radioButton_yearInput) %>% 
      # 2 year Median
      mutate(Median = median(estimated_annual_compensation_us)) %>% 
      mutate(Low = min(estimated_annual_compensation_us)) %>% 
      mutate(High = max(estimated_annual_compensation_us)) %>% 
      select(-estimated_annual_compensation_us) %>% 
      pivot_longer(cols = c(Low, High, Median),
                   names_to = "range", values_to = "values") %>% 
      left_join(placement_size, by = "class_year")
    
  } # END else statement

  })       

  # render plotly ----
  renderPlotly({
    
    # create ggplot
    salary_gg <- ggplot(data = salary(), aes(x = reorder(range, values), y = values, fill = range, 
                                           text = paste0(range, ": ", "$", round(values, 2), "\n", "Number of respondents: ", responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(
        values = c("Low" = "#dcd6cc", "Median" = "#047c91", "High" = "#003660")) +
      labs(title = "MEDS Alumni Low, Median, and High Salary Compensation", # removed year from title bc I couldn't get this working for some reason. Old code: `title = paste0("MEDS Alumni Low, Median, and High Salary Compensation", "\n", "(", radioButton_yearInput, ")")`
           x = NULL, y = "Dollars ($)", ill = NULL)
    
    # conver to plotly
    plotly::ggplotly(salary_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}