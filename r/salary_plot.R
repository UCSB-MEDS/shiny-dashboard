#' salary_plot
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly output
#' @export
#'
#' @examples
salary_plot <- function(input, data, program_acronym) {
  
  # wrangle data/create reactive df for salary plot ----
  salary <- reactive({
    
    # determine which inputId, *_placement_size df, and response_num to use based on program_acronym supplied ----
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salary_year
      placement_size <- mesm_placement_size
      response_num <- sum(placement_size$responses)
      
      ## SC NOTE 2023-02-04: eventually will use this fxn for MEDS, but currently only 1 year data, so will make static plot using ____
    } else if (program_acronym == "MEDS") {
      
      # radioButton_yearInput <- input$meds_salary_year
      # placement_size <- meds_placement_size
      # response_num <- sum(placement_size$responses)
      
    }
    
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
        # 3 year Median
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
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>% 
        # filter for year 
        filter(class_year %in% radioButton_yearInput) %>% 
        # 3 year Median
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
    
    # determine which inputId and response_num to use based on program_acronym supplied ----
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salary_year
      response_num <- sum(mesm_placement_size$responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salary_year
      response_num <- sum(meds_placement_size$responses)
      
    }
    
    # create ggplot
    salary_gg <- ggplot(data = salary(), aes(x = reorder(range, values), y = values, fill = range,
                            text = paste0(range, ": ", "$", round(values, 2), "\n", "Number of respondents: ", responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(
        values = c("Low" = "#dcd6cc", "Median" = "#047c91", "High" = "#003660")) +
      labs(title = paste0(program_acronym ," Alumni Low, Median, and High Salary Compensation", "\n", "(", radioButton_yearInput, ")"), 
           x = NULL, y = "Dollars ($)", ill = NULL)
    
    # conver to plotly
    plotly::ggplotly(salary_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}
