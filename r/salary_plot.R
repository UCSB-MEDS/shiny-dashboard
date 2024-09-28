#' Creates the "Salary" plot, which shows the lowest, median, and highest salary for FULL TIME jobs (DOES NOT INCLUDE data for Internship, Part-Time Job, Self-Employed/Freelance e.g. Eco-E) (Career tab)
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' 
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return a renderPlotly output
#' @export
#'
#' @examples
salary_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................wrangle reactive df of salaries.................
  salary <- reactive({
    
    #.........get appropriate input values & place/resp nums.........
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salary_year
      placement_size <- mesm_placement_size
      response_num <- sum(placement_size$responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salary_year
      placement_size <- meds_placement_size
      response_num <- sum(placement_size$responses)
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      data %>% 
        select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) |> 
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        select(-estimated_annual_compensation_us) |>
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |>
        mutate(responses = response_num)
      
    } # END if `All Years` statement
    
    #.................if any single year is selected.................
    else {

      data %>% 
        select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        filter(class_year %in% radioButton_yearInput) |>
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        select(-estimated_annual_compensation_us) |>
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |>
        left_join(placement_size, by = "class_year")
      
    } # END single year else statement
    
  }) # END reactive df
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #...................render salary plotly oject...................
  renderPlotly({
    
    # #..........get appropriate input values & response nums..........
    # if (program_acronym == "MESM") {
    #   
    #   radioButton_yearInput <- input$mesm_salary_year
    #   response_num <- sum(mesm_placement_size$responses)
    #   
    # } else if (program_acronym == "MEDS") {
    #   
    #   radioButton_yearInput <- input$meds_salary_year
    #   response_num <- sum(meds_placement_size$responses)
    #   
    # }
    
    #...................create ggplot object first...................
    salary_gg <- ggplot(data = salary(), 
                        aes(x = reorder(range, values), 
                            y = values, 
                            fill = range,
                            text = paste0(range, 
                                          ": ", "$", 
                                          round(values, 2), 
                                          "\n", 
                                          "Number of respondents: ", 
                                          responses))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(
        values = c("Low" = "#dcd6cc", "Median" = "#047c91", "High" = "#003660")) +
      labs(title = paste0(program_acronym ," Alumni Low, Median, and High Salary Compensation"), #
           x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    #..................then convert to plotly object.................
    plotly::ggplotly(salary_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16))) |> 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian")
             ) # END ggplotly
    
  }) # END renderPlotly
  
} # END fxn
