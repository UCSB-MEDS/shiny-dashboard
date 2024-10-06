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
  
  # #................wrangle reactive df of salaries.................
  # salary <- reactive({
  
  #.........get appropriate input values & place/resp nums.........
  if (program_acronym == "MESM") {
    
    #radioButton_yearInput <- input$mesm_salary_year
    placement_size <- mesm_placement_size
    response_num <- sum(placement_size$responses)
    
  } else if (program_acronym == "MEDS") {
    
    # radioButton_yearInput <- input$meds_salary_year
    placement_size <- meds_placement_size
    response_num <- sum(placement_size$responses)
    
  }
  
  #..........................wrangle data..........................
  salary <- data |> 
    select(class_year, employment_type, employer_sector, compensation_frequency,
           estimated_annual_compensation_us) |>
    filter(employment_type == "Full-Time Job") |>
    filter(estimated_annual_compensation_us != 0) |>
    filter(compensation_frequency != "Stipend") |>
    group_by(class_year) |> 
    mutate(Median = median(estimated_annual_compensation_us)) |>
    mutate(Low = min(estimated_annual_compensation_us)) |>
    mutate(High = max(estimated_annual_compensation_us)) |>
    left_join(placement_size, by = "class_year") |> 
    pivot_longer(cols = c(Low, High, Median),
                 names_to = "range", values_to = "values") |> 
    mutate(class_year = as.factor(class_year)) |> 
    mutate(range = fct_relevel(range, c("Low", "Median", "High")))
  
  salary_high_low <- salary |> 
    group_by(class_year) |> 
    summarize(min_val = min(values), max_val = max(values))
  
  #   #...................if `All Years` is selected...................
  #   if (radioButton_yearInput == "All Years") { 
  #     
  #     data %>% 
  #       select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) |> 
  #       filter(employment_type == "Full-Time Job") |>
  #       filter(estimated_annual_compensation_us != 0) |>
  #       filter(compensation_frequency != "Stipend") |>
  #       mutate(Median = median(estimated_annual_compensation_us)) |>
  #       mutate(Low = min(estimated_annual_compensation_us)) |>
  #       mutate(High = max(estimated_annual_compensation_us)) |>
  #       select(-estimated_annual_compensation_us) # |>
  #       # pivot_longer(cols = c(Low, High, Median),
  #       #              names_to = "range", values_to = "values") |>
  #       # mutate(responses = response_num) 
  #     
  #   } # END if `All Years` statement
  #   
  #   #.................if any single year is selected.................
  #   else {
  # 
  #     data %>% 
  #       select(class_year, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
  #       filter(employment_type == "Full-Time Job") |>
  #       filter(estimated_annual_compensation_us != 0) |>
  #       filter(compensation_frequency != "Stipend") |>
  #       filter(class_year %in% radioButton_yearInput) |>
  #       mutate(Median = median(estimated_annual_compensation_us)) |>
  #       mutate(Low = min(estimated_annual_compensation_us)) |>
  #       mutate(High = max(estimated_annual_compensation_us)) |>
  #       select(-estimated_annual_compensation_us) |>
  #       # pivot_longer(cols = c(Low, High, Median),
  #       #              names_to = "range", values_to = "values") |>
  #       left_join(placement_size, by = "class_year")
  #     
  #   } # END single year else statement
  #   
  # }) # END reactive df
  
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
    # # formatted ggtext not supported by plotly :(
    # formatted_title <- glue::glue("MEDS Alumni
    #                    <span style='color:#9CBEBD'>**Low**</span>,
    #                     <span style='color:#047c91'>**Median**</span>,
    #                 and <span style='color:#003660'>**High**</span>
    #                    Salaries")

    salary_gg <- ggplot() +
      geom_segment(data = salary_high_low, 
                   aes(x = min_val, xend = max_val,
                       y = class_year, yend = class_year), color = "black") +
      geom_point(data = salary, aes(x = values, y = class_year, 
                     fill = range, shape = range, size = range,
                     text = paste0(range, ": $",  round(values, 2), "\n", 
                                   responses, "/", program_size, " survey respondents"))) +
      geom_point(data = salary, aes(x = estimated_annual_compensation_us, y = class_year),
                 color = "gray50", alpha = 0.8, size = 1.5) +
      scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
      scale_shape_manual(values = c(21, 24, 21)) + 
      scale_size_manual(values = c(6, 6, 6)) +
      scale_x_continuous(labels = scales::dollar_format()) + 
      labs(title = paste0(program_acronym ," Initial Job Placement Salaries"), 
           x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
    ## OLD VERSION
    # salary_gg <- ggplot(data = salary) +
    #   geom_segment(aes(x = Low, xend = High,
    #                    y = class_year, yend = class_year), color = "black") +
    #   geom_point(aes(x = Low, 
    #                  y = class_year,
    #                  text = paste0("Low: $",  round(Low, 2), "\n", responses, "/", program_size, " survey respondents")),
    #              fill = "#9CBEBD", color = "black", shape = 21, size = 6) +
    #   geom_point(aes(x = Median, 
    #                  y = class_year,
    #                  text = paste0("Median: $",  round(Median, 2), "\n", responses, "/", program_size, " survey respondents")), 
    #              fill = "#047c91", color = "black", shape = 24, size = 7) + # pch = "|", cex = 5
    #   geom_point(aes(x = High, 
    #                  y = class_year,
    #                  text = paste0("High: $", round(High, 2), "\n", responses, "/", program_size, " survey respondents")), 
    #              fill = "#003660", color = "black", shape = 21, size = 7) +
    #   geom_point(aes(x = estimated_annual_compensation_us, y = class_year),
    #              color = "gray50", alpha = 0.8, size = 1.5) +
    #   scale_x_continuous(labels = scales::dollar_format()) + 
    #   labs(title = paste0(program_acronym ," Initial Job Placement Salaries (Low, Median, High)"), 
    #        x = NULL, y = NULL, fill = NULL) +
    #   theme_minimal() 
    
    # salary_gg <- ggplot(data = salary(), 
    #                     aes(x = reorder(range, values), 
    #                         y = values, 
    #                         fill = range,
    #                         text = paste0(range, 
    #                                       ": ", "$", 
    #                                       round(values, 2), 
    #                                       "\n", 
    #                                       "Number of respondents: ", 
    #                                       responses))) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   scale_y_continuous(labels = scales::dollar_format()) +
    #   scale_fill_manual(
    #     values = c("Low" = "#dcd6cc", "Median" = "#047c91", "High" = "#003660")) +
    #   labs(title = paste0(program_acronym ," Alumni Low, Median, and High Salary Compensation"), #
    #        x = NULL, y = NULL, fill = NULL) +
    #   theme_minimal() +
    #   theme(legend.position = "none")
    
    #..................then convert to plotly object.................
    plotly::ggplotly(salary_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", x = 0.3, y = -0.2)) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly
  
} # END fxn
