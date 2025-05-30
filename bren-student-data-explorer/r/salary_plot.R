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

  #.........get appropriate input values & place/resp nums.........
  if (program_acronym == "MESM") {
    
    placement_size <- mesm_placement_size
    response_num <- sum(placement_size$responses)
    
  } else if (program_acronym == "MEDS") {

    placement_size <- meds_placement_size
    response_num <- sum(placement_size$responses)
    
  }
  
  #..........................wrangle data..........................
  
  # df for creating data points ----
  salary <- data |> 
    select(year, employment_type, employer_sector, compensation_frequency,
           estimated_annual_compensation_us) |>
    filter(employment_type == "Full-Time Job") |>
    filter(estimated_annual_compensation_us != 0) |>
    filter(compensation_frequency != "Stipend") |>
    group_by(year) |> 
    mutate(Median = median(estimated_annual_compensation_us)) |>
    mutate(Low = min(estimated_annual_compensation_us)) |>
    mutate(High = max(estimated_annual_compensation_us)) |>
    left_join(placement_size, by = "year") |> 
    pivot_longer(cols = c(Low, High, Median),
                 names_to = "range", values_to = "values") |> 
    mutate(year = as.factor(year)) |> 
    mutate(range = fct_relevel(range, c("Low", "Median", "High")))
  
  # df for creating segments ----
  salary_highlights <- salary |> 
    group_by(year) |> 
    summarize(min_val = min(values), max_val = max(values))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #...................render salary plotly oject...................
  renderPlotly({

    salary_gg <- ggplot() +
      geom_segment(data = salary_highlights, 
                   aes(x = min_val, xend = max_val,
                       y = year, yend = year), color = "black") +
      geom_point(data = salary, aes(x = values, y = year, 
                     fill = range, shape = range, size = range,
                     text = paste0(range, ": $",  round(values, 2), "\n", 
                                   responses, "/", program_size, " survey respondents"))) +
      geom_point(data = salary, aes(x = estimated_annual_compensation_us, y = year),
                 color = "gray50", alpha = 0.8, size = 1.5) +
      scale_fill_manual(values = c("#9CBEBD", "black", "#003660")) +
      scale_shape_manual(values = c(21, 3, 21)) + 
      scale_size_manual(values = c(6, 5, 6)) +
      scale_x_continuous(labels = scales::dollar_format()) + 
      labs(title = paste0(program_acronym ," Initial Job Placement Salaries"), 
           x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
    #..................then convert to plotly object.................
    plotly::ggplotly(salary_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", x = 0.31, y = -0.2)) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly
  
} # END fxn
