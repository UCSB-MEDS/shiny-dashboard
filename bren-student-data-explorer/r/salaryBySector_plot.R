#' Creates the "Salary by Sector" plot, which shows the loweset, median, and highest salary for FULL Time jobs (DOES NOT INCLUDE data for Internship, Part-Time Job, Self-Employed/Freelance e.g. Eco-E), broken out by sector (Private, Public, Non-Profit) (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' 
#' @param program_acronym chr str; either "MESM" or "MEDS"
#'
#' @return
#' @export
#'
#' @examples
salaryBySector_plot <- function(input, data, program_acronym) { 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #............wrangle reactive df of salary by sector.............
  salary_sector <- reactive({
    
    #............get appropriate inputId, df, response num...........
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salarySector_year
      placement_size <- mesm_placement_size
      status_size <- mesm_status_size
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salarySector_year
      placement_size <- meds_placement_size
      status_size <- meds_status_size
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      # calculate total responses and program size across yrs ----
      status_size_allYrs <- status_size |> 
        summarize(
          total_responses = sum(responses),
          total_program_size = sum(program_size)
        )
      
      # pull values ---
      total_responses <- status_size_allYrs |> pull(total_responses)
      total_program_size <- status_size_allYrs |> pull(total_program_size)
      
      data |> 
        select(year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High")))
        
      
    } # END if 'All Years' is selected
    
    #.................if any single year is selected.................
    else {
      
      data |> 
        select(year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(year, employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        left_join(placement_size, by = "year") |> 
        mutate(class = as.factor(year)) |> 
        filter(year == radioButton_yearInput) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(year = as.factor(year)) |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High")))
      
    } # END if 'any single year' is selected
    
  }) # END reactive salary_sector df
  
  #..........wrangle reactive df of highlighted salaries...........
  salary_range <- reactive({
    
    #............get appropriate inputId, df, response num...........
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salarySector_year
      placement_size <- mesm_placement_size
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salarySector_year
      placement_size <- meds_placement_size
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      data |> 
        select(year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(year, employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        left_join(placement_size, by = "year") |> 
        mutate(year = as.factor(year)) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(year = as.factor(year)) |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
        group_by(employer_sector) |> 
        summarize(min_val = min(values), 
                  max_val = max(values),
                  median_val = median(values))
      
    } # END if 'All Years' is selected
    
    #.................if any single year is selected.................
    else {
      
      data |> 
        select(year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(year, employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        left_join(placement_size, by = "year") |> 
        mutate(year = as.factor(year)) |> 
        filter(year == radioButton_yearInput) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(year = as.factor(year)) |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
        group_by(year, employer_sector) |> 
        summarize(min_val = min(values), 
                  max_val = max(values),
                  median_val = median(values))
      
    } # END if 'any single year' is selected
    
  }) # END reactive salary_sector df
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............render sector by salary plotly object.............
  plotly::renderPlotly({
    
    #........get values necessary for constructing plot title........
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_salarySector_year
      selected_class_year <- radioButton_yearInput
      placement_size <- mesm_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(year == selected_class_year) |> pull(responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salarySector_year
      selected_class_year <- radioButton_yearInput
      placement_size <- meds_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(year == selected_class_year) |> pull(responses)
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      #...................create ggplot object first...................
      comp_sector_gg <- ggplot() +
        geom_segment(data = salary_range(),
                     aes(x = min_val, xend = max_val,
                         y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
        geom_point(data = salary_sector(), aes(x = values, y = employer_sector, 
                                               fill = range, shape = range, size = range,
                                               text = paste0(range,  ": $",  round(values, 2)))) +
        geom_point(data = salary_sector(), 
                   aes(x = estimated_annual_compensation_us, y = employer_sector),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_fill_manual(values = c("#9CBEBD", "black", "#003660")) +
        scale_shape_manual(values = c(21, 3, 21)) + 
        scale_size_manual(values = c(6, 5, 6)) +
        scale_x_continuous(labels = scales::dollar_format()) + 
        labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
                            "(", allYrs_response, " survey respondents out of ", allYrs_size, " graduates)"), 
             x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      #...................create ggplot object first...................
      comp_sector_gg <- ggplot() +
        geom_segment(data = salary_range(),
                     aes(x = min_val, xend = max_val,
                         y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
        geom_point(data = salary_sector(), aes(x = values, y = employer_sector, 
                                               fill = range, shape = range, size = range,
                                               text = paste0(range, ": $",  round(values, 2)))) +
        geom_point(data = salary_sector(), 
                   aes(x = estimated_annual_compensation_us, y = employer_sector),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_fill_manual(values = c("#9CBEBD", "black", "#003660")) +
        scale_shape_manual(values = c(21, 3, 21)) +
        scale_size_manual(values = c(6, 5, 6)) +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
                            "(", yr_response, " survey respondents out of ", yr_size, " graduates)"),
             x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
    } # END if `any single year` is selected
    
    #..................then convert to plotly object.................
    plotly::ggplotly(comp_sector_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", x = 0.3, y = -0.2)) |>
      config(displayModeBar = FALSE)
    
  }) # END render plotly
  
} # END fxn

