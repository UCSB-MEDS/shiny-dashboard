#' Creates the "Salary by Specialization" plot (MESM only), which shows the lowest, median, and highest salary for FULL TIME jobs (DOES NOT INCLUDE Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)), broken out by specialization (Career tab)
#'
#' @param input input
#' @param data df; 'mesm_placement' ONLY (specializations don't apply to MEDS program)
#'
#' @return
#' @export
#'
#' @examples
salarySpecialization_plot <- function(input, data) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #........wrangle reactive df of salary by specialization.........
  salary_specialization <- reactive({
    
    radioButton_yearInput <- input$mesm_salary_by_specialization_year
    placement_size <- mesm_placement_size
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      mesm_placement |> 
        select(class_year, 
               mesm_program_enrollment_specializations, 
               employment_type, 
               compensation_frequency, 
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |> 
        filter(!is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High")))
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      mesm_placement |> 
        select(class_year, 
               mesm_program_enrollment_specializations, 
               employment_type, 
               compensation_frequency, 
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |> 
        filter(!is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        filter(class_year == radioButton_yearInput) |>
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(class_year = as.factor(class_year)) |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High")))
      
    } # END if `any single year` is selected
    
  }) # END salary_specialization reactive df
  
  #..............wrangle reactive df of salary ranges..............
  salary_range <- reactive({
    
    radioButton_yearInput <- input$mesm_salary_by_specialization_year
    placement_size <- mesm_placement_size
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      mesm_placement |> 
        select(class_year, 
               mesm_program_enrollment_specializations, 
               employment_type, 
               compensation_frequency, 
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |> 
        filter(!is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
        group_by(mesm_program_enrollment_specializations) |> 
        summarize(min_val = min(values), 
                  max_val = max(values),
                  median_val = median(values))
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      mesm_placement |> 
        select(class_year, 
               mesm_program_enrollment_specializations, 
               employment_type, 
               compensation_frequency, 
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |> 
        filter(!is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        filter(class_year == radioButton_yearInput) |>
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |> 
        pivot_longer(cols = c(Low, High, Median),
                     names_to = "range", values_to = "values") |> 
        mutate(class_year = as.factor(class_year)) |> 
        mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
        group_by(class_year, mesm_program_enrollment_specializations) |> 
        summarize(min_val = min(values), 
                  max_val = max(values),
                  median_val = median(values))
      
    } # END if `any single year` is selected
    
  }) # END salary_specialization reactive df
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............render sector by salary plotly object.............
  plotly::renderPlotly({
    
    #..................define necessary variables....................
    radioButton_yearInput <- input$mesm_salary_by_specialization_year
    selected_class_year <- radioButton_yearInput
    placement_size <- mesm_placement_size
    allYrs_size <- sum(placement_size$program_size)
    allYrs_response <- sum(placement_size$responses)
    yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
    yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      comp_sector_gg <- ggplot() +
        geom_segment(data = salary_range(),
                     aes(x = min_val, xend = max_val,
                         y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
        geom_point(data = salary_specialization(), aes(x = values, y = mesm_program_enrollment_specializations, 
                                                       fill = range, shape = range, size = range,
                                                       text = paste0(range, ": $",  round(values, 2)))) +
        geom_point(data = salary_specialization(), 
                   aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_fill_manual(values = c("#9CBEBD", "black", "#003660")) +
        scale_shape_manual(values = c(21, 3, 21)) +
        scale_size_manual(values = c(6, 5, 6)) +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(title = paste0("MESM Salaries by Specialization", "\n", 
                            "(", allYrs_response, "/", allYrs_size, " survey respondents)"),
             x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
    } # END if `All Years` is selected
    
    #................if `any single year` is selected................
    else {
      
      comp_sector_gg <- ggplot() +
        geom_segment(data = salary_range(),
                     aes(x = min_val, xend = max_val,
                         y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
        geom_point(data = salary_specialization(), aes(x = values, y = mesm_program_enrollment_specializations, 
                                                       fill = range, shape = range, size = range,
                                                       text = paste0(range, ": $",  round(values, 2)))) +
        geom_point(data = salary_specialization(), 
                   aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_fill_manual(values = c("#9CBEBD", "black", "#003660")) +
        scale_shape_manual(values = c(21, 3, 21)) +
        scale_size_manual(values = c(6, 5, 6)) +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(title = paste0("MESM Salaries by Specialization", "\n", 
                            "(", yr_response, "/", yr_size, " survey respondents)"),
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
             legend = list(orientation = "h", x = 0.3, y = -0.12)) |>
      config(displayModeBar = FALSE)
    
  }) # END render plotly
  
} # END fxn
