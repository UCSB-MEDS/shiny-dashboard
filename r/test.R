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
  
  #............wrangle reactive df of salary by sector.............
  salary_specialization <- reactive({
    
    radioButton_yearInput <- input$mesm_salary_by_specialization_year
    placement_size <- mesm_placement_size
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      test <- mesm_placement |> 
        select(class_year, 
               mesm_program_enrollment_specializations, 
               employment_type, 
               compensation_frequency, 
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |> 
        filter(!is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
        unnest(mesm_program_enrollment_specializations) |>
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us))
      
    } # END if 'All Years' is selected
    
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
        filter(class_year == radioButton_yearInput) |> 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
        unnest(mesm_program_enrollment_specializations) |>
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        )) |> 
        group_by(mesm_program_enrollment_specializations) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us))
      
    } # END if 'any single year' is selected
    
  }) # END reactive df
  
  
  # #...................get mesm placement size df...................
  # placement_size <- mesm_placement_size
  # 
  # #............calculate response number for All Years.............
  # 
  # response_num <- sum(placement_size$responses)
  # 
  # #..........wrangle reactive salary by specialization df..........
  # salary_special <- reactive({
  #   
  #   #...................if `All Years` is selected...................
  #   if (input$mesm_salary_by_specialization_year == "All Years") {
  #       mesm_placement |> 
  #       select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
  #       filter(employment_type == "Full-Time Job", 
  #              estimated_annual_compensation_us != 0, 
  #              compensation_frequency != "Stipend",
  #              !is.na(mesm_program_enrollment_specializations)) |>
  #       mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
  #       unnest(mesm_program_enrollment_specializations) |>
  #       group_by(mesm_program_enrollment_specializations) |>
  #       summarize(Median = median(estimated_annual_compensation_us),
  #                 Low = min(estimated_annual_compensation_us),
  #                 High = max(estimated_annual_compensation_us)) |>
  #       pivot_longer(cols = c("Median", "Low", "High"),
  #                    names_to = "range", values_to = "values") |>
  #       mutate(range = factor(range, levels = c("High", "Median", "Low")),
  #              responses = response_num) |> 
  #       mutate(mesm_program_enrollment_specializations = case_when(
  #         mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
  #         mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
  #         mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
  #         TRUE ~ mesm_program_enrollment_specializations
  #       ))
  #     
  #   } # END `All Years` if statement
  #   
  #   #.................if any single year is selected.................
  #   else {
  #     data |> 
  #       select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
  #       filter(employment_type == "Full-Time Job", 
  #              estimated_annual_compensation_us != 0, 
  #              compensation_frequency != "Stipend", 
  #              class_year == input$mesm_salary_by_specialization_year, 
  #              !is.na(mesm_program_enrollment_specializations)) |> 
  #       mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
  #       unnest(mesm_program_enrollment_specializations) |>
  #       group_by(class_year, mesm_program_enrollment_specializations) |>
  #       summarize(Median = median(estimated_annual_compensation_us),
  #                 Low = min(estimated_annual_compensation_us),
  #                 High = max(estimated_annual_compensation_us)) |>
  #       pivot_longer(cols = c("Median", "Low", "High"),
  #                    names_to = "range", values_to = "values") |>
  #       mutate(range = factor(range, levels = c("High", "Median", "Low"))) |>
  #       left_join(placement_size, by = "class_year") |> 
  #       mutate(mesm_program_enrollment_specializations = case_when(
  #         mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
  #         mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
  #         mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
  #         TRUE ~ mesm_program_enrollment_specializations
  #       ))
  #     
  #   } # END `any single year` else statement
  #   
  # }) 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                render plotly                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
      
      #...................create ggplot object first...................
      salary_specialization_gg <- ggplot(data = salary_specialization()) +
        geom_segment(aes(x = Low, xend = High,
                         y = fct_reorder(mesm_program_enrollment_specializations, Median), 
                         yend = mesm_program_enrollment_specializations), color = "black") +
        geom_point(aes(x = Low, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("Low: $", round(Low, 2))), 
                   fill = "#9CBEBD", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = Median, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("Median: $", round(Median, 2))), 
                   fill = "#047c91", color = "black", shape = 24, size = 7) + 
        geom_point(aes(x = High, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("High: $", round(High, 2))), 
                   fill = "#003660", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_x_continuous(labels = scales::dollar_format()) + 
        labs(title = paste0("MESM Salaries (Low, Median, High) by Specialization", "\n", 
                            "(", allYrs_response, "/", allYrs_size, " survey respondents)"), 
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal() 
      
      #   #...................create ggplot object first...................
      # salary_special_gg <- ggplot(data = salary_special(), 
      #                             aes(x = fct_relevel(mesm_program_enrollment_specializations, 
      #                                                 c("Water Resources Management", 
      #                                                   "Pollution Prevention and Remediation", 
      #                                                   "Environmental Policy", 
      #                                                   "Energy and Climate",
      #                                                   "Economics and Politics of the Environment",
      #                                                   "Corporate Environmental Management",
      #                                                   "Conservation Planning",
      #                                                   "Coastal Resources Management",
      #                                                   "Coastal Marine Resources Management",
      #                                                   "Business and Sustainability")), 
      #                                 #aes(x = mesm_program_enrollment_specializations, 
      #                                 y = values,fill = reorder(range, values),
      #                                 text = paste0(mesm_program_enrollment_specializations, 
      #                                               "\n", range, 
      #                                               ": ", "$", 
      #                                               values, "\n", 
      #                                               "Number of respondents: ", 
      #                                               response_num))) +
      #   geom_bar(stat = "identity", position = "dodge") +
      #   coord_flip() +
      #   theme_minimal() +
      #   scale_y_continuous(labels = scales::dollar_format(), 
      #                      breaks = seq(from = 0, to = max(salary_special()$values), 50000)) +
      #   scale_x_discrete(
      #     labels = function(x)
      #       str_wrap(x, width = 25)
      #   ) +
      #   scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc" )) +
      #   labs(title = paste0("Salary Compensation by MESM Specialization"),
      #        x = NULL, y = NULL, fill = NULL)
      #   
    } # END if `All Years` is selected
    
    else {
      
      #...................create ggplot object first...................
      salary_specialization_gg <- ggplot(data = salary_specialization()) +
        geom_segment(aes(x = Low, xend = High,
                         y = fct_reorder(mesm_program_enrollment_specializations, Median), 
                         yend = mesm_program_enrollment_specializations), color = "black") +
        geom_point(aes(x = Low, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("Low: $", round(Low, 2))), 
                   fill = "#9CBEBD", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = Median, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("Median: $", round(Median, 2))), 
                   fill = "#047c91", color = "black", shape = 24, size = 7) + 
        geom_point(aes(x = High, 
                       y = mesm_program_enrollment_specializations,
                       text = paste0("High: $", round(High, 2))), 
                   fill = "#003660", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_x_continuous(labels = scales::dollar_format()) + 
        labs(title = paste0("MESM Salaries (Low, Median, High) by Specialization", "\n", 
                            "(", yr_response, "/", yr_size, " survey respondents)"), 
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal()
      
    } # END if `any single year` is selected
    
    #....................then create plotly object...................
    plotly::ggplotly(salary_specialization_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly 
  
} # END fxn
