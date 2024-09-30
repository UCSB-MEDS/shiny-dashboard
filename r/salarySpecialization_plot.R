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
  
  #...................get mesm placement size df...................
  placement_size <- mesm_placement_size
  
  #............calculate response number for All Years.............
  
  response_num <- sum(placement_size$responses)
  
  #..........wrangle reactive salary by specialization df..........
  salary_special <- reactive({
    
    #...................if `All Years` is selected...................
    if (input$mesm_salary_by_specialization_year == "All Years") {
        mesm_placement |> 
        select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job", 
               estimated_annual_compensation_us != 0, 
               compensation_frequency != "Stipend",
               !is.na(mesm_program_enrollment_specializations)) |>
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
        unnest(mesm_program_enrollment_specializations) |>
        group_by(mesm_program_enrollment_specializations) |>
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) |>
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") |>
        mutate(range = factor(range, levels = c("High", "Median", "Low")),
               responses = response_num) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        ))
      
    } # END `All Years` if statement
    
    #.................if any single year is selected.................
    else {
      data |> 
        select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job", 
               estimated_annual_compensation_us != 0, 
               compensation_frequency != "Stipend", 
               class_year == input$mesm_salary_by_specialization_year, 
               !is.na(mesm_program_enrollment_specializations)) |> 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) |>
        unnest(mesm_program_enrollment_specializations) |>
        group_by(class_year, mesm_program_enrollment_specializations) |>
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) |>
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") |>
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) |>
        left_join(placement_size, by = "class_year") |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        ))
      
    } # END `any single year` else statement
    
  }) 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                render plotly                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  plotly::renderPlotly({
    
    #...................create ggplot object first...................
    salary_special_gg <- ggplot(data = salary_special(), 
                                aes(x = fct_relevel(mesm_program_enrollment_specializations, 
                                                    c("Water Resources Management", 
                                                      "Pollution Prevention and Remediation", 
                                                      "Environmental Policy", 
                                                      "Energy and Climate",
                                                      "Economics and Politics of the Environment",
                                                      "Corporate Environmental Management",
                                                      "Conservation Planning",
                                                      "Coastal Resources Management",
                                                      "Coastal Marine Resources Management",
                                                      "Business and Sustainability")), 
                                #aes(x = mesm_program_enrollment_specializations, 
                                    y = values,fill = reorder(range, values),
                                    text = paste0(mesm_program_enrollment_specializations, 
                                                  "\n", range, 
                                                  ": ", "$", 
                                                  values, "\n", 
                                                  "Number of respondents: ", 
                                                  response_num))) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(), 
                         breaks = seq(from = 0, to = max(salary_special()$values), 50000)) +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 25)
      ) +
      scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc" )) +
      labs(title = paste0("Salary Compensation by MESM Specialization"),
           x = NULL, y = NULL, fill = NULL)
    
    #....................then create plotly object...................
    plotly::ggplotly(salary_special_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) |> 
      config(displayModeBar = FALSE)
      # config(modeBarButtonsToRemove = list("pan", 
      #                                      "select", 
      #                                      "lasso2d", 
      #                                      "autoScale2d", 
      #                                      "hoverClosestCartesian", 
      #                                      "hoverCompareCartesian")) # END ggplotly
    
  }) # END renderPlotly 
  
} # END fxn
