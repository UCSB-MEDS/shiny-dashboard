#' salarySpecialization_plot
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
  ##                               data wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # get mesm placement size df from global.R ----
  placement_size <- mesm_placement_size
  
  # calculate reponse # for All Years ----
  response_num <- sum(placement_size$responses)
  
  # reactive df ----
  salary_special <- reactive({
    
    # if "All Years" chosen
    if (input$mesm_salary_by_specialization_year == "All Years") {
        mesm_placement %>% 
        select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        filter(employment_type == "Full-Time Job", 
               estimated_annual_compensation_us != 0, # remove $0 compensation (5 tot)
               compensation_frequency != "Stipend", # remove stipend compensation_frequency
               !is.na(mesm_program_enrollment_specializations)) %>% #remove NA specializations
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low")),
               responses = response_num) |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        ))
      
    } # END if statement
    
    # if an individual year is chosen (e.g. 2019, 2020, or 2021)
    else {
      data %>% 
        select(class_year, mesm_program_enrollment_specializations, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        filter(employment_type == "Full-Time Job", 
               estimated_annual_compensation_us != 0, # remove $0 compensation (5 tot)
               compensation_frequency != "Stipend", # remove stipend compensation_frequency
               class_year == input$mesm_salary_by_specialization_year, #filter for year
               !is.na(mesm_program_enrollment_specializations)) %>% #remove NA specializations 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(class_year, mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) %>%
        left_join(placement_size, by = "class_year") |> 
        mutate(mesm_program_enrollment_specializations = case_when(
          mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
          mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
          mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
          TRUE ~ mesm_program_enrollment_specializations
        ))
      
    } # END else statement
    
  }) 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                render plotly                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  plotly::renderPlotly({
    
    # create ggplot
    salary_special_gg <- ggplot(data = salary_special(), 
                                aes(x = mesm_program_enrollment_specializations, 
                                                             y = values,fill = reorder(range, values),
                                                             text = paste0(mesm_program_enrollment_specializations, 
                                                                           "\n", range, ": ", "$", 
                                                                           values, "\n", "Number of respondents: ", 
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
      scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc" )) + # ucsb navy, ucsb aqua, ucsb clay
      labs(title = paste0("Salary Compensation by MESM Specialization"),
           x = NULL, y = NULL, fill = NULL)
    
    # conver to plotly
    plotly::ggplotly(salary_special_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}
