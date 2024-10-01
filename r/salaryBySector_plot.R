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

    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salarySector_year
      placement_size <- meds_placement_size

    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      data %>% 
        select(class_year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) 
      
      } # END if 'All Years' is selected
    
    #.................if any single year is selected.................
    else {
      
      data %>% 
        select(class_year, employment_type, employer_sector, compensation_frequency,
               estimated_annual_compensation_us) |>
        filter(employment_type == "Full-Time Job") |>
        filter(estimated_annual_compensation_us != 0) |>
        filter(compensation_frequency != "Stipend") |>
        group_by(class_year, employer_sector) |> 
        mutate(Median = median(estimated_annual_compensation_us)) |>
        mutate(Low = min(estimated_annual_compensation_us)) |>
        mutate(High = max(estimated_annual_compensation_us)) |>
        left_join(placement_size, by = "class_year") |> 
        mutate(class_year = as.factor(class_year)) |> 
        filter(class_year == radioButton_yearInput)
      
    } # END if 'any single year' is selected
    
    # #...................if `All Years` is selected...................
    # #............get appropriate inputId, df, response num...........
    # if (program_acronym == "MESM") {
    #   
    #   radioButton_yearInput <- input$mesm_salarySector_year
    #   placement_size <- mesm_placement_size
    #   response_num <- sum(mesm_placement_size$responses)
    #   
    # } else if (program_acronym == "MEDS") {
    #   
    #   radioButton_yearInput <- input$meds_salarySector_year
    #   placement_size <- meds_placement_size
    #   response_num <- sum(meds_placement_size$responses)
    #   
    # }
    # 
    # if (radioButton_yearInput == "All Years") {
    #   data |> 
    #     select(class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us) |> 
    #      mutate(sector_type = case_when(
    #        employer_sector %in% c("Consulting", 
    #                               "Corporate") ~ "Private",
    #        employer_sector %in% c("Federal Government", 
    #                               "Local Government", 
    #                               "State Government", 
    #                               "Research/Education") ~ "Public",
    #        employer_sector %in% c("Foreign Government", 
    #                               "Other") ~ "Other",
    #        TRUE ~ employer_sector
    #      )) |> 
    #     mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) |>
    #     filter(employment_type == "Full-Time Job") |>
    #     filter(estimated_annual_compensation_us != 0) |>
    #     filter(compensation_frequency != "Stipend") |>
    #     group_by(sector_type) %>% 
    #     summarize(Median = median(estimated_annual_compensation_us),
    #               Low = min(estimated_annual_compensation_us),
    #               High = max(estimated_annual_compensation_us)) |>
    #     pivot_longer(cols = c("Median", "Low", "High"),
    #                  names_to = "range", values_to = "values") |>
    #     mutate(responses = response_num)
    #   
    # }  # END if `All Years` statement
    # 
    # #.................if any single year is selected.................
    # else {
      # data %>% 
      #   select(class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us) |> 
      #   mutate(sector_type = case_when(
      #     employer_sector %in% c("Consulting", 
      #                            "Corporate") ~ "Private",
      #     employer_sector %in% c("Federal Government", 
      #                            "Local Government", 
      #                            "State Government", 
      #                            "Research/Education") ~ "Public",
      #     employer_sector %in% c("Foreign Government", 
      #                            "Other") ~ "Other",
      #     TRUE ~ employer_sector
      #   )) |>  
      #   mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) |> 
      #   filter(employment_type == "Full-Time Job") |> 
      #   filter(estimated_annual_compensation_us != 0) |> 
      #   filter(compensation_frequency != "Stipend") |>
      #   filter(class_year == radioButton_yearInput) |>
      #   group_by(class_year, sector_type) %>% 
      #   summarize(Median = median(estimated_annual_compensation_us),
      #             Low = min(estimated_annual_compensation_us),
      #             High = max(estimated_annual_compensation_us)) |>
      #   pivot_longer(cols = c("Median", "Low", "High"),
      #                names_to = "range", values_to = "values") |>
      #   left_join(placement_size, by = "class_year")
      #
    # } # END `any single year` else statement
    
  }) # END reactive df
  
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
      yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_salarySector_year
      selected_class_year <- radioButton_yearInput
      placement_size <- meds_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      #...................create ggplot object first...................
      comp_sector_gg <- ggplot(data = salary_sector()) +
        geom_segment(aes(x = Low, xend = High,
                         y = fct_reorder(employer_sector, Median), yend = employer_sector), color = "black") +
        geom_point(aes(x = Low, 
                       y = employer_sector,
                       text = paste0("Low: $", round(Low, 2))), 
                   fill = "#9CBEBD", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = Median, 
                       y = employer_sector,
                       text = paste0("Median: $", round(Median, 2))), 
                   fill = "#047c91", color = "black", shape = 24, size = 7) + 
        geom_point(aes(x = High, 
                       y = employer_sector,
                       text = paste0("High: $", round(High, 2))), 
                   fill = "#003660", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = estimated_annual_compensation_us, y = employer_sector),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_x_continuous(labels = scales::dollar_format()) + 
        labs(title = paste0(program_acronym ," Initial Job Placement Salaries (Low, Median, High) by Sector", "\n",
                            "(", allYrs_response, "/", allYrs_size, " survey respondents)"), 
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal() 
      
      } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      #...................create ggplot object first...................
      comp_sector_gg <- ggplot(data = salary_sector()) +
        geom_segment(aes(x = Low, xend = High,
                         y = fct_reorder(employer_sector, Median), yend = employer_sector), color = "black") +
        geom_point(aes(x = Low, 
                       y = employer_sector,
                       text = paste0("Low: $", round(Low, 2))), 
                   fill = "#9CBEBD", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = Median, 
                       y = employer_sector,
                       text = paste0("Median: $", round(Median, 2))), 
                   fill = "#047c91", color = "black", shape = 24, size = 7) + 
        geom_point(aes(x = High, 
                       y = employer_sector,
                       text = paste0("High: $", round(High, 2))), 
                   fill = "#003660", color = "black", shape = 21, size = 6) +
        geom_point(aes(x = estimated_annual_compensation_us, y = employer_sector),
                   color = "gray50", alpha = 0.8, size = 1.5) +
        scale_x_continuous(labels = scales::dollar_format()) + 
        labs(title = paste0(program_acronym ," Initial Job Placement Salaries (Low, Median, High) by Sector", "\n",
                            "(", yr_response, "/", yr_size, " survey respondents)"), 
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal() 
      
    } # END if `any single year` is selected
    
    # comp_sector_gg <- ggplot(data = salary_sector()) +
      # geom_segment(aes(x = Low, xend = High,
      #                  y = fct_reorder(employer_sector, Median), yend = employer_sector), 
      #              color = "gray40") +
      # geom_point(aes(x = Low, 
      #                y = employer_sector),
      #                #text = paste0("$", round(Low, 2), "\n", "Number of respondents: ", responses)), 
      #            fill = "#9CBEBD", shape = 21, size = 6) +
      # geom_point(aes(x = Median, 
      #                y = employer_sector),
      #                #text = paste0("$", round(Low, 2), "\n", "Number of respondents: ", responses)), 
      #            fill = "#047c91", shape = 24, size = 5) + 
      # geom_point(aes(x = High, 
      #                y = employer_sector),
      #                #text = paste0("$", round(Low, 2), "\n", "Number of respondents: ", responses)), 
      #            fill = "#003660", shape = 21, size = 6) +
      # geom_point(aes(x = estimated_annual_compensation_us, y = employer_sector),
      #            pch = "|", cex = 3) +
      # scale_x_continuous(labels = scales::dollar_format()) + 
      # labs(title = paste0(program_acronym ," Initial Job Placement Salaries (Low, Median, High)"), 
      #      x = NULL, y = NULL, fill = NULL) +
      # theme_minimal() 
    
    # comp_sector_gg <- ggplot(data = salary_sector(), 
    #                       aes(x = fct_relevel(sector_type, c("Other", "Non-Profit", "Public", "Private")), 
    #                                           y = values,
    #                                           fill = reorder(range, values),
    #                                           text = paste0(sector_type, 
    #                                                         "\n", range, ": ", "$", 
    #                                                         values, "\n", 
    #                                                         "Number of respondents: ", 
    #                                                         response_num))) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   coord_flip() +
    #   scale_y_continuous(labels = scales::dollar_format(), 
    #                      breaks = seq(from = 0, to = max(salary_sector()$values), by = 50000)) +
    #   scale_x_discrete(
    #     labels = function(x)
    #       str_wrap(x, width = 25)) +
    #   scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc")) + 
    #   labs(title = paste0(program_acronym, " Alumni Salary Compensation by Sector"),
    #        x = NULL, y = NULL, fill = NULL) +
    #   theme_minimal() 
    
    #..................then convert to plotly object.................
    plotly::ggplotly(comp_sector_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 16))) |>
      config(displayModeBar = FALSE)
      # config(modeBarButtonsToRemove = list("pan",
      #                                      "select",
      #                                      "lasso2d",
      #                                      "autoScale2d",
      #                                      "hoverClosestCartesian",
      #                                      "hoverCompareCartesian")) # END ggplotly

  }) # END renderPlotly 
  
} # END fxn

