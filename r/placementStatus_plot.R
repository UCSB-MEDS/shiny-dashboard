#' Creates the "Placement Status" plot, which indicates what type of employment students have secured 6 months post-grad (Career tab)
#'
#' @param input input
#' @param data  df; either 'mesm_status' or 'meds_status'
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return a renderPlotly object
#' @export
#'
#' @examples
placementStatus_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................wrangle reactive df of salaries.................
  salary <- reactive({
    
    #...............get appropriate `*_status_size` df...............
    if (program_acronym == "MESM") {
      
      status_size <- mesm_status_size
      radioButton_yearInput <- input$mesm_placementStatus_year
      
    } else if (program_acronym == "MEDS") {
      
      status_size <- meds_status_size
      radioButton_yearInput <- input$meds_placementStatus_year
      
    }
    
    #..................list of items to filter out...................
    filter_out <- c("PT Eco-E", "TA-ship", "Time Off", "Time-Off")
    
    
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
        select(class_year, member_status) |>
        filter(!member_status %in% filter_out) |>
        mutate(status = case_when(
          member_status %in% c("FT Career",
                               "FT Temporary Career",
                               "PT Temporary Career",
                               "FT Career-Sponsored",
                               "PT Career",
                               "FT Career-Sponsored") ~ "Career",
          member_status %in% c("FT New Business",
                               "FT Eco-E") ~ "Eco-Entrepreneurship/New Business",
          member_status %in% c("Internship/Fellowship",
                               "Continuing Internship",
                               "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
          TRUE ~ member_status
        )) |>
        group_by(status) |>
        summarize(count = n()) |>
        mutate(responses = rep(total_responses),
               program_size = rep(total_program_size)) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `All Years` is selected 
    
    #.................if any single year is selected.................
    else {
      
      data |>
        select(class_year, member_status) |>
        filter(class_year == radioButton_yearInput) |> 
        filter(!member_status %in% filter_out) |>
        mutate(status = case_when(
          member_status %in% c("FT Career",
                               "FT Temporary Career",
                               "PT Temporary Career",
                               "FT Career-Sponsored",
                               "PT Career",
                               "FT Career-Sponsored") ~ "Career",
          member_status %in% c("FT New Business",
                               "FT Eco-E") ~ "Eco-Entrepreneurship/New Business",
          member_status %in% c("Internship/Fellowship",
                               "Continuing Internship",
                               "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
          TRUE ~ member_status
        )) |>
        group_by(class_year, status) |>
        summarize(count = n()) |>
        left_join(status_size, by = "class_year") |>
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `any single year` is selected
    
  }) # END reactive df
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............render placement status plotly object.............
  plotly::renderPlotly({
    
    status_gg <- ggplot(data = salary(),
           aes(x = percent,
               y = fct_relevel(status, c("Searching",
                                         "Advanced Degree/Another Degree",
                                         "Internship, Fellowship, or Short-term Project",
                                         "Career")),
               text = paste0(status,
                             " (", percent, "%", ")",
                             "\n",
                             "Number of respondents: ",
                             responses))) +
      geom_col(fill = "#003660") +
      scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                         limits = c(0, 100),
                         labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_y_discrete(labels = scales::label_wrap(20)) +
      labs(title = paste0(program_acronym, " Alumni Placement Status 6 months after graduation"),
           x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    
    #..................then convert to plotly object.................
    plotly::ggplotly(status_gg, tooltip = "text") |> 
      layout(legend = list(orientation = "h", y = -0.1),
             title = list(font = list(size = 16))) |> 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian")
      ) # END ggplotly
    
  }) # END renderPlotly
  
} # END fxn
