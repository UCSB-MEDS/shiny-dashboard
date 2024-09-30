#' Creates plot that compares the percentage of international, domestic (out of state), and domestic (CA) alumni initial job locatations (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
geographicComparison_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..........wrangle reactive df of geographic placements..........
  placement_location <- reactive({
    
    #......get appropriate `*_placement_size` df and input value.....
    if (program_acronym == "MESM") {
      
      placement_size <- mesm_placement_size
      radioButton_yearInput <- input$mesm_placementLocation_year
      
    } else if (program_acronym == "MEDS") {
      
      placement_size <- meds_placement_size
      radioButton_yearInput <- input$meds_placementLocation_year
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      # calculate total responses and program size across yrs ----
      location_allYrs <- placement_size |> 
        summarize(
          total_responses = sum(responses),
          total_program_size = sum(program_size)
        )
      
      # pull values ---
      total_responses <- location_allYrs |> pull(total_responses)
      total_program_size <- location_allYrs |> pull(total_program_size)
      
      data |> 
        select(c(
          employer_account_name,
          work_location_city,
          class_year,
          work_location_state,
          work_location_country,
          location
        )) |> 
        group_by(location) |> 
        summarize(location_count = n()) |> 
        filter(!is.na(location)) |> 
        mutate(responses = rep(total_responses),
               program_size = rep(total_program_size)) |> 
        mutate(percent = round((location_count / responses) * 100, 1))
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      test <- data |> 
        select(c(
          employer_account_name,
          work_location_city,
          class_year,
          work_location_state,
          work_location_country,
          location
        )) |> 
        filter(class_year == radioButton_yearInput) |> 
        group_by(class_year, location) |> 
        summarize(location_count = n()) |> 
        filter(!is.na(location)) |> 
        left_join(placement_size, by = "class_year") |> 
        mutate(percent = round((location_count / responses) * 100, 1))
      
    } # END if `if any single year` is selected
    
  }) # END reactive
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............render placement status plotly object.............
  plotly::renderPlotly({
    
    #....................make ggplot object first....................
    location_gg <- ggplot(data = placement_location(),
                          aes(x = percent,
                              y = fct_relevel(location, c("International",
                                                          "Domestic (Out of State)",
                                                          "Domestic (California)")),
                              text = paste0(location, 
                                            " (", percent, 
                                            "%", ")", "\n", 
                                            "Number of respondents: ", 
                                            responses))) + 
      geom_col(fill = "#003660") +
      scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                         limits = c(0, 100),
                         labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_y_discrete(labels = scales::label_wrap(20)) +
      labs(title = str_wrap(paste0("Where ", program_acronym, 
                          " alumni are working 6 months after graduating"), width = 38),
           x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    
    #..................then convert to plotly object.................
    plotly::ggplotly(location_gg, tooltip = "text") |> 
      layout(legend = list(orientation = "h", y = -0.1),
             title = list(font = list(size = 15.5))) |> 
      config(displayModeBar = FALSE)
      # config(modeBarButtonsToRemove = list("pan", 
      #                                      "select", 
      #                                      "lasso2d", 
      #                                      "autoScale2d", 
      #                                      "hoverClosestCartesian", 
      #                                      "hoverCompareCartesian")) # END ggplotly
    
  }) # END renderPlotly
  
} # END fxn
