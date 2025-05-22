#' Creates the "Underrepresented Minority Trends" plot which visualizes the percentage of URMs across years, by program
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
urmTrends_plot <- function(input) {
  
  #......................define URM variables......................
  category_urms <- c("African American / Black", "American Indian / Alaska Native")
  visa_urms <- "Permanent Resident"
  
  #................wrangle data & create reactive df...............
  urm_trends_df <- reactive({
    
    if (input$urm_trends_input == "All Programs") {
      
      enrolled |> 
        select(admission_year, application_id, program, citizenship_country,
               residency_country, birth_country, visa, background,
               racial_categories, hispanic_latino, urm_status) |> 
        group_by(admission_year, urm_status) |> 
        summarize(count = n()) |> 
        filter(urm_status == "Y") |> 
        left_join(total_students_yr, by = "admission_year") |> 
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
     enrolled |> 
        select(admission_year, application_id, program, citizenship_country,
               residency_country, birth_country, visa, background,
               racial_categories, hispanic_latino, urm_status) |> 
        group_by(admission_year, urm_status, program) |> 
        summarize(count = n()) |> 
        filter(urm_status == "Y") |> 
        left_join(program_size, by = c("admission_year", "program")) |> 
        mutate(percent = round((count / size) * 100, 1)) |> 
        filter(program == input$urm_trends_input)

    } # END else statement
    
  }) # END reactive
  
  #..........................render plotly.........................
  plotly::renderPlotly({
    
    # create empty var to hold color ----
    color <- NULL 
    
    # set plot colors based on prog_input ----
    if (input$urm_trends_input == "All Programs") {
      color <- all_programs_color
    } 
    
    else if (input$urm_trends_input == "MESM") {
      color <- mesm_color
    } 
    
    else if (input$urm_trends_input == "MEDS") {
      color <- meds_color
    }
    
    else if (input$urm_trends_input == "PhD") {
      color <- phd_color
    } 
    
    # build ggplot ----
    urm_trends_gg <- ggplot(data = urm_trends_df(),
                            aes(x = admission_year, y = percent,
                                text = paste0("URM ", "(", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color, width = 0.9) +
      coord_cartesian(xlim = c(2020, 2024), expand = TRUE) + # using this FOR NOW since PhD plot doesn't show 2022 data (no URMs in 2022 and since it's the last year of available data, ggplot excludes it when using the scale_x_continous() code below where max and min x values are set based on data)
      # scale_x_continuous(breaks = seq(max(urm_trends_df()$app_submission_year),
      #                                 min(urm_trends_df()$app_submission_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0("Underrepresented Minority Trends", " (", input$urm_trends_input, ")"),
           y = NULL, x = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) 
    
    # convert to plotly ----
    plotly::ggplotly(urm_trends_gg, tooltip = "text") |> 
      config(displayModeBar = FALSE)
    
  })
  
}


