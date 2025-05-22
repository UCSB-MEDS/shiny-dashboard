#' Title
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
ipedsTrends_plot <- function(input) {
  
  #................wrangle data & create reactive df...............
  category_ipeds_stats_time <- reactive({
    
    if (input$race_trends_input == "All Programs") {
      ipeds |> 
        group_by(admission_year, category_ipeds) |>  
        summarize(count = n()) |> 
        left_join(total_students_yr, by = "admission_year") |> 
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
      ipeds |> 
        group_by(admission_year, program, category_ipeds) |> 
        summarize(count = n()) |> 
        left_join(program_size, by = c("admission_year", "program")) |> 
        mutate(percent = round((count / size) * 100, 1)) |> 
        filter(program == input$race_trends_input)
      
    } # END else statement
    
  })
  
  #..........................render plotly.........................
  plotly::renderPlotly({
    
    # build ggplot ----
    race_trends_gg <- ggplot(data = category_ipeds_stats_time(),
                             aes(x = admission_year, y = percent, fill = reorder(category_ipeds, percent),
                                 text = paste0(category_ipeds, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_continuous(breaks = seq(max(category_ipeds_stats_time()$admission_year),
                                      min(category_ipeds_stats_time()$admission_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(values = c(
        "American Indian or Alaska Native" = "#003660", 
        "Asian" = "#047c91", 
        "Black or African American" = "#dcd6cc",
        "Hispanic or Latino" = "#6d7d33", 
        "Native Hawaiian or Other Pacific Islander" = "#9cbebe", 
        "White" = "#dce1e5",
        "Two or more races" = "#79a540", 
        "Unknown race and ethnicity" = "#09847a" 
      )) +
      labs(title = paste0("IPEDS Race / Category Trends", " (", input$race_trends_input, ")"),
           y = NULL, x = NULL, fill = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) 
    
    # convert to plotly ----
    plotly::ggplotly(race_trends_gg, tooltip = "text") |> 
      layout(legend = list(orientation = "h", y = -0.1)) |> 
      config(displayModeBar = FALSE)
    
  })  
  
}