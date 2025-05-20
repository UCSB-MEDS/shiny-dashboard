#' Creates the "Diversity Demographics" plot which visualizes a variety of diversity metrics across cohorts and for all available years of data
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
diversityDemographics_plot <- function(input, curr_year) {
  
  #............create year range chr str (for subtitle)............
  year_min <- curr_year - 4
  year_max <- curr_year
  year_range <- paste0(year_min, "-", year_max)
  
  #..................create vector of plot colors..................
  plot_colors <- c("California Resident" = "#047c91",
                   "Nonresident" = "#047c91",
                   "International" = "#047c91",
                   "Female" = "#9cbebe",
                   "Age 25+" = "#9cbebe",
                   "Military" = "#9cbebe",
                   "First generation" = "#9cbebe",
                   "Undocumented" = "#9cbebe",
                   "URM" = "#09847a",
                   "American Indian or Alaska Native" = "#09847a",
                   "Asian" = "#09847a",
                   "Black or African American" = "#09847a",
                   "Hispanic or Latino" = "#09847a",
                   "Native Hawaiian or Other Pacific Islander" = "#09847a",
                   "White" = "#09847a",
                   "Two or more races" = "#09847a",
                   "Unknown race and ethnicity" = "#09847a")
  
  #................wrangle data & create reactive df...............
  diversity_overall <- reactive({
    
   diversity_overall <- diversity_stats |> 
      filter(program == input$diversity_stats_all_input)
    
  }) 
  
  #..........................render plotly.........................
  renderPlotly({
    
    overall_demo <- ggplot(data = diversity_overall(), 
                           aes(x = demographic, y = percent, fill = demographic,
                               text = paste0(demographic, 
                                             " (", percent, "%", ")", 
                                             "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(diversity_overall()$demographic))) + 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(values = plot_colors) +
      labs(title = paste0(input$diversity_stats_all_input, " Diversity Demographics", "\n",
                          "(", year_range, ")"),
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.title.position = "plot",
        legend.position = "none",
        panel.grid.minor = element_blank()
        ) 
    
    # convert to plotly (2017 - curr_year)
    plotly::ggplotly(overall_demo, tooltip = "text") |> 
      config(displayModeBar = FALSE)
    
  }) 
  
}