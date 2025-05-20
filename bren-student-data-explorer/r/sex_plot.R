#' Create "Sex" plot, which visualizes sex diversity and distribution across time and programs
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
sex_plot <- function(input) {

  renderPlotly({
    
    #..........................create ggplot.........................
    sex_all <- ggplot(data = sex_stats_time,
                      aes(x = admission_year, y = percent, fill = reorder(gender, percent),
                          text = paste0(gender, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(sex_stats_time$admission_year),
                                      max(sex_stats_time$admission_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Sex diversity and distribution trends by degree program",
           x = NULL, y = NULL, fill = NULL) +
      scale_fill_manual(values = c("Female" = "#9cbebe", "Male" = "#003660", "Undeclared" = "#dcd6cc")) +
      facet_wrap(~program, ncol = 1)
    
    #........................convert to plotly.......................
    plotly::ggplotly(sex_all, tooltip = "text") |> 
      config(displayModeBar = FALSE)
    
  }) 
  
}


