#' race_plot
#'
#' @param df data frame used to create category_ipeds
#' @param prog_input input id 
#'
#' @return interactive race plot depending on degree program selected by input 
#'
#' @examples race_plot(df = ipeds, prog_input = input$race_input)
#' 
race_plot <- function(df, prog_input){

  #...............wrangle data & create reactive dfs...............
  category_ipeds_stats <- reactive({
    if (prog_input == "All Programs") {
      df |> 
        group_by(category_ipeds) |> 
        summarize(count = n()) |> 
        mutate(size = totStudents_allPrograms_5yr) |> 
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      df |>
        group_by(program, category_ipeds) |> 
        summarize(count = n()) |>
        left_join(tot_5yr, by = "program") |>
        mutate(percent = round((count / size) * 100, 1)) |> 
        filter(program == prog_input)
      
    } # END else statement
    
  }) # END reactive category_ipeds_stats df
  
  
  #..........................build ggplot..........................
  ipeds_gg <- ggplot(data = category_ipeds_stats(),
                     aes(x = category_ipeds,
                         y = percent,
                         fill = category_ipeds,
                         text = paste0(category_ipeds, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(category_ipeds_stats()$category_ipeds)),
                     labels = function(x)
                       str_wrap(x, width = 35)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#003660", 
        "Asian" = "#047c91", 
        "Black or African American" = "#dcd6cc", 
        "Hispanic or Latino" = "#6d7d33", 
        "Native Hawaiian or Other Pacific Islander" = "#9cbebe", 
        "White" = "#dce1e5", 
        "Two or more races" = "#79a540", 
        "Unknown race and ethnicity" = "#09847a")
    ) +
    labs(title = paste0("IPEDS Categories and Distribution", "\n", "(", prog_input, ")"),
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.subtitle = element_text(face = "italic")
    ) 
  
  #........................convert to plotly.......................
  plotly::ggplotly(ipeds_gg, 
                   source = "race_plot",
                   tooltip = "text") |> 
    config(displayModeBar = FALSE) |> 
    event_register("plotly_click") # had to add event_register to register source "race_plot"
  
} # END race plot function
