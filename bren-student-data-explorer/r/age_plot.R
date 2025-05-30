#' Create "Age" plot which visualizes the aggregated distribution of ages by program
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
age_plot <- function(input, curr_year){
  
  #............create year range chr str (for subtitle)............
  year_min <- curr_year - 4
  year_max <- curr_year
  year_range <- paste0(year_min, "-", year_max)
  
  #...............wrangle data & create reactive df................
  age_stats <- reactive({
    
    left_join(age_program_groups, tot_5yr, by = "program") |> 
      mutate(percent = round((count / size) * 100, 1)) |> 
      filter(program == input$age_prog_input)
    
  }) 
  
  #..........................render plotly.........................
  plotly::renderPlotly({
    
    # empty vars ----
    color <- NULL
    #year_str <- NULL
    
    if (input$age_prog_input == "MESM") {
      color <- mesm_color
      #year_str <- "2020-2024"
    } 
    
    else if (input$age_prog_input == "MEDS") {
      color <- meds_color
      #year_str <- "2020-2024"
    } 
    
    else if (input$age_prog_input == "PhD") {
      color <- phd_color
      #year_str <- "2020-2024"
    } 
    
    # build ggplot ----
    age_gg <- ggplot(data = age_stats(), 
                     aes(x = age_group, y = percent,
                         text = paste0("Age group: ", age_group, 
                                       " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0("Age of graduate students at start of ", 
                          input$age_prog_input, " program"),
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.subtitle = element_text(face = "italic")) 
    
    # convert to plotly ----
    plotly::ggplotly(age_gg, tooltip = "text") |> 
      layout(
        annotations = list(
          list(
            text = paste0("(Data aggregated across years, ", year_range, ")"),
            x = 0,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 13)
          )
        )
      ) |> 
      #layout(title = list(font = list(size = 16))) |> 
      config(displayModeBar = FALSE)
    
  })
  
}

