#' Creates the "IPEDS Backgrounds and Distributions" plots, which are selected by clicking ona group in the "IPEDS Categories and Distribution" plot
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
ipedsBackground_plot <- function(input) { 
  
  #.....plot params (title & color) based on category clicked .....
  plot_params <- list(
    list("Native Hawaiian or Other Pacific Islander", "#9cbebe"),
    list("American Indian or Alaska Native", "#003660"),
    list("Black or African American", "#dcd6cc"),
    list("Unknown race and ethnicity", "#09847a"),
    list("Two or more races", "#79a540"),
    list("Asian", "#047c91"),
    list("Hispanic or Latino", "#6d7d33"),
    list("White", "#dce1e5"))
  
  
  #..........................render plotly.........................
  renderPlotly({
    
    # reactive event based on plotly click in race plot ----
    background_click <- reactive({
      event_data(event = "plotly_click", source = "race_plot")
    }) 
    
    # initial validation message with instructions to click on race plot bars ----
    validation_message <- "Click a bar in the IPEDS Categories and Distribution plot to see the background distribution of the selected racial category. If nothing appears after clicking, try refreshing the page."
    validate(need(!is.null(background_click()), validation_message))
    
    # get plot params based on choice ----
    race_num <- background_click()$y
    race_str <- plot_params[[race_num]][[1]]
    color <- plot_params[[race_num]][[2]]
    
    # wrangle data & create reactive df ----
    background_stats <- reactive({
      
      if (background_click()$y == race_num && input$race_input == "All Programs") {
        
        # breakdown of background all programs ----
        ipeds |> 
          filter(category_ipeds == race_str) |> 
          mutate(background = case_when(
            is.na(background) == TRUE ~ "Unknown race and ethnicity",
            TRUE ~ background
          ))|> 
          mutate(background = str_split(background, "; ")) |> 
          unnest(background) |> 
          group_by(background) |> 
          summarize(count = n()) |> 
          mutate(size = totStudents_allPrograms_5yr) |> 
          mutate(percent = round((count / size) * 100, 1)) |> 
          mutate(background = fct_reorder(background, percent))
        
      }
      
      else {
        
        # breakdown of background by program ----
        ipeds |> 
          filter(category_ipeds == race_str) |> 
          mutate(background = case_when(
            is.na(background) == TRUE ~ "Unknown race and ethnicity",
            TRUE ~ background
          )) |>
          mutate(background = str_split(background, "; ")) |> 
          unnest(background) |>
          group_by(program, background) |> 
          summarize(count = n()) |>
          left_join(tot_5yr, by = "program") |> 
          mutate(percent = round((count / size) * 100, 1)) |> 
          filter(program == input$race_input) |> 
          mutate(background = fct_reorder(background, percent))
        
      } 
      
    }) # END reactive df
    
    # validation message appears when no background data available for particular program and/or category ----
    validate(need(nrow(background_stats()) > 0, paste0("There are no data on ", race_str, " racial category for ", input$race_input, " degree program."))) 
    
    # create ggplot ----
    background_gg <- ggplot(data = background_stats(),
                            aes(x = background, y = percent,
                                text = paste0(background, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color) +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = paste0(race_str, " Category ", "\n", "Backgrounds (", input$race_input, ")"),
           x = NULL, y = NULL, fill = NULL)
    
    # convert to plotly ----
    plotly::ggplotly(background_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15))) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly 
  
} 
