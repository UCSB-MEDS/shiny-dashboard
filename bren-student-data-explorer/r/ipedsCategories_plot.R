#' Creates the clickable "IPEDS Categories and Distributions" plot
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
ipedsCategories_plot <- function(input) {
  
  #................observer to check for bar click.................
  observeEvent(input$race_tabsetPanel, {
    
    if (input$race_tabsetPanel == "Race / Category (IPEDS)") { # if the 'Race / Category (IPEDS)" tab is selected, show background box on right
      
      shinyjs::show(id = "background_box")
      shinydashboardPlus::updateBox(id = "race_box", action = "update", options = list(width = 6))
      
    } # END if statement
    
    else { # else (i.e. if ^ tab not selected) fill width of screen with entire tabBox
      
      shinyjs::hide(id = "background_box")
      shinydashboardPlus::updateBox(id = "race_box", action = "update", options = list(width = 12))
      
    } # END else statement
    
  }) # END observe event
  
  #..................wrangle & create reactive df..................
  category_ipeds_stats <- reactive({
    
    if (input$race_input == "All Programs") {
      
      ipeds |> 
        group_by(category_ipeds) |> 
        summarize(count = n()) |> 
        mutate(size = totStudents_allPrograms_5yr) |>
        mutate(percent = round((count / size) * 100, 1)) #|> 
        #mutate(category_ipeds = fct_reorder(category_ipeds, if_else(n == 0, Inf, -n)))
        #mutate(category_ipeds = fct_reorder(category_ipeds, percent, .desc = TRUE))
      
    } # END if statement
    
    else {
      
      ipeds |>
        group_by(program, category_ipeds) |>
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "program") |> 
        mutate(percent = round((count / size) * 100, 1)) |> 
        filter(program == input$race_input) #|> 
        #mutate(category_ipeds = fct_reorder(category_ipeds, if_else(n == 0, Inf, -n)))
        #mutate(category_ipeds = fct_reorder(category_ipeds, percent, .desc = TRUE))
      
    } # END else statement
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # build ggplot ----
    ipeds_gg <- ggplot(data = category_ipeds_stats(),
                       aes(x = category_ipeds, y = percent, fill = category_ipeds,
                           text = paste0(category_ipeds, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(face = "italic")
      ) +
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
          "Unknown race and ethnicity" = "#09847a" )) + 
      labs(title = paste0("IPEDS Categories and Distribution", "\n", "(", input$race_input, ")"),
           x = NULL, y = NULL)
    
    # convert to plotly ----
    plotly::ggplotly(ipeds_gg, source = "race_plot", tooltip = "text") |> 
      config(displayModeBar = FALSE) |> 
      event_register("plotly_click") # had to add event_register to register source "race_plot"
    
  }) 
  
  
}