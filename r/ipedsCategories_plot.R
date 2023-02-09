ipedsCategories_plot <- function(input) {
  
  # observe event ----
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
  
  # create reactive df (2017 - curr_year) ----
  category_ipeds_stats <- reactive({
    
    if (input$race == "All Programs") {
      
      ipeds %>% 
        group_by(category_ipeds) %>% 
        summarize(count = n()) %>% 
        mutate(size = totStudents_allPrograms_5yr) %>% # SC NOTE 2022-02-08: was hardcoded as 627
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
      ipeds %>% 
        group_by(objective1, category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$race)
      
    } # END else statement
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # NOTE(HD): could not plotly_click and source to work from a script / function
    # NOTE(CONT): so had to pull race plot function out of script
    # # race plot function
    # race_plot(
    #   df = ipeds,
    #   prog_input = input$race
    # ) # EO race plot function
    
    # create ggplot
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
          "American Indian or Alaska Native" = "#003660", # ucsb navy
          "Asian" = "#047c91", # ucsb aqua
          "Black or African American" = "#dcd6cc", # ucsb clay
          "Hispanic or Latino" = "#6d7d33", # ucsb moss
          "Native Hawaiian or Other Pacific Islander" = "#9cbebe", # ucsb mist
          "White" = "#dce1e5", # ucsb light grey
          "Two or more races" = "#79a540", # bren leaf green
          "Unknown race and ethnicity" = "#09847a" )) + # ucsb sea green
      labs(title = paste0("IPEDS Categories and Distribution", "\n", "(", input$race, ")"),
           x = NULL, y = NULL)
    
    # convert to plotly
    plotly::ggplotly(ipeds_gg, source = "race_plot", tooltip = "text") %>% 
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
      # had to add event_register to register source "race_plot"
      event_register("plotly_click")
    
  }) 
  
  
}