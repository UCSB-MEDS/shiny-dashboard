ipedsBackgrounds_plot <- function(input) {
  
  renderPlotly({
    
    # reactive element based on plotly click in race plot ----
    background_click <- reactive({
      event_data("plotly_click", source = "race_plot")
    }) 
    
    ## KEY-VALUE PAIRS FOR `background_click` REACTIVE ELEMENT AND RACE VALUE ##
    # "American Indian or Alaska Native", = 8
    # "Asian", = 7
    # "Black or African American", = 6
    # "Native Hawaiian or Other Pacific Islander", = 5
    # "White", = 4
    # "Hispanic or Latino", = 3
    # "Two or more races", = 2
    # "Unknown race and ethnicity" = 1
    
    print(paste0("Y INFO: ", background_click()$y))
    
    # validate plotly click ----
    validate(need(!is.null(background_click()), "Click a bar in the IPEDS Categories and Distribution plot to see the background distribution of the selected racial category. If nothing appears after clicking, try refreshing the page."))
    
    # empty vars ----
    race_num <- NULL
    race_str <- NULL
    color <- NULL
    
    # if American Indian or Alaska Native
    if (background_click()$y == 8) {
      race_num <- 8
      race_str <- "American Indian or Alaska Native"
      color <- "#003660" # ucsb navy
    } 
    
    # if Asian
    else if (background_click()$y == 7) {
      race_num <- 7
      race_str <- "Asian"
      color <- "#047c91" # ucsb aqua
    } 
    
    # if Black or African American
    else if (background_click()$y == 6) {
      race_num <- 6
      race_str <- "Black or African American"
      color <- "#dcd6cc" # ucsb clay
    } 
    
    # if Native Hawaiian or Other Pacific Islander
    else if (background_click()$y == 5) {
      race_num <- 5
      race_str <- "Native Hawaiian or Other Pacific Islander"
      color <- "#9cbebe" # ucsb mist
    } 
    
    # if White
    else if (background_click()$y == 4) {
      race_num <- 4
      race_str <- "White"
      color <- "#dce1e5" # ucsb light grey
    } 
    
    # if Hispanic or Latino
    else if (background_click()$y == 3) {
      race_num <- 3
      race_str <- "Hispanic or Latino"
      color <- "#6d7d33" # ucsb moss
    } 
    
    # if Two or more races
    else if (background_click()$y == 2) {
      race_num <- 2
      race_str <- "Two or more races"
      color <- "#79a540" # bren leaf green
    } 
    
    # if Unknown race and ethnicity
    else if (background_click()$y == 1) {
      race_num <- 1
      race_str <- "Unknown race and ethnicity"
      color <- "#09847a" # ucsb sea green
    } 
    
    
    # background distribution function
    background_distribution(
      prog_input = input$race,
      race_num = race_num,
      df = ipeds,
      race_str = race_str,
      color = color
    ) # EO background_distribution()
    
  }) 
  
}