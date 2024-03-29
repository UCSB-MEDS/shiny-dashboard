urmTrends_plot <- function(input) {
  
  # urm vars ----
  category_urms <- c("African American / Black", "American Indian / Alaska Native")
  visa_urms <- "Permanent Resident"
  
  # data wrangling & create reactive dfs ----
  urm_trends_df <- reactive({
    
    if (input$urm_trends == "All Programs") {
      
      enrolled %>%
        select("ay_year", "application_id", "objective1", "citizenship_country",
               "residency_country", "birth_country", "visa", "background",
               "category", "hispanic_latino") %>%
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>%
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        mutate(urm_status = case_when(
          # us citizens are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>%
        group_by(ay_year, urm_status) %>%
        summarize(count = n()) %>%
        filter(urm_status == "Y") %>%
        left_join(total_students_yr, by = "ay_year") %>%
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
     enrolled %>%
        select("ay_year", "application_id", "objective1", "citizenship_country",
               "residency_country", "birth_country", "visa", "background",
               "category", "hispanic_latino") %>%
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>%
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        mutate(urm_status = case_when(
          # us citizens are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>%
        group_by(ay_year, urm_status, objective1) %>%
        summarize(count = n()) %>%
        filter(urm_status == "Y") %>%
        left_join(program_size, by = c("ay_year", "objective1")) %>%
        mutate(percent = round((count / size) * 100, 1)) %>%
        filter(objective1 == input$urm_trends)

    } # END else statement
    
  }) # END reactive
  
  # render plotly ----
  plotly::renderPlotly({
    
    # empty vars
    color <- NULL 
    
    # set plot colors based on prog_input 
    if (input$urm_trends == "All Programs") {
      color <- all_programs_color
    } 
    
    else if (input$urm_trends == "MESM") {
      color <- mesm_color
    } 
    
    else if (input$urm_trends == "MEDS") {
      color <- meds_color
    }
    
    else if (input$urm_trends == "PhD") {
      color <- phd_color
    } 
    
    # create ggplot
    urm_trends_gg <- ggplot(data = urm_trends_df(),
                            aes(x = ay_year, y = percent,
                                text = paste0("URM ", "(", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color, width = 0.9) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      coord_cartesian(xlim = c(2018, 2022), expand = TRUE) + # using this FOR NOW since PhD plot doesn't show 2022 data (no URMs in 2022 and since it's the last year of available data, ggplot excludes it when using the scale_x_continous() code below where max and min x values are set based on data)
      # scale_x_continuous(breaks = seq(max(urm_trends_df()$ay_year),
      #                                 min(urm_trends_df()$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0("Underrepresented Minority Trends", " (", input$urm_trends, ")"),
           y = NULL, x = NULL)
    
    # convert to plotly
    plotly::ggplotly(urm_trends_gg, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  })
  
}


