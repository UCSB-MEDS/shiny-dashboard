urmTrends_plot <- function(input) {
  
  # urm vars ----
  category_urms <- c("African American / Black", "American Indian / Alaska Native")
  visa_urms <- "Permanent Resident"
  
  # data wrangling & create reactive dfs ----
  urm_trends_df <- reactive({
    
    if (input$urm_trends == "All Programs") {
      
      # select relevant cols ----
      enrolled %>%
        select("app_submission_year", "application_id", "objective1", "citizenship_country",
               "residency_country", "birth_country", "visa", "background",
               "racial_categories", "hispanic_latino") %>%
        
        # replace NULL string with NA ----
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        
        # SC NOTE: not sure why this is happening? ----
        mutate(hispanic_latino = unlist(hispanic_latino)) %>%
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        
        # classify urm status ----
        mutate(urm_status = case_when(
          # us citizens who are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          hispanic_latino == TRUE & citizenship_country == "United States" ~ "Y",
          # permanent residents who are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens who identify as urms
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & citizenship_country == "US" ~ "Y",
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & citizenship_country == "United States" ~ "Y",
          # permanent residents who identify as urms
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & visa %in% visa_urms ~ "Y",
          # everything else not urm
          TRUE ~ "N")) %>%

        # summarize total numbers ----
        group_by(app_submission_year, urm_status) %>%
        summarize(count = n()) %>%
        
        # only keep urms ----
        filter(urm_status == "Y") %>%
        left_join(total_students_yr, by = "app_submission_year") %>%

        # calculate percentage ----
        mutate(percent = round((count / size) * 100, 1))
      
    } # END if statement
    
    else {
      
     enrolled %>%

        # select relevant cols ----
        select("app_submission_year", "application_id", "objective1", "citizenship_country",
               "residency_country", "birth_country", "visa", "background",
               "racial_categories", "hispanic_latino") %>%
        
        # replace NULL string with NA ----
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        
        # SC NOTE: not sure why this is happening? ----
        mutate(hispanic_latino = unlist(hispanic_latino)) %>%
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        
        # classify urm status ----
        mutate(urm_status = case_when(
          # us citizens are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          hispanic_latino == TRUE & citizenship_country == "United States" ~ "Y",
          # permanent residents are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens identify as urms
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & citizenship_country == "US" ~ "Y",
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & citizenship_country == "United States" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = racial_categories, pattern = category_urms) == TRUE & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>%

        # summarize total numbers ----
        group_by(app_submission_year, urm_status, objective1) %>%
        summarize(count = n()) %>%
        
        # only keep urms ----
        filter(urm_status == "Y") %>%
        left_join(program_size, by = c("app_submission_year", "objective1")) %>%
        
        # calculate percentage ----
        mutate(percent = round((count / size) * 100, 1)) %>%
        
        # filter for program ----
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
                            aes(x = app_submission_year, y = percent,
                                text = paste0("URM ", "(", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color, width = 0.9) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      coord_cartesian(xlim = c(2020, 2024), expand = TRUE) + # using this FOR NOW since PhD plot doesn't show 2022 data (no URMs in 2022 and since it's the last year of available data, ggplot excludes it when using the scale_x_continous() code below where max and min x values are set based on data)
      # scale_x_continuous(breaks = seq(max(urm_trends_df()$app_submission_year),
      #                                 min(urm_trends_df()$app_submission_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0("Underrepresented Minority Trends", " (", input$urm_trends, ")"),
           y = NULL, x = NULL)
    
    # convert to plotly
    plotly::ggplotly(urm_trends_gg, tooltip = "text") |> 
      config(displayModeBar = FALSE)
    
  })
  
}


