
age_plot <- function(input){
  
  # wrangle data (2017 - curr_year) ----
  age_program_groups <- enrolled %>% 
    select(c("app_submission_year", "application_id", "objective1", "dob")) %>%
    # calculate age
    mutate(dob_year = year(dob)) %>%
    mutate(age = app_submission_year - dob_year) %>% 
    filter(age > 18) %>% # remove outliers incorrectly submitted dobs
    mutate(age_group = case_when(age >= 20 & age <= 22 ~ "20-22",
                                 age >= 23 & age <= 24 ~ "23-24",
                                 age >= 25 & age <= 29 ~ "25-29",
                                 age >= 30 & age <= 34 ~ "30-34",
                                 age >= 35 & age <= 39 ~ "35-39",
                                 age >= 40 & age <= 49 ~ "40-49",
                                 age >= 50 & age <= 64 ~ "50+",
                                 age >= 65 ~ "50+")) %>% 
    group_by(objective1, age_group) %>% 
    summarize(count = n())
  
  # create reactive df ----
  age_stats <- reactive({
    
    left_join(age_program_groups, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / size) * 100, 1)) %>% 
      filter(objective1 == input$age_prog)
    
  }) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # empty vars
    color <- NULL
    year_str <- NULL
    
    if (input$age_prog == "MESM") {
      color <- mesm_color
      year_str <- "2019-2023"
    } 
    
    else if (input$age_prog == "MEDS") {
      color <- meds_color
      year_str <- "2021-2023"
    } 
    
    else if (input$age_prog == "PhD") {
      color <- phd_color
      year_str <- "2019-2023"
    } 
    
    # create ggplot 
    age_gg <- ggplot(data = age_stats(), aes(x = age_group, y = percent,
                                             text = paste0("Age group: ", age_group, " (", percent, "%", ")", "\n", "Sample size: ", size))) +
      geom_bar(stat = "identity", fill = color) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.subtitle = element_text(face = "italic")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0("Age of graduate students at start of ", input$age_prog, " program (", year_str, ")"),
           x = NULL, y = NULL)
    
    # conver to plotly
    plotly::ggplotly(age_gg, tooltip = "text") %>%
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  })
  
}

