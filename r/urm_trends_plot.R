#' URM TRENDS PLOTLY
#'
#' @param df data frame used to create urm_trends
#' @param color program color used to fill bar plots 
#' @param prog_input input id
#'
#' @return
#'
#' @examples urm_trends_plot(df = enrolled, color = meds_color, prog_input = input$urm_trends)
#' 
urm_trends_plot <- function(df, color, prog_input){
  
  ## DATA WRANGLING ##
  # urm vars
  category_urms <- c("African American / Black",
                     "American Indian / Alaska Native")
  
  visa_urms <- "Permanent Resident"
  
  urm_trends <- reactive({
    if (prog_input == "All Programs") {
      df %>% 
        select("ay_year",
               "application_id",
               "objective1",
               "citizenship_country",
               "residency_country",
               "birth_country",
               "visa",
               "background",
               "category",
               "hispanic_latino") %>% 
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
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>% 
        group_by(ay_year,
                 urm_status) %>% 
        summarize(count = n()) %>% 
        filter(urm_status == "Y") %>% 
        left_join(total_students_yr, by = "ay_year") %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      df %>% 
        select("ay_year",
               "application_id",
               "objective1",
               "citizenship_country",
               "residency_country",
               "birth_country",
               "visa",
               "background",
               "category",
               "hispanic_latino") %>% 
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
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>% 
        group_by(ay_year,
                 urm_status,
                 objective1) %>% 
        summarize(count = n()) %>% 
        filter(urm_status == "Y") %>% 
        left_join(program_size, by = c("ay_year", "objective1")) %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == prog_input)
    } # EO else statement
    
  }) # EO urm trends reactive df
  
  
  ## PLOTTING ##
  urm_trends_gg <- ggplot(data = urm_trends(),
                          aes(x = ay_year,
                              y = percent,
                              text = paste0("URM ", "(", percent, "%", ")", "\n",
                                            "Sample size: ", size))) +
    geom_bar(stat = "identity",
             fill = color) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(max(urm_trends()$ay_year),
                                    min(urm_trends()$ay_year))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(title = paste0("Underrepresented Minority Trends", " (", prog_input, ")"),
         y = NULL,
         x = NULL) 
  
  plotly::ggplotly(urm_trends_gg, tooltip = "text") %>%
    config(modeBarButtonsToRemove = list("pan", 
                                         "select", 
                                         "lasso2d", 
                                         "autoScale2d", 
                                         "hoverClosestCartesian", 
                                         "hoverCompareCartesian"))
  
} # EO urm trends plot function