#' BACKGROUND DISTRIBUTION PLOTLY
#'
#' @param prog_input input id
#' @param race_num id number for racial category provided by event_data
#' @param df data frame used to create background_stats
#' @param race_str racial category to filter for
#' @param color racial category color used to fill bars in plot
#'
#' @return interactive background distribution plot depending on racial category bar selected in race plot
#'
#' @examples background_distribution(prog_input = input$race, race_num = 7, df = ipeds, race_str = "Asian, color = "#047c91")
#' 
background_distribution <- function(prog_input, race_num, df, race_str, color){
  
  # reactive element based on plotly click in race plot
  background_click <- reactive({
    event_data("plotly_click", source = "race_plot")
  }) # EO background_click reactive
  
  ## KEY-VALUE PAIRS FOR `background_click` REACTIVE ELEMENT AND RACE VALUE ##
  # "American Indian or Alaska Native", = 8
  # "Asian", = 7
  # "Black or African American", = 6
  # "Native Hawaiian or Other Pacific Islander", = 5
  # "White", = 4
  # "Hispanic or Latino", = 3
  # "Two or more races", = 2
  # "Unknown race and ethnicity" = 1
  
  
  ## DATA WRANGLING ## 
  # reactive df
  background_stats <- reactive({
    
    if (background_click()$y == race_num && prog_input == "All Programs") {
      # breakdown of background all programs
      df %>% 
        filter(category_ipeds == race_str) %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        # total number of enrolled students in the past 5 years
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
    } # EO if statement
    
    else {
      # breakdown of background by program
      df %>% 
        filter(category_ipeds == race_str) %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        # join with df with tot of enrolled students (5 yrs) by degree program
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == prog_input)
    } # EO else statement
    
  }) # EO reactive df
  
  
  # validate data in background_stats df
  validate(
    need(nrow(background_stats()) > 0,
         paste0("There are no data on ", race_str, " racial category for ", prog_input, " degree program."))
  ) # EO validate
  
  
  ## PLOTTING ##
  background_gg <- ggplot(data = background_stats(),
                          aes(x = background,
                              y = percent,
                              text = paste0(background, " (", percent, "%", ")", "\n",
                                            "Sample size: ", size
                              )
                          )) +
    geom_bar(stat = "identity",
             fill = color) +
    coord_flip() +
    scale_x_discrete(
      labels = function(x)
        str_wrap(x, width = 35)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste0("IPEDS Backgrounds and Distribution of ", "\n",
                        race_str, " Category ", "(", prog_input, ")"),
         x = NULL,
         y = NULL,
         fill = NULL)
  
  plotly::ggplotly(background_gg, tooltip = "text") %>% 
    layout(title = list(font = list(size = 15))) %>% 
    config(
      modeBarButtonsToRemove = list(
        "pan",
        "select",
        "lasso2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  
} # EO function