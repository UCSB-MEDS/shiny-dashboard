#' sectorTrends_plot
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
sectorTrends_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                wrangle data                              ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  sector_trends_data <- reactive({
    
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_sector_trends_year
      placement_size <- mesm_placement_size
      resoponse_num <- sum(placement_size$responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_sector_trends_year
      placement_size <- meds_placement_size
      resoponse_num <- sum(placement_size$responses)
      
    }
    
    # if "All Years" is selected ----
    if (radioButton_yearInput == "All Years") {
      
      data %>%
        select(c(class_year, employer_sector)) %>%
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>%
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        group_by(class_year, sector_type) %>%
        summarize(count = n()) %>%
        ungroup() |> 
        filter(!is.na(sector_type)) |>
        left_join(placement_size, by = "class_year") |>
        group_by(sector_type) |> 
        summarize(count = sum(count),
                  responses = sum(responses),
                  program_size = sum(program_size)) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    }
    
    # if any single year is selected ----
    else {
      
      data %>%
        select(c(class_year, employer_sector)) %>%
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>%
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        group_by(class_year, sector_type) %>%
        summarize(count = n()) %>%
        ungroup() |> 
        filter(!is.na(sector_type)) |>
        left_join(placement_size, by = "class_year") |>
        filter(class_year == radioButton_yearInput) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    }
    
  })
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                render plotly                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  renderPlotly({
    
    # determine which inputId and response_num to use based on program_acronym supplied ----
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_sector_trends_year
      response_num <- sum(mesm_placement_size$responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_sector_trends_year
      response_num <- sum(meds_placement_size$responses)
      
    }
    
    # create ggplot ----
    sector_trends_gg <- ggplot(data = sector_trends_data(), aes(x = percent, 
                                                                y = fct_relevel(sector_type, c("Other", "Non-Profit", "Public", "Private")),
                                                                #fill = sector_type,
                                                                text = paste0(sector_type,
                                                                              " (", percent, "%", ")",
                                                                              "\n", "Number of respondents: ",
                                                                              responses))) +
      geom_col(fill = "#003660") +
      # scale_fill_manual(values = c("Private" = "#003660", "Public" = "#047c91",
      #                              "Non-Profit" = "#dcd6cc", "Other" = "#9cbebe")) +
      scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                         limits = c(0, 100),
                         labels = scales::percent_format(accuracy = 1, scale = 1)) +
      labs(title = paste0(program_acronym ," Alumni Placement by Sector"),
           x = "Percent of Respondents",
           y = NULL,
           fill = NULL) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm"))
      )
    
    # create plotly ----
    plotly::ggplotly(sector_trends_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  })
  
}  

