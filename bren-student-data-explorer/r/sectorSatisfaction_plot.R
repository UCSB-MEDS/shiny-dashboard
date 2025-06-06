#' Creates the "Sector Satisfaction" plot, which shows satisfaction across sectors (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' 
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return a renderPlotly object
#' @export
#'
#' @examples
sectorSatisfaction_plot <- function(input, data, program_acronym) {

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............calculate tot num of alumni by sector.............
  sector_totals <- data |> 
    select(c(employer_sector, placement_satisfaction)) |>
    group_by(employer_sector) |>
    drop_na() |> 
    summarize(sector_count = n())
  
  #..........wrangle reactive df of satisfaction by sector.........
  sector_satisfaction <- reactive({
    
    #....................get appropriate inputId.....................
    if (program_acronym == "MESM") {

      radioButton_sectorInput <- input$mesm_sector_types_input
      
    } else if (program_acronym == "MEDS") {

      radioButton_sectorInput <- input$meds_sector_types_input
      
    }
    
    #..........................wrangle data..........................
    data |>
      select(c(employer_sector, placement_satisfaction)) |>
      group_by(employer_sector, placement_satisfaction) |>
      summarize(count = n()) |>
      drop_na() |> 
      left_join(sector_totals, by = "employer_sector") |>
      mutate(percent = round((count / sector_count) * 100, 1)) |>
      mutate(placement_satisfaction = factor(placement_satisfaction, 
                                             levels = c("Very Satisfied",
                                                        "Satisfied", 
                                                        "Somewhat Satisfied", 
                                                        "Unsatisfied"),
                                             labels = c("Very Satisfied", 
                                                        "Satisfied", 
                                                        "Somewhat Satisfied", 
                                                        "Unsatisfied"))) |> 
      mutate(employer_sector = case_when(
        employer_sector == "Other" ~ "Eco-Entrepreneurship/New Business",
        TRUE ~ employer_sector
      )) |> 
      filter(employer_sector %in% radioButton_sectorInput)
    
  }) # END reactive df
  
  #............render sector satisfaction plotly object............
  plotly::renderPlotly({
    
    #............get appropriate input and number of years...........
    if (program_acronym == "MESM") {
    
      radioButton_sectorInput <- input$mesm_sector_types_input
      
      
    } else if (program_acronym == "MEDS") {

      radioButton_sectorInput <- input$meds_sector_types_input
      
    }
    
    #..create validation message when no alumni from chosen sector...
    validate(need(nrow(sector_satisfaction()) > 0, paste0("We do not have any ", program_acronym, " alumni who accepted positions in this sector for their initial placements. Check back next year!")))
    
    #...................create ggplot object first...................
    sector_satisfaction_gg <- ggplot(data = sector_satisfaction(), 
                                     aes(x = placement_satisfaction, 
                                         y = percent, 
                                         text = paste0(percent, "%"))) +
                                         # text = paste0(placement_satisfaction, " (", percent, "%", ")", "\n", "Number of respondents: ", sector_count))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.9, fill = "#003660") +
      coord_flip(ylim = c(0, 100)) +
      scale_x_discrete(limits = rev(levels(sector_satisfaction()$placement_satisfaction))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none") +
      labs(title = paste0(program_acronym, " Placement Satisfaction in ", radioButton_sectorInput, "\n", 
                          "(", unique(sector_satisfaction()$sector_count), " respondents across all years)"), 
           x = NULL, y = NULL, fill = NULL) 
    
    #....................then create plotly object...................
    plotly::ggplotly(sector_satisfaction_gg, tooltip = "text") |> 
      layout(title = list(font = list(size = 15))) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly
  
} # END fxn
