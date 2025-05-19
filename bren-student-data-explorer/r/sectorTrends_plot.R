#' Creates the "Sector Trends" plot, which shows which percentage of alumni found jobs in the private, public, and non-profit sectors (Career tab)
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement'
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
sectorTrends_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #..............wrangle reactive df of sector trends..............
  sector_trends_data <- reactive({
    
    #............get appropriate input and placement_size............
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_sector_trends_year
      placement_size <- mesm_placement_size
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_sector_trends_year
      placement_size <- meds_placement_size
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") {
      
      # calculate total responses and program size across yrs ----
      placement_size_allYrs <- placement_size |> 
        summarize(
          total_responses = sum(responses),
          total_program_size = sum(program_size)
        )
      
      # pull values ---
      total_responses <- placement_size_allYrs |> pull(total_responses)
      total_program_size <- placement_size_allYrs |> pull(total_program_size)
      
      # wrangle data ----
      data |> 
        select(c(year, employer_sector)) |> 
        group_by(employer_sector) |> 
        summarize(count = n()) |> 
        filter(!is.na(employer_sector)) |>
        mutate(responses = rep(total_responses),
               program_size = rep(total_program_size)) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      data |> 
        select(c(year, employer_sector)) |> 
        group_by(year, employer_sector) |> 
        summarize(count = n()) |> 
        ungroup() |> 
        filter(!is.na(employer_sector)) |> 
        left_join(placement_size, by = "year") |>
        filter(year == radioButton_yearInput) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `any single year` is selected
    
  }) # END reactive df
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  renderPlotly({
    
    #........get values necessary for constructing plot title........
    if (program_acronym == "MESM") {

      radioButton_yearInput <- input$mesm_sector_trends_year
      selected_class_year <- radioButton_yearInput
      placement_size <- mesm_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(year == selected_class_year) |> pull(responses)

    } else if (program_acronym == "MEDS") {

      radioButton_yearInput <- input$meds_sector_trends_year
      selected_class_year <- radioButton_yearInput
      placement_size <- meds_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(year == selected_class_year) |> pull(responses)

    }

    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") {

      #...................create ggplot object first...................
      sector_trends_gg <- ggplot(data = sector_trends_data(), 
                                 aes(x = percent, 
                                     y = fct_reorder(employer_sector, percent),
                                     text = paste0(percent, "%"))) +
        geom_col(fill = "#003660") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                           limits = c(0, 100),
                           labels = scales::percent_format(accuracy = 1, scale = 1)) +
        labs(title = paste0(program_acronym ," Initial Job Placement by Sector", "\n",
                            "(", allYrs_response, " survey respondents out of ", allYrs_size, " graduates)"),
             x = NULL,
             y = NULL,
             fill = NULL) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm"))
        )
      
     } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      #...................create ggplot object first...................
      sector_trends_gg <- ggplot(data = sector_trends_data(), 
                                 aes(x = percent, 
                                     y = fct_reorder(employer_sector, percent),
                                     text = paste0(percent, "%"))) +
        geom_col(fill = "#003660") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                           limits = c(0, 100),
                           labels = scales::percent_format(accuracy = 1, scale = 1)) +
        labs(title = paste0(program_acronym ," Initial Job Placement by Sector", "\n",
                            "(", yr_response, " survey respondents out of ", yr_size, " graduates)"),
             x = NULL,
             y = NULL,
             fill = NULL) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm"))
        )
      
    } # END if `any single year` is selected
    
    #....................then render plotly object...................
    plotly::ggplotly(sector_trends_gg, tooltip = "text") |>  
      layout(title = list(font = list(size = 16))) |> 
      config(displayModeBar = FALSE)
    
  }) # END renderPlotly
  
} # END fxn

