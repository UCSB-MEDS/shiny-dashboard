#' Creates the "Job Source" plot which indicates how alumni found / secured their initial jobs (Career tab)
#'
#' @param input input 
#' @param data df; either 'mesm_placement' or 'meds_placement'
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
jobSource_plot <- function(input, data, program_acronym) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                               Data Wrangling                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #...............wrangle reactive df of job sources...............
  job_source <- reactive({
    
    #........get appropriate `*_status_size df and input value.......
    if (program_acronym == "MESM") {
      
      status_size <- mesm_status_size
      placement_size <- mesm_placement_size
      radioButton_yearInput <- input$mesm_job_source_year
      
    } else if (program_acronym == "MEDS") {
      
      status_size <- meds_status_size
      placement_size <- meds_placement_size
      radioButton_yearInput <- input$meds_job_source_year
      
    }
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") { 
      
      # calculate total responses and program size across yrs ----
      status_size_allYrs <- status_size |>
        summarize(
          total_responses = sum(responses),
          total_program_size = sum(program_size)
        )
      
      # pull values ---
      total_responses <- status_size_allYrs |> pull(total_responses)
      total_program_size <- status_size_allYrs |> pull(total_program_size)
      
      data |> 
        select(c(class_year, job_source)) |>
        group_by(job_source) |>
        summarize(count = n()) |>
        drop_na() |>
        mutate(responses = rep(total_responses),
               program_size = rep(total_program_size)) |> 
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `All Years` is selected 
    
    #.................if any single year is selected.................
    else {
      
      data |> 
        select(c(class_year, job_source)) |>
        filter(class_year == radioButton_yearInput) |>
        group_by(class_year, job_source) |>
        summarize(count = n()) |>
        drop_na() |>
        left_join(placement_size, by = "class_year") |>
        mutate(percent = round((count / responses) * 100, 1))
      
    } # END if `any single year` is selected
    
  }) # END reactive df
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................render job source plotly object.................
  plotly::renderPlotly({
    
    #........get values necessary for constructing plot title........
    if (program_acronym == "MESM") {
      
      radioButton_yearInput <- input$mesm_job_source_year
      selected_class_year <- radioButton_yearInput
      placement_size <- mesm_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
      
    } else if (program_acronym == "MEDS") {
      
      radioButton_yearInput <- input$meds_job_source_year
      selected_class_year <- radioButton_yearInput
      placement_size <- meds_placement_size
      allYrs_size <- sum(placement_size$program_size)
      allYrs_response <- sum(placement_size$responses)
      yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
      yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
      
    } # END if `All Years` is selected
    
    #...................if `All Years` is selected...................
    if (radioButton_yearInput == "All Years") {
      
      #...................create ggplot object first...................
      source_gg <- ggplot(data = job_source(),
                          aes(x = percent,
                              y = fct_relevel(job_source, c("Other",
                                                            "Company website",
                                                            "Internet posting",
                                                            "Personal/Professional Contact",
                                                            "Bren School Network")),
                              text = paste0(percent, "%"))) +
        geom_col(fill = "#003660") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                           limits = c(0, 100),
                           labels = scales::percent_format(accuracy = 1, scale = 1)) +
        scale_y_discrete(labels = scales::label_wrap(20)) +
        labs(title = paste0("Job Sources ", program_acronym, " alumni are using to secure jobs", "\n",
                            "(", allYrs_response, "/", allYrs_size, " survey respondents)"),
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal() +
        theme(panel.grid.minor = element_blank())
      
    } # END if `All Years` is selected
    
    #.................if any single year is selected.................
    else {
      
      #...................create ggplot object first...................
      source_gg <- ggplot(data = job_source(),
                          aes(x = percent,
                              y = fct_relevel(job_source, c("Other",
                                                            "Company website",
                                                            "Internet posting",
                                                            "Personal/Professional Contact",
                                                            "Bren School Network")),
                              text = paste0(percent, "%"))) +
    
        geom_col(fill = "#003660") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                           limits = c(0, 100),
                           labels = scales::percent_format(accuracy = 1, scale = 1)) +
        scale_y_discrete(labels = scales::label_wrap(20)) +
        labs(title = paste0("Job Sources ", program_acronym, " alumni are using to secure jobs", "\n",
                            "(", yr_response, "/", yr_size, " survey respondents)"),
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal() +
        theme(panel.grid.minor = element_blank())
      
    } # END if `any single year` is selected

    # source_gg <- ggplot(data = job_source, 
    #                     aes(x = class_year, 
    #                         y = percent, 
    #                         fill = reorder(job_source, percent),
    #                         text = paste0(job_source, 
    #                                       " (", percent, "%", ")", 
    #                                       "\n", 
    #                                       "Number of respondents: ", 
    #                                       responses))) +
    #   geom_bar(position = "dodge", stat = "identity") +
    #   scale_x_continuous(breaks = seq(min(job_source$class_year), 
    #                                   max(job_source$class_year))) +
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    #   scale_fill_manual(values = c("Bren School Network" = "#003660", 
    #                                "Company website" = "#047c91", 
    #                                "Internet posting" = "#9cbebe", 
    #                                "Other" = "#6d7d33", 
    #                                "Personal/Professional Contact" = "#79a540")) +
    #   labs(title = paste0("Job Sources ", program_acronym, " alumni are using to secure jobs"),
    #        x = NULL, y = "Percent of Respondents", fill = NULL) +
    #   theme_minimal() +
    #   theme(panel.grid.minor = element_blank()) 
    
    #..................then convert to plotly object.................
    plotly::ggplotly(source_gg, tooltip = "text") |> 
      layout(legend = list(orientation = "h", y = -0.1)) |> 
      config(displayModeBar = FALSE)
      # config(modeBarButtonsToRemove = list("pan", 
      #                                      "select", 
      #                                      "lasso2d", 
      #                                      "autoScale2d", 
      #                                      "hoverClosestCartesian", 
      #                                      "hoverCompareCartesian")) # END ggplotly
    
  }) # END renderPlotly
  
} # END fxn

