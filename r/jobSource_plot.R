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
  
  #..............get appropriate `*_placement_size` df.............
  if (program_acronym == "MESM") {
    
    placement_size <- mesm_placement_size
    
  } else if (program_acronym == "MEDS") {
    
    placement_size <- meds_placement_size
    
  }

  #................wrangle data for job source plot................
  job_source <- data |> 
    select(c(class_year, job_source)) |>
    group_by(class_year, job_source) |>
    summarize(count = n()) |>
    drop_na() |>
    left_join(placement_size, by = "class_year") |>
    mutate(percent = round((count / responses) * 100, 1))  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                Visualization                             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #................render job source plotly object.................
  plotly::renderPlotly({
    
    #...................create ggplot object first...................
    source_gg <- ggplot(data = job_source, 
                        aes(x = class_year, 
                            y = percent, 
                            fill = reorder(job_source, percent),
                            text = paste0(job_source, 
                                          " (", percent, "%", ")", 
                                          "\n", 
                                          "Number of respondents: ", 
                                          responses))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(job_source$class_year), 
                                      max(job_source$class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(values = c("Bren School Network" = "#003660", 
                                   "Company website" = "#047c91", 
                                   "Internet posting" = "#9cbebe", 
                                   "Other" = "#6d7d33", 
                                   "Personal/Professional Contact" = "#79a540")) +
      labs(title = paste0("Job Sources ", program_acronym, " alumni are using to secure jobs"),
           x = NULL, y = "Percent of Respondents", fill = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) 
    
    #..................then convert to plotly object.................
    plotly::ggplotly(source_gg, tooltip = "text") |> 
      layout(legend = list(orientation = "h", y = -0.1)) |> 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian")
             ) # END ggplotly
    
  }) # END renderPlotly

} # END fxn

