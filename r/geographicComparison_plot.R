#' geographicComparison_plot
#'
#' @param input input
#' @param data df; either 'mesm_placement' or 'meds_placement' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return renderPlotly object
#' @export
#'
#' @examples
geographicComparison_plot <- function(input, data, program_acronym) {
  
  # determine which *_placement_size df (see global.R) to use based on program_acronym supplied ----
  if (program_acronym == "MESM") {
    
    placement_size <- mesm_placement_size
    
  } else if (program_acronym == "MEDS") {
    
    placement_size <- meds_placement_size
    
  }
  
  # wrangle geographic comparison data ----
  placement_location <- data %>%
    select(c(
      employer_account_name,
      work_location_city,
      class_year,
      work_location_state,
      work_location_country,
      location
    )) %>%
    group_by(class_year,
             location) %>%
    summarize(location_count = n()) %>%
    filter(!is.na(location))
  
  # calculating percentages
  placement_location_stats <- placement_location %>%
    left_join(placement_size, by = "class_year") %>%
    mutate(percent = round((location_count / responses) * 100, 1))
  
  # render plotly ----
  plotly::renderPlotly({
    
    # make ggplot
    location_gg <- ggplot(data = placement_location_stats,
                          aes(x = class_year, y = percent, fill = reorder(location, percent),
                              text = paste0(location, " (", percent, "%", ")", "\n", "Number of respondents: ", responses))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(placement_location_stats$class_year),
                                      max(placement_location_stats$class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = paste0("Where ", program_acronym, " alumni are working 6 months after graduating"),
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      scale_fill_manual(values = c("Domestic (California)" = "#9cbebe",
                                   "Domestic (Out of State)" = "#003660",
                                   "International" = "#dcd6cc"))
    
    # convert to plotly
    plotly::ggplotly(location_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.1),
             title = list(font = list(size = 15.5))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  })
  
}
