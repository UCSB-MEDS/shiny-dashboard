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
    # # standardize state abbreviation values; # standardize state values; SC NOTE 2023-02-03 figure out a better way to do this (NOTE: this code is repeated in domesticPlacement_plot())
    # mutate(work_location_state = case_when(
    #   work_location_state %in% ca_names ~ "CA",
    #   work_location_state == "Maryland" ~ "MD",
    #   work_location_state == "Washington" ~ "WA",
    #   work_location_state == "District of Columbia" ~ "DC",
    #   work_location_state == "Indiana" ~ "IN",
    #   work_location_state == "Mt" ~ "MT",
    #   work_location_state == "Hawaii" ~ "HI",
    #   work_location_state == "N/A" ~ NA_character_,
    #   work_location_state == "michoacan" ~ "Michoacan",
    #   # specifically assign correct work location state
    #   work_location_city == "Washington DC" ~ "DC",
    #   work_location_city == "Oxnard" ~ "CA",
    #   work_location_city == "Santa Cruz" ~ "CA",
    #   work_location_city == "Fort Collins" ~ "CO",
    #   work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
    #   TRUE ~ work_location_state
    # )) %>%
    # # standardize united states values
    # mutate(work_location_country = case_when(
    #   work_location_country %in% us_names ~ "United States",
    #   # specificallly assign correct country values
    #   work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
    #   work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
    #   TRUE ~ work_location_country
    # )) %>%
    # # assign ca / out of state / international
    # mutate(location = case_when(
    #   work_location_state == "CA" ~ "Domestic (California)",
    #   work_location_state != "CA" & work_location_country == "United States" ~ "Domestic (Out of State)",
    #   work_location_country != "United States" ~ "International"
    # )) %>%
    group_by(class_year,
             location) %>%
    summarize(location_count = n())
  
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
