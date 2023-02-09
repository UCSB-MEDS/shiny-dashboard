#' placementStatus_plot
#'
#' @param input input
#' @param data  df; either 'mesm_status' or 'meds_status' (see global.R)
#' @param program_acronym chr str; "MEDS" or "MESM"
#'
#' @return a renderPlotly object
#' @export
#'
#' @examples
placementStatus_plot <- function(input, data, program_acronym) {
  
  if (program_acronym == "MESM") {
    
    
    
  } else if (program_acronym == "MEDS") {
    
    
  }
  
  # wrangle data (program sizes + tot responsdees) ----
  status_size <- data %>% 
    select(class_year) %>% 
    group_by(class_year) %>% 
    summarize(responses = n()) %>% 
    mutate(program_size = case_when(
      class_year == 2021 ~ 83, # SC NOTE 2023-02-03: would be better to not hard-code this here
      class_year == 2020 ~ 92,
      class_year == 2019 ~ 93
    ))
  
  # wrangle data for placement status plot ----
  status <- data %>% 
    select(class_year, member_status) %>% 
    mutate(status = case_when(
      member_status %in% c("FT Career", "FT Temporary Career", "PT Temporary Career", "FT Career-Sponsored", "PT Career", "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off", "Searching", "Time-Off") ~ "Searching or Time Off",
      member_status %in% c("FT New Business", "FT Eco-E") ~ "Eco-Entrepreneurship/New Business",
      member_status %in% c("Internship/Fellowship", "Continuing Internship", "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
      TRUE ~ member_status
    )) %>% 
    group_by(class_year, status) %>% 
    summarize(count = n()) %>% 
    left_join(status_size, by = "class_year") %>% 
    mutate(percent = round((count / responses) * 100, 1)) 
  
  # render plotly ----
  plotly::renderPlotly({
    
    # create ggplot
    status_gg <- ggplot(data = status, aes(x = class_year, y = percent, fill = reorder(status, percent),
                                           text = paste0(status, " (", percent, "%", ")", "\n", "Number of respondents: ", responses))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(breaks = seq(min(status$class_year),  max(status$class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = paste0(program_acronym, "Alumni Placement Status 6 months after graduation"),
           x = NULL, y = "Percent of Respondents", fill = NULL) +
      scale_fill_manual(values = c("Advanced Degree/Another Degree" = "#003660", "Career" = "#047c91", 
                                   "Internship, Fellowship, or Short-term Project" = "#9cbebe", 
                                   "Eco-Entrepreneurship/New Business" = "#6d7d33", "Searching or Time Off" = "#79a540")) # ucsb navy, ucsb aqua, ucsb mist, ucsb moss, bren leaf green
    
    # convert to plotly
    plotly::ggplotly(status_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.1),
             title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian","hoverCompareCartesian"))
  }) 
  
}



## ORIGINAL FXN
# placementStatus_plot <- function(input) {
#   
#   # wrangle data (program sizes + tot responsdees) ----
#   status_size <- mesm_status %>% 
#     select(mesm_class_year) %>% 
#     group_by(mesm_class_year) %>% 
#     summarize(mesm_responses = n()) %>% 
#     mutate(program_size = case_when(
#       mesm_class_year == 2021 ~ 83, # SC NOTE 2023-02-03: would be better to not hard-code this
#       mesm_class_year == 2020 ~ 92,
#       mesm_class_year == 2019 ~ 93
#     ))
#   
#   # wrangle data for placement status plot ----
#   status <- data %>% 
#     select(mesm_class_year, member_status) %>% 
#     mutate(status = case_when(
#       member_status %in% c("FT Career", "FT Temporary Career", "PT Temporary Career", "FT Career-Sponsored", "PT Career", "FT Career-Sponsored") ~ "Career",
#       member_status %in% c("Time Off", "Searching") ~ "Searching or Time Off",
#       member_status %in% c("FT New Business", "FT Eco-E") ~ "Eco-Entrepreneurship/New Business",
#       member_status %in% c("Internship/Fellowship", "Continuing Internship", "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
#       TRUE ~ member_status
#     )) %>% 
#     group_by(mesm_class_year, status) %>% 
#     summarize(count = n()) %>% 
#     left_join(status_size, by = "mesm_class_year") %>% 
#     mutate(percent = round((count / mesm_responses) * 100, 1)) 
#   
#   # render plotly ----
#   plotly::renderPlotly({
#     
#     # create ggplot
#     status_gg <- ggplot(data = status, aes(x = mesm_class_year, y = percent, fill = reorder(status, percent),
#                                            text = paste0(status, " (", percent, "%", ")", "\n", "Number of respondents: ", mesm_responses))) +
#       geom_bar(position = "dodge", stat = "identity") +
#       scale_x_continuous(breaks = seq(min(status$mesm_class_year),  max(status$mesm_class_year))) +
#       scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
#       theme_minimal() +
#       theme(panel.grid.minor = element_blank()) +
#       labs(title = paste0(program_acronym, "Alumni Placement Status 6 months after graduation"),
#            x = NULL, y = "Percent of Respondents", fill = NULL) +
#       scale_fill_manual(values = c("Advanced Degree/Another Degree" = "#003660", "Career" = "#047c91", 
#                                    "Internship, Fellowship, or Short-term Project" = "#9cbebe", 
#                                    "Eco-Entrepreneurship/New Business" = "#6d7d33", "Searching or Time Off" = "#79a540")) # ucsb navy, ucsb aqua, ucsb mist, ucsb moss, bren leaf green
#     
#     # convert to plotly
#     plotly::ggplotly(status_gg, tooltip = "text") %>%
#       layout(legend = list(orientation = "h", y = -0.1),
#              title = list(font = list(size = 16))) %>%
#       config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian","hoverCompareCartesian"))
#   }) 
#   
# }