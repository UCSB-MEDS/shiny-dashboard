#' Creates the "Admissions" plot, which visualizes the number of applications, admits, and enrolled students across years for each program
#'
#' @param input 
#'
#' @returns
#' @export
#'
#' @examples
admissions_plot <- function(input) {
  
  #..............create reactive df of admissions data.............
  admissions_stacked_all_df <- reactive({
    
   admissions |> 
      select(c(admission_year, program, enrolled, applied, admitted)) |> 
      filter(program == input$admit_stats_all_input) |> 
      pivot_longer(cols = c(enrolled, applied, admitted),
                   names_to = "admin_tots", values_to = "counts") |>  
      mutate(admin_tots = factor(admin_tots, 
                                 levels = c("applied", "admitted", "enrolled"),
                                 labels = c("Applied", "Admitted", "Enrolled")))
    
  }) 
  
  #..............create reactive df of admissions rate.............
  admissions_rate_df <- reactive({
    
    admissions |> 
      group_by(program) |> 
      summarize(mean = round(mean(admit_rate_pct), 1)) |> 
      filter(program == input$admit_stats_all_input)
    
  }) 
  
  #..........................render plotly.........................
  renderPlotly({
    
    # create ggplot ----
    admissions_all_plot <- ggplot(data = admissions_stacked_all_df(), aes(x = admission_year, y = counts, fill = reorder(admin_tots, counts))) +
      geom_bar(data = admissions_stacked_all_df() |> filter(admin_tots == "Applied"),
               stat = "identity", aes(text = paste0("Applied: ", counts))) +
      geom_bar(data = admissions_stacked_all_df() |> filter(admin_tots == "Admitted"),
               stat = "identity", width = 0.75, aes(text = paste0("Admitted: ", counts))) +
      geom_bar(data = admissions_stacked_all_df() |> filter(admin_tots == "Enrolled"),
               stat = "identity", width = 0.6, aes(text = paste0("Enrolled: ", counts))) +
      scale_x_continuous(breaks = seq(min(admissions_stacked_all_df()$admission_year),
                                      max(admissions_stacked_all_df()$admission_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c("Applied" = "#dcd6cc", "Admitted" = "#9cbebe", "Enrolled" = "#003660")) +
      labs(title = paste0(input$admit_stats_all_input, " Admissions", "\n", "<i>", "<sup>", "Average acceptance rate: ", 
                          admissions_rate_df()$mean, "%", "</i>", "</sup>"),
           x = NULL, y = NULL, fill = NULL)
    
    # convert to plotly ----
    plotly::ggplotly(admissions_all_plot, tooltip = "text") |> 
      config(displayModeBar = FALSE)
    
  }) 
  
}