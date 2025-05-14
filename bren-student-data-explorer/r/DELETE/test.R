# salarySpecialization_plot <- function(input, data) {
#   
#   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ##                               Data Wrangling                             ----
#   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #........wrangle reactive df of salary by specialization.........
#   salary_specialization <- reactive({
#     
#     radioButton_yearInput <- input$mesm_salary_by_specialization_year
#     placement_size <- mesm_placement_size
#     
#     #...................if `All Years` is selected...................
#     if (radioButton_yearInput == "All Years") { 
#       
#       mesm_placement |> 
#         select(class_year, 
#                mesm_program_enrollment_specializations, 
#                employment_type, 
#                compensation_frequency, 
#                estimated_annual_compensation_us) |>
#         filter(employment_type == "Full-Time Job") |>
#         filter(estimated_annual_compensation_us != 0) |>
#         filter(compensation_frequency != "Stipend") |> 
#         filter(!is.na(mesm_program_enrollment_specializations)) |> 
#         mutate(mesm_program_enrollment_specializations = case_when(
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
#           TRUE ~ mesm_program_enrollment_specializations
#         )) |> 
#         group_by(mesm_program_enrollment_specializations) |> 
#         mutate(Median = median(estimated_annual_compensation_us)) |>
#         mutate(Low = min(estimated_annual_compensation_us)) |>
#         mutate(High = max(estimated_annual_compensation_us)) |> 
#         pivot_longer(cols = c(Low, High, Median),
#                      names_to = "range", values_to = "values") |> 
#         mutate(range = fct_relevel(range, c("Low", "Median", "High")))
#       
#       } # END if `All Years` is selected
#     
#     #.................if any single year is selected.................
#     else {
#       
#       mesm_placement |> 
#         select(class_year, 
#                mesm_program_enrollment_specializations, 
#                employment_type, 
#                compensation_frequency, 
#                estimated_annual_compensation_us) |>
#         filter(employment_type == "Full-Time Job") |>
#         filter(estimated_annual_compensation_us != 0) |>
#         filter(compensation_frequency != "Stipend") |> 
#         filter(!is.na(mesm_program_enrollment_specializations)) |> 
#         mutate(mesm_program_enrollment_specializations = case_when(
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
#           TRUE ~ mesm_program_enrollment_specializations
#         )) |> 
#         filter(class_year == radioButton_yearInput) |>
#         group_by(mesm_program_enrollment_specializations) |> 
#         mutate(Median = median(estimated_annual_compensation_us)) |>
#         mutate(Low = min(estimated_annual_compensation_us)) |>
#         mutate(High = max(estimated_annual_compensation_us)) |> 
#         pivot_longer(cols = c(Low, High, Median),
#                      names_to = "range", values_to = "values") |> 
#         mutate(class_year = as.factor(class_year)) |> 
#         mutate(range = fct_relevel(range, c("Low", "Median", "High")))
#       
#     } # END if `any single year` is selected
#     
#   }) # END salary_specialization reactive df
#   
#   #..............wrangle reactive df of salary ranges..............
#   salary_range <- reactive({
#     
#     radioButton_yearInput <- input$mesm_salary_by_specialization_year
#     placement_size <- mesm_placement_size
#     
#     #...................if `All Years` is selected...................
#     if (radioButton_yearInput == "All Years") { 
#       
#       mesm_placement |> 
#         select(class_year, 
#                mesm_program_enrollment_specializations, 
#                employment_type, 
#                compensation_frequency, 
#                estimated_annual_compensation_us) |>
#         filter(employment_type == "Full-Time Job") |>
#         filter(estimated_annual_compensation_us != 0) |>
#         filter(compensation_frequency != "Stipend") |> 
#         filter(!is.na(mesm_program_enrollment_specializations)) |> 
#         mutate(mesm_program_enrollment_specializations = case_when(
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
#           TRUE ~ mesm_program_enrollment_specializations
#         )) |> 
#         group_by(mesm_program_enrollment_specializations) |> 
#         mutate(Median = median(estimated_annual_compensation_us)) |>
#         mutate(Low = min(estimated_annual_compensation_us)) |>
#         mutate(High = max(estimated_annual_compensation_us)) |> 
#         pivot_longer(cols = c(Low, High, Median),
#                      names_to = "range", values_to = "values") |> 
#         mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
#         group_by(mesm_program_enrollment_specializations) |> 
#         summarize(min_val = min(values), 
#                   max_val = max(values),
#                   median_val = median(values))
#       
#     } # END if `All Years` is selected
#     
#     #.................if any single year is selected.................
#     else {
#       
#       mesm_placement |> 
#         select(class_year, 
#                mesm_program_enrollment_specializations, 
#                employment_type, 
#                compensation_frequency, 
#                estimated_annual_compensation_us) |>
#         filter(employment_type == "Full-Time Job") |>
#         filter(estimated_annual_compensation_us != 0) |>
#         filter(compensation_frequency != "Stipend") |> 
#         filter(!is.na(mesm_program_enrollment_specializations)) |> 
#         mutate(mesm_program_enrollment_specializations = case_when(
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
#           mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
#           mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
#           mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
#           TRUE ~ mesm_program_enrollment_specializations
#         )) |> 
#         filter(class_year == radioButton_yearInput) |>
#         group_by(mesm_program_enrollment_specializations) |> 
#         mutate(Median = median(estimated_annual_compensation_us)) |>
#         mutate(Low = min(estimated_annual_compensation_us)) |>
#         mutate(High = max(estimated_annual_compensation_us)) |> 
#         pivot_longer(cols = c(Low, High, Median),
#                      names_to = "range", values_to = "values") |> 
#         mutate(class_year = as.factor(class_year)) |> 
#         mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
#         group_by(class_year, mesm_program_enrollment_specializations) |> 
#         summarize(min_val = min(values), 
#                   max_val = max(values),
#                   median_val = median(values))
#       
#     } # END if `any single year` is selected
#     
#   }) # END salary_specialization reactive df
#   
#   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ##                                Visualization                             ----
#   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #..............render sector by salary plotly object.............
#   plotly::renderPlotly({
#     
#     #...................if `All Years` is selected...................
#     if (radioButton_yearInput == "All Years") { 
#       
#       ggplot() +
#         geom_segment(data = salary_range(),
#                      aes(x = min_val, xend = max_val,
#                          y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
#         geom_point(data = salary_specialization(), aes(x = values, y = mesm_program_enrollment_specializations, 
#                                          fill = range, shape = range, size = range,
#                                          text = paste0(range, ": $",  round(values, 2)))) +
#         geom_point(data = salary_specialization(), 
#                    aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
#                    color = "gray50", alpha = 0.8, size = 1.5) +
#         scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
#         scale_shape_manual(values = c(21, 24, 21)) +
#         scale_size_manual(values = c(6, 6, 6)) +
#         scale_x_continuous(labels = scales::dollar_format()) +
#         labs(title = paste0("MESM Salaries by Specialization", "\n", 
#                             "(", allYrs_response, "/", allYrs_size, " survey respondents)"),
#              x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
#         theme_minimal() +
#         theme(
#           legend.position = "bottom",
#           legend.title = element_blank()
#         )
#       
#       } # END if `All Years` is selected
#     
#     #................if `any single year` is selected................
#     else {
#       
#       ggplot() +
#         geom_segment(data = salary_range(),
#                      aes(x = min_val, xend = max_val,
#                          y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
#         geom_point(data = salary_specialization(), aes(x = values, y = mesm_program_enrollment_specializations, 
#                                            fill = range, shape = range, size = range,
#                                            text = paste0(range, ": $",  round(values, 2)))) +
#         geom_point(data = salary_specialization(), 
#                    aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
#                    color = "gray50", alpha = 0.8, size = 1.5) +
#         scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
#         scale_shape_manual(values = c(21, 24, 21)) +
#         scale_size_manual(values = c(6, 6, 6)) +
#         scale_x_continuous(labels = scales::dollar_format()) +
#         labs(title = paste0("MESM Salaries by Specialization", "\n", 
#                             "(", yr_response, "/", yr_size, " survey respondents)"),
#              x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
#         theme_minimal() +
#         theme(
#           legend.position = "bottom",
#           legend.title = element_blank()
#         )
#       
#     } # END if `any single year` is selected
#     
#     
#   }) # END render plotly
#   
# } # END fxn
# 
# 
# 
# # all_years <- mesm_placement |> 
# #   select(class_year, 
# #          mesm_program_enrollment_specializations, 
# #          employment_type, 
# #          compensation_frequency, 
# #          estimated_annual_compensation_us) |>
# #   filter(employment_type == "Full-Time Job") |>
# #   filter(estimated_annual_compensation_us != 0) |>
# #   filter(compensation_frequency != "Stipend") |> 
# #   filter(!is.na(mesm_program_enrollment_specializations)) |> 
# #   mutate(mesm_program_enrollment_specializations = case_when(
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
# #     TRUE ~ mesm_program_enrollment_specializations
# #   )) |> 
# #   group_by(mesm_program_enrollment_specializations) |> 
# #   mutate(Median = median(estimated_annual_compensation_us)) |>
# #   mutate(Low = min(estimated_annual_compensation_us)) |>
# #   mutate(High = max(estimated_annual_compensation_us)) |> 
# #   pivot_longer(cols = c(Low, High, Median),
# #                names_to = "range", values_to = "values") |> 
# #   mutate(range = fct_relevel(range, c("Low", "Median", "High")))
# 
# # all_years_range <- mesm_placement |> 
# #   select(class_year, 
# #          mesm_program_enrollment_specializations, 
# #          employment_type, 
# #          compensation_frequency, 
# #          estimated_annual_compensation_us) |>
# #   filter(employment_type == "Full-Time Job") |>
# #   filter(estimated_annual_compensation_us != 0) |>
# #   filter(compensation_frequency != "Stipend") |> 
# #   filter(!is.na(mesm_program_enrollment_specializations)) |> 
# #   mutate(mesm_program_enrollment_specializations = case_when(
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
# #     TRUE ~ mesm_program_enrollment_specializations
# #   )) |> 
# #   group_by(mesm_program_enrollment_specializations) |> 
# #   mutate(Median = median(estimated_annual_compensation_us)) |>
# #   mutate(Low = min(estimated_annual_compensation_us)) |>
# #   mutate(High = max(estimated_annual_compensation_us)) |> 
# #   pivot_longer(cols = c(Low, High, Median),
# #                names_to = "range", values_to = "values") |> 
# #   mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
# #   group_by(mesm_program_enrollment_specializations) |> 
# #   summarize(min_val = min(values), 
# #             max_val = max(values),
# #             median_val = median(values))
# 
# # ggplot() +
# #   geom_segment(data = all_years_range,
# #                aes(x = min_val, xend = max_val,
# #                    y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
# #   geom_point(data = all_years, aes(x = values, y = mesm_program_enrollment_specializations, 
# #                                      fill = range, shape = range, size = range,
# #                                      text = paste0(range, ": $",  round(values, 2)))) +
# #   geom_point(data = all_years, 
# #              aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
# #              color = "gray50", alpha = 0.8, size = 1.5) +
# #   scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# #   scale_shape_manual(values = c(21, 24, 21)) +
# #   scale_size_manual(values = c(6, 6, 6)) +
# #   scale_x_continuous(labels = scales::dollar_format()) +
# #   labs(title = paste0("MESM Salaries by Specialization", "\n", 
# #                       "(", allYrs_response, "/", allYrs_size, " survey respondents)"),
# #        x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# #   theme_minimal() +
# #   theme(
# #     legend.position = "bottom",
# #     legend.title = element_blank()
# #   )
# 
# # single year ------------------------
# 
# # single_year <- mesm_placement |> 
# #   select(class_year, 
# #          mesm_program_enrollment_specializations, 
# #          employment_type, 
# #          compensation_frequency, 
# #          estimated_annual_compensation_us) |>
# #   filter(employment_type == "Full-Time Job") |>
# #   filter(estimated_annual_compensation_us != 0) |>
# #   filter(compensation_frequency != "Stipend") |> 
# #   filter(!is.na(mesm_program_enrollment_specializations)) |> 
# #   mutate(mesm_program_enrollment_specializations = case_when(
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
# #     TRUE ~ mesm_program_enrollment_specializations
# #   )) |> 
# #   filter(class_year == radioButton_yearInput) |>
# #   group_by(mesm_program_enrollment_specializations) |> 
# #   mutate(Median = median(estimated_annual_compensation_us)) |>
# #   mutate(Low = min(estimated_annual_compensation_us)) |>
# #   mutate(High = max(estimated_annual_compensation_us)) |> 
# #   pivot_longer(cols = c(Low, High, Median),
# #                names_to = "range", values_to = "values") |> 
# #   mutate(class_year = as.factor(class_year)) |> 
# #   mutate(range = fct_relevel(range, c("Low", "Median", "High"))) 
# 
# # single_year_range <- mesm_placement |> 
# #   select(class_year, 
# #          mesm_program_enrollment_specializations, 
# #          employment_type, 
# #          compensation_frequency, 
# #          estimated_annual_compensation_us) |>
# #   filter(employment_type == "Full-Time Job") |>
# #   filter(estimated_annual_compensation_us != 0) |>
# #   filter(compensation_frequency != "Stipend") |> 
# #   filter(!is.na(mesm_program_enrollment_specializations)) |> 
# #   mutate(mesm_program_enrollment_specializations = case_when(
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE); Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Economics and Politics of the Environment; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Corporate Environmental Management; Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Conservation Planning; Water Resources Management" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Conservation Planning" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM); Business and Sustainability (formerly CEM)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Environmental Policy (formerly EPE)" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM); Energy and Climate" ~ "Dual Specialization",
# #     mesm_program_enrollment_specializations == "Business and Sustainability (formerly CEM)" ~ "Business and Sustainability",
# #     mesm_program_enrollment_specializations == "Coastal Resources Management (formerly CMRM)" ~ "Coastal Resources Management",
# #     mesm_program_enrollment_specializations == "Environmental Policy (formerly EPE)" ~ "Environmental Policy",
# #     TRUE ~ mesm_program_enrollment_specializations
# #   )) |> 
# #   filter(class_year == radioButton_yearInput) |>
# #   group_by(mesm_program_enrollment_specializations) |> 
# #   mutate(Median = median(estimated_annual_compensation_us)) |>
# #   mutate(Low = min(estimated_annual_compensation_us)) |>
# #   mutate(High = max(estimated_annual_compensation_us)) |> 
# #   pivot_longer(cols = c(Low, High, Median),
# #                names_to = "range", values_to = "values") |> 
# #   mutate(class_year = as.factor(class_year)) |> 
# #   mutate(range = fct_relevel(range, c("Low", "Median", "High"))) |> 
# #   group_by(class_year, mesm_program_enrollment_specializations) |> 
# #   summarize(min_val = min(values), 
# #             max_val = max(values),
# #             median_val = median(values))
# 
# 
# # ggplot() +
# #   geom_segment(data = single_year_range,
# #                aes(x = min_val, xend = max_val,
# #                    y = fct_reorder(mesm_program_enrollment_specializations, median_val), yend = mesm_program_enrollment_specializations), color = "black") +
# #   geom_point(data = single_year, aes(x = values, y = mesm_program_enrollment_specializations, 
# #                                          fill = range, shape = range, size = range,
# #                                          text = paste0(range, ": $",  round(values, 2)))) +
# #   geom_point(data = single_year, 
# #              aes(x = estimated_annual_compensation_us, y = mesm_program_enrollment_specializations),
# #              color = "gray50", alpha = 0.8, size = 1.5) +
# #   scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# #   scale_shape_manual(values = c(21, 24, 21)) +
# #   scale_size_manual(values = c(6, 6, 6)) +
# #   scale_x_continuous(labels = scales::dollar_format()) +
# #   labs(title = paste0("MESM Salaries by Specialization", "\n", 
# #                       "(", yr_response, "/", yr_size, " survey respondents)"),
# #        x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# #   theme_minimal() +
# #   theme(
# #     legend.position = "bottom",
# #     legend.title = element_blank()
# #   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # salaryBySector_plot <- function(input, data, program_acronym) { 
# #   
# #   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #   ##                               Data Wrangling                             ----
# #   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #   
# #   #............wrangle reactive df of salary by sector.............
# #   salary_sector <- reactive({
# #     
# #     #............get appropriate inputId, df, response num...........
# #     if (program_acronym == "MESM") {
# #       
# #       radioButton_yearInput <- input$mesm_salarySector_year
# #       placement_size <- mesm_placement_size
# #       
# #     } else if (program_acronym == "MEDS") {
# #       
# #       radioButton_yearInput <- input$meds_salarySector_year
# #       placement_size <- meds_placement_size
# #       
# #     }
# #     
# #     #...................if `All Years` is selected...................
# #     if (radioButton_yearInput == "All Years") { 
# #       
# #       data |> 
# #         select(class_year, employment_type, employer_sector, compensation_frequency,
# #                estimated_annual_compensation_us) |>
# #         filter(employment_type == "Full-Time Job") |>
# #         filter(estimated_annual_compensation_us != 0) |>
# #         filter(compensation_frequency != "Stipend") |>
# #         group_by(class_year, employer_sector) |> 
# #         mutate(Median = median(estimated_annual_compensation_us)) |>
# #         mutate(Low = min(estimated_annual_compensation_us)) |>
# #         mutate(High = max(estimated_annual_compensation_us)) |>
# #         left_join(placement_size, by = "class_year") |> 
# #         mutate(class_year = as.factor(class_year)) |> 
# #         pivot_longer(cols = c(Low, High, Median),
# #                      names_to = "range", values_to = "values") |> 
# #         mutate(class_year = as.factor(class_year)) |> 
# #         mutate(range = fct_relevel(range, c("Low", "Median", "High")))
# #       
# #     } # END if 'All Years' is selected
# #     
# #     #.................if any single year is selected.................
# #     else {
# #       
# #       data |> 
# #         select(class_year, employment_type, employer_sector, compensation_frequency,
# #                estimated_annual_compensation_us) |>
# #         filter(employment_type == "Full-Time Job") |>
# #         filter(estimated_annual_compensation_us != 0) |>
# #         filter(compensation_frequency != "Stipend") |>
# #         group_by(class_year, employer_sector) |> 
# #         mutate(Median = median(estimated_annual_compensation_us)) |>
# #         mutate(Low = min(estimated_annual_compensation_us)) |>
# #         mutate(High = max(estimated_annual_compensation_us)) |>
# #         left_join(placement_size, by = "class_year") |> 
# #         mutate(class_year = as.factor(class_year)) |> 
# #         filter(class_year == radioButton_yearInput) |> 
# #         pivot_longer(cols = c(Low, High, Median),
# #                      names_to = "range", values_to = "values") |> 
# #         mutate(class_year = as.factor(class_year)) |> 
# #         mutate(range = fct_relevel(range, c("Low", "Median", "High")))
# #       
# #     } # END if 'any single year' is selected
# #     
# #   }) # END reactive salary_sector df
# #   
# #   #..........wrangle reactive df of highlighted salaries...........
# #   highlighted_salaries <- reactive({
# #     
# #     #............get appropriate inputId, df, response num...........
# #     if (program_acronym == "MESM") {
# #       
# #       radioButton_yearInput <- input$mesm_salarySector_year
# #       placement_size <- mesm_placement_size
# #       
# #     } else if (program_acronym == "MEDS") {
# #       
# #       radioButton_yearInput <- input$meds_salarySector_year
# #       placement_size <- meds_placement_size
# #       
# #     }
# #     
# #     #...................if `All Years` is selected...................
# #     if (radioButton_yearInput == "All Years") { 
# #       
# #       salary_sector() |> 
# #         group_by(employer_sector) |> 
# #         summarize(min_val = min(values), 
# #                   max_val = max(values),
# #                   median_val = median(values))
# #       
# #     } # END if 'All Years' is selected
# #     
# #     #.................if any single year is selected.................
# #     else {
# #       
# #       salary_sector() |> 
# #         group_by(class_year, employer_sector) |> 
# #         summarize(min_val = min(values), 
# #                   max_val = max(values),
# #                   median_val = median(values))
# #       
# #     } # END if 'any single year' is selected
# #     
# #   }) # END reactive salary_sector df
# #   
# # }
# # 
# # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ##                                Visualization                             ----
# # ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 
# # #..............render sector by salary plotly object.............
# # plotly::renderPlotly({
# #   
# #   #........get values necessary for constructing plot title........
# #   if (program_acronym == "MESM") {
# #     
# #     radioButton_yearInput <- input$mesm_salarySector_year
# #     selected_class_year <- radioButton_yearInput
# #     placement_size <- mesm_placement_size
# #     allYrs_size <- sum(placement_size$program_size)
# #     allYrs_response <- sum(placement_size$responses)
# #     yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
# #     yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
# #     
# #   } else if (program_acronym == "MEDS") {
# #     
# #     radioButton_yearInput <- input$meds_salarySector_year
# #     selected_class_year <- radioButton_yearInput
# #     placement_size <- meds_placement_size
# #     allYrs_size <- sum(placement_size$program_size)
# #     allYrs_response <- sum(placement_size$responses)
# #     yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
# #     yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
# #     
# #   }
# #   
# #   #...................if `All Years` is selected...................
# #   if (radioButton_yearInput == "All Years") { 
# #     
# #     #...................create ggplot object first...................
# #     comp_sector_gg <- ggplot() +
# #       geom_segment(data = salary_highlights(), 
# #                    aes(x = min_val, xend = max_val,
# #                        y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
# #       geom_point(data = salary_sector(), aes(x = values, y = employer_sector, 
# #                                        fill = range, shape = range, size = range,
# #                                        text = paste0(range,  ": $",  round(values, 2), "\n",
# #                                                      responses, "/", program_size, " survey respondents"))) +
# #       geom_point(data = salary_sector(), 
# #                  aes(x = estimated_annual_compensation_us, y = employer_sector),
# #                  color = "gray50", alpha = 0.8, size = 1.5) +
# #       scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# #       scale_shape_manual(values = c(21, 24, 21)) + 
# #       scale_size_manual(values = c(6, 6, 6)) +
# #       scale_x_continuous(labels = scales::dollar_format()) + 
# #       labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
# #                           "(", allYrs_response, "/", allYrs_size, " survey respondents)"), 
# #            x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# #       theme_minimal() +
# #       theme(
# #         legend.position = "bottom",
# #         legend.title = element_blank()
# #       )
# #     
# #   } # END if `All Years` is selected
# #   
# #   #.................if any single year is selected.................
# #   else {
# #     
# #     #...................create ggplot object first...................
# #     comp_sector_gg <- ggplot() +
# #       geom_segment(data = salary_highlights(),
# #                    aes(x = min_val, xend = max_val,
# #                        y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
# #       geom_point(data = salary_sector(), aes(x = values, y = employer_sector,
# #                                              fill = range, shape = range, size = range,
# #                                              text = paste0(range, ": $",  round(values, 2), "\n",
# #                                                            responses, "/", program_size, " survey respondents"))) +
# #       geom_point(data = salary_sector(), 
# #                  aes(x = estimated_annual_compensation_us, y = employer_sector),
# #                  color = "gray50", alpha = 0.8, size = 1.5) +
# #       scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# #       scale_shape_manual(values = c(21, 24, 21)) +
# #       scale_size_manual(values = c(6, 6, 6)) +
# #       scale_x_continuous(labels = scales::dollar_format()) +
# #       labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
# #                           "(", yr_response, "/", yr_size, " survey respondents)"),
# #            x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# #       theme_minimal() +
# #       theme(
# #         legend.position = "bottom",
# #         legend.title = element_blank()
# #       )
# #     
# #   } # END if `any single year` is selected
# #   
# #   #..................then convert to plotly object.................
# #   plotly::ggplotly(comp_sector_gg, tooltip = "text") |> 
# #     layout(title = list(font = list(size = 16)),
# #            legend = list(orientation = "h", x = 0.3, y = -0.2)) |>
# #     config(displayModeBar = FALSE)
# #   
# # }) # END render plotly
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # --------------------------
# # 
# # # data <- meds_placement
# # # radioButton_yearInput <- 2023
# # # program_acronym <- "MEDS"
# # # selected_class_year <- radioButton_yearInput
# # # placement_size <- meds_placement_size
# # # allYrs_size <- sum(placement_size$program_size)
# # # allYrs_response <- sum(placement_size$responses)
# # # yr_size <- placement_size |> filter(class_year == selected_class_year) |> pull(program_size)
# # # yr_response <- placement_size |> filter(class_year == selected_class_year) |> pull(responses)
# # # 
# # 
# # # single_year <- data |> 
# # #   select(class_year, employment_type, employer_sector, compensation_frequency,
# # #          estimated_annual_compensation_us) |>
# # #   filter(employment_type == "Full-Time Job") |>
# # #   filter(estimated_annual_compensation_us != 0) |>
# # #   filter(compensation_frequency != "Stipend") |>
# # #   group_by(class_year, employer_sector) |> 
# # #   mutate(Median = median(estimated_annual_compensation_us)) |>
# # #   mutate(Low = min(estimated_annual_compensation_us)) |>
# # #   mutate(High = max(estimated_annual_compensation_us)) |>
# # #   left_join(placement_size, by = "class_year") |> 
# # #   mutate(class_year = as.factor(class_year)) |> 
# # #   filter(class_year == radioButton_yearInput) |> 
# # #   pivot_longer(cols = c(Low, High, Median),
# # #                names_to = "range", values_to = "values") |> 
# # #   mutate(class_year = as.factor(class_year)) |> 
# # #   mutate(range = fct_relevel(range, c("Low", "Median", "High")))
# # # 
# # # salary_highlights <- single_year |> 
# # #   group_by(class_year, employer_sector) |> 
# # #   summarize(min_val = min(values), 
# # #             max_val = max(values),
# # #             median_val = median(values))
# # 
# # 
# # # ggplot() +
# # #   geom_segment(data = salary_highlights,
# # #                aes(x = min_val, xend = max_val,
# # #                    y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
# # #   geom_point(data = single_year, aes(x = values, y = employer_sector,
# # #                                      fill = range, shape = range, size = range,
# # #                                      text = paste0(range, ": $",  round(values, 2), "\n",
# # #                                                    responses, "/", program_size, " survey respondents"))) +
# # #   geom_point(data = single_year, aes(x = estimated_annual_compensation_us, y = employer_sector),
# # #              color = "gray50", alpha = 0.8, size = 1.5) +
# # #   scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# # #   scale_shape_manual(values = c(21, 24, 21)) +
# # #   scale_size_manual(values = c(6, 6, 6)) +
# # #   scale_x_continuous(labels = scales::dollar_format()) +
# # #   labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
# # #                       "(", yr_response, "/", yr_size, " survey respondents)"),
# # #        x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# # #   theme_minimal() +
# # #   theme(
# # #     legend.position = "bottom",
# # #     legend.title = element_blank()
# # #   )
# # 
# # 
# # 
# # # -----------------------------------------------------
# # 
# # # all_years <- data |> 
# # #   select(class_year, employment_type, employer_sector, compensation_frequency,
# # #          estimated_annual_compensation_us) |>
# # #   filter(employment_type == "Full-Time Job") |>
# # #   filter(estimated_annual_compensation_us != 0) |>
# # #   filter(compensation_frequency != "Stipend") |>
# # #   group_by(class_year, employer_sector) |> 
# # #   mutate(Median = median(estimated_annual_compensation_us)) |>
# # #   mutate(Low = min(estimated_annual_compensation_us)) |>
# # #   mutate(High = max(estimated_annual_compensation_us)) |>
# # #   left_join(placement_size, by = "class_year") |> 
# # #   mutate(class_year = as.factor(class_year)) |> 
# # #   pivot_longer(cols = c(Low, High, Median),
# # #                names_to = "range", values_to = "values") |> 
# # #   mutate(class_year = as.factor(class_year)) |> 
# # #   mutate(range = fct_relevel(range, c("Low", "Median", "High")))
# # 
# # # salary_highlights <- all_years |> 
# # #   group_by(employer_sector) |> 
# # #   summarize(min_val = min(values), 
# # #             max_val = max(values),
# # #             median_val = median(values))
# # 
# # 
# # # ggplot() +
# # #   geom_segment(data = salary_highlights, 
# # #                aes(x = min_val, xend = max_val,
# # #                    y = fct_reorder(employer_sector, median_val), yend = employer_sector), color = "black") +
# # #   geom_point(data = all_years, aes(x = values, y = employer_sector, 
# # #                                    fill = range, shape = range, size = range,
# # #                                    text = paste0(range,  ": $",  round(values, 2), "\n",
# # #                                                  responses, "/", program_size, " survey respondents"))) +
# # #   geom_point(data = all_years, aes(x = estimated_annual_compensation_us, y = employer_sector),
# # #              color = "gray50", alpha = 0.8, size = 1.5) +
# # #   scale_fill_manual(values = c("#9CBEBD", "#047c91", "#003660")) +
# # #   scale_shape_manual(values = c(21, 24, 21)) + 
# # #   scale_size_manual(values = c(6, 6, 6)) +
# # #   scale_x_continuous(labels = scales::dollar_format()) + 
# # #   labs(title = paste0(program_acronym ," Initial Job Placement Salaries by Sector", "\n",
# # #                       "(", allYrs_response, "/", allYrs_size, " survey respondents)"), 
# # #        x = NULL, y = NULL, fill = NULL, shape = NULL, size = NULL) +
# # #   theme_minimal() +
# # #   theme(
# # #     legend.position = "bottom",
# # #     legend.title = element_blank()
# # #   )
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
