
# just salary ---------------

# mesm ----
placement <- mesm_placement
placement_size <- mesm_placement_size
response_num <- sum(placement_size$responses)

# meds ----
placement <- meds_placement
placement_size <- meds_placement_size
response_num <- sum(placement_size$responses)

# wrangle sector ----
test2 <- placement %>% 
  select(class_year, employment_type, employer_sector, compensation_frequency,
         estimated_annual_compensation_us) |>
  filter(employment_type == "Full-Time Job") |>
  filter(estimated_annual_compensation_us != 0) |>
  filter(compensation_frequency != "Stipend") |>
  group_by(class_year, employer_sector) |> 
  mutate(Median = median(estimated_annual_compensation_us)) |>
  mutate(Low = min(estimated_annual_compensation_us)) |>
  mutate(High = max(estimated_annual_compensation_us)) |>
  #select(-estimated_annual_compensation_us) |>
  left_join(placement_size, by = "class_year") |> 
  mutate(class_year = as.factor(class_year)) |> 
  filter(class_year == 2023) 
  # filter(class_year == radioButton_yearInput) |>

ggplot(data = test2) +
  geom_segment(aes(x = Low, xend = High,
                   y = fct_reorder(employer_sector, Median), yend = employer_sector), color = "gray40") +
  geom_point(aes(x = Low, y = employer_sector, size = 3.25), fill = "#9CBEBD", shape = 21, size = 6) +
  geom_point(aes(x = Median, y = employer_sector), fill = "#047c91", shape = 24, size = 5) + # pch = "|", cex = 5
  geom_point(aes(x = High, y = employer_sector, size = 3.25), fill = "#003660", shape = 21, size = 6) +
  geom_point(aes(x = estimated_annual_compensation_us, y = employer_sector),
             pch = "|", cex = 3) +
  labs(title = "title", #paste0(program_acronym ," Alumni Low, Median, and High Salary Compensation"), #
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    #plot.title = ggtext::element_textbox_simple()
  )



# wrangle ----
test <- placement %>% 
  select(class_year, employment_type, compensation_frequency,
         estimated_annual_compensation_us) |>
  filter(employment_type == "Full-Time Job") |>
  filter(estimated_annual_compensation_us != 0) |>
  filter(compensation_frequency != "Stipend") |>
  group_by(class_year) |> 
  # filter(class_year %in% radioButton_yearInput) |>
  mutate(Median = median(estimated_annual_compensation_us)) |>
  mutate(Low = min(estimated_annual_compensation_us)) |>
  mutate(High = max(estimated_annual_compensation_us)) |>
  select(-estimated_annual_compensation_us) |>
  left_join(placement_size, by = "class_year") |> 
  mutate(class_year = as.factor(class_year))

# plot ----
formatted_title <- glue::glue("MEDS Alumni
                       <span style='color:#9CBEBD'>**Low**</span>,
                        <span style='color:#047c91'>**Median**</span>,
                    and <span style='color:#003660'>**High**</span>
                       Salaries")


ggplot(data = test2) +
  geom_segment(aes(x = Low, xend = High,
                   y = class_year, yend = class_year), color = "gray40") +
  geom_point(aes(x = Low, y = class_year, size = 3.25), fill = "#9CBEBD", shape = 21, size = 6) +
  geom_point(aes(x = Median, y = class_year), fill = "#047c91", shape = 24, size = 5) + # pch = "|", cex = 5
  geom_point(aes(x = High, y = class_year, size = 3.25), fill = "#003660", shape = 21, size = 6) +
  labs(title = formatted_title, #paste0(program_acronym ," Alumni Low, Median, and High Salary Compensation"), #
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    legend.position = "none",
    plot.title = ggtext::element_textbox_simple()
  )

