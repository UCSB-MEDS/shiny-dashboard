
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------------------- HEADER-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader(
  
  title = span(tags$a(img(src = "logos/bren_hex.png"), href = "https://bren.ucsb.edu/", target = "_blank"), "Bren Student Data Explorer") 
  
) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- SIDEBAR-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "sidebarID",
              
              menuItem(tabName = "career_page", text = "Career Outcomes", icon = icon("road", lib = "glyphicon")),
              menuItem(tabName = "demographics_page", text = "Demographics", icon = icon("user", lib = "glyphicon")),
              menuItem(tabName = "about_page", text = "About this Dashboard", icon = icon("star", lib = "glyphicon"), badgeColor = "green") 
              
  ) # END sidebarMenu
  
) # END dashboardSidebar

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------------- BODY--------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

body <- dashboardBody(
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##            body header (add stylesheet, google analytics, etc.)          ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("www/google-analytics.html")
  ), # END header
  
  useShinyjs(), # call shinyjs; Note(HD): initially added to use hidden() ----
  
  tabItems(

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                          career tab (career_page)                        ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # START career tabItem ----
    tabItem(tabName = "career_page",
            
            # START career tabsetPanel ----
            tabsetPanel(id = "career_tabpanels",
                        
                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        ##                            MEDS CAREER TABPANEL                          ----
                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        
                        # START MEDS career tabPanel ----
                        tabPanel(title = "MEDS Initial Career Placements",
                                 
                                 # START fluidRow ----
                                 fluidRow(
                                   
                                   # START left-hand column ----
                                   column(width = 4,
                                          
                                          # START fluidRow ----
                                          fluidRow(
                                            
                                            # START explore MEDS career info box ----
                                            box(width = 12,
                                                title = "About the MEDS Career Data",
                                                solidHeader = TRUE, status = "navy", 
                                                uiOutput("meds_about_career_data_text")) # END explore MEDS career info box
                                            
                                          ), # END fluidRow
                                          
                                          # START fluidRow (valueBoxes 1) ----
                                          fluidRow(
                                            
                                            valueBoxOutput(outputId = "meds_placement_stat", width = 6),
                                            valueBoxOutput(outputId = "meds_satisfied_stat", width = 6)
                                            
                                          ), # END fluidRow (valuBoxes 1)
                                          
                                          # START fluidRow (valueBoxes 2) ----
                                          fluidRow(
                                            
                                            valueBoxOutput(outputId = "meds_brenNet_stat", width = 12)
                                            
                                          ) # END fluidRow (valueBoxes 2)
                                          
                                   ), # END left-hand column
                                   
                                   # START right-hand column ----
                                   column(width = 8,
                                          
                                          # START meds career data visualizations tabBox ----
                                          tabBox(width = 12,
                                                 
                                                 # START (TAB 1) meds salary tabPanel ----
                                                 tabPanel(title = "Salary",
                                                          
                                                          # meds salary plotly output ----
                                                          plotlyOutput(outputId = "meds_salary_plot") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # caption ----
                                                          div(style = "text-align: right; font-style: italic;", "Data includes Full-Time Career positions only.")
                                                          
                                                 ), # END (TAB 1) meds salary tabPanel
                                                 
                                                 # START (TAB 2) meds salary by sector tabPanel ----
                                                 tabPanel(title = "Salary by Sector",
                                                          
                                                          # meds salary by sector plotly output ----
                                                          plotlyOutput(outputId = "meds_salary_by_sector_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "meds_salarySector_year_input"),
                                                          
                                                          # caption ----
                                                          div(style = "text-align: right; font-style: italic;", "Data includes Full-Time Career positions only.")
                                                          
                                                 ), # END (TAB 2) meds salary by sector tabPanel
                                                 
                                                 # START (TAB 3) meds placement status tabPanel ----
                                                 tabPanel(title = "Placement Status",
                                                          
                                                          # mesm placement plotly output ----
                                                          plotly::plotlyOutput(outputId = "meds_placement_status_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "meds_placementStatus_year_input")
                                                          
                                                 ), # END (TAB 3) meds placement status tabPanel
                                                 
                                                 # START (TAB 4) meds sector trends tabPanel ----
                                                 tabPanel(title = "Sector Trends",
                                                          
                                                          # meds sector trends plotly output ----
                                                          plotly::plotlyOutput(outputId = "meds_sector_trends_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "meds_sector_trends_year_input")
                                                          
                                                 ), # END (TAB 4) meds sector trends tabPanel
                                                 
                                                 # START (TAB 5) meds sector satisfaction tabPanel -----
                                                 tabPanel(title = "Sector Satisfaction",
                                                          
                                                          # meds sector satisfaction plotly output ----
                                                          plotly::plotlyOutput(outputId = "meds_sector_satisfaction_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # sector type radioButtons input ----
                                                          sectorType_radioButtons(inputId = "meds_sector_types_input", selected = "Consulting")
                                                          
                                                 ), # END (TAB 5) meds sector satisfaction tabPanel
                                                 
                                                 
                                                 # START (TAB 6) meds job source tabPanel ----
                                                 tabPanel(title = "Job Source",
                                                          
                                                          # mesm job source plotly output ---
                                                          plotly::plotlyOutput(outputId = "meds_job_source_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "meds_job_source_year_input")
                                                          
                                                 ) # END (TAB 6) meds job source tabPanel
                                                 
                                          ) # END meds career data visualizations tabBox
                                          
                                   ) # END right-hand column
                                   
                                 ), # END fluidRow
                                 
                                 # START fluidRow (contains location info & initial employers table) ----
                                 fluidRow(
                                   
                                   # START mesm career location tabBox ----
                                   tabBox(width = 6,
                                          
                                          # START (TAB 1) meds domestic placement map tabPanel ----
                                          tabPanel(title = HTML("Domestic Placement"),
                                                   
                                                   # meds leaflet output ----
                                                   leaflet::leafletOutput(outputId = "meds_domesticPlacement_map") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ), # END (TAB 1) meds domestic placement map tabPanel
                                          
                                          # START (TAB 2) meds international placement tabPanel ----
                                          
                                          tabPanel(title = HTML("International Placement"),
                                                   
                                                   # DT datatable output ----
                                                   DT::dataTableOutput(outputId = "meds_internationalPlacement_tbl") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ), # END (TAB 2) meds international placement tabPanel
                                          
                                          # START (TAB 3) meds geographic comparison tabPanel ----
                                          tabPanel("Geographic Comparison",
                                                   
                                                   # meds geographic comparison plotly output ----
                                                   plotly::plotlyOutput(outputId = "meds_geogComparison_plot") |>
                                                     withSpinner(color = "#003660", type = 1),
                                                   
                                                   # year radioButtons input ----
                                                   year_radioButtons(inputId = "meds_placementLocation_year_input"),
                                                   
                                                   # note about percentages that don't add to 100% ----
                                                   div(style = "text-align: right; font-style: italic;", includeMarkdown("text/geog_comparison_perc.md"))
                                                   
                                          ) # (TAB 3) meds geographic comparison tabPanel
                                          
                                   ), # END meds career location tabBox
                                   
                                   # START MEDS initial employers & titles
                                   tabBox(
                                     
                                     # START (TAB 1) meds initial employers -----
                                     tabPanel("Initial Employers and Sectors",
                                              
                                              # meds initial employers & sectors table ----
                                              DT::dataTableOutput(outputId = "meds_career_employ_sector_tbl") |>
                                                withSpinner(color = "#003660", type = 1)), # END (TAB 1)
                                     
                                     # START (TAB 2) meds initial titls ----
                                     tabPanel("Job Titles",
                                              
                                              # confidentiality statement ----
                                              includeMarkdown("text/confidentiality.md"),
                                              
                                              # START meds initial employers & sectors table ----
                                              DT::dataTableOutput(outputId = "meds_title_tbl") |>
                                                withSpinner(color = "#003660", type = 1)
                                              
                                     ) # END (TAB 2)
                                     
                                   ) # END MEDS tabBox (initial employers & titles)
                                   
                                 ) # END fluidRow (contains location info & initial employers table)
                                 
                        ), # END MEDS career tabPanel
                        
                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        ##                            MESM CAREER TABPANEL                          ----
                        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        
                        # START MESM career tabPanel ----
                        tabPanel(title = "MESM Initial Career Placements",
                                 
                                 # START fluidRow ----
                                 fluidRow(
                                   
                                   # START left-hand column ----
                                   column(width = 4,
                                          
                                          # START fluidRow ----
                                          fluidRow(
                                            
                                            # START explore MESM career info box ----
                                            box(width = 12,
                                                title = "About the MESM Career Data",
                                                solidHeader = TRUE, status = "navy", 
                                                uiOutput("mesm_about_career_data_text")
                                            ) # END explore MESM career info box
                                            
                                          ), # END fluidRow
                                          
                                          # START fluidRow (valueBoxes 1) ----
                                          fluidRow(
                                            
                                            valueBoxOutput(outputId = "placement_stat", width = 6),
                                            valueBoxOutput(outputId = "mesm_satisfied_stat", width = 6)
                                            
                                          ), # END fluidRow (valuBoxes 1)
                                          
                                          # START fluidRow (valueBoxes 2) ----
                                          fluidRow(
                                            
                                            valueBoxOutput(outputId = "mesm_brenNet_stat", width = 12)
                                            
                                          ) # END fluidRow (valueBoxes 2)
                                          
                                   ), # END left-hand column
                                   
                                   # START right-hand column ----
                                   column(width = 8,
                                          
                                          # START mesm career data visualizations tabBox ----
                                          tabBox(width = 12,
                                                 
                                                 # START (TAB 1) mesm salary tabPanel ----
                                                 tabPanel(title = "Salary",
                                                          
                                                          # mesm salary plotly output ----
                                                          plotlyOutput(outputId = "mesm_salary_plot") |> withSpinner(color = "#003660", type = 1),
                                                          
                                                          # caption ----
                                                          div(style = "text-align: right; font-style: italic;", "Data includes Full-Time Career positions only.")

                                                 ), # END (TAB 1) mesm salary tabPanel
                                                 
                                                 # START (TAB 2) mesm salary by sector tabPanel ----
                                                 tabPanel(title = "Salary by Sector",
                                                          
                                                          # mesm salary by sector plotly output ----
                                                          plotlyOutput(outputId = "mesm_salary_by_sector_plot") |>
                                                            withSpinner(color = "#003660", type = 1),

                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_salarySector_year_input"),
                                                          
                                                          # caption ----
                                                          div(style = "text-align: right; font-style: italic;", "Data includes Full-Time Career positions only.")
                                                          
                                                 ), # END (TAB 2) mesm salary by sector tabPanel
                                                 
                                                 # START (TAB 3) mesm salary by specialization tabPanel ----
                                                 tabPanel(title = "Salary by Specialization",
                                                          
                                                          # mesm salary by specialization plotly output ----
                                                          plotlyOutput(outputId = "mesm_salary_by_specialization_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_salary_by_specialization_year_input"),
                                                          
                                                          # caption ----
                                                          div(style = "text-align: right; font-style: italic;", "Data includes Full-Time Career positions only.")
                                                          
                                                 ), # END (TAB 3) mesm salary by specialization tabPanel
                                                 
                                                 # START (TAB 4) mesm placement status tabPanel ----
                                                 tabPanel(title = "Placement Status",
                                                          
                                                          # mesm placement plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_placement_status_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_placementStatus_year_input")
                                                          
                                                 ), # END (TAB 4) mesm placement status tabPanel
                                                 
                                                 # START (TAB 5) mesm sector trends tabPanel ----
                                                 tabPanel(title = "Sector Trends",
                                                          
                                                          # mesm sector trends plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_sector_trends_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_sector_trends_year_input")
                                                          
                                                 ), # END (TAB 5) mesm sector trends tabPanel
                                                 
                                                 # START (TAB 6) mesm sector satisfaction tabPanel -----
                                                 tabPanel(title = "Sector Satisfaction",
                                                          
                                                          # mesm sector satisfaction plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_sector_satisfaction_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # sector type radioButtons input ----
                                                          sectorType_radioButtons(inputId = "mesm_sector_types_input", selected = "Consulting")
                                                          
                                                 ), # END (TAB 6) mesm sector satisfaction tabPanel
                                                 
                                                 # START (TAB 7) mesm job source tabPanel ----
                                                 tabPanel(title = "Job Source",
                                                          
                                                          # mesm job source plotly output ---
                                                          plotly::plotlyOutput(outputId = "mesm_job_source_plot") |>
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_job_source_year_input")
                                                          
                                                 ) # END (TAB 7) mesm job source tabPanel
                                                 
                                          ) # END mesm career data visualizations tabBox
                                          
                                   ) # END right-hand column
                                   
                                 ), # END fluidRow
                                 
                                 # START fluidRow (contains location info & initial employers table) ----
                                 fluidRow(
                                   
                                   # START mesm career location tabBox ----
                                   tabBox(width = 6,
                                          
                                          # START (TAB 1) mesm domestic placement map tabPanel ----
                                          tabPanel(title = HTML("Domestic Placement"),
                                                   
                                                   # mesm leaflet output ----
                                                   leaflet::leafletOutput(outputId = "mesm_domesticPlacement_map") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ), # END (TAB 1) mesm domestic placement map tabPanel
                                          
                                          # START (TAB 2) mesm international placement tabPanel ----
                                          tabPanel(title = HTML("International Placement"),
                                                   
                                                   # DT datatable output ----
                                                   DT::dataTableOutput(outputId = "mesm_internationalPlacement_tbl") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ), # END (TAB 2) mesm international placement tabPanel
                                          
                                          # START (TAB 3) mesm geographic comparison tabPanel ----
                                          tabPanel("Geographic Comparison",
                                                   
                                                   # mesm geographic comparison plotly output ----
                                                   plotly::plotlyOutput(outputId = "mesm_geogComparison_plot") |>
                                                     withSpinner(color = "#003660", type = 1),
                                                   
                                                   # year radioButtons input ----
                                                   year_radioButtons(inputId = "mesm_placementLocation_year_input")
                                                   
                                          ) # END (TAB 3) mesm geographic comparison tabPanel
                                          
                                   ), # END mesm career location tabBox
                                   
                                   # START employers & sectors tbl box ----
                                   tabBox(width = 6,
                                          
                                          # START (TAB 1) mesm initial employers ----
                                          tabPanel("Initial Employers and Sectors",
                                                   
                                                   # mesm initial employers & sectors table ----
                                                   DT::dataTableOutput(outputId = "mesm_career_employ_sector_tbl") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ), # END (TAB 1)
                                          
                                          # START (TAB 2) mesm initial employers ----
                                          tabPanel("Job Titles",
                                                   
                                                   # confidentiality statement ----
                                                   includeMarkdown("text/confidentiality.md"),
                                                   
                                                   # mesm initial employers & sectors table ----
                                                   DT::dataTableOutput(outputId = "mesm_title_tbl") |>
                                                     withSpinner(color = "#003660", type = 1)
                                                   
                                          ) # END (TAB 2)
                                          
                                   ) # END tabBox
                                   
                                 ) # END fluidRow (contains location info & initial employers table)
                                 
                        ) # END MESM career tabPanel
                        
            ) # END tabsetPanel career_page
            
    ), # END tabItem career_page

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                    demographics tab (demographics_page)                  ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    

    tabItem(tabName = "demographics_page",
            
            # START fluidRow ----
            fluidRow(
              
              # START left-hand column -----
              column(width = 5,
                     
                     # START learn more box ----
                     box(width = 12,
                         title = "About Bren's Programs & Demographics Data", #"Learn more about students at Bren!",
                         solidHeader = TRUE,  status = "navy",
                         uiOutput("demographic_programs_text")), # END learn more box
                     
                     #includeMarkdown("text/demo_about.md")
                     
                     # START map box ----
                     box(width = 12,
                         title = "Where are students coming from?",
                         solidHeader = TRUE, status = "navy",
                         
                         # origins map output ----
                         leaflet::leafletOutput(outputId = "origins_map") |>
                           withSpinner(color = "#003660", type = 1),
                         
                         # origins map caption ----
                         # see data-cleaning/undergrad_map_caption_values.R script to calculate updated caption values ----
                         includeMarkdown("text/undergrad_map_caption.md")
                         
                     ) # END map box
                     
              ), # END left-hand column
              
              # START right-hand column ----
              column(width = 7,
                     
                     # START valueBox fluidRow ----
                     fluidRow(
                       
                       valueBoxOutput(outputId = "meds_curr_size_stat", width = 4),
                       valueBoxOutput(outputId = "mesm_curr_size_stat", width = 4),
                       valueBoxOutput(outputId = "phd_curr_size_stat", width = 4)
                       
                     ), # END valueBox fluidRow
                     
                     # START data viz fluidRow ----
                     fluidRow(
                       
                       # START create tabBox for data visualizations ----
                       tabBox(width = 12,
                              
                              # START (TAB 1) overall diversity demographics tabPanel ----
                              tabPanel(title = "Diversity Demographics",
                                       
                                       # overall_diversity plotly output ----
                                       plotly::plotlyOutput(outputId = "overall_diversity_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "diversity_stats_all_input", selected = "MEDS"),
                                       
                                       # urm definition caption ----
                                       div(style = "text-align: right; font-style: italic;", includeMarkdown("text/urm_definition.md"))
                                       
                              ), # END (TAB 1) overall diversity demographics tabPanel
                              
                              # START (TAB 2) admissions tabPanel ----
                              tabPanel(title = "Admissions",
                                       
                                       # admissions plotly output ----
                                       plotly::plotlyOutput(outputId = "admit_stats_all_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "admit_stats_all_input", selected = "MEDS")
                                       
                              ), # END (TAB 2) admissions tabPanel
                              
                              # START (TAB 3) gender tabPanel ----
                              tabPanel(title = "Sex",
                                       
                                       # gender plotly output ----
                                       plotly::plotlyOutput(outputId = "sex_all_plot") |>
                                         withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 3) gender tabPanel
                              
                              # START (TAB 4) age tabPanel ----
                              tabPanel(title = "Age",
                                       
                                       # age plotly output ----
                                       plotly::plotlyOutput(outputId = "age_all_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ---
                                       program_radioButtons(inputId = "age_prog_input", selected = "MEDS")
                                       
                              ), # END (TAB 4) age tabPanel
                              
                              # START (TAB 5) residency tabPanel ----
                              tabPanel(title = "Residency",
                                       
                                       # residency plotly output ----
                                       plotly::plotlyOutput(outputId = "residency_all_plot") |>
                                         withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 5) residency tabPanel
                              
                              # START (TAB 6) international unis tbl ----
                              tabPanel(title = "International",
                                       
                                       # international unis DT datatable output ----
                                       DT::dataTableOutput(outputId = "intl_unis_tbl") |>
                                         withSpinner(color = "#003660", type = 1)
                                       
                              ) # END (TAB 6) international tabPanel
                              
                       ) # END data visualizations tabBox
                       
                     ) # END data viz fluidRow
                     
              ) # END right-hand column
              
            ), # END fluidRow
            
            # START fluidRow (contains IPEDS definitions) ----
            fluidRow(
              
              # START definitions box ----
              box(width = 12, id = "ipeds_def",
                  title = "Race and Background Reporting Defintions",
                  collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status = "navy", # was background = "green"
                  span(tags$div(class = "lrg-bold", includeMarkdown("text/ipeds_text.md")))) # END definitions box
              
            ), # END fluidRow (contains IPEDS definitions) ----
            
            # START fluidRow (contains race & ethnicity plots) ----
            fluidRow(
              
              # START race / category box ----
              box(width = 12, id = "race_box",
                  
                  title = "Race / Category",
                  solidHeader = TRUE, status = "navy",
                  
                  # START race/category box tabsetPanel ----
                  tabsetPanel(id = "race_tabsetPanel",
                              
                              # START (TAB 1) URM trends tabPanel -----
                              tabPanel("URM Trends",
                                       
                                       # urm trends plotly output ----
                                       plotly::plotlyOutput(outputId = "urm_trends_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButton input ----
                                       program_radioButtons(inputId = "urm_trends_input", selected = "All Programs", include_all = TRUE)
                                       
                                       # # urm definition caption ----
                                       # div(style = "text-align: right; font-style: italic;", includeMarkdown("text/urm_definition.md"))
                                       
                              ), # END (TAB 1) URM trends tabPanel
                              
                              # START (TAB 2) IPEDS trends tabPanel ----
                              tabPanel(title = "Race / Category Trends (IPEDS)",
                                       
                                       # IPEDS trends plotly output
                                       plotly::plotlyOutput(outputId = "race_trends_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "race_trends_input", selected = "All Programs", include_all = TRUE)
                                       
                                       # # IPEDS definition caption ----
                                       # div(style = "text-align: right; font-style: italic;", includeMarkdown("text/ipeds_definition.md"))
                                       
                              ), # END (TAB 2) IPEDS trends tabPanel
                              
                              # START (TAB 3) IPEDS tabPanel ----
                              tabPanel(title = "Race / Category (IPEDS)",
                                       
                                       # IPEDS plotly output ----
                                       plotly::plotlyOutput(outputId = "race_plot") |>
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "race_input", selected = "All Programs", include_all = TRUE)
                                       
                              ) # END (TAB 3) IPEDS tabPanel
                              
                  ) # END race/category box tabsetPanel
                  
              ), # END race/category box
              
              # START ethnicity/background box ----
              box(width = 6, id = "background_box",
                  
                  title = "IPEDS Backgrounds and Distribution of Selected Race / Category",
                  solidHeader = TRUE, status = "navy",
                  
                  # background plotly output ----
                  plotly::plotlyOutput(outputId = "background_plot") |>
                    withSpinner(color = "#003660", type = 1)
                  
                  # # IPEDS definition caption ----
                  # div(style = "text-align: right; font-style: italic;", includeMarkdown("text/ipeds_definition.md"))
                  
              ) # END ethnicity/background box
              
            ) # END fluidRow (contains race & ethnicity plots)
            
    ), # END demographics tab (demographics_page)

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                           about tab (about_page)                         ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tabItem(tabName = "about_page",
            
            # add banner image ----
            tags$img(class = "banner", src = "images/Bren-hi-res.jpg"),
            
            # START fluidRow ----
            fluidRow(
              
              # START left-hand column ----
              column(width = 6,
                     
                     # START intro box ----
                     box(width = NULL, id = "intro_what_box",
                         title = "Our motivation",
                         solidHeader = TRUE, status = "navy",
                         includeMarkdown("text/about-motivation.md")) # END intro box

              ), # END left-hand column
              
              # START right-hand column ----
              column(width = 6,
                       
                       # START about the data box ----
                       box(width = NULL, id = "intro_data_box",
                           title = "Data sources & code",
                           solidHeader = TRUE, status = "navy",
                           includeMarkdown("text/about-data.md")), # END about the data box
                     
                     # footer ----
                     div(style = "text-align: right;", uiOutput("footer_date_text"))
                       
              ) # END right-hand column
              
            ) # END fluidRow
            
    ) # END about_page tabItem
    
  ) # END tabItems
  
) # END dashboardBody

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- COMBINE ALL INTO DASHBOARDPAGE-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dashboardPage(header, sidebar, body)
