
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
              
              menuItem(tabName = "career_db", text = "Career Outcomes", icon = icon("road", lib = "glyphicon")),
              menuItem(tabName = "demo_db", text = "Demographics", icon = icon("user", lib = "glyphicon")),
              menuItem(tabName = "about", text = "About this Dashboard", icon = icon("star", lib = "glyphicon"), badgeColor = "green") 
              
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
    ##                           career tab (career_db)                         ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tabItem(tabName = "career_db",
            
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
                                                solidHeader = TRUE, status = "success",
                                                includeMarkdown("text/meds_career_data_info.md")
                                                
                                            ) # END explore MEDS career info box
                                            
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
                                                            plotlyOutput(outputId = "meds_salary") |> withSpinner(color = "#003660", type = 1),
                                                            
                                                            # caption ----
                                                            tags$p(tags$em("Data includes Full-Time Career positions only."))
                                                            
                                                   ), # END (TAB 1) meds salary tabPanel 
                                                   
                                                   # START (TAB 2) meds salary by sector tabPanel ----
                                                   tabPanel(title = "Salary by Sector",
                                                            
                                                            # meds salary by sector plotly output ----
                                                            plotlyOutput(outputId = "meds_salary_by_sector") |> 
                                                              withSpinner(color = "#003660", type = 1),
                                                            
                                                            # caption ----
                                                            tags$p(tags$em("Data includes Full-Time Career positions only.")),
                                                            
                                                            # year radioButtons input ----
                                                            meds_year_radioButtons(inputId = "meds_salarySector_year")
                                                            
                                                   ), # END (TAB 2) meds salary by sector tabPanel 
                                                   
                                                   # START (TAB 3) meds placement status tabPanel ----
                                                   tabPanel(title = "Placement Status",
                                                            
                                                            # mesm placement plotly output ----
                                                            plotly::plotlyOutput(outputId = "meds_placement_status") |> 
                                                              withSpinner(color = "#003660", type = 1),
                                                            
                                                            # year radioButtons input ----
                                                            meds_year_radioButtons(inputId = "meds_placementStatus_year")
                                                            
                                                   ), # END (TAB 3) meds placement status tabPanel 
                                                   
                                                   # START (TAB 4) meds sector trends tabPanel ----
                                                   tabPanel(title = "Sector Trends",
                                                            
                                                            # meds sector trends plotly output ----
                                                            plotly::plotlyOutput(outputId = "meds_sector_trends") |> 
                                                              withSpinner(color = "#003660", type = 1),
                                                            
                                                            # year radioButtons input ----
                                                            meds_year_radioButtons(inputId = "meds_sector_trends_year")
                                                            
                                                   ), # END (TAB 4) meds sector trends tabPanel 
                                                   
                                                   # START (TAB 5) meds sector satisfaction tabPanel -----
                                                   tabPanel(title = "Sector Satisfaction",
                                                            
                                                            # meds sector satisfaction plotly output ----
                                                            plotly::plotlyOutput(outputId = "meds_sector_satisfaction") |> 
                                                              withSpinner(color = "#003660", type = 1),
                                                            
                                                            # sector type radioButtons input ----
                                                            sectorType_radioButtons(inputId = "meds_sector_types", selected = "Consulting")
                                                            
                                                   ), # END (TAB 5) meds sector satisfaction tabPanel 
                                                   
                                                   
                                                   # START (TAB 6) meds job source tabPanel ----
                                                   tabPanel(title = "Job Source",
                                                            
                                                            # mesm job source plotly output ---
                                                            plotly::plotlyOutput(outputId = "meds_job_source") |> 
                                                              withSpinner(color = "#003660", type = 1),
                                                            
                                                            # year radioButtons input ----
                                                            meds_year_radioButtons(inputId = "meds_job_source_year")
                                                            
                                                   ) # END (TAB 6) meds job source tabPanel
                                                   
                                            ) # END meds career data visualizations tabBox
                                            
                                          #) # END meds career data viz tabBox fluidRow 
                                          
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
                                                   
                                                   # note re: no international MEDS alumni yet (add in table when appropriate) ----
                                                   includeMarkdown("text/meds_internationalPlacement.md")
                                                   
                                          ), # END (TAB 2) meds international placement tabPanel
                                          
                                          # START (TAB 3) meds geographic comparison tabPanel ----
                                          tabPanel("Geographic Comparison",
                                                   
                                                   # meds geographic comparison plotly output ----
                                                   plotly::plotlyOutput(outputId = "meds_geogComparison_plot") |> 
                                                     withSpinner(color = "#003660", type = 1),
                                                   
                                                   # note about percentages that don't add to 100% ----
                                                   includeMarkdown("text/geog_comparison_perc.md"),
                                                   
                                                   # year radioButtons input ----
                                                   meds_year_radioButtons(inputId = "meds_placementLocation_year")
                                                   
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
                                                solidHeader = TRUE, status = "success",
                                                includeMarkdown("text/mesm_career_data_info.md")
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
                                                          plotlyOutput(outputId = "mesm_salary") |> withSpinner(color = "#003660", type = 1),
                                                          
                                                          # caption ----
                                                          tags$p(tags$em("Data includes Full-Time Career positions only."))
                                                          
                                                          # # year radioButtons input ----
                                                          # year_radioButtons(inputId = "mesm_salary_year")
                                                          
                                                 ), # END (TAB 1) mesm salary tabPanel 
                                                 
                                                 # START (TAB 2) mesm salary by sector tabPanel ----
                                                 tabPanel(title = "Salary by Sector",
                                                          
                                                          # mesm salary by sector plotly output ----
                                                          plotlyOutput(outputId = "mesm_salary_by_sector") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # caption ----
                                                          tags$p(tags$em("Data includes Full-Time Career positions only.")),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_salarySector_year")
                                                          
                                                 ), # END (TAB 2) mesm salary by sector tabPanel 
                                                 
                                                 # START (TAB 3) mesm salary by specialization tabPanel ----
                                                 tabPanel(title = "Salary by Specialization",
                                                          
                                                          # mesm salary by specialization plotly output ----
                                                          plotlyOutput(outputId = "mesm_salary_by_specialization") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # caption ----
                                                          tags$p(tags$em("Data includes Full-Time Career positions only.")),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_salary_by_specialization_year")
                                                          
                                                 ), # END (TAB 3) mesm salary by specialization tabPanel 
                                                 
                                                 # START (TAB 4) mesm placement status tabPanel ----
                                                 tabPanel(title = "Placement Status",
                                                          
                                                          # mesm placement plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_placement_status") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_placementStatus_year")
                                                          
                                                 ), # END (TAB 4) mesm placement status tabPanel 
                                                 
                                                 # START (TAB 5) mesm sector trends tabPanel ----
                                                 tabPanel(title = "Sector Trends",
                                                          
                                                          # mesm sector trends plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_sector_trends") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # # extra space ----
                                                          # br(),
                                                          # 
                                                          # # sector definitions caption ----
                                                          # includeMarkdown("text/sector_definitions.md"),

                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_sector_trends_year")
                                                          
                                                 ), # END (TAB 5) mesm sector trends tabPanel 
                                                 
                                                 # START (TAB 6) mesm sector satisfaction tabPanel -----
                                                 tabPanel(title = "Sector Satisfaction",
                                                          
                                                          # mesm sector satisfaction plotly output ----
                                                          plotly::plotlyOutput(outputId = "mesm_sector_satisfaction") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # sector type radioButtons input ----
                                                          sectorType_radioButtons(inputId = "mesm_sector_types", selected = "Consulting")
                                                          
                                                 ), # END (TAB 6) mesm sector satisfaction tabPanel 
                                                 
                                                 # START (TAB 7) mesm job source tabPanel ----
                                                 tabPanel(title = "Job Source",
                                                          
                                                          # mesm job source plotly output ---
                                                          plotly::plotlyOutput(outputId = "mesm_job_source") |> 
                                                            withSpinner(color = "#003660", type = 1),
                                                          
                                                          # year radioButtons input ----
                                                          year_radioButtons(inputId = "mesm_job_source_year")
                                                          
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
                                                   year_radioButtons(inputId = "mesm_placementLocation_year")
                                                   
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
                                                   
                                          ), # END (TAB 2)
                                          
                                   ) # END tabBox
                                   
                                 ) # END fluidRow (contains location info & initial employers table) 
                                 
                        ) # END MESM career tabPanel
                        
            ) # END tabsetPanel career_db
            
    ), # END tabItem career_db
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                         demographics tab (demo_db)                       ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tabItem(tabName = "demo_db",
            
            # START fluidRow ----
            fluidRow(
              
              # START left-hand column -----
              column(width = 5,
                     
                     # START learn more box ----
                     box(width = 12,
                         title = "Learn more about students at Bren!",
                         includeMarkdown("text/demo_about.md"),
                         solidHeader = TRUE, status = "success"), # END learn more box
                     
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
                       
                       valueBoxOutput(outputId = "meds_curr_size", width = 4), 
                       valueBoxOutput(outputId = "mesm_curr_size", width = 4),
                       valueBoxOutput(outputId = "phd_curr_size", width = 4)
                       
                     ), # END valueBox fluidRow
                     
                     # START data viz fluidRow ----
                     fluidRow(
                       
                       # START create tabBox for data vizualizations ----
                       tabBox(width = 12,
                              
                              # START (TAB 1) overall diversity demographics tabPanel ----
                              tabPanel(title = "Diversity Demographics",
                                       
                                       # overall_diversity plotly output ----
                                       plotly::plotlyOutput(outputId = "overall_diversity") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "diversity_stats_all", selected = "MEDS"),
                                       
                                       # urm definition caption ----
                                       includeMarkdown("text/urm_definition.md")
                                       
                              ), # END (TAB 1) overall diversity demographics tabPanel
                              
                              # START (TAB 2) admissions tabPanel ----
                              tabPanel(title = "Admissions",
                                       
                                       # admissions plotly output ----
                                       plotly::plotlyOutput(outputId = "admit_stats_all") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "admit_stats_all", selected = "MEDS")
                                       
                              ), # END (TAB 2) admissions tabPanel
                              
                              # START (TAB 3) gender tabPanel ----
                              tabPanel(title = "Sex",
                                       
                                       # gender plotly output ----
                                       plotly::plotlyOutput(outputId = "sex_all") |> 
                                         withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 3) gender tabPanel 
                              
                              # START (TAB 4) age tabPanel ----
                              tabPanel(title = "Age",
                                       
                                       # age plotly output ----
                                       plotly::plotlyOutput(outputId = "age_all") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ---
                                       program_radioButtons(inputId = "age_prog", selected = "MEDS")
                                       
                              ), # END (TAB 4) age tabPanel
                              
                              # START (TAB 5) residency tabPanel ----
                              tabPanel(title = "Residency",
                                       
                                       # residency plotly output ----
                                       plotly::plotlyOutput(outputId = "residency_all") |>  
                                         withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 5) residency tabPanel
                              
                              # START (TAB 6) international unis tbl ----
                              tabPanel(title = "International",
                                       
                                       # international unis DT datatable output ----
                                       DT::dataTableOutput(outputId = "intl_unis") |> 
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
                  collapsible = TRUE, collapsed = FALSE,
                  span(tags$div(class = "lrg-bold", includeMarkdown("text/ipeds_text.md"))), 
                  background = "green"), # END definitions box
              
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
                                       plotly::plotlyOutput(outputId = "urm_trends_pltly") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButton input ----
                                       program_radioButtons(inputId = "urm_trends", selected = "All Programs", include_all = TRUE),
                                       
                                       # urm definition caption ----
                                       includeMarkdown("text/urm_definition.md")
                                       
                              ), # END (TAB 1) URM trends tabPanel
                              
                              # START (TAB 2) IPEDS trends tabPanel ----
                              tabPanel(title = "Race / Category Trends (IPEDS)",
                                       
                                       # IPEDS trends plotly output
                                       plotly::plotlyOutput(outputId = "race_trends_pltly") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "race_trends", selected = "All Programs", include_all = TRUE),
                                       
                                       # IPEDS definition caption ----
                                       includeMarkdown("text/ipeds_definition.md")
                                       
                              ), # END (TAB 2) IPEDS trends tabPanel
                              
                              # START (TAB 3) IPEDS tabPanel ----
                              tabPanel(title = "Race / Category (IPEDS)",
                                       
                                       # IPEDS plotly output ----
                                       plotly::plotlyOutput(outputId = "race_pltly") |> 
                                         withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       program_radioButtons(inputId = "race", selected = "All Programs", include_all = TRUE)
                                       
                              ) # END (TAB 3) IPEDS tabPanel
                              
                  ) # END race/category box tabsetPanel
                  
              ), # END race/category box 
              
              # START ethnicity/background box ----
              box(width = 6, id = "background_box",
                  
                  title = "IPEDS Backgrounds and Distribution of Selected Race / Category",
                  solidHeader = TRUE, status = "navy",
                  
                  # background plotly output ----
                  plotly::plotlyOutput(outputId = "background_pltly") |> 
                    withSpinner(color = "#003660", type = 1),
                  
                  # IPEDS definition caption ----
                  includeMarkdown("text/ipeds_definition.md")
                  
              ) # END ethnicity/background box
              
            ) # END fluidRow (contains race & ethnicity plots) 
            
    ), # END demographics tab (demo_db)
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                  about tab                               ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tabItem(tabName = "about",
            
            # add banner image ----
            tags$img(class = "banner", src = "images/Bren-hi-res.jpg"),
            
            # START fluidRow ----
            fluidRow(
              
              # START left-hand column ----
              column(width = 6,
                     
                     # START intro box ----
                     box(width = NULL, id = "intro_what_box",
                         
                         title = tagList(icon("users"), "Learn about students at the Bren School"),
                         solidHeader = TRUE, status = "navy",
                         includeMarkdown("text/welcome_what_text.md")
                         
                     ) # END intro box
                     
              ), # END left-hand column
              
              # START right-hand column ----
              column(width = 6,
                     
                     # START about the data box ----
                     box(width = NULL, id = "intro_data_box",
                         
                         title = tagList(icon("database"), "About the data"),
                         solidHeader = TRUE, status = "navy",
                         includeMarkdown("text/welcome_data_text.md")
                         
                     ) # END about the data box 
                     
              ) # END right-hand column
              
            ), # END fluidRow
            
            # footer ----
            includeHTML("text/footer.html")
            
    ) # END about tabItem
    
  ) # END tabItems
  
) # END dashboardBody

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- COMBINE ALL INTO DASHBOARDPAGE-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dashboardPage(header, sidebar, body)
