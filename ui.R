# shiny dashboard has three main components:
# header, sidebar, body

#.............................header.............................
header <- dashboardHeader(
  
  title = span(tags$a(img(src = "logos/bren_hex.png"), href = "https://bren.ucsb.edu/", target = "_blank"), "Bren Student Data Explorer") # _blank opens link in a new tab
  
) # END dashboardHeader

#............................sidebar.............................
sidebar <- dashboardSidebar(
  
  # sidebarMenu ----
  sidebarMenu(id = "sidebarID",
              
              # menuItems ----
              menuItem(tabName = "welcome", text = "Welcome", icon = icon("star", lib = "glyphicon"), badgeColor = "green"), # Note(HD): previously curr_yr
              menuItem(tabName = "demo_db", text = "Demographics", icon = icon("user", lib = "glyphicon")),
              menuItem(tabName = "career_db", text = "Career Outcomes", icon = icon("road", lib = "glyphicon"))
              
  ) # END sidebarMenu
  
) # END dashboardSidebar

#..............................body..............................
body <- dashboardBody(
  
  # header (add styleshett) ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ), # END header
  
  # call shinyjs; Note(HD): initially added to use hidden() ----
  useShinyjs(),
  
  ###########################################################
  #### THREE tabItems below: welcome, demo_db, career_db ####
  ###########################################################
  
  # tabItems ----
  tabItems(
    
    ###################
    # welcome tab ----
    ###################
    tabItem(tabName = "welcome",
            
            # add banner image ----
            tags$img(class = "banner", src = "images/Bren-hi-res.jpg"),
            
            # fluidRow ----
            fluidRow(
              
              # left-hand column ----
              column(width = 6,
                     
                     # intro box ----
                     box(width = NULL, id = "intro_what_box",
                         title = tags$div(class = "intro_box_title", span(tags$i(class="fa-solid fa-users"), tags$p("Learn about students at the Bren School"))), 
                         solidHeader = TRUE, status = "navy",
                         includeMarkdown("text/welcome_what_text.md")) # END intro box
                     
              ), # END left-hand column
              
              # right-hand column ----
              column(width = 6,
                     
                     # about the data box ----
                     box(width = NULL, id = "intro_data_box",
                         title = tags$div(class = "intro_box_title", span(tags$i(class="fa-solid fa-database"), tags$p("About the data"))), 
                         solidHeader = TRUE, status = "navy",
                         includeMarkdown("text/welcome_data_text.html")) # END about the data box 
                     
              ) # END right-hand column
              
            ), # END fluidRow
            
            # footer ----
            includeHTML("text/footer.html")
            
    ), # END welcome tabItem
    
    #################################
    # demographics tab (demo_df) ----
    #################################
    tabItem(tabName = "demo_db",
            
            # fluidRow ----
            fluidRow(
              
              # left-hand column -----
              column(width = 5,
                     
                     # learn more box ----
                     box(width = 12,
                         title = "Learn more about students at Bren!",
                         includeMarkdown("text/demo_about.md"),
                         solidHeader = TRUE, status = "success"), # END learn more box
                     
                     # map box ----
                     box(width = 12,
                         title = "Where are students coming from?",
                         solidHeader = TRUE, status = "navy",
                         
                         # tmap output ----
                         tmap::tmapOutput(outputId = "origins_map") |>withSpinner(color = "#003660", type = 1),
                         
                         # tmap caption ----
                         includeMarkdown("text/undergrad_map_caption.md")
                         
                     ) # END map box 
                     
              ), # END left-hand column
              
              # right-hand column ----
              column(width = 7,
                     
                     # valueBox fluidRow ----
                     fluidRow(
                       valueBoxOutput(outputId = "meds_curr_size", width = 4), 
                       valueBoxOutput(outputId = "mesm_curr_size", width = 4),
                       valueBoxOutput(outputId = "phd_curr_size", width = 4)
                     ), # END valueBox fluidRow
                     
                     # data viz fluidRow ----
                     fluidRow(
                       
                       # create tabBox for data vizualizations ----
                       tabBox(width = 12,
                              
                              # (TAB 1) overall diversity demographics tabPanel ----
                              tabPanel(title = "Diversity Demographics",
                                       
                                       # overall_diversity plotly output ----
                                       plotly::plotlyOutput(outputId = "overall_diversity") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       radioButtons(inputId = "diversity_stats_all", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD"),
                                                    selected = "MESM",
                                                    inline = TRUE), 
                                       
                                       # urm definition caption ----
                                       includeMarkdown("text/urm_definition.md")
                                       
                              ), # END (TAB 1) overall diversity demographics tabPanel
                              
                              # (TAB 2) admissions tabPanel ----
                              tabPanel(title = "Admissions",
                                       
                                       # admissions plotly output ----
                                       plotly::plotlyOutput(outputId = "admit_stats_all") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       radioButtons(inputId = "admit_stats_all", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD"),
                                                    selected = "MESM",
                                                    inline = TRUE)
                                       
                              ), # END (TAB 2) admissions tabPanel
                              
                              # (TAB 3) gender tabPanel ----
                              tabPanel(title = "Sex",
                                       
                                       # gender plotly output ----
                                       plotly::plotlyOutput(outputId = "sex_all") |> withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 3) gender tabPanel 
                              
                              # (TAB 4) age tabPanel ----
                              tabPanel(title = "Age",
                                       
                                       # age plotly output ----
                                       plotly::plotlyOutput(outputId = "age_all") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ---
                                       radioButtons(inputId = "age_prog", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD"),
                                                    selected = "MESM",
                                                    inline = TRUE)
                                       
                              ), # END (TAB 4) age tabPanel
                              
                              # (TAB 5) residency tabPanel ----
                              tabPanel(title = "Residency",
                                       
                                       # residency plotly output ----
                                       plotly::plotlyOutput(outputId = "residency_all") |>  withSpinner(color = "#003660", type = 1)
                                       
                              ), # END (TAB 5) residency tabPanel
                              
                              # (TAB 6) international unis tbl ----
                              tabPanel(title = "International",
                                       
                                       # international unis DT datatable output ----
                                       DT::dataTableOutput(outputId = "intl_unis"))
                              
                       ) # END data visualizations tabBox
                       
                     ) # END data viz fluidRow 
                     
              ) # END right-hand column
              
            ), # END fluidRow 
            
            # fluidRow (contains IPEDS definitions) ----
            fluidRow(
              
              # definitions box ----
              box(width = 12, id = "ipeds_def",
                  title = "Race and Background Reporting Defintions",
                  collapsible = TRUE, collapsed = FALSE,
                  span(tags$div(class = "lrg-bold", includeMarkdown("text/ipeds_text.md"))), 
                  background = "green"), # END definitions box
              
            ), # END fluidRow (contains IPEDS definitions) ----
            
            # fluidRow (contains race & ethnicity plots) ----
            fluidRow(
              
              # race / category box ----
              box(width = 12, id = "race_box",
                  title = "Race / Category",
                  solidHeader = TRUE, status = "navy",
                  
                  # race/category box tabsetPanel ----
                  tabsetPanel(id = "race_tabsetPanel",
                              
                              # URM trends tabPanel -----
                              tabPanel("URM Trends",
                                       
                                       # urm trends plotly output ----
                                       plotly::plotlyOutput(outputId = "urm_trends_pltly") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButton input ----
                                       radioButtons(inputId = "urm_trends", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                                    selected = "All Programs",
                                                    inline = TRUE),
                                       
                                       # urm definition caption ----
                                       includeMarkdown("text/urm_definition.md")
                                       
                              ), # END URM trends tabPanel
                              
                              # IPEDS trends tabPanel ----
                              tabPanel(title = "Race / Category Trends (IPEDS)",
                                       
                                       # IPEDS trends plotly output
                                       plotly::plotlyOutput(outputId = "race_trends_pltly") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       radioButtons(inputId = "race_trends", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                                    selected = "All Programs",
                                                    inline = TRUE),
                                       
                                       # IPEDS definition caption ----
                                       includeMarkdown("text/ipeds_definition.md")
                                       
                              ), # END IPEDS trends tabPanel
                              
                              # IPEDS tabPanel ----
                              tabPanel(title = "Race / Category (IPEDS)",
                                       
                                       # IPEDS plotly output ----
                                       plotly::plotlyOutput(outputId = "race_pltly") |> withSpinner(color = "#003660", type = 1),
                                       
                                       # program radioButtons input ----
                                       radioButtons(inputId = "race", label = NULL,
                                                    choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                                    selected = "All Programs",
                                                    inline = TRUE)
                                       
                              ) # END IPEDS tabPanel
                              
                  ) # END race/category box tabsetPanel
                  
              ), # END race/category box 
              
              # ethnicity/background box ----
              box(width = 6, id = "background_box",
                  title = "IPEDS Backgrounds and Distribution of Selected Race / Category",
                  solidHeader = TRUE, status = "navy",
                  
                  # background plotly output ----
                  plotly::plotlyOutput(outputId = "background_pltly") |> withSpinner(color = "#003660", type = 1),
                  
                  # IPEDS definition caption ----
                  includeMarkdown("text/ipeds_definition.md")
                  
              ) # END ethnicity/background box
              
            ) # END fluidRow (contains race & ethnicity plots) 
            
    ), # END demographics tab (demo_db)
    
    #############################
    #career tab (career_db) ----- 
    #############################
    
    tabItem(tabName = "career_db",
            
            # career tabsetPanel ----
            tabsetPanel(id = "career_tabpanels",
                        
                        # MESM career tabPanel ----
                        tabPanel(title = "MESM Initial Career Placements",
                                 
                                 # fluidRow ----
                                 fluidRow(
                                   
                                   # left-hand column ----
                                   column(width = 4,
                                          
                                          # fluidRow ----
                                          fluidRow(
                                            
                                            # explore MESM career info box ----
                                            box(width = 12,
                                                title = "Explore MESM Alumni Career Outcomes",
                                                solidHeader = TRUE, status = "success",
                                                
                                                # explore MESM career info tabsetPanel ----
                                                tabsetPanel(
                                                  
                                                  # welcome tabPanel ----
                                                  tabPanel(title = "Welcome!", includeMarkdown("text/career_about.md")), 
                                                  
                                                  # about MESM career data tabPanel ----
                                                  tabPanel(title = "About the Data", includeMarkdown("text/career_data_info.md")) 
                                                
                                                  ) # END explore MESM career info tabsetPanel
                                                
                                            ) # END explore MESM career info box
                                            
                                          ), # END fluidRow
                                          
                                          # fluidRow (valueBoxes 1) ----
                                          fluidRow(
                                            valueBoxOutput(outputId = "brenNet_stat", width = 6),
                                            valueBoxOutput(outputId = "placement_stat", width = 6)
                                          ), # END fluidRow (valuBoxes 1)
                                          
                                          # fluidRow (valueBoxes 2) ----
                                          fluidRow(
                                            valueBoxOutput(outputId = "mesm_satisfied_stat", width = 12)
                                          ) # END fluidRow (valueBoxes 2)
                                          
                                   ), # END left-hand column
                                   
                                   # right-hand column ----
                                   column(width = 8,
                                          
                                          # career data visualizations tabBox ----
                                          tabBox(width = 12,
                                                 
                                                 # placement status tabPanel ----
                                                 tabPanel(title = "Placement Status",
                                                          plotly::plotlyOutput(outputId = "mesm_placement_status") |> withSpinner(color = "#003660", type = 1)), 
                                                 
                                                 # job source tabPanel ----
                                                 tabPanel(title = "Job Source",
                                                          plotly::plotlyOutput(outputId = "mesm_job_source") |> withSpinner(color = "#003660", type = 1)), 
                                                 
                                                 # sector trends tabPanel ----
                                                 tabPanel(title = "Sector Trends",
                                                          plotly::plotlyOutput(outputId = "sector_trends") |> withSpinner(color = "#003660", type = 1),
                                                          includeMarkdown("text/sector_definitions.md")),
                                                 
                                                 # sector satisfaction tabPanel -----
                                                 tabPanel(title = "Sector Satisfaction",
                                                          plotly::plotlyOutput(outputId = "sector_satisfaction") |> withSpinner(color = "#003660", type = 1),
                                                          radioButtons(inputId = "sector_types", label = NULL,
                                                                       choices = c("Consulting", "Corporate", "Eco-Entrepreneurship/New Business",
                                                                                   "Federal Government", "Foreign Government", 
                                                                                   "Local Government", "Non-Profit", 
                                                                                   "Research/Education", "State Government"),
                                                                       selected = "Consulting",
                                                                       inline = TRUE)), 
                                                 
                                                 # salary tabPanel ----
                                                 tabPanel(title = "Salary",
                                                          plotlyOutput(outputId = "compensation"),
                                                          tags$p(class = "italic_sector", "Data includes Full-Time Career positions only."),
                                                          radioButtons(inputId = "compensation_year",
                                                                       label = NULL,
                                                                       choices = c(2019, 2020, 2021, "All Years"),
                                                                       selected = "All Years",
                                                                       inline = TRUE)
                                                 ), # EO tabPanel compensation in box 2
                                                 
                                                 # salary by sector tabPanel ----
                                                 tabPanel(title = "Salary by Sector",
                                                          plotlyOutput(outputId = "comp_sector"),
                                                          tags$p(class = "italic_sector",
                                                                 "Data includes Full-Time Career positions only."),
                                                          radioButtons(inputId = "compSector_year", label = NULL,
                                                                       choices = c(2019, 2020, 2021, "All Years"),
                                                                       selected = "All Years",
                                                                       inline = TRUE)
                                                 ), # EO tabPanel compensation sector in box 2
                                                 
                                                 # salary by specialization tabPanel ----
                                                 tabPanel(title = "Salary by Specialization",
                                                          plotlyOutput(outputId = "comp_specialization"),
                                                          tags$p(class = "italic_sector",
                                                                 "Data includes Full-Time Career positions only."),
                                                          radioButtons(inputId = "compSpecialization_year", label = NULL,
                                                                       choices = c(2019, 2020, 2021, "All Years"),
                                                                       selected = "All Years",
                                                                       inline = TRUE)
                                                 ) # EO tabPanel compensation specialization in box 2
                                          ) # EO tabBox career second plot
                                          
                                   ) # EO column 2
                                   
                                 ), # EO FR first row
                                 
                                 
                                 
                                 fluidRow(
                                   # ** career maps ----
                                   tabBox(width = 6,
                                          tabPanel(title = HTML(paste("Domestic Placement", "(Over 3 Years)", sep = "<br/>")),
                                                   tmap::tmapOutput(outputId = "car_alumniMap") |> withSpinner(color = "#003660", type = 1)#,
                                                   # tags$p(class = "italic_sector",
                                                   #        "Click a state to see the number of alumni at each location.")
                                          ), # EO tabPanel leaflet map
                                          tabPanel(title = HTML(paste("International Placement", "(Over 3 Years)", sep = "<br/>")),
                                                   DT::dataTableOutput(outputId = "international_place") |> withSpinner(color = "#003660", type = 1)
                                          ), # EO tabPanel international placements
                                          tabPanel("Geographic Comparison",
                                                   plotly::plotlyOutput(outputId = "mesm_location") |> withSpinner(color = "#003660", type = 1)
                                          ) # EO tabPanel bar plot location of MESM alumni
                                   ), # EO tabBox employers map / info
                                   
                                   # ** employers & sectors tbl ----
                                   box(width = 6,
                                       title = "Initial Employers and Sectors (Over 3 Years)",
                                       solidHeader = TRUE, status = "navy",
                                       DT::dataTableOutput(outputId = "career_employ_sector_tbl") |> withSpinner(color = "#003660", type = 1)
                                   ) # EO employers and sectors box
                                 ) # EO FR second row
                                 
                        ), # END MESM career tabPanel
                        
                        # * MEDS tabPanel ----
                        tabPanel(
                          id = "meds_career_tab",
                          title = "MEDS Initial Career Placements",
                          tags$p(class = "meds_career_update_text",
                                 "Data for MEDS 2022 will be added in January/February of 2023.
                   If you have any immediate questions please reach out to 
                   admissions@bren.ucsb.edu.")
                        ) # EO tabPanel MEDS
                        
            ) # EO tabsetPanel career_db
    ) # EO tabItem career_db
    
  ) # EO tabItems
  
) # EO dashboardBody


#.................combine all into dashboardPage.................
dashboardPage(header, sidebar, body)
