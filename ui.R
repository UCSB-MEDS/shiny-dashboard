# shiny dashboard has three main components:
# header, sidebar, body
ui <- dashboardPage(
  # HEADER ----
  dashboardHeader(title = 
                    span(tags$a(img(src = "logos/bren_hex.png"),
                                         href = "https://bren.ucsb.edu/",
                                         target = "_blank"), # _blank opens link in a new tab
                                  "Bren Dashboard"
                         ) # EO span
                  ), # EO dashboardHeader title

  
  # SIDEBAR ----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarID",
      
      # welcome ----
      menuItem(
        tabName = "welcome", # Note(HD): previously curr_yr
        text = "Welcome",
        icon = icon("star", lib = "glyphicon"),
        badgeColor = "green"
      ), # EO curr yr menuItem
      
      # student demographics ----
      menuItem(
        tabName = "demo_db",
        text = "Student Demographics",
        icon = icon("user", lib = "glyphicon")
      ), # EO student demographics menuItem
      
      # career ----
      menuItem(
        tabName = "career_db",
        text = "Career Outcomes",
        icon = icon("road", lib = "glyphicon")
      ) # EO career menuItem
      
    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  
  # DASHBOARD BODY ----
  dashboardBody(
    # call styles sheet
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
    ), # EO tags$head
    
    # Note(HD): initially added to use hidden()
    useShinyjs(),
    
    
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      
      # tabs welcome ----
      tabItem(
        tabName = "welcome",
        #h2("Welcome to the Bren Dashboard!"),
        tags$img(class = "banner",
                 src = "images/bren-banner.jpeg"),
        
        
        # * intro text ----
        fluidRow(
          column(width = 6,
            box(id = "intro_what_box",
                title = 
                  tags$div(class = "intro_box_title",
                           span(
                             tags$i(class="fa-solid fa-users"),
                             tags$p("Learn More About Students at Bren")
                           ), # EO span
                  ), # EO div
                width = NULL,
                solidHeader = TRUE,
                status = "navy",
                includeMarkdown("text/welcome_what_text.md")
              ) # EO what box
          ), # EO column 1
          column(width = 6,
            box(id = "intro_data_box",
                title = 
                  tags$div(class = "intro_box_title",
                           span(
                             tags$i(class="fa-solid fa-database"),
                             tags$p("About the Data and the App")
                           ), # EO span
                  ), # EO div
                width = NULL,
                solidHeader = TRUE,
                status = "navy",
                includeMarkdown("text/welcome_data_text.html")
            ) # EO why box
            ) # EO column 2
        ), # EO FR first row
        
        # * footer----
        hr(),
        includeHTML("text/footer.html")
        #print("This Shiny application was developed by MEDS 2022 Alum, Halina Do-Linh, and will be updated annually. You can find the source code on GitHub. If you see mistakes or want to suggest changes, please submit an issue on the source repository.")
        ), # EO curr yr tabItem
      

      
      
      # TABS DEMOGRAPHICS ----
      tabItem(
        tabName = "demo_db",
        
        fluidRow(
          column(5,
                 ## * intro box ----
                 box(width = 12,
                     title = "Learn more about students at Bren!",
                     includeMarkdown("text/demo_about.md"),
                     solidHeader = TRUE,
                     status = "success"
                 ), # EO intro box
                 
                 box(width = 12,
                     ## * undergrad map ----
                     title = "Where are students coming from?",
                     solidHeader = TRUE,
                     status = "navy",
                     tmap::tmapOutput(outputId = "origins_map") %>%
                       withSpinner(color = "#003660", type = 1),
                     tags$p(class = "italic_sector",
                            "In the last 5 years, students have come from 
                            200+ U.S. universities across 41 states, and 66 
                            international universities  from 24 countries. See 
                            International tab to view table of international 
                            universities.")
                 ) # EO map box 
          ), # EO column 1 in FR 1
          
          column(7,
                 fluidRow(
                   ## * valueBoxes ----
                   valueBoxOutput(outputId = "meds_curr_size",
                                  width = 4), 
                   valueBoxOutput(outputId = "mesm_curr_size",
                                  width = 4),
                   valueBoxOutput(outputId = "phd_curr_size",
                                  width = 4)
                 ), # EO FR 1 in column 2
                 fluidRow(
                   tabBox(width = 12,
                          ## * overall diversity demographics ----
                          tabPanel("Diversity Demographics",
                                   plotly::plotlyOutput(outputId = "overall_diversity") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "diversity_stats_all",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE), 
                                   tags$p(class = "italic_sector",
                                          "*URM or Underrepresented Minority is
                                        defined as a U.S. citizen who identifies
                                        as Black/African American, Hispanic/Latino,
                                        or American Indian, which is consistent 
                                        with the UC definition of URM.")
                          ), # EO 2021 demographics tabPanel
                          ## * admissions ----
                          tabPanel("Admissions",
                                   plotly::plotlyOutput(outputId = "admit_stats_all") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "admit_stats_all",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE)
                          ), # EO tabPanel previous admissions
                          ## * gender ----
                          tabPanel("Sex",
                                   plotly::plotlyOutput(outputId = "sex_all") %>%
                                     withSpinner(color = "#003660", type = 1)
                          ), # EO gender tabPanel
                          ## * age ----
                          tabPanel("Age",
                                   plotly::plotlyOutput(outputId = "age_all") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "age_prog",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE)
                          ), # EO age tabPanel
                          ## * residency ----
                          tabPanel("Residency",
                                   plotly::plotlyOutput(outputId = "residency_all") %>%
                                     withSpinner(color = "#003660", type = 1)
                          ), # EO residency tabPanel
                          ## * international unis tbl ----
                          tabPanel("International",
                                   DT::dataTableOutput(outputId = "intl_unis"))
                   ) # EO demographics tabBox
                 ) # EO FR 2 in column 2
          ) # EO column 2 in FR 1
          
        ), # EO FR first row
          
        
        fluidRow(
          ## * definitions ----
          box(id = "ipeds_def",
              title = "Integrated Postsecondary Education Data System (IPEDS) Definitions for Race and Ethnicity Categories",
              width = 12,
              span(
                tags$div(class = "lrg-bold",
                         includeMarkdown("text/ipeds_text.md"))
              ), # EO span tag
              background = "green"
          ), # EO info box
          # remove title from box
          tags$head(tags$style('#ipeds_def .box-header{ display: none}')),
          
          box(
            id = "race_ethnicity_text",
            width = 12,
            includeMarkdown("text/race_ethnicity_text.md")
          ), # EO race / category / ethnicity plots text
          tags$head(tags$style('#race_ethnicity_text .box-header{ display: none}')),
        ), # EO FR fourth row
        
        ## * race & ethnicity plots ----
        fluidRow(
          # race / category
          box(id = "race_box",
              width = 12,
              title = "Race / Category",
              solidHeader = TRUE,
              status = "navy",
              tabsetPanel(
                id = "race_tabsetPanel",
                tabPanel("URM Trends",
                         plotly::plotlyOutput(outputId = "urm_trends_pltly") %>% 
                           withSpinner(color = "#003660", type = 1),
                         radioButtons(inputId = "urm_trends",
                                      label = NULL,
                                      choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                      selected = "All Programs",
                                      inline = TRUE),
                         tags$p(class = "italic_sector",
                                "*URM or Underrepresented Minority is
                                 defined as a U.S. citizen who identifies
                                 as Black/African American, Hispanic/Latino,
                                 or American Indian, which is consistent 
                                 with the UC definition of URM.")
                ), # EO urm distribution tabpanel
                tabPanel("Race / Category Trends",
                         plotly::plotlyOutput(outputId = "race_trends_pltly") %>% 
                           withSpinner(color = "#003660", type = 1),
                         radioButtons(inputId = "race_trends",
                                      label = NULL,
                                      choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                      selected = "All Programs",
                                      inline = TRUE)
                ), # EO race / category over time tabpanel
                tabPanel("Race / Category",
                         plotly::plotlyOutput(outputId = "race_pltly") %>% 
                           withSpinner(color = "#003660", type = 1),
                         radioButtons(inputId = "race",
                                      label = NULL,
                                      choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                      selected = "All Programs",
                                      inline = TRUE)
                ) # EO race / category distribution tabPanel
              ) # EO race_tabsetPanel 
          ), # EO race box
        
          # ethnicity / background
          box(id = "background_box",
              width = 6,
              title = "IPEDS Backgrounds and Distribution of selected Race / Category",
              solidHeader = TRUE,
              status = "navy",
              plotly::plotlyOutput(outputId = "background_pltly") %>% 
                withSpinner(color = "#003660", type = 1)
          ) # EO box
        ) # EO FR fifth row
        
        # fixedRow(
        #   actionButton(inputId = "reset",
        #                "Reset Selection")
        # ) # EO FR sixth row
      ), # EO student demographics tabItem
      
      # TABS CAREER ----
      tabItem(
        tabName = "career_db",
        tabsetPanel(
          id = "career_tabpanels",
          # * MESM tabPanel ----
          tabPanel(
            title = "MESM Initial Career Placements",
            
            fluidRow(
              column(4,
                     # ** intro box ----
                     fluidRow(
                       box(
                         width = 12,
                         title = "Explore MESM Alumni Career Outcomes",
                         solidHeader = TRUE,
                         status = "success",
                         tabsetPanel(
                           tabPanel(
                             "Welcome!",
                             includeMarkdown("text/career_about.md")
                           ), # EO intro tabPanel
                           tabPanel(
                             "About the Data",
                             includeMarkdown("text/career_data_info.md")
                           ) # EO intro about data
                         ) # EO tabsetPanel intro box
                       ) # EO intro box
                     ), # EO FR
                     
                     
                     ## ** valueBox stats ----
                     fluidRow(
                       valueBoxOutput(outputId = "brenNet_stat",
                                      width = 6),
                       valueBoxOutput(outputId = "placement_stat",
                                      width = 6)
                     ), # EO FR valueBox 1
                     fluidRow(
                       # placement stat
                       valueBoxOutput(outputId = "mesm_satisfied_stat",
                                      width = 12)
                     ) # EO FR valueBox 2
              ), # EO column 1
              
              column(8,
                     # ** info box ----
                     box(
                       title = "Initial Employers and Sectors (Over 3 Years)",
                       width = 12,
                       solidHeader = TRUE,
                       status = "navy",
                       DT::dataTableOutput(outputId = "career_employ_sector_tbl") %>%
                         withSpinner(color = "#003660", type = 1)
                     ) # EO employers and sectors box
              ) # EO column 2
              
            ), # EO FR first row
            

            
            fluidRow(
              # ** career maps ----
              tabBox(width = 6,
                     tabPanel(title = HTML(paste("Domestic Placement", "(Over 3 Years)", sep = "<br/>")),
                              tmap::tmapOutput(outputId = "car_alumniMap") %>%
                                withSpinner(color = "#003660", type = 1)#,
                              # tags$p(class = "italic_sector",
                              #        "Click a state to see the number of alumni at each location.")
                     ), # EO tabPanel leaflet map
                     tabPanel(title = HTML(paste("International Placement", "(Over 3 Years)", sep = "<br/>")),
                              DT::dataTableOutput(outputId = "international_place") %>%
                                withSpinner(color = "#003660", type = 1)
                     ), # EO tabPanel international placements
                     tabPanel("Geographic Comparison",
                              plotly::plotlyOutput(outputId = "mesm_location") %>%
                                withSpinner(color = "#003660", type = 1)
                     ) # EO tabPanel bar plot location of MESM alumni
              ), # EO tabBox employers map / info
              
              # ** career plots ----
              tabBox(width = 6,
                     tabPanel("Placement Status",
                              plotly::plotlyOutput(outputId = "mesm_placement_status") %>%
                                withSpinner(color = "#003660", type = 1)
                     ), # EO tabPanel placement status in box 2
                     
                     tabPanel("Job Source",
                              plotly::plotlyOutput(outputId = "mesm_job_source") %>%
                                withSpinner(color = "#003660", type = 1)
                     ), # EO tabPanel placement source in box 2
                     
                     tabPanel("Sector Trends",
                              plotly::plotlyOutput(outputId = "sector_trends") %>% 
                                withSpinner(color = "#003660", type = 1),
                              tags$p(class = "italic_sector",
                                     "Private = Consulting and Corporate | 
                                           Public = Federal, Local & State Government & Research/Education | 
                                           Non-Profit = Non-Profit & NGO | Other = Foreign Government, 
                                           Eco-Entrepreneurship & New Business")
                     ), # EO tabPanel sector over time
                     
                     tabPanel("Sector Satisfaction",
                              plotly::plotlyOutput(outputId = "sector_satisfaction") %>%
                                withSpinner(color = "#003660", type = 1),
                              radioButtons(inputId = "sector_types",
                                           label = NULL,
                                           choices = c("Consulting", "Corporate", "Eco-Entrepreneurship/New Business",
                                                       "Federal Government", "Foreign Government", 
                                                       "Local Government", "Non-Profit", 
                                                       "Research/Education", "State Government"),
                                           selected = "Consulting",
                                           inline = TRUE)
                     ), # EO tabPanel placement sector satisfaction in box 2
                     
                     tabPanel("Salary",
                              plotlyOutput(outputId = "compensation"),
                              tags$p(class = "italic_sector",
                                     "Data includes Full-Time Career positions only."),
                              radioButtons(inputId = "compensation_year",
                                           label = NULL,
                                           choices = c(2019, 2020, 2021, "All Years"),
                                           selected = "All Years",
                                           inline = TRUE)
                     ), # EO tabPanel compensation in box 2
                     
                     tabPanel("Salary by Sector",
                              plotlyOutput(outputId = "comp_sector"),
                              tags$p(class = "italic_sector",
                                     "Data includes Full-Time Career positions only."),
                              radioButtons(inputId = "compSector_year",
                                           label = NULL,
                                           choices = c(2019, 2020, 2021, "All Years"),
                                           selected = "All Years",
                                           inline = TRUE)
                     ), # EO tabPanel compensation sector in box 2
                     
                     tabPanel("Salary by Specialization",
                              plotlyOutput(outputId = "comp_specialization"),
                              tags$p(class = "italic_sector",
                                     "Data includes Full-Time Career positions only."),
                              radioButtons(inputId = "compSpecialization_year",
                                           label = NULL,
                                           choices = c(2019, 2020, 2021, "All Years"),
                                           selected = "All Years",
                                           inline = TRUE)
                     ) # EO tabPanel compensation specialization in box 2
              ) # EO tabBox career second plot
              
            )# EO FR second row
          ), # EO tabPanel MESM
          
          # * MEDS tabPanel ----
          tabPanel(
            title = "MEDS Initial Career Placements",
            "Data for MEDS 2022 will be added in January/February of 2023.
                If you have any immediate questions please reach out to
                bren-admissions@ucsb.edu."
          ) # EO tabPanel MEDS
          
        ) # EO tabsetPanel career_db
      ) # EO tabItem career_db
      
      ) # EO tabItems
      
    ) # EO dashboardBody

  ) # EO dashboardPage
