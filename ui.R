# shiny dashboard has three main components:
# header, sidebar, body
ui <- dashboardPage(
  # HEADER ----
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  
  # SIDEBAR ----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarID",
      
      # curr yr ----
      menuItem(
        tabName = "curr_yr",
        text = "2021 Data",
        icon = icon("star", lib = "glyphicon"),
        badgeColor = "green"
      ), # EO curr yr menuItem
      
      # career ----
      menuItem(
        tabName = "career_db",
        text = "MESM Career Placement",
        icon = icon("road", lib = "glyphicon")
      ), # EO career menuItem
      
      # student demographics ----
      menuItem(
        tabName = "demo_db",
        text = "Student Demographics",
        icon = icon("user", lib = "glyphicon")
      )#, # EO student demographics menuItem
      
    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  
  # BODY ----
  dashboardBody(
    # call styles sheet
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
    ), # EO tags$head
    
    # Note(HD): initally added to use hidden()
    useShinyjs(),
    
    
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      
      # tabs curr yr ----
      tabItem(
        tabName = "curr_yr",
        h2("2021 Dashboard Content"),
        # * intro text ----
        fluidRow(
          box(title = "Get to know our students",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining the dashboard, 
              why it's important, and what a user can do with it")
        ), # EO FR first row
        # * valueBoxes ----
        fluidRow(
          valueBoxOutput(outputId = "meds_curr_size"), 
          valueBoxOutput(outputId = "mesm_curr_size"),
          valueBoxOutput(outputId = "phd_curr_size")
        ), # EO FR second row
        # * admissions plots ----
        fluidRow(
          tabBox(width = 6,
                 tabPanel("2021 Admissions",
                          plotly::plotlyOutput(outputId = "admit_2021") %>%
                            withSpinner(color = "#003660", type = 1)
                          ), # EO tab panel curr admissions
                 tabPanel("Previous Admissions",
                          plotly::plotlyOutput(outputId = "admit_stats_all") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "admit_stats_all",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PHD"),
                                       selected = "MESM",
                                       inline = TRUE)
                 ), # EO tabPanel previous admissions
          ), # EO tabBox
          # * demo and car plots ----
          tabBox(width = 6,
                 tabPanel("2021 Demographics",
                          plotly::plotlyOutput(outputId = "diversity_2021") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "diversity_stats_all",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PHD"),
                                       selected = "MESM",
                                       inline = TRUE)),
                 tabPanel("2021 Bren Alumni Career Placements",
                          DT::dataTableOutput(outputId = "careerP_tbl_21") %>%
                            withSpinner(color = "#003660", type = 1))
                 
          ) # EO tabBox
        ) # EO FR third row
      ), # EO curr yr tabItem
      
      
      # tabs career ----
      tabItem(
        tabName = "career_db",
        h2("Career Placement Content"),
        fluidRow(
          # * valueBox stats ----
          valueBox(paste0(88, "%"), 
                   paste0("LANDED THAT DREAM JOB - MESM Class of 2021 were 
                          satisfied or very satisfied with their job hunt"),
                   icon = icon("star")
          ), # EO valueBox 88% stat
          valueBox(paste0("$", 70, ",", 730),
                   "AVG STARTING SALARY - MESM Class of 2021 for Full Time 
                   Career Positions in 6 months of graduation",
                   icon = icon("dollar-sign"),
                   color = "green"
          ), # EO valueBox starting salary stat
          valueBox(paste0(76, "%"),
                   "STAY CONNECTED - Bren alumni are on BrenConnect, our
                   exclusive networking platform",
                   icon = icon("globe"),
                   color = "blue"
          ) # EO valueBox stay connected stat
        ), # EO FR first row
        fluidRow(
          # * valueBox stats ----
          valueBox(paste0(88, "%"), 
                   "ADDITIONAL STAT",
                   icon = icon("star")
          ), # EO valueBox 88% stat
          valueBox(paste0("$", 70, ",", 730),
                   "ADDITIONAL STAT",
                   icon = icon("dollar-sign"),
                   color = "green"
          ), # EO valueBox starting salary stat
          valueBox(paste0(76, "%"),
                   "ADDITIONAL STAT",
                   icon = icon("globe"),
                   color = "blue"
          ) # EO valueBox stay connected stat
        ), # EO FR second row
        
        fluidRow(
          # * career map ----
          box(width = 6, 
              # solidHeader = TRUE,
              # status = "primary",
              title = "Bren Alumni Solve Environmental Problems Nationwide",
              leaflet::leafletOutput(outputId = "car_alumniMap") %>%
                withSpinner(color = "#003660", type = 1),
              checkboxGroupInput(inputId = "alumniMap_check",
                                 label = NULL,
                                 choices = c(2019, 2020, 2021),
                                 selected = 2021,
                                 inline = TRUE)
          ), # EO box career map
          
          # * career second plot ----
          tabBox(width = 6,
                 tabPanel("Sector",
                          d3treeR::d3tree2Output(outputId = "sector_tree") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "sector_tree_check",
                                             label = NULL,
                                             choices = c(2019, 2020, 2021),
                                             selected = 2021,
                                             inline = TRUE)
                          ), # EO tabPanel 1 in box 2 
                 tabPanel("Satisfaction",
                          plotOutput(outputId = "satisfaction") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "satisfy_all",
                                       label = NULL,
                                       choices = c("2019", "2020", "2021"),
                                       selected = "2021",
                                       inline = TRUE)
                 ), # EO tabPanel placement satisfaction in box 2
                 tabPanel("Average Compensation",
                          plotlyOutput(outputId = "compensation")
                 ), # EO tabPanel avg comp in box 2
                 tabPanel("Source",
                          plotOutput(outputId = "curr_mesm_source") %>%
                            withSpinner(color = "#003660", type = 1)
                 ), # EO tabPanel placement source in box 2
                 tabPanel("Status",
                          plotOutput(outputId = "curr_mesm_status") %>%
                            withSpinner(color = "#003660", type = 1)
                 ) # EO tabPanel placement status in box 2
          ) # EO tabBox career second plot
        ) # EO FR third row
      ), # EO career home tabItem
      
      
      # tabs demographics ----
      tabItem(
        tabName = "demo_db",
        h2("Student Demographics Home Content"),
        fluidRow(
          ## * definitions ----
          box(title = "IPEDS and UC Demographics Definitions",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining important information and definitions 
              so a user can interpret these visuals accurately."
          ) # EO info box
        ), # EO FR first row
        
        fluidRow(
          ## * valueBoxes ----
          valueBox(2, "Undocumented students in 2020 programs", icon = icon("users")),
          valueBox(3, "Military students in 2021 programs", icon = icon("flag")),
          valueBox(11, "First generation students in 2021 programs", icon = icon("users"))
        ), # EO FR second row
        
        ## * plots ----
        fluidRow(
          box(width = 6,
              title = "Where students are coming from",
              leaflet::leafletOutput(outputId = "origins_map") %>%
                withSpinner(color = "#003660", type = 1),
              checkboxGroupInput(inputId = "origins_map_check",
                                 label = "Input is not active yet (only 2021 data)",
                                 choices = c(2016, 2017, 2018, 2019, 2020, 2021),
                                 selected = 2021,
                                 inline = TRUE)
              ), # EO map box  
          tabBox(width = 6,
                 tabPanel("Gender",
                          plotly::plotlyOutput(outputId = "gender_all") %>%
                            withSpinner(color = "#003660", type = 1)
                          ), # EO gender tabPanel
                 tabPanel("Age",
                          plotly::plotlyOutput(outputId = "age_all") %>%
                            withSpinner(color = "#003660", type = 1)
                          ), # EO age tabPanel
                 tabPanel("Race & Ethnicity",
                          "NOTE: need to style tree map",
                          d3treeR::d3tree2Output(outputId = "race_ethn_mesm") %>%
                            withSpinner(color = "#003660", type = 1)),
                 tabPanel("Residency",
                          plotly::plotlyOutput(outputId = "residency_all") %>%
                            withSpinner(color = "#003660", type = 1)
                          ) # EO resdiency tabPanel
                 ), # EO demographics over time tabBox
          
        ) # EO FR third row
      ) # EO demo home tabItem
      
    ) # EO tabItems

  ) # EO dashboardBody
) # EO dashboardPage
