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
        icon = icon("star", lib = "glyphicon")
      ), # EO curr yr menuItem
      
      # career ----
      menuItem(
        tabName = "career_db",
        text = "Career Placements",
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
                          plotly::plotlyOutput(outputId = "admit_2021")),
                 tabPanel("Previous Admissions",
                          plotly::plotlyOutput(outputId = "admit_stats_all"),
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
                          plotly::plotlyOutput(outputId = "diversity_2021"),
                          radioButtons(inputId = "diversity_stats_all",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PHD"),
                                       selected = "MESM",
                                       inline = TRUE)),
                 tabPanel("2021 Bren Alumni Career Placements",
                          DT::dataTableOutput(outputId = "careerP_tbl_21"))
                 
          ) # EO tabBox
        ) # EO FR third row
      ), # EO curr yr tabItem
      
      
      # tabs career ----
      tabItem(
        tabName = "career_db",
        h2("Career Placement Home Content"),
        fluidRow(
          # * valueBox stats ----
          valueBox(paste0(88, "%"), 
                   "LANDED THAT DREAM JOB",
                   icon = icon("star")
          ), # EO valueBox 88% stat
          valueBox(paste0("$", 70, ",", 730),
                   "AVERAGE STARTING SALARY",
                   icon = icon("usd"),
                   color = "green"
          ), # EO valueBox starting salary stat
          valueBox(paste0(76, "%"),
                   "STAY CONNECTED",
                   icon = icon("globe"),
                   color = "blue"
          ) # EO valueBox stay connected stat
        ), # EO FR first row
        fluidRow(
          # * career map ----
          box(width = 6, 
              # solidHeader = TRUE,
              # status = "primary",
              title = "Bren Alumni Solve Environmental Problems Nationwide",
              leafletOutput(outputId = "car_alumniMap"),
              checkboxGroupInput(inputId = "alumniMap_check",
                                 label = NULL,
                                 choices = c(2019, 2020, 2021),
                                 selected = 2021,
                                 inline = TRUE)
          ), # EO box career map
          
          # * career second plot ----
          tabBox(width = 6,
                 title = "Title 2",
                 tabPanel("Title", "content in box",
                          plotOutput(outputId = "curr_mesm_source")
                 ), # EO tabPanel 1 in box 2
                 tabPanel("Placement Status", "What kind of jobs are MESM graduates obtaining 6 months after graduation?",
                          plotOutput(outputId = "curr_mesm_status")
                 ) # EO tabPanel 2 in box 2
          ) # EO tabBox career second plot
        ) # EO FR second row
      ), # EO career home tabItem
      
      
      # tabs demo ----
      tabItem(
        tabName = "demo_db",
        h2("Student Demographics Home Content"),
        fluidRow(
          valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
        ), # EO FR first row
        fluidRow(
          box(width = 12, title = "Title 1", "box 1 content")
        ) # EO FR second row
      ) # EO demo home tabItem
      
    ) # EO tabItems

  ) # EO dashboardBody
) # EO dashboardPage
