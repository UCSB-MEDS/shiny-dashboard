# shiny dashboard has three main components:
# header, sidebar, body
dashboardPage(
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
        id = "career_id",
        expandedName = "CAREER",
        tabName = "career_home",
        text = "Career Outcomes",
        icon = icon("road", lib = "glyphicon"),
        # sub curr career ----
        menuSubItem(
          tabName = "curr_career",
          text = "Current Career Data"
        ), # EO sub curr career menuSubItem
        # sub 5 yr career ----
        menuSubItem(
          tabName = "career_5yr",
          text = "5 Year Career Data"
        ) # EO sub 5 yr career
      ), # EO career menuItem
      # hidden career menuItem ----
      hidden(menuItem("hidden_career", 
                      tabName = "hidden_career")
             ), # EO hidden_career menuItem
      
      
      # student demographics ----
      menuItem(
        id = "demo_id",
        expandedName = "DEMO",
        tabName = "demo_home",
        text = "Student Demographics",
        icon = icon("user", lib = "glyphicon"),
        # sub curr yr demo ----
        menuSubItem(
          tabName = "curr_demo",
          text = "Current Demographics"
          ), # EO sub curr yr demo
        # sub 5 yr demo ----
        menuSubItem(
          tabName = "demo_5yr",
          text = "5 Year Demographics"
        ) # EO sub 5 yr demo 
      ), # EO student demographics menuItem
      # hidden demo menuItem ----
      hidden(menuItem("hidden_demo",
                      tabName = "hidden_demo")
             ) # EO hidden demo menuItem

    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  # BODY ----
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      
      # tabs curr yr ----
      tabItem(
        tabName = "curr_yr",
        h2("2021 Dashboard Content"),
        # curr yr db layout
        fluidRow(
          # Note(HD): value boxes are larger than info boxes
          valueBoxOutput(outputId = "military_stat"), 
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
        ), # EO FR first row
        fluidRow(
          box(title = "Title 1", width = 12, solidHeader = TRUE, status = "primary",
            "Box content")
        ), # EO FR second row
        fluidRow(
          box(title = "Title 2", "box 2 content"),
          box(title = "Title 3", "box 3 content")
        ) # EO FR third row
      ), # EO curr yr tabItem
      
      
      # tabs career ----
      tabItem(
        tabName = "hidden_career",
        h2("Career Outcomes Home Content"),
        fluidRow(
          valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
        ), # EO FR first row
        fluidRow(
          box(width = 12, title = "Title 1", "box 1 content")
        ) # EO FR second row
      ), # EO career home tabItem
      tabItem(
        tabName = "curr_career",
        h3("Current Career Content"),
        fluidRow(
          box(width = 6, title = "Title 1", "box 1 content"),
          box(width = 6, title = "Title 2", "box 2 content")
        ), # EO FR first row
        fluidRow(
          tabBox(width = 12, title = "Title 3",
                 tabPanel("Tab1", "First tab content"),
                 tabPanel("Tab2", "Tab content 2"))
        ) # EO FR second row
      ), # EO sub curr career tabItem
      tabItem(
        tabName = "career_5yr",
        h3("5 yr Career Content"),
        fluidRow(
          box(width = 6, title = "Title 1", "box 1 content"),
          box(width = 6, title = "Title 2", "box 2 content")
        ), # EO FR first row
        fluidRow(
          tabBox(width = 12, title = "Title 3",
                 tabPanel("Tab1", "First tab content"),
                 tabPanel("Tab2", "Tab content 2"))
        ) # EO FR second row
      ), # EO sub 5 yr career tabItem
      
      
      # tabs demo ----
      tabItem(
        tabName = "hidden_demo",
        h2("Student Demographics Home Content"),
        fluidRow(
          valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
          infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
        ), # EO FR first row
        fluidRow(
          box(width = 12, title = "Title 1", "box 1 content")
        ) # EO FR second row
      ), # EO demo home tabItem
      tabItem(
        tabName = "curr_demo",
        h3("Current Student Demographics Content"),
        fluidRow(
          box(width = 6, title = "Title 1", "box 1 content"),
          box(width = 6, title = "Title 2", "box 2 content")
        ), # EO FR first row
        fluidRow(
          tabBox(width = 12, title = "Title 3",
                 tabPanel("Tab1", "First tab content"),
                 tabPanel("Tab2", "Tab content 2"))
        ) # EO FR second row
      ), # EO curr student demo tabItem
      tabItem(
        tabName = "demo_5yr",
        h3("5 yr Student Demographics Content"),
        fluidRow(
          box(width = 6, title = "Title 1", "box 1 content"),
          box(width = 6, title = "Title 2", "box 2 content")
        ), # EO FR first row
        fluidRow(
          tabBox(width = 12, title = "Title 3",
                 tabPanel("Tab1", "First tab content"),
                 tabPanel("Tab2", "Tab content 2"))
        ) # EO FR second row
      ) # EO 5 yr demo tabItem
  
    ) # EO tabItems
    
  ) # EO dashboardBody
) # EO dashboardPage
