# shiny dashboard has three main components:
# header, sidebar, body
dashboardPage(
  # HEADER ----
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  
  # SIDEBAR ----
  dashboardSidebar(
    title = "Menu",
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
        h2("2021 Dashboard Content")
      ), # EO curr yr tabItem
      
      
      # tabs career ----
      tabItem(
        tabName = "hidden_career",
        h2("Career Outcomes Home Content")
      ), # EO career home tabItem
      tabItem(
        tabName = "curr_career",
        "Current Career Content"
      ), # EO sub curr career tabItem
      tabItem(
        tabName = "career_5yr",
        "5 yr Career Content"
      ), # EO sub 5 yr career tabItem
      
      
      # tabs demo ----
      tabItem(
        tabName = "hidden_demo",
        h2("Student Demographics Home Content")
      ), # EO demo home tabItem
      tabItem(
        tabName = "curr_demo",
        "Current Student Demographics Content"
      ), # EO curr student demo tabItem
      tabItem(
        tabName = "demo_5yr",
        "5 yr Student Demographics Content"
      ) # EO 5 yr demo tabItem
  
    ) # EO tabItems
    
  ) # EO dashboardBody
) # EO dashboardPage
