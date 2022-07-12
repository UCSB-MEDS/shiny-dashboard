# shiny dashboard has three main components:
# header, sidebar, body
dashboardPage(
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  dashboardSidebar(
    id = "tabs", # Note(HD): not sure why tabs is showing up?
    title = "Menu",
    menuItem(id = "current_yr",
             text = "2021 Dashboard", # Note(HD): difference btwn text and tabName arg?
             icon = icon("dashboard")
             ), # EO current_yr menuItem
    menuItem(id = "career_db",
             text = "Career Outcomes",
             icon = icon("th")
             ), # EO career_db menuItem
    menuItem(id = "student_demo",
             text = "Student Demographics",
             icon = icon("circle"))
    
    
  ), # EO dashboardSideBar
  
  dashboardBody(
    
    
    
  ) # EO dashboardBody
) # EO dashboardPage
