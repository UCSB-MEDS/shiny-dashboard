# server instructions
server <- function(input, output, session){
  
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "CAREER"){
      updateTabItems(session, "sidebarID", selected = "hidden_career")
    }

  }) # EO OE
  
  # observeEvent(input$sidebarItemExpanded, {
  #   if(input$sidebarItemExpanded == "DEMO"){
  #     updateTabItems(session, "sidebarID", selected = "hidden_demo")
  #   }
  #   
  # }) # EO OE
  
} # EO server