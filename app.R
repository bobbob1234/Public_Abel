

library(shiny)
library(shinydashboard)
library(shinyAce)

names <- c("Ingestion","Transformation","Data Overview + Summary","Tuning","Attr","Model","Eval","Traceback")
 sidebar <-  dashboardSidebar(
        sidebarMenu(
            menuItem("Data Ingestion",tabName = names[1]),
            menuItem("Data Transformation",tabName = names[2]),
            menuItem("Data Overview",tabName = names[3]),
            menuItem("Hyper Parameter Tuning",tabName = names[4]),
            menuItem("Attribution",tabName = names[5]),
            menuItem("AR - Construction",tabName = names[6]),
            menuItem("Modelling",tabName = names[7]),
            menuItem("Model Evaluation",tabName = names[8]),
            menuItem("Traceback + Results",tabName = names[9])
            
        )
    )
   body <-  dashboardBody(
        tabItems(
        tabItem(tabName = names[1]),
        tabItem(tabName = names[2],
        aceEditor("code",mode = "r"),
        actionButton("eval", "Update UI"),
        htmlOutput("shinyUI")
        ),
        
        tabItem(tabName = names[3]),
        tabItem(tabName = names[4]),
        tabItem(tabName = names[5]),
        tabItem(tabName = names[6]),
        tabItem(tabName = names[7]),
        tabItem(tabName = names[8]),
        tabItem(tabName = names[9])
        
        )
    )
    
        

   ui <- dashboardPage(
        dashboardHeader(title = "Simple tabs"),
        sidebar,
        body
    )
server <- function(input, output) {
    
    output$shinyUI <- renderUI({
        input$eval
        eval(parse(text = isolate(input$code)))
    })
}

shinyApp(ui, server)