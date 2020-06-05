

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        tabItems(tabName = "Data Ingestion",
                 fluidRow(
                     
                 )),
        
))
server <- function(input, output) { }

shinyApp(ui, server)