

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
        tabItem(tabName = names[1],
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")
                
        ),
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
    
    ## Data Ingestion
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
}

shinyApp(ui, server)