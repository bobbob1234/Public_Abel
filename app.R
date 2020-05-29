

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Personalied Retargeting App"),
    tabsetPanel(

        tabPanel("Data Ingestion"),
        tabPanel("Data Transformation"),
        tabPanel("Data Overview + Summary"),
        tabPanel("Filtering Parameters"),
        tabPanel("Attribution"),
        tabPanel("AR - Construction"),
        tabPanel("Modelling"),
        tabPanel("Model Evaluation"),
        tabPanel("Traceback + Results")
        
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
