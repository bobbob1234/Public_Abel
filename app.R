
rm(list=ls())
## Setting Upload Size to 1GB
options(shiny.maxRequestSize = 1000000000)
options("esquisse.display.mode" = "browser")
setwd('..')

source("rshiny_library.r")
source("library_wrapper.r")
source("Core Functions.r")
names <- c("Ingestion","Orverview","Modelling","Traceback")
 sidebar <-  dashboardSidebar(
        sidebarMenu(
          id = "tabs",
            menuItem("Data Ingestion",tabName = names[1]),
            menuItem("Data Overview",tabName = names[2]),
            menuItem("Modelling",tabName = names[3]),
            menuItem("Traceback",tabName = names[4])
            )
    )
   body <-  dashboardBody(
        tabItems( 
        tabItem(tabName = names[1],
                tags$hr(),
                radioButtons("datasets","Pick a dataset/process to use when investigating this app:",c("NBA  Historical Play By Play Data" = "History","RealTime NBA Play by Play Data(not implemented yet)" = "Real-Time"),selected = character(0)),
                
               
        DT::dataTableOutput("contents")        
        ),
        tabItem(tabName = names[2],
        
        h1("Data Overview"),
        tags$h2("Choose Global Data : send_to_global"),
        chooseDataUI(id = "choose1"),
          esquisserUI(
            
            id = "esquisse",
            header = FALSE, # dont display gadget title
            choose_data = TRUE, # dont display button to change data
            container = esquisseContainer(height = "700px")
            
            ),
    
          
          
        ),
      

        
        tabItem(tabName = names[3],
              
                
                fluidRow(
                  tabBox(
                    title = "10 Rows of the Data Transformation Process",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "450px",
                    tabPanel("Tab1",
                             actionButton("Data_Transform_Button","Transform Data Button"),
                             fluidRow(
                             tableOutput("processed_data")
                             
                             
                             
                             )),
                    tabPanel("Tab2", "Tab content 2")
                  ),
                  tabBox(
                    side = "left", height = "600px",
                    selected = "Association Rules",
                    tabPanel("Association Rules",
                             actionButton("AR","Produce Association Rules"),
                             fluidRow(
                               tableOutput("AR_output")
                             )
                             ),
                    tabPanel("Model Inspection", "Tab content 2"),
                    tabPanel("Model Confirmation & Results")
                  )
                ),
                
        tabItem(tabName = names[4])
        )
        
        )
        )
  
    
        

   ui <- dashboardPage(
        dashboardHeader(title = "Abel Web App"),
        sidebar,
        body
    )
server <- function(input, output) {
    
  ## TAB 1
 
  
  myData <- reactive({
   if(input$datasets == "History"){
      df <- readRDS("./rshiny_original_dataset.rds")
      df  
    }
     else 
      {
      return (NULL)
      }
      
  
    
  })
   output$contents <- DT::renderDataTable({
     send_to_global <<- myData()
      DT::datatable(myData())
      
    })
   
   

    
    ## TAB 2
  
   observeEvent(input$tabs,if(input$tabs == names[2]){
      callModule(module = esquisserServer, id = "esquisse",data = send_to_global)
     })
       



## Tab 3
     
observeEvent(input$Data_Transform_Button,{ 
out <- reactive({
  x <- data_transformation_function(send_to_global)
    x <- as.data.frame(x)
    ALL_FLAGS <<- x
    x <- head(x,10)
    x })

    output$processed_data <- renderTable({out()})

    
    
})
observeEvent(input$AR,
             if(exists("ALL_FLAGS") == TRUE && (input$AR == 1)) {
  exeucte_function_first(ALL_FLAGS)
               delete_tranpose()
               
             }
  

  else
  {
    
  }
)
}
  
                                              
                                            
  




      
    


shinyApp(ui, server)