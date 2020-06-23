options(shiny.maxRequestSize = 1000000000)
options("esquisse.display.mode" = "browser")

source("rshiny_library.r")
source("library_wrapper.r")
source("Core Functions.r")
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