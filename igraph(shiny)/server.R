library(igraph)
options(shiny.maxRequestSize=-1) 

shinyServer(
  function(input, output) {
    
    filedata <- reactive({
      inFile = input$graph
      if (!is.null(inFile))
      read.graph(file=inFile$datapath, format="pajek")
     
    })
    
    DataDeg <- reactive({
        
      df <- filedata()
      vorder <-order(degree(df), decreasing=TRUE)
      DF <- data.frame(ID=as.numeric(V(df)[vorder]), degree=degree(df)[vorder])
      
    })
    DataBtwn <- reactive({
      df <- filedata()
      vorder <- order(betweenness(df), decreasing = TRUE)
      DF <- data.frame(ID=as.numeric(V(df)[vorder]), betweenness=betweenness(df)[vorder])
      
    })
    DataTrans <- reactive({
      df <- filedata()
      vorder <- order(transitivity(df, type="local"), decreasing = TRUE)
      DF <- data.frame(ID=as.numeric(V(df)[vorder]), transitivity=transitivity(df, type="local")[vorder])
      
      
    })
    
    output$tb <- renderUI({
      if(is.null(filedata()))
      h2("Please select Pajek file..")
    else
      tabsetPanel(type = "tabs", 
                  tabPanel("Table", tableOutput("view")) ) 
      
    }      
      )
    output$view <- renderTable({
     if(input$radio == "sorted-degree"){
      DataDeg()
     } else if(input$radio == "sorted-betweenness"){
       DataBtwn()
     } else if(input$radio == "sorted-transitivity"){
       DataTrans()
     }
      
    })
    
       output$downloadData <- downloadHandler(
      
       filename = function() {
        paste(input$radio, '.csv', sep='')
      },
     content = function(file) {
      if(input$radio == "sorted-degree"){
      write.csv(DataDeg(), file) }
      else if(input$radio == "sorted-betweenness"){
      write.csv(DataBtwn(), file)
      }
      else if(input$radio == "sorted-transitivity"){
        write.csv(DataTrans(), file)
      }
      }
      
    )
    
    
    })