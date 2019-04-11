library(shiny)


server <- function(input, output){
  
  output$prediction <- renderValueBox({
    valueBox(
      "d"
      ,"Prediction"
    )
  })
  
  
  output$irisgen <- renderPlot(
    width = 700
    , height = 600
    , {
      ggplot()+
        geom_point(aes(iris[, input$xaxis], iris[,input$yaxis], fill = factor(iris$Species)), size = 2, shape=21, color = 'white') +
        xlab(input$xaxis) +
        ylab(input$yaxis) + 
        guides(fill=guide_legend(title="Iris Species"))
    }
  )
  
  
  observe({
    # Model action button
    observeEvent(
      input$savePoint
      , {
      maxrow <- get_max_row()
      print("Creating a new row")
      new_row <- cbind(input$seplength, input$sepwidth, input$petlength, input$petwidth)
      df_points <<- rbind(df_points, new_row)
      names(df_points) <- var.names
      inputandoutputs <<- cbind(df_points, get_probs(df_points))
      output$customdt <- DT::renderDataTable(inputandoutputs)
      
      output$irisgen <- renderPlot(width = 700, height = 600,
        ggplot()+
          geom_point(aes(iris[, input$xaxis], iris[,input$yaxis], fill = factor(iris$Species)), size = 2, shape=21, color = 'white') +
          geom_point(aes(inputandoutputs[, input$xaxis], inputandoutputs[,input$yaxis], fill = factor(inputandoutputs$prediction)), size = 3, shape=21, color='black') +
          xlab(input$xaxis) +
          ylab(input$yaxis) + 
          guides(fill=guide_legend(title="Iris Species"))
      )
    })
    
    observeEvent(
      input$clearPoints
      , {
        print("Deleting custom data")
        df_points <<- df_points[0,]
        names(df_points) <- var.names
        inputandoutputs <<- inputandoutputs[0,]
        output$customdt <- DT::renderDataTable(inputandoutputs)
        
        output$irisgen <- renderPlot(
          width = 700
          , height = 600
          , ggplot() +
            geom_point(
              aes(iris[, input$xaxis], iris[, input$yaxis], fill = factor(iris$Species)),
              size = 2,
              shape = 21,
              color = 'white'
            ) +
            xlab(input$xaxis) +
            ylab(input$yaxis) +
            guides(fill = guide_legend(title =
                                         "Iris Species"))
        )
      }
    )
    
  })
  
  # 
  # deleterowdf <- reactive(
  #   input$clearPoints
  #   , if (!exists(df_points)){
  #     print("Doing nothing")
  #   } else {
  #     df_points <- df_points[0,]
  #   }
  # )
  
  
  # output$customdt <- DT::renderDataTable(
  #   df_points
  # )
  
  
  
}