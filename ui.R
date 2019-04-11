library(shinydashboard)



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Iris Custom Predictions", icon = icon("pencil"), tabName = "preds", badgeLabel = "try me", badgeColor = "green")
    , menuItem("Iris Overview", tabName = "irisoverview", icon = icon("map"))
  )
)
#"The four slider inputs allow you to test various points among the features. Hit Create to save point or clear to remove all points.  The X-axis and Y-axis selectors are just for the visual display."
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "preds"
      , h3("The four slider inputs allow you to test various points among the features. Hit Create to save point or clear to remove all points.  The X-axis and Y-axis selectors are just for the visual display.")
      , fluidRow(
        box(
          width=12
          , title = "Custom Data Inputs"
          , splitLayout(
            sliderInput(
              inputId = 'seplength'
              , label = "Sepal Length"
              , value = round( mean(iris$Sepal.Length),1)
              , min = 0
              , max = round(max(iris$Sepal.Length+2))
              , step = .1
              , round = FALSE
              , width = 200
            )
            , sliderInput(
              inputId = 'sepwidth'
              , label = "Sepal Width"
              , value = round( mean(iris$Sepal.Width),1)
              , min = 0
              , max = round(max(iris$Sepal.Width+2))
              , step = .1
              , round = FALSE
              , width = 200
            )
            , sliderInput(
              inputId = 'petlength'
              , label = "Petal Length"
              , value = round( mean(iris$Petal.Length),1)
              , min = 0
              , max = round(max(iris$Petal.Length+2))
              , step = .1
              , round = FALSE
              , width = 200
            )
            , sliderInput(
              inputId = 'petwidth'
              , label = "Petal Width"
              , value = round( mean(iris$Petal.Width),1)
              , min = 0
              , max = round(max(iris$Petal.Width+2))
              , step = .1
              , round = FALSE
              , width = 200
            )
            , radioButtons(
              inputId = "xaxis"
              , label = "X-Axis"
              , selected = "Petal.Length"
              , choiceNames = gsub(x = var.names, pattern = "[.]",  replacement =  " ")
              , choiceValues = var.names
            )
            , radioButtons(
              inputId = "yaxis"
              , label = "Y-Axis"
              , selected = "Petal.Width"
              , choiceNames = gsub(x = var.names, pattern = "[.]",  replacement =  " ")
              , choiceValues = var.names
            )
          )
          , actionButton("savePoint", "Create Point")
          , actionButton("clearPoints", "Clear Points")
        )
      )
      , fluidRow(
        column(
          6
          , plotOutput('irisgen', width = "600px", height = "400px")
        )
        , column(
          6
          , plotOutput('irisdens', width = "600px", height = "400px")
        )
      )
      , hr()
      , fluidRow(
        DT::dataTableOutput('customdt')
      )
      )
    
    , tabItem(
      tabName = "Overview"
      , h2("???")
      )
  )

)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Iris Predictions"),
  sidebar,
  body
)