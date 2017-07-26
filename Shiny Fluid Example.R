library(plotly)

ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel('Shiny App'),
    sidebarPanel(
      selectInput("Distribution", "Please Select Distribution Type",
                  choice = c("Normal", "Exponential")),
      sliderInput ("SampleSize", "Please Select Sample Size", 
                   min = 100, max = 5000, value = 1000, step = 100),
      conditionalPanel(condition = "input.Distribution == 'Normal'",
                       textInput("mean", "Please select the mean", 10),
                       textInput("Sd", "Select sd", 1)),
      conditionalPanel(condition = "input.Distribution == 'Exponential'",
                       textInput("lamda", "Please select the lamda:",1))
    ),
    mainPanel(
      plotlyOutput('myPlot')
      
      
    )
  )
)

#####





server <- function(input, output, session){
  
  #    output$myPlot <- reactivePlot(function(){
  output$myPlot <- renderPlotly({
    
    distType <- input$Distribution
    size <- input$SampleSize
    
    if(distType == "Normal"){
      
      randomVec <- as.data.frame(rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$Sd)))
      #randomVec <- (rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$Sd)))
    }else {
      randomVec <- as.data.frame(rexp(size, rate = 1/as.numeric(input$lamda)))
      #randomVec <- (rexp(size, rate = 1/as.numeric(input$lamda)))
      
    }
    p <- ggplot(randomVec, aes(x = randomVec[,1]))+geom_histogram(color = 'cyan', fill = 'pink', binwidth =size/1000 )
    #p <- plot_ly(x = randomVec, type= "histogram")
    ggplotly(p)
  })
}

shinyApp(ui,server)
