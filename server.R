library(shiny)

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  #selectedData <- reactive({
#    iris[, c(input$xcol, input$ycol)]
  #})
  #selectedData<-readRDS("/srv/shiny-server/Data_Dashboard/airq_data.rds")
  
  
  selectedData <- reactive({
    selectedData00<-readRDS("airq_data.rds")
    a <- subset(selectedData00, site_id == input$sites)
    b<-a[, c(input$xcol, input$ycol)]
#   b$site_ids<-as.factor(b$site_id)
#    levs<-length(levels(b$site_ids))
#    levels(b$site_ids)<-seq(1,levs,1)
    return(b)
  })
  
  site_colors<-reactive({
    selectedData00<-readRDS("airq_data.rds")
  c <- subset(selectedData00, site_id == input$sites)
  sc<-c$site_id
  sc2<-as.factor(sc)
  levs<-length(levels(sc2))
  levels(sc2)<-seq(1,levs,1)
  return(sc2)
  })
  

  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
    col=site_colors(),
    pch = 20, cex = 3)
    
  },height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.45,0)) )
  
}

