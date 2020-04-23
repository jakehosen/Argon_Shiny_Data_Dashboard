#rm(list = ls())
library(shiny)
library(ggplot2)


selectedData00<-readRDS("airq_data.rds")



vars <- setdiff(names(selectedData00), "Sensor Variables")


ui <- fluidPage(
  titlePanel(title="Purdue FNR498 Air Quality Sensor Data"),
  headerPanel(h4("Select Variables")),
  sidebarPanel( width=2,
    selectInput('xcol', 'X Variable', vars, selected = vars[[10]]),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[7]]),
    selectInput('colorby', 'Color Variable', vars, selected = vars[[11]]),
    selectInput("sites", "Choose Site", choices=unique(selectedData00$site_id),multiple=TRUE, selected=c("1036907","1036916")),
    p("To zoom in on the plot: click and drag over the area over which you want to zoom in. Then double-click. To zoom out, double click the plot again."),
    tags$a(href="http://ecosystemscience.io:3838/Data_Downloader/", "Download Data"),
    h5("Site details and map coming soon.")
    ),
  
#    sliderInput("num", "Number:",min = 0, max = 5,step=1,value=c(1,2)),
  mainPanel(plotOutput("plot2",
                       dblclick = "plot1_dblclick",
                       brush = brushOpts(
                         id = "plot1_brush",
                         resetOnNew = TRUE
                       ))))

server <- function(input,output){
  #selectedData2<-reactive({
  ranges <- reactiveValues(x = NULL, y = NULL)
    #})
  
  selectedData <- reactive({
    selectedData00<-readRDS("airq_data.rds")
    #  selectedData00<-readRDS("airq_data.rds")
    b0 <- subset(selectedData00, site_id == input$sites)
    if(input$xcol=="dt"){
      b<-data.frame(x=b0[,c(input$xcol)],y=as.numeric(b0[,c(input$ycol)]),colorby=b0[,c(input$colorby)])
      }else{b<-data.frame(x=as.numeric(b0[,c(input$xcol)]),y=as.numeric(b0[,c(input$ycol)]),colorby=b0[,c(input$colorby)])}
    
    
    #  b<-a[, c("Temperature","Pressure")]
    #   b$site_ids<-as.factor(b$site_id)
    #    levs<-length(levels(b$site_ids))
    #    levels(b$site_ids)<-seq(1,levs,1)
    #print(b)
    return(b)
  })

  
  output$plot2<-renderPlot({
    colorby_title<-gsub("site_id","Site ID",input$colorby,fixed=TRUE)
    if(input$xcol=="dt"){
      ggplot(selectedData(),aes(x=x,y=y,color=colorby))+
        geom_point()+
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
        xlab("")+
        ylab(paste(input$ycol))+
        labs(colour=paste(colorby_title))
    }else{ggplot(selectedData(),aes(x=x,y=y,color=colorby))+
        geom_point()+
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
        xlab(paste(input$xcol))+
        ylab(paste(input$ycol))+
        labs(colour=paste(colorby_title))
    }
  },height = 500,width = 600)
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      if(input$xcol=="dt"){ranges$x <- c(as.POSIXct(brush$xmin,origin = "1970-01-01"), as.POSIXct(brush$xmax,origin = "1970-01-01"))
      }else{ranges$x <- c(brush$xmin, brush$xmax)}
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  }







shinyApp(ui, server)







