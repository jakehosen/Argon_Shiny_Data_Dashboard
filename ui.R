
library(shiny)
# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
#plotOutput("plot1", click = "plot_click")


selectedData<-readRDS("airq_data.rds")
vars <- setdiff(names(selectedData), "Species")

pageWithSidebar(
  headerPanel('Air Quality Sensor Data'),
  sidebarPanel(width=3,
    selectInput('xcol', 'X Variable', vars, selected = vars[[4]]),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    fluidPage(
      #User dropbox
      selectInput("sites", "Choose Site", choices=unique(selectedData$site_id),multiple=TRUE, selected=c("1036907","1036916")),
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            '))
    ),
    tags$a(href="http://ecosystemscience.io:3838/Data_Downloader/", "Download Data"),
  ),
  mainPanel(
    plotOutput('plot1'),width=9
  )
)