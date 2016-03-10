
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
source("global_utils.R", local=TRUE)


navbarPage(
  title = "ES Innovation and Productivity Indicators",
  windowTitle = "Productivity", collapsible = TRUE, 
  inverse = FALSE, position = "fixed-top",
  #theme = shinythemes::shinytheme("flatly"),
  
  tabPanel(title = "Data checks",
           
  # Application title
    titlePanel("ES Innovation and Productivity Indicators"),
    shinyjs::useShinyjs(), # to make hide/show work
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        # input variables
        selectInput("inCountry", "Select country:", countryList, selected="Afghanistan"),
        selectInput("inSector", "Select sector:", sectorList, selected="All sectors"),
        selectInput("inIndicator","Select indicator",indicatorList,selected = "labor cost (n2a) over sales (d2)"),
        selectInput("inIndicatorQuant","Select indicator with which to calculate quantiles",indicatorList,selected = "sales (d2) over labor cost (n2a)"),
        # types of firm
        #selectInput("inFirmType","Filter by firm type",firmTypeList,selected = "All firms"),
        shinyjs::hidden( # hide firm Types by default until Manufacturing is selected
          div(id="firmTypes",
            sliderInput("inFirmAge","Select firm age range",min = 0,max = 200, value = c(0,200), 
                        step = 5),
            selectInput("inFirmSize","Select firm size range",firmSizeList,selected = "All firms"),
            sliderInput("inFirmExpStatus","Select firm export status % range",min = 0,max = 100, value = c(0,100), 
                        step = 10),
            #selectInput("inFirmTechInnov","Select firm tech innovation",firmTechInnovList,selected = "All firms"),
            sliderInput("inFirmForeignOwner","Select firm foreign ownership % range",min = 0,max = 100, value = c(0,100), 
                        step = 10)
          )
        ),
        br(),
        actionButton("goButton","View table",width="200px",icon = icon("cog", lib = "glyphicon"),style = "background-color: #337ab7"),
        div(id="showFiltersButton",
            actionButton("showFilters","Show filters")
        ),
        shinyjs::hidden( # hide firm Types by default until Manufacturing is selected
          div(id="filters",
            actionButton("hideFilters","Hide filters"),
            br(),  
            radioButtons("inOutliers","Remove outliers?",choices = list("Yes"=1,"No"=0),selected = 0),
            textInput("inIQRfactor","Outlier threshold (Ot): [Q1 - Ot*IQR,Q3 + Ot*IQR]",value=3),
            radioButtons("inWeights","Select type of weight",choices = list("Sampling"=1,"Market share"=2,"Employment share"=3),selected = 1)
          )
        )
        #actionButton("goPlotsButton","View table")
      ),
  
      mainPanel(
        # output objects
        #textOutput("ageOutput"),
        h4(textOutput("efficiency")),
        br(),
        h5(textOutput("outliersText")),
        br(),
        tableOutput("statsHeader"),
        tableOutput("statsTable"),
        plotOutput("statsPlots"),
        br()
      )
    )
  ),
  tabPanel(title = "Data navigation",
           
           # Application title
           titlePanel("ES Innovation and Productivity Indicators")
           
           # Sidebar with a slider input for number of bins
#            sidebarLayout(
#              sidebarPanel(
#                # input variables
#                selectInput("inCountry", "Select a country:", countryList, selected="Afghanistan"),
#                radioButtons("inOutliers","Remove outliers?",choices = list("Yes"=1,"No"=0),selected = 0),
#                textInput("inIQRfactor","Outlier threshold (Ot): [Q1 - Ot*IQR,Q3 + Ot*IQR]",value=3),
#                selectInput("inIndicator","Select indicator",indicatorList,selected = "labor cost (n2a) over sales (d2)"),
#                selectInput("inIndicatorQuant","Select indicator with which to calculate quantiles",indicatorList,selected = "sales (d2) over labor cost (n2a)"),
#                radioButtons("inWeights","Select type of weight",choices = list("Sampling"=1,"Market share"=2,"Employment share"=3),selected = 1),
#                actionButton("goButton","View table")
#                #actionButton("goPlotsButton","View table")
#              ),
#              
#              mainPanel(
#                # output objects
#                h4(textOutput("efficiency")),
#                br(),
#                h5(textOutput("outliersText")),
#                br(),
#                tableOutput("statsHeader"),
#                tableOutput("statsTable"),
#                plotOutput("statsPlots"),
#                br()
#              )
#            )
  )
)
