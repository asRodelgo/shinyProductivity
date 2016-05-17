# Productivity indicators exploratory data analysis interface

library(shiny)
library(shinyjs)
source("global_utils.R", local=TRUE)

#fluidPage(
  
#  fluidRow(
    
#    column(12, h3("ES Innovation and Productivity Indicators Exploratory Analysis", style="color:#696969")),
    
    navbarPage(
      title = "ES Innovation and Productivity Indicators Exploratory Analysis",
      windowTitle = "Productivity", collapsible = TRUE, 
      inverse = FALSE, position = "fixed-top",
      theme = shinythemes::shinytheme("flatly"),
      
      
      tabPanel(title = "Summary Statistics",
               
               # Application title
               titlePanel("ES Innovation and Productivity Indicators"),
               shinyjs::useShinyjs(), # to make hide/show work
               # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
    #                # input variables
    #                selectInput("inCountry", "Select a country:", countryList, selected="Afghanistan"),
    #                radioButtons("inOutliers","Remove outliers?",choices = list("Yes"=1,"No"=0),selected = 0),
    #                textInput("inIQRfactor","Outlier threshold (Ot): [Q1 - Ot*IQR,Q3 + Ot*IQR]",value=3),
                    selectInput("inSectorSum", "Sector:", sectorList, selected="All sectors"),
                    shinyjs::hidden( # hide firm Types by default until Manufacturing is selected
                      div(id="firmTypesSum",
                      selectInput("inFirmTypeSum", "Firm characteristic:", firmTypeList, selected="All firms")
                    )),  
                    selectInput("inIndicatorSum","Indicator:",indicatorList,selected = "Labor share"),
                    shinyjs::hidden( 
                      div(id="tfpSectors",
                          selectInput("inTfpSector", "Industry:", industryList, selected="All industries")
                    )),
                    radioButtons("inWhichTable","Tables:",choices = list("Countries"=1,#"Summary Stats"=2,
                                                                         "Income groups"=3,"Region groups"=4),selected = 1),
                    actionButton("goSummaryButton","Show results"),
                    #downloadButton("downloadReport", "Download PDF report"),
                    h6("Download: ",downloadLink("summaryDownTable","data"))
    #                actionButton("goPlotsButton","View table")
                  ),
    #              
                  mainPanel(
    #                # output objects
    #                h4(textOutput("efficiency")),
    #                br(),
    #                h5(textOutput("outliersText")),
    #                br(),
                    #textOutput("showInputs"),
                    uiOutput("summaryTable")
                    #shinyjs::hidden( # hide firm Types by default until Manufacturing is selected
                    #  div(id="summPlots",
                    #      plotOutput("summPlots")
                    #))
                    
    #                tableOutput("statsTable"),
    #                plotOutput("statsPlots"),
    #                br()
                 )
                )
              ),
      tabPanel(title = "TFP By Country",
               # Application title
               titlePanel("ES Innovation and Productivity Indicators"),
               shinyjs::useShinyjs(), # to make hide/show work
               # Sidebar with a slider input for number of bins
               sidebarLayout(
                 sidebarPanel(
                   selectInput("inCountryTFP", "Country:", countryOnlyList, selected="Afghanistan"),
                   selectInput("inIndicatorTFP","Indicator:",indicatorTFPList,selected = "Total factor productivity YKL"),
                   selectInput("inFirmTypeTFP", "Firm characteristic:", firmTypeList, selected="All firms"),
                   actionButton("goTFPButton","Show results"),
                   h6("Download: ",downloadLink("tfpDownTable","data"))
                 ),
                 #              
                 mainPanel(
                   uiOutput("tfpTable")
                 )
               )   
      ),
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
                   actionButton("goButton","View table",width="200px",icon = icon("cog", lib = "glyphicon"),style = "color:white ;background-color: #337ab7"),
                   div(id="showFiltersButton",
                       actionButton("showFilters","Show tools")
                   ),
                   shinyjs::hidden( # hide firm Types by default until Manufacturing is selected
                     div(id="filters",
                         actionButton("hideFilters","Hide tools"),
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
      )
    
    )
