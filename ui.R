
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("global_utils.R", local=TRUE)


shinyUI(fluidPage(

  # Application title
  titlePanel("ES Innovation and Productivity Indicators"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # input variables
      selectInput("inCountry", "Select a country:", countryList, selected="Afghanistan"),
      radioButtons("inOutliers","Remove outliers?",choices = list("Yes"=1,"No"=0),selected = 0),
      textInput("inIQRfactor","Outlier threshold (Ot): [Q1 - Ot*IQR,Q3 + Ot*IQR]",value=3),
      selectInput("inIndicator","Select indicator",indicatorList,selected = "labor cost (n2a) over sales (d2)"),
      selectInput("inIndicatorQuant","Select indicator with which to calculate quantiles",indicatorList,selected = "sales (d2) over labor cost (n2a)"),
      actionButton("goButton","View table")
      #actionButton("goPlotsButton","View table")
    ),

    mainPanel(
      # output objects
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
))
