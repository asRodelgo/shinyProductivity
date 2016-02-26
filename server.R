
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("global_utils.R", local=TRUE)

shinyServer(function(input, output,session) {

  statsTable <- eventReactive(input$goButton,{
    
    do.call(".statsTable", args = list(
      countryYear <- input$inCountry,
      removeOutliers <- input$inOutliers,
      outlierIQRfactor <- input$inIQRfactor,
      indicatorDesc <- input$inIndicator,
      indicatorQuantileDesc <- input$inIndicatorQuant,
      weightType <- input$inWeights
    ))
    
  })
  
  output$efficiency <- renderText({
    
    input$goButton
    
    isolate({
      
      readData <- select(.statsTable(input$inCountry,
                         input$inOutliers,
                         input$inIQRfactor,
                         input$inIndicator,
                         input$inIndicatorQuant,
                         input$inWeights),Country=country,Income=income,OPcov,OPcovNoWeights,
             ratio_median_emp90_50,ratio_median_emp10_50)
      if ((readData$OPcov > 0) & (readData$ratio_median_emp90_50/readData$ratio_median_emp10_50 > 1)){
        return(paste0(input$inIndicator," is Allocation Efficient (O-P covariance > 0 and 50-90percentile/employment > 10-50percentile/employment)"))
      } else if (readData$OPcov > 0){
        return(paste0(input$inIndicator," is Directly Allocation Efficient (O-P covariance > 0)"))
      } else if (readData$ratio_median_emp90_50/readData$ratio_median_emp10_50 > 1){
        return(paste0(input$inIndicator," is Indirectly Allocation Efficient (50-90percentile/employment > 10-50percentile/employment)"))
      } else {
        return(paste0(input$inIndicator," is Allocation Inefficient"))
      }
      
    })
    
  })
  # Inform on number of outliers removed
  output$outliersText <- renderText({
    
    input$goButton
    
    isolate({
      
      readData <- select(.statsTable(input$inCountry,
                                     input$inOutliers,
                                     input$inIQRfactor,
                                     input$inIndicator,
                                     input$inIndicatorQuant,
                                     input$inWeights),outliersOut)
      
      return(paste0("Number of outliers removed from sample: ",readData$outliersOut))
      
    })
    
  })
  
  output$statsHeader <- renderTable({
    
    input$goButton
    
    isolate({
      
      select(.statsTable(input$inCountry,
                         input$inOutliers,
                         input$inIQRfactor,
                         input$inIndicator,
                         input$inIndicatorQuant,
                         input$inWeights),Country=country,Income=income,OPcov,OPcovNoWeights,
                                                ratio_median_emp90_50,ratio_median_emp10_50)
      
    })
    
  })
  
  output$statsTable <- renderTable({
    
    input$goButton
    
    isolate({
      
      # data to plot
      select(.statsTable(input$inCountry,
                input$inOutliers,
                input$inIQRfactor,
                input$inIndicator,
                input$inIndicatorQuant,
                input$inWeights),-country,-income,-OPcov,-OPcovNoWeights,
                                           -ratio_median_emp90_50,-ratio_median_emp10_50,
                                            -outliersOut)
    
    })
    
  })
  
  
  output$statsPlots <- renderPlot({
    
    input$goButton
    
    isolate({
      
      .statsPlots(input$inCountry,
                         input$inOutliers,
                         input$inIQRfactor,
                         input$inIndicator,
                         input$inIndicatorQuant)
      
    })
    
  })

  
})
