
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("global_utils.R", local=TRUE)

shinyServer(function(input, output,session) {
  
  # source server files
  files <- list.files("server_files", full.names=TRUE,recursive = TRUE)
  for (f in files) source(f,local = TRUE)
  
  # check out output object
  #output$ageOutput <- renderText(paste(input$inFirmAge[1],input$inFirmExpStatus[1],input$inFirmForeignOwner[1]))
  
  statsTable <- eventReactive(input$goButton,{
    
    do.call(".statsTable", args = list(
      sector <- input$inSector,
      countryYear <- input$inCountry,
      removeOutliers <- input$inOutliers,
      outlierIQRfactor <- input$inIQRfactor,
      indicatorDesc <- input$inIndicator,
      indicatorQuantileDesc <- input$inIndicatorQuant,
      weightType <- input$inWeights,
      ageRange <- input$inFirmAge,
      sizeRange <- input$inFirmSize,
      expStatus <- input$inFirmExpStatus,
      forOwner <- input$inFirmForeignOwner
    ))
    
  })
  
  output$efficiency <- renderText({
    
    input$goButton
    
    isolate({
      
      readData <- select(.statsTable(input$inSector,
                                     input$inCountry,
                                     input$inOutliers,
                                     input$inIQRfactor,
                                     input$inIndicator,
                                     input$inIndicatorQuant,
                                     input$inWeights,
                                     input$inFirmAge,
                                     input$inFirmSize,
                                     input$inFirmExpStatus,
                                     input$inFirmForeignOwner),Country=country,Income=income,OPcov,OPcovNoWeights,
             ratio_median_emp90_50,ratio_median_emp10_50)
      if (!is.na(readData$OPcov)){
        if ((readData$OPcov > 0) & (readData$ratio_median_emp90_50/readData$ratio_median_emp10_50 > 1)){
          return(paste0(input$inIndicator," is Allocation Efficient"))
          # (O-P covariance > 0 and 50-90percentile/employment > 10-50percentile/employment)"))
        } else if (readData$OPcov > 0){
          return(paste0(input$inIndicator," is Directly Allocation Efficient")) #(O-P covariance > 0)"))
        } else if (readData$ratio_median_emp90_50/readData$ratio_median_emp10_50 > 1){
          return(paste0(input$inIndicator," is Indirectly Allocation Efficient")) #(50-90percentile/employment > 10-50percentile/employment)"))
        } else {
          return(paste0(input$inIndicator," is Allocation Inefficient"))
        }
      } else{
        return("Sample size too small to show results. Please expand the selection")
      }
    })
    
  })
  # Inform on number of outliers removed
  output$outliersText <- renderText({
    
    input$goButton
    
    isolate({
      
      readData <- select(.statsTable(input$inSector,
                                     input$inCountry,
                                     input$inOutliers,
                                     input$inIQRfactor,
                                     input$inIndicator,
                                     input$inIndicatorQuant,
                                     input$inWeights,
                                     input$inFirmAge,
                                     input$inFirmSize,
                                     input$inFirmExpStatus,
                                     input$inFirmForeignOwner),outliersOut)
      
      if (!is.na(readData$outliersOut)){
        return(paste0("Number of outliers removed from sample: ",readData$outliersOut))
      } else {
        return("")        
      }
    })
    
  })
  
  output$statsHeader <- renderTable({
    
    input$goButton
    
    isolate({
      
      select(.statsTable(input$inSector,
                         input$inCountry,
                         input$inOutliers,
                         input$inIQRfactor,
                         input$inIndicator,
                         input$inIndicatorQuant,
                         input$inWeights,
                         input$inFirmAge,
                         input$inFirmSize,
                         input$inFirmExpStatus,
                         input$inFirmForeignOwner),Country=country,Income=income,OPcov,OPcovNoWeights,
                                                ratio_median_emp90_50,ratio_median_emp10_50)
      
    })
    
  },include.rownames=FALSE)
  
  output$statsTable <- renderTable({
    
    input$goButton
    
    isolate({
      
      # data to plot
      select(.statsTable(input$inSector,
                         input$inCountry,
                         input$inOutliers,
                         input$inIQRfactor,
                         input$inIndicator,
                         input$inIndicatorQuant,
                         input$inWeights,
                         input$inFirmAge,
                         input$inFirmSize,
                         input$inFirmExpStatus,
                         input$inFirmForeignOwner),-country,-income,-OPcov,-OPcovNoWeights,
                                           -ratio_median_emp90_50,-ratio_median_emp10_50,
                                            -outliersOut)
    
    })
    
  },include.rownames=FALSE)
  
  
  output$statsPlots <- renderPlot({
    
    input$goButton
    
    isolate({
      
      .statsPlots(input$inSector,
                  input$inCountry,
                  input$inOutliers,
                  input$inIQRfactor,
                  input$inIndicator,
                  input$inIndicatorQuant,
                  input$inFirmAge,
                  input$inFirmSize,
                  input$inFirmExpStatus,
                  input$inFirmForeignOwner)
      
    })
    
  })
  output$showInputs <- renderText({
    
    c(input$inSectorSum,
    input$inFirmTypeSum,
    input$inIndicatorSum,
    input$inTfpSector,
    input$inWhichTable,
    input$goSummaryButton)
    
    })
  
  output$countryName <- renderText({
    
    input$goButtonCOU
    
    isolate({
      countryRegions[countryRegions$country==input$inCountryCOU,]$countryDes
    })
  })
  
  output$indicatorName <- renderText({
    
    input$goButtonCOU
    
    isolate({
      input$inIndicatorCOU
    })
  })
  
})
