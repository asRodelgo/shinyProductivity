# Summary Tables ---------------------------------
# .summaryStats <- function(sector,indicatorDesc,firmType)
summaryTableGo <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum
  ))
  
})

output$summaryTable <- renderTable({
  
  input$goSummaryButton
  
  isolate({
    
    select(.summaryStats(input$inSectorSum,
                       input$inIndicatorSum,
                       input$inFirmTypeSum))
    
  })
  
},include.rownames=TRUE)

