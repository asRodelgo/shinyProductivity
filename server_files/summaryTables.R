# Summary Tables ---------------------------------
# .summaryStats <- function(sector,indicatorDesc,firmType)
summaryTableGo <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    allocEff <- input$inWhichAllocation,
    whichTable <- input$inWhichTable
  ))
  
})

output$summaryTable <- renderDataTable({
  
  input$goSummaryButton
  
  isolate({
    
    summTable <- .summaryStats(input$inSectorSum,
                      input$inIndicatorSum,
                      input$inFirmTypeSum,
                      input$inWhichAllocation,
                      input$inWhichTable
                      )
    return(summTable)
  })
  
})#,include.rownames=TRUE)

