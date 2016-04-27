# summary Plots  ----------------------------------------------------
observe({
  if (!(input$inWhichTable==1)){
    shinyjs::show(id="summPlots")
  } else {
    shinyjs::hide(id="summPlots")
  }
})

summPlots <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".summaryPlots", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    allocEff <- input$inWhichAllocation,
    whichTable <- input$inWhichTable
  ))
})

output$summPlots <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .summaryPlots(input$inSectorSum,
                input$inIndicatorSum,
                input$inFirmTypeSum,
                input$inWhichAllocation,
                input$inWhichTable
  )
  #})
}, bg = "transparent")