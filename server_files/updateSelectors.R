# update indicator selector based on sector selected
indicatorList <- reactive({
  
  if (input$inSector == "All sectors") {
    selectedSector <- filter(dataMaps, allSectors == 1)
  } else if (input$inSector == "manufacturing") {
    selectedSector <- filter(dataMaps, manufacturing == 1)
  } else {
    selectedSector <- filter(dataMaps, services == 1)
  }
  
  indList <- sort(unique(selectedSector$indicator))
  
  return(indList)
  
})

observe({
  
  updateSelectInput(session, "inIndicator",
                    choices = indicatorList())
  
})

# update indicatorQuantile selector based on sector selected
indicatorQuantList <- reactive({
  
  if (input$inSector == "All sectors") {
    selectedSector <- filter(dataMaps, allSectors == 1)
  } else if (input$inSector == "manufacturing") {
    selectedSector <- filter(dataMaps, manufacturing == 1)
  } else {
    selectedSector <- filter(dataMaps, services == 1)
  }
  
  indQuantList <- sort(unique(selectedSector$indicator))
  
  return(indQuantList)
  
})

observe({
  
  updateSelectInput(session, "inIndicatorQuant",
                    choices = indicatorQuantList())
  
})
