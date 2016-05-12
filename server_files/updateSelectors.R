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

# Same for the summary presentation --------------------
# update indicator selector based on sector selected
indicatorListSum <- reactive({
  
  if (input$inSectorSum == "All sectors") {
    selectedSectorSum <- filter(summaryMaps, allSectors == 1 & sectorLevel==1)
  } else if (input$inSectorSum == "Manufacturing") {
    selectedSectorSum <- filter(summaryMaps, manufacturing == 1 & sectorLevel==1)
  } else {
    selectedSectorSum <- filter(summaryMaps, services == 1 & sectorLevel==1)
  }
  
  indListSum <- sort(unique(selectedSectorSum$indicator))
  
  return(indListSum)
  
})

observe({
  
  updateSelectInput(session, "inIndicatorSum",
                    choices = indicatorListSum())
  
})

# update firm type selector based on sector selected
firmTypeListSum <- reactive({
  
  if (!(input$inSectorSum == "Manufacturing")) {
    selectedFirmTypeSum <- c("All firms")
  } else {
    selectedFirmTypeSum <- input$inFirmTypeSum
  }
  
  firmTypeListSum <- selectedFirmTypeSum
  
  return(firmTypeListSum)
  
})

observe({
  
  updateSelectInput(session, "inFirmTypeSum",
                    selected = firmTypeListSum())
  
})


# Firm list selector
# firmListSum <- reactive({
#   
#   if (input$inFirmTypeSum == "Manufacturing") {
#     firmTypeListSum <- firmTypeList
#   } else {
#     firmTypeListSum <- c("All firms")
#   }
#   
#   return(firmTypeListSum)
#   
# })
# 
# observe({
#   
#   updateSelectInput(session, "inFirmTypeSum",
#                     choices = firmListSum())
#   
# })

