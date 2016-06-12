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

# -----------------------------------------------
# Country level presentation --------------------

# update indicator selector based on sector selected
indicatorListCOU <- reactive({
  
  if (input$inSectorCOU == "All sectors") {
    selectedSectorCOU <- filter(summaryMaps, allSectors == 1 & sectorLevel==1)
  } else if (input$inSectorCOU == "Manufacturing") {
    selectedSectorCOU <- filter(summaryMaps, manufacturing == 1 & sectorLevel==1)
  } else {
    selectedSectorCOU <- filter(summaryMaps, services == 1 & sectorLevel==1)
  }
  
  indListCOU <- sort(unique(selectedSectorCOU$indicator))
  
  return(indListCOU)
  
})

observe({
  
  updateSelectInput(session, "inIndicatorCOU",
                    choices = indicatorListCOU())
  
})

# update firm type selector based on sector selected
firmTypeListCOU <- reactive({
  
  if (!(input$inSectorCOU == "Manufacturing")) {
    selectedFirmTypeCOU <- c("All firms")
  } else {
    selectedFirmTypeCOU <- firmTypeList
  }
  
  firmTypeListCOU <- selectedFirmTypeCOU
  
  return(firmTypeListCOU)
  
})

observe({
  
  updateSelectInput(session, "inFirmTypeCOU",
                    selected = firmTypeListCOU())
  
})

# update industry selector based on sector selected
industryListCOU <- reactive({
  
    industryListCOU <- c("All industries",.industryList(input$inSectorCOU))
  
  return(industryListCOU)
  
})

observe({
  
  updateSelectInput(session, "inIndustryCOU",
                    selected = industryListCOU())
  
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

