# Firm types selector
# read sector. If manufacturing then show the firm type panel, else hide it
# Firm type panel:
# inFirmAge, inFirmSize, inFirmExpStatus, inFirmForeignOwner

# If manufacturing then show the firm type panel, else hide it
observe({
  if(input$inSector=="Manufacturing"){
    shinyjs::show(id="firmTypes")
  } else {
    shinyjs::hide(id="firmTypes")
  }
})
# Show/hide weights and outliers filters
observe({
  if(input$showFilters>0){ #button is clicked
    shinyjs::show(id="filters")
    shinyjs::hide(id="showFiltersButton")
  } else {
    shinyjs::hide(id="filters")
    shinyjs::show(id="showFiltersButton")
  }
})
observe({
  if(input$hideFilters>0){ #button is clicked
    shinyjs::hide(id="filters")
    shinyjs::show(id="showFiltersButton")
  } else {
    #shinyjs::hide(id="filters")
    #shinyjs::show(id="showFiltersButton")
  }
})

# Summary tab -----------------------
# If manufacturing then show the firm type panel, else hide it
observe({
  if(input$inSectorSum=="Manufacturing"){
    shinyjs::show(id="firmTypesSum")
  } else {
    shinyjs::hide(id="firmTypesSum")
  }
})

# download data ----------------------------
output$summaryDownTable <- downloadHandler(
  filename = function() { 
    paste0("ProductivityTable.csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.summaryStats(input$inSectorSum,
                            input$inIndicatorSum,
                            input$inFirmTypeSum,
                            input$inWhichAllocation,
                            input$inWhichTable
    ), file, row.names = FALSE)
  }
)

