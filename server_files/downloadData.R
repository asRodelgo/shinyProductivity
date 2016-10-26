# Download data ------------------

output$DownTableCOU <- downloadHandler(
  filename = function() { 
    paste0("ProductivityCountryTable.csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.COUTable(input$inCountryCOU,
                        input$inSectorCOU,
                        input$inIndicatorCOU,
                        input$inFirmTypeCOU
    ), file, row.names = TRUE)
  }
)

# Summary data
output$summaryDownTable <- downloadHandler(
  filename = function() { 
    paste0("ProductivityTable.csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.summaryStats(input$inSectorSum,
                            input$inIndicatorSum,
                            input$inFirmTypeSum,
                            input$inTfpSector,
                            input$inWhichTable
    ), file, row.names = TRUE)
  }
)

# TFP data
output$tfpDownTable <- downloadHandler(
  filename = function() { 
    paste0("TFP_Table.csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.tfpTable(input$inCountryTFP,
                            input$inIndicatorTFP,
                            input$inFirmTypeTFP
    ), file, row.names = FALSE)
  }
)


