# download PDF report -----------------------------
output$downloadReport <- downloadHandler(
  filename = paste0("pdf/Productivity_",
                    .indicatorToCode(input$inIndicatorSum),
                    .sectorToCode(input$inSectorSum),
                    .firmTypeCode(input$inFirmTypeSum),".pdf"),
  
  content = function(file) file.copy(paste0("pdf/Productivity_",
                                            .indicatorToCode(input$inIndicatorSum),
                                            .sectorToCode(input$inSectorSum),
                                            .firmTypeCode(input$inFirmTypeSum),".pdf"),
                                     file),
  contentType = 'application/pdf'
)
