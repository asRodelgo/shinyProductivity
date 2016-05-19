# Metadata and methodology tables ----------------------
# Indicators -----------------------------
output$metaIndicators <- DT::renderDataTable({
  metaIndicTable <- summaryMaps
  return(metaIndicTable)
},rownames = FALSE)

# Firm characteristics -----------------------------
output$metaFirms <- DT::renderDataTable({
  metaFirmsTable <- firmCharacteristicsNotes
  return(metaFirmsTable)
},rownames = FALSE,options = list(dom = 't'))

# Firm characteristics -----------------------------
output$metaGeneral <- DT::renderDataTable({
  metaGeneralTable <- metadata_generalNotes
  return(metaGeneralTable)
},rownames = FALSE,options = list(dom = 't'))
