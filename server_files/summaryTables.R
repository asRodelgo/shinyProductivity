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

# # a custom table container
# headT <- reactive({
#   
#   if (input$inFirmTypeSum == "By age") {
#     headTable = htmltools::withTags(table(
#       class = 'display',
#       thead(
#         tr(
#           th(rowspan = 2, ''),
#           th(colspan = 3, '0 to 10 years'), 
#           th(colspan = 3, '11 to 20 years'),
#           th(colspan = 3, '> 20 years')
#         ),
#         tr(
#           lapply(rep(c('median', 'sd','IQR'), 3), th)
#         )
#       )
#     )) 
#   } else if (input$inFirmTypeSum == "By size") {
#     headTable = htmltools::withTags(table(
#       class = 'display',
#       thead(
#         tr(
#           th(rowspan = 2, ''),
#           th(colspan = 3, 'Small firms'), 
#           th(colspan = 3, 'Medium firms'),
#           th(colspan = 3, 'Large firms')
#         ),
#         tr(
#           lapply(rep(c('median', 'sd','IQR'), 3), th)
#         )
#       )
#     ))
#   }  else if (input$inFirmTypeSum == "By export status") {
#     headTable = htmltools::withTags(table(
#       class = 'display',
#       thead(
#         tr(
#           th(rowspan = 2, ''),
#           th(colspan = 3, 'Do not export'), 
#           th(colspan = 3, 'Export')
#         ),
#         tr(
#           lapply(rep(c('median', 'sd','IQR'), 3), th)
#         )
#       )
#     ))
#   }  else if (input$inFirmTypeSum == "By foregin ownership") {
#     headTable = htmltools::withTags(table(
#       class = 'display',
#       thead(
#         tr(
#           th(rowspan = 2, ''),
#           th(colspan = 3, 'Local ownership'), 
#           th(colspan = 3, 'Foreign ownership')
#         ),
#         tr(
#           lapply(rep(c('median', 'sd','IQR'), 3), th)
#         )
#       )
#     ))
#   } else {
#     headTable = htmltools::withTags(table(
#     class = 'display',
#     thead(
#       tr(
#         th(rowspan = 1, ''),
#         lapply(rep(c('median', 'sd','IQR'), 3), th)
#         )
#       )
#     ))
#   }
#   return(headTable)
# })

output$summaryTable <- DT::renderDataTable({
  
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

})#,container = headT())

