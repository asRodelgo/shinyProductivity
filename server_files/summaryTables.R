# Summary Tables ---------------------------------
# .summaryStats <- function(sector,indicatorDesc,firmType)
# summaryTableGo <- eventReactive(input$goSummaryButton,{
#   
#   do.call(".summaryStats", args = list(
#     sector <- input$inSectorSum,
#     indicatorDesc <- input$inIndicatorSum,
#     firmType <- input$inFirmTypeSum,
#     industry <- input$inTfpSector,
#     whichTable <- input$inWhichTable
#   ))
#   
# })

tableAllFirms <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    industry <- input$inTfpSector,
    whichTable <- input$inWhichTable
  ))
  
})
tableByAge <- eventReactive(input$goSummaryButton,{
    
    do.call(".summaryStats", args = list(
      sector <- input$inSectorSum,
      indicatorDesc <- input$inIndicatorSum,
      firmType <- input$inFirmTypeSum,
      industry <- input$inTfpSector,
      whichTable <- input$inWhichTable
    ))
    
})
tableBySize <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    industry <- input$inTfpSector,
    whichTable <- input$inWhichTable
  ))
  
})
tableByExpStatus <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    industry <- input$inTfpSector,
    whichTable <- input$inWhichTable
  ))
  
})
tableByImpStatus <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    industry <- input$inTfpSector,
    whichTable <- input$inWhichTable
  ))
  
})
tableByForeignOwner <- eventReactive(input$goSummaryButton,{
  
  do.call(".summaryStats", args = list(
    sector <- input$inSectorSum,
    indicatorDesc <- input$inIndicatorSum,
    firmType <- input$inFirmTypeSum,
    industry <- input$inTfpSector,
    whichTable <- input$inWhichTable
  ))
  
})
# Custom table containers
    headTableAge = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(rowspan = 2, 'Number of countries'),
          th(colspan = 5, 'Young'), 
          th(colspan = 5, 'Mature'),
          th(colspan = 5, 'Old')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    
    headTableSize = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(rowspan = 2, 'Number of countries'),
          th(colspan = 5, 'Small'), 
          th(colspan = 5, 'Medium'),
          th(colspan = 5, 'Large')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableExpStatus = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(rowspan = 2, 'Number of countries'),
          th(colspan = 5, 'Non Exporter'), 
          th(colspan = 5, 'Exporter')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableImpStatus = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(rowspan = 2, 'Number of countries'),
          th(colspan = 5, 'Non Importer'), 
          th(colspan = 5, 'Importer')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableForeignOwner = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Group'),
          th(rowspan = 2, 'Number of countries'),
          th(colspan = 5, 'Domestic'), 
          th(colspan = 5, 'Foreign')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    
    headTableAge_1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Country'),
          th(rowspan = 2, 'Year'),
          th(colspan = 5, 'Young'), 
          th(colspan = 5, 'Mature'),
          th(colspan = 5, 'Old'),
          th(rowspan = 2, 'Outliers removed')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    
    headTableSize_1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Country'),
          th(rowspan = 2, 'Year'),
          th(colspan = 5, 'Small'), 
          th(colspan = 5, 'Medium'),
          th(colspan = 5, 'Large'),
          th(rowspan = 2, 'Outliers removed')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableExpStatus_1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Country'),
          th(rowspan = 2, 'Year'),
          th(colspan = 5, 'Non Exporter'), 
          th(colspan = 5, 'Exporter'),
          th(rowspan = 2, 'Outliers removed')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableImpStatus_1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Country'),
          th(rowspan = 2, 'Year'),
          th(colspan = 5, 'Non Importer'), 
          th(colspan = 5, 'Importer'),
          th(rowspan = 2, 'Outliers removed')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
    headTableForeignOwner_1 = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Country'),
          th(rowspan = 2, 'Year'),
          th(colspan = 5, 'Domestic'), 
          th(colspan = 5, 'Foreign'),
          th(rowspan = 2, 'Outliers removed')
        ),
        tr(
          lapply(rep(c('N','median', 'sd','IQR','OPcov'), 3), th)
        )
      )
    ))
# output$summaryTable <- DT::renderDataTable({
#   
#   input$goSummaryButton
#   
#   isolate({
#       
#     summTable <- .summaryStats(input$inSectorSum,
#                       input$inIndicatorSum,
#                       input$inFirmTypeSum,
#                       input$inTfpSector,
#                       input$inWhichTable
#                       )
#     return(summTable)
#   
#   })
# 
# },rownames = FALSE) #,container = headTable)

# try to customize withTags table headers
output$summaryTable <- renderUI({
  
  #if (is.null(input$inFirmTypeSum))
  #  return()
  
  input$goSummaryButton
    
  isolate({
    
    if (!(input$inWhichTable==1)){
      switch(input$inFirmTypeSum,
             "All firms" = {output$tableAllFirms <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE)
             dataTableOutput("tableAllFirms")},
             "By age" = {output$tableByAge <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableAge)
             dataTableOutput("tableByAge")},
             "By size" = {output$tableBySize <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableSize)
             dataTableOutput("tableBySize")},
             "By exports status" = {output$tableByExpStatus <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableExpStatus)
             dataTableOutput("tableByExpStatus")},
             "By imports status" = {output$tableByImpStatus <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableImpStatus)
             dataTableOutput("tableByImpStatus")},
             "By foreign ownership" = {output$tableByForeignOwner <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableForeignOwner)
             dataTableOutput("tableByForeignOwner")}
      
      )  
    } else {
      switch(input$inFirmTypeSum,
             "All firms" = {output$tableAllFirms <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE)
             dataTableOutput("tableAllFirms")},
             "By age" = {output$tableByAge <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableAge_1)
             dataTableOutput("tableByAge")},
             "By size" = {output$tableBySize <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableSize_1)
             dataTableOutput("tableBySize")},
             "By exports status" = {output$tableByExpStatus <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableExpStatus_1)
             dataTableOutput("tableByExpStatus")},
             "By imports status" = {output$tableByImpStatus <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableImpStatus_1)
             dataTableOutput("tableByImpStatus")},
             "By foreign ownership" = {output$tableByForeignOwner <- DT::renderDataTable({
               .summaryStats(input$inSectorSum,
                             input$inIndicatorSum,
                             input$inFirmTypeSum,
                             input$inTfpSector,
                             input$inWhichTable
               )
             },rownames = FALSE,container = headTableForeignOwner_1)
             dataTableOutput("tableByForeignOwner")}
             
      )
    }
    
  }) #,container = headTable)
})