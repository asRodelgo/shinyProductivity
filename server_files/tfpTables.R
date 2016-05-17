# TFP Tables ---------------------------------

v2 <- reactiveValues(showTFPTable = TRUE)

observeEvent(input$goTFPButton, {
  v2$showTFPTable <- input$goTFPButton
})

observeEvent(input$inIndicatorSum, {
  v2$showTFPTable <- FALSE
})
observeEvent(input$inFirmTypeTFP, {
  v2$showTFPTable <- FALSE
})
observeEvent(input$inCountryTFP, {
  v2$showTFPTable <- FALSE
})

tfpAllFirms <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})
tfpByAge <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})
tfpBySize <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})
tfpByExpStatus <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})
tfpByImpStatus <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})
tfpByForeignOwner <- eventReactive(input$goTFPButton,{
  
  do.call(".tfpTable", args = list(
    cou <- input$inCountryTFP,
    indicatorDesc <- input$inIndicatorTFP,
    firmType <- input$inFirmTypeTFP
  ))
  
})

# Custom table containers
headTFPAge = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Industry'),
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

headTFPSize = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Industry'),
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
headTFPExpStatus = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Industry'),
      th(colspan = 5, 'Non Exporter'), 
      th(colspan = 5, 'Exporter'),
      th(rowspan = 2, 'Outliers removed')
    ),
    tr(
      lapply(rep(c('N','median', 'sd','IQR','OPcov'), 2), th)
    )
  )
))
headTFPImpStatus = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Industry'),
      th(colspan = 5, 'Non Importer'), 
      th(colspan = 5, 'Importer'),
      th(rowspan = 2, 'Outliers removed')
    ),
    tr(
      lapply(rep(c('N','median', 'sd','IQR','OPcov'), 2), th)
    )
  )
))
headTFPForeignOwner = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Industry'),
      th(colspan = 5, 'Domestic'), 
      th(colspan = 5, 'Foreign'),
      th(rowspan = 2, 'Outliers removed')
    ),
    tr(
      lapply(rep(c('N','median', 'sd','IQR','OPcov'), 2), th)
    )
  )
))

# try to customize withTags table headers
output$tfpTable <- renderUI({
  
  #if (is.null(input$inFirmTypeTFP))
  #  return()
  
  if (v2$showTFPTable == FALSE) return(div(br(),br(),br(),
                                       h3("Make your selection and click on 'Show Results'")))
  
  isolate({
    
      switch(input$inFirmTypeTFP,
             "All firms" = {output$tfpAllFirms <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE)
             dataTableOutput("tfpAllFirms")},
             "By age" = {output$tfpByAge <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE,container = headTFPAge)
             dataTableOutput("tfpByAge")},
             "By size" = {output$tfpBySize <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE,container = headTFPSize)
             dataTableOutput("tfpBySize")},
             "By exports status" = {output$tfpByExpStatus <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE,container = headTFPExpStatus)
             dataTableOutput("tfpByExpStatus")},
             "By imports status" = {output$tfpByImpStatus <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE,container = headTFPImpStatus)
             dataTableOutput("tfpByImpStatus")},
             "By foreign ownership" = {output$tfpByForeignOwner <- DT::renderDataTable({
               .tfpTable(input$inCountryTFP,
                         input$inIndicatorTFP,
                         input$inFirmTypeTFP
               )
             },rownames = FALSE,container = headTFPForeignOwner)
             dataTableOutput("tfpByForeignOwner")}
             
      )
  })
  
  
  #}) #,container = headTable)
})