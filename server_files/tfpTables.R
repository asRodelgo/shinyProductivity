# TFP Tables ---------------------------------

v2 <- reactiveValues(showCOUTable = TRUE)

observeEvent(input$goButtonCOU, {
  v2$showCOUTable <- input$goButtonCOU
})

observeEvent(input$inIndicatorCOU, {
  v2$showCOUTable <- FALSE
})
observeEvent(input$inFirmTypeCOU, {
  v2$showCOUTable <- FALSE
})
observeEvent(input$inCountryCOU, {
  v2$showCOUTable <- FALSE
})

AllFirmsCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})
ByAgeCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})
BySizeCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})
ByExpStatusCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})
ByImpStatusCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})
ByForeignOwnerCOU <- eventReactive(input$goButtonCOU,{
  
  do.call(".COUTable", args = list(
    cou <- input$inCountryCOU,
    indicatorDesc <- input$inIndicatorCOU,
    firmType <- input$inFirmTypeCOU
  ))
  
})

# Custom table containers
headCOUAge = htmltools::withTags(table(
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

headCOUSize = htmltools::withTags(table(
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
headCOUExpStatus = htmltools::withTags(table(
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
headCOUImpStatus = htmltools::withTags(table(
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
headCOUForeignOwner = htmltools::withTags(table(
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
output$TableCOU <- renderUI({
  
  #if (is.null(input$inFirmTypeCOU))
  #  return()
  
  if (v2$showCOUTable == FALSE) return(div(br(),br(),br(),
                                       h3("Make your selection and click on 'Show Results'")))
  
  isolate({
    
      switch(input$inFirmTypeCOU,
             "All firms" = {output$AllFirmsCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,options = list(dom = 't'))
             dataTableOutput("AllFirmsCOU")},
             "By age" = {output$ByAgeCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,container = headCOUAge,options = list(dom = 't'))
             dataTableOutput("ByAgeCOU")},
             "By size" = {output$BySizeCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,container = headCOUSize,options = list(dom = 't'))
             dataTableOutput("BySizeCOU")},
             "By exports status" = {output$ByExpStatusCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,container = headCOUExpStatus,options = list(dom = 't'))
             dataTableOutput("ByExpStatusCOU")},
             "By imports status" = {output$ByImpStatusCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,container = headCOUImpStatus,options = list(dom = 't'))
             dataTableOutput("ByImpStatusCOU")},
             "By foreign ownership" = {output$ByForeignOwnerCOU <- DT::renderDataTable({
               .COUTable(input$inCountryCOU,
                         input$inIndicatorCOU,
                         input$inFirmTypeCOU
               )
             },rownames = FALSE,container = headCOUForeignOwner,options = list(dom = 't'))
             dataTableOutput("ByForeignOwnerCOU")}
             
      )
  })
  
  
  #}) #,container = headTable)
})