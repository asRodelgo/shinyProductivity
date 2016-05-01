# dynamic HTML report generation with Rmarkdown ----------------------------
output$downloadReport <- downloadHandler(
  
  filename= "productivityReport.html",
  
  content = function(file) {
    inputEnv <- new.env()
    inputEnv$city <- input$city  # or whatever
    inputEnv$cityData <- cityData()
    out = rmarkdown::render("productivityReport.Rmd", envir = inputEnv)
    file.rename(out, file)
  },
  
  contentType = 'application/pdf'
)