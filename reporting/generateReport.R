# Generate reports -----------------------------
# setwd() to handle images and other files
setwd('/Users/asanchez3/Desktop/Work/shinyProductivity/')
source('global_utils.R') # data and functions needed
# Create the data reports --------------------------------------

#for (sect in sectorList){
for (sect in c("Manufacturing")){  
  #for (type in .firmTypeListDesc(sect)){
  for (type in c("All firms")){  
    #for (ind in .indicatorList(sect)) {
    for (ind in c("labor cost (n2a) over sales (d2)")){ 
      indCode <- .indicatorToCode(ind)
      #
      sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
      typeCode <- .firmTypeCode(type)
      knit2pdf('productivityReport.Rnw', clean = TRUE,
           encoding = "UTF-8",
           output = paste0("Productivity_",indCode,sectCode,typeCode,".tex"))
      # copy pdf to final location
      file.copy(paste0("Productivity_",indCode,sectCode,typeCode,".pdf"), "/Users/asanchez3/Desktop/Work/shinyProductivity/pdf/",overwrite=TRUE)
    }
  }
}
