# .industryList <- function(){
#   if (substr(indicatorDesc,1,5)=="Total"){
#     
#   }
# }

.indicatorList <- function(sector){
  
  if (sector == "Manufacturing"){
    indList <- filter(summaryMaps, manufacturing == 1)$indicator
  } else if (sector == "Services"){
    indList <- filter(summaryMaps, services == 1)$indicator
  } else {
    indList <- filter(summaryMaps, allSectors == 1)$indicator
  }
  return(indList)   
}

.firmTypeList <- function(sector){
  
  if (sector == "Manufacturing"){
    firmList <- c("all","age","size","expStatus","impStatus","forOwner")
  } else {
    firmList <- c("all")
  }
  return(firmList)   
}

.firmTypeListDesc <- function(sector){
  
  if (sector == "Manufacturing"){
    firmList <- firmTypeList
  } else {
    firmList <- c("All firms")
  }
  return(firmList)   
}
.sectorToCode <- function(sector){
  
  sect <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  return(sect)
}

.firmTypeCode <- function(type){
  if (type=="All firms"){
    typeCode <- "all"
  } else if (type=="By age"){
    typeCode <- "age"
  } else if (type=="By size"){
    typeCode <- "size"
  } else if (type=="By exports status"){
    typeCode <- "expStatus"
  } else if (type=="By imports status"){
    typeCode <- "impStatus"
  } else {
    typeCode <- "forOwner"
  }
  return(typeCode)
}

.indicatorToCode <- function(indicatorDesc,industry){
  
  indicatorCode <- filter(summaryMaps,indicator == indicatorDesc)$code
  if (substr(indicatorCode,1,4)=="tfpr" & !(industry=="All industries")){
    indicatorCode <- paste(indicatorCode,industryMaps[industryMaps$industry==industry,]$industryCode,sep="_")
  }
  return(indicatorCode)
}
# This one is used when pre-computing the dataBlocks
.indicatorToCodeAllIndustries <- function(indicatorDesc){
  indicatorCode <- filter(summaryMaps,indicator == indicatorDesc)$code
  return(indicatorCode)
}

.indicatorToCodeOld <- function(indicatorDesc){
  
  indicatorCode <- filter(dataMaps,indicator == indicatorDesc)$code
}
