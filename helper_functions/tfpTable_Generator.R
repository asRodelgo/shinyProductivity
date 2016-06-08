# ---------------------
# Generate tables to display indicators by Country
tfpList <- indicatorList
sectCode <- "Manuf"

COUBlock <- data.frame(t(rep(NA,111)))
names(COUBlock) <- c(as.character(countryRegions$country),"indicator","firmType","var")
for (type in .firmTypeList("Manufacturing")){
#for (type in c("age")){
  for (ind in tfpList) {
  #for (ind in c("Total factor productivity YKL: Textiles")) {  
    indCode <- .indicatorToCodeAllIndustries(ind)
    thisBlock <- read.csv(paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),stringsAsFactors = FALSE)
    thisBlock <- select(thisBlock, countryOnly,outliersOut,starts_with("N"),
                        starts_with("median"),starts_with("sd"),starts_with("iqr")
                        ,starts_with("OPcov_"))
    thisBlock <- filter(thisBlock, !is.na(countryOnly))
    col_names <- thisBlock$countryOnly
    # remove columns generated from NA adCountry to avoid errors
    thisBlock <- select(thisBlock, everything(), -ends_with("_NA"),-countryOnly)
    row_names <- names(thisBlock)
    thisBlock <- as.data.frame(t(thisBlock))
    names(thisBlock) <- col_names
    thisBlock <- mutate(thisBlock, indicator = indCode, firmType = type, var = row_names)
    
    #thisBlock <- thisBlock[-1,]
    # bind to tfpBlock
    COUBlock <- bind_rows(COUBlock,thisBlock)
  }
}

# filter data ----------------------

.COUTable <- function(cou,indicatorDesc,firmType){
  
  #cou <- "Argentina"
  #indicatorDesc <- "Total factor productivity YKL"
  #firmType <- "By imports status"
  ind <- .indicatorToCodeAllIndustries(indicatorDesc)
  ind2 <- ifelse(substr(ind,8,8)=="M","tfp2",ifelse(substr(ind,8,8)=="L","tfp3","tfp1"))
  
  
  groupByVar <- "all"
  lenVar <- 1
  
  if (firmType == "By age") {
    groupByVar <- "age"
    lenVar <- length(firmAgeList)-1
    thisList <- firmAgeList
    #dataBlock <- dataBlock_age
  }
  if (firmType == "By size") {
    groupByVar <- "size"
    lenVar <- length(firmSizeList)-1
    thisList <- firmSizeList
    #dataBlock <- dataBlock_size
  }
  if (firmType == "By exports status") {
    groupByVar <- "expStatus"
    lenVar <- length(firmExpStatusList)-1
    thisList <- firmExpStatusList
    #dataBlock <- dataBlock_expStatus
  }
  if (firmType == "By foreign ownership") {
    groupByVar <- "forOwner"
    lenVar <- length(firmForeignOwnerList)-1
    thisList <- firmForeignOwnerList
    #dataBlock <- dataBlock_forOwner
  }
  if (firmType == "By imports status") {
    groupByVar <- "impStatus"
    lenVar <- length(firmImpStatusList)-1
    thisList <- firmImpStatusList
    #dataBlock <- dataBlock_forOwner
  }
  
  
  thisCountry <- COUBlock %>%
    select(country = one_of(cou),indicator,firmType,var) %>%
    mutate(indicator2 = ifelse(substr(indicator,8,8)=="M","tfp2",ifelse(substr(indicator,8,8)=="L","tfp3","tfp1"))) %>%
    filter(indicator2 == ind2,firmType == groupByVar) #indicator == ind & 
  
  thisCountry$country <- round(thisCountry$country,2)
  
  thisCountry2 <- spread(thisCountry, var, country)
  subString <- substr(thisCountry2$indicator,nchar(thisCountry2$indicator)-1,nchar(thisCountry2$indicator))
  thisCountry2$indicator <- ifelse(subString %in% c("KL","LM"),0,subString)
  
  thisCountry2 <- merge(thisCountry2,industryMaps,by.x="indicator",by.y="industryCode",all.x = TRUE)
  thisCountry2 <- arrange(thisCountry2, indicator)
  
  # table formatting
  if (lenVar==3){
    COUTable <- thisCountry2 %>%
      select(industry,starts_with("N"),starts_with("median"),starts_with("sd"),starts_with("iqr")
             ,starts_with("OPcov_"),outliersOut) %>%
      select(industry,ends_with(thisList[2]),ends_with(thisList[3]),ends_with(thisList[4]),outliersOut)
  } else if (lenVar==2){
    COUTable <- thisCountry2 %>%
      select(industry,starts_with("N"),starts_with("median"),starts_with("sd"),starts_with("iqr")
             ,starts_with("OPcov_"),outliersOut) %>%
      select(industry,ends_with(thisList[2]),ends_with(thisList[3]),outliersOut)
  } else{
    COUTable <- thisCountry2 %>%
      select(industry,starts_with("N"),starts_with("median"),starts_with("sd"),starts_with("iqr")
             ,starts_with("OPcov_"),outliersOut)
  }
  
  # remove NAs
  COUTable <- COUTable[complete.cases(COUTable),]
  COUTable[is.na(COUTable)] <- "---"
  
  return(COUTable)

}

  
