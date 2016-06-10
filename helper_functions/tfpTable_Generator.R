# --------------------- Generate tables to display indicators by Country
#
# Process the selected type and industry block: COUBlock 
# (All indicators for all sectors for all countries in one dataBlock)

.generateCOUBlock <- function(sect,type){

  # initialize variables
  COUBlock <- data.frame(t(rep(NA,112)))
  names(COUBlock) <- c(as.character(countryRegions$country),"indicator","firmType","industry","var")
  
  # populate COUBlock
  #for (sector in sectorList){
  #  for (type in .firmTypeList(sector)){
    #for (type in c("age")){
      for (ind in .indicatorList(sect)) {
      #for (ind in c("Total factor productivity YKL: Textiles")) {  
        for (indus in c("All industries",.industryList(sect))){
          indCode <- .indicatorToCodeAllIndustries(ind)
          sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
          # when industry is specified dataBlock adds the isic code at the end
          if (indus == "All industries"){
            isicCode <- "0"
            thisBlock <- read.csv(paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),stringsAsFactors = FALSE)
          } else {
            isicCode <- .industryToCode(indus)
            blockPath <- paste0("data/dataBlock_",paste(sectCode,type,indCode,isicCode,sep="_"),".csv")
            if (file.exists(blockPath)){ 
              thisBlock <- read.csv(blockPath,stringsAsFactors = FALSE)
            } else {thisBlock <- data.frame()}
            
          }
          if (nrow(thisBlock)>0){ # empty dataBlock will return errors
            thisBlock <- select(thisBlock, countryOnly,outliersOut,starts_with("N"),
                                starts_with("median"),starts_with("sd"),starts_with("iqr")
                                ,starts_with("OPcov_"))
            thisBlock <- filter(thisBlock, !is.na(countryOnly))
            col_names <- thisBlock$countryOnly
            # remove columns generated from NA adCountry to avoid errors
            thisBlock <- select(thisBlock, everything(), -ends_with("_NA"),-countryOnly)
            row_names <- names(thisBlock)
            # transpose: countries as columns are fixed. Append rows
            thisBlock <- as.data.frame(t(thisBlock))
            names(thisBlock) <- col_names
            thisBlock <- mutate(thisBlock, indicator = indCode, firmType = type, industry = as.character(isicCode), var = row_names)
            
            #thisBlock <- thisBlock[-1,]
            # bind to tfpBlock
            COUBlock <- bind_rows(COUBlock,thisBlock)
            print(paste(sectCode,type,indCode,isicCode,sep="_"))
          }
        }
      }
   # }
  #}
}

# filter data ----------------------
.COUTable <- function(cou,sect,indicatorDesc,firmType){
  
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
  
  COUBlock <- .generateCOUBlock(sect,groupByVar)
  thisCountry <- COUBlock %>%
    select(country = one_of(cou),indicator,firmType,industryCode = industry,var) %>%
    mutate(indicator2 = ifelse(substr(indicator,8,8)=="M","tfp2",ifelse(substr(indicator,8,8)=="L","tfp3","tfp1"))) %>%
    filter(indicator2 == ind2,firmType == groupByVar & indicator == ind) %>%
    distinct()
  
  thisCountry$country <- round(thisCountry$country,2)
  
  thisCountry2 <- spread(thisCountry, var, country)
  subString <- substr(thisCountry2$indicator,nchar(thisCountry2$indicator)-1,nchar(thisCountry2$indicator))
  thisCountry2$indicator <- ifelse(subString %in% c("KL","LM"),0,subString)
  # get industry descriptors
  thisCountry2 <- merge(thisCountry2,industryMaps,by="industryCode",all.x = TRUE)
  thisCountry2 <- arrange(thisCountry2, industry)
  
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

  
