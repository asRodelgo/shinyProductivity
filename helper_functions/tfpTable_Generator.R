# ---------------------
# Generate tables to display country indicators by Manufacturing sector/industry
tfpList <- .indicatorList("Manufacturing")[which(substr(.indicatorList("Manufacturing"),1,5)=="Total")]
sectCode <- "Manuf"

tfpBlock <- data.frame(t(rep(NA,111)))
names(tfpBlock) <- c(as.character(countryRegions$country),"indicator","firmType","var")
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
    tfpBlock <- bind_rows(tfpBlock,thisBlock)
  }
}

cou <- "Argentina"
ind <- "tfprYKL"
ind2 <- ifelse(substr(ind,8,8)=="M","tfp2",ifelse(substr(ind,8,8)=="L","tfp3","tfp1"))
type <- "size"

thisCountry <- tfpBlock %>%
  select(country = one_of(cou),indicator,firmType,var) %>%
  mutate(indicator2 = ifelse(substr(indicator,8,8)=="M","tfp2",ifelse(substr(indicator,8,8)=="L","tfp3","tfp1"))) %>%
  filter(indicator2 == ind2,firmType == type) #indicator == ind & 

thisCountry2 <- spread(thisCountry, var, country)
  
  
