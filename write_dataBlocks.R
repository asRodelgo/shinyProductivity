# Pre-compute dataBlocks ----------------------------

# Calculate datablocks (for the actual UI, precalculate these and read.csv)
#takes about 1/2 min per dataBlock. Creates list with all dataBlocks and saves it in disk
dataBlock <- list()
#for (sect in sectorList){
for (sect in c("Manufacturing")){  
  #for (type in .firmTypeList(sect)[-1]){
  for (type in c("forOwner")){
    for (ind in .indicatorList(sect)[-c(7,8)]) {
    #for (ind in c("Labor share")){
      for (indus in c("All industries",.industryList(sect))) {
      #for (indus in c("Food")) {  
        isicCode <- ifelse(indus == "All industries","", .industryToCode(indus))
        indCode <- .indicatorToCodeAllIndustries(ind)
        sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
        dataBlock[[paste(sectCode,type,indCode,isicCode,sep="_")]] <- .calculateDataBlock(type,sect,ind,indus)
        write.csv(dataBlock[[paste(sectCode,type,indCode,isicCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,isicCode,sep="_"),".csv"),row.names = FALSE)
        print(paste("Created:",sectCode,type,indCode,isicCode))
      }
    }
  }
}

# # write dataBlocks -----------------
# for (sect in sectorList){
#   for (type in .firmTypeList(sect)){
#     for (ind in .indicatorList(sect)) {
#       indCode <- .indicatorToCode(ind)
#       sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
#       write.csv(dataBlock[[paste(sectCode,type,indCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),row.names = FALSE)
#     }
#   }
# }
