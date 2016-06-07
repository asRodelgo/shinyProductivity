# Pre-compute dataBlocks ----------------------------

# Calculate datablocks (for the actual UI, precalculate these and read.csv)
#takes about 1/2 min per dataBlock. Creates list with all dataBlocks and saves it in disk
dataBlock <- list()
#for (sect in sectorList){
for (sect in c("Manufacturing")){  
  for (type in .firmTypeList(sect)[-1]){
  #for (type in c("all")){
    #for (ind in .indicatorList(sect)) {
    for (ind in c("Labor productivity","Labor share",
                  "Materials share"
      ,"Labor share (net sales)","Materials share (net sales)","Labor productivity (net sales)"
      )){
      for (indus in .industryList(sect)[-1]) {
      #for (indus in c("Machinery and equipment (29 and 30)")) {  
        isicCode <- .industryToCode(indus)
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
