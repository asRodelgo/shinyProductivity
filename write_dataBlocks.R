# Pre-compute dataBlocks ----------------------------

# Calculate datablocks (for the actual UI, precalculate these and read.csv)
#takes about 1/2 min per dataBlock. Creates list with all dataBlocks and saves it in disk
dataBlock <- list()
for (sect in sectorList){
  for (type in .firmTypeList(sect)){
    for (ind in .indicatorList(sect)) {
      indCode <- .indicatorToCode(ind)
      sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
      dataBlock[[paste(sectCode,type,indCode,sep="_")]] <- .calculateDataBlock(type,sect,ind)
      write.csv(dataBlock[[paste(sectCode,type,indCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),row.names = FALSE)
    }
  }
}

# write dataBlocks -----------------
for (sect in sectorList){
  for (type in .firmTypeList(sect)){
    for (ind in .indicatorList(sect)) {
      indCode <- .indicatorToCode(ind)
      sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
      write.csv(dataBlock[[paste(sectCode,type,indCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),row.names = FALSE)
    }
  }
}
