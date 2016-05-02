# Chunk functions for PDF reports -----------------
# -------------------------------------------------

## ---- table2 ----
summaryStats <- function(sector,indicatorDesc,firmType,whichTable){
  #ageRange,sizeRange,expRange,ownRange,firmType){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By size"
  # allocEff <- "All countries"
  # whichTable <- 2
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # Filter by allocation has to be done from the all firms dataBlock for Manufacturing
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  # If sector is Manufacturing then group by groupByVar
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    
    reorder <- .reorderColumns(lenVar,col_per_block = 2) # call the reorder function to arrange columns
    
    sumStatsAux <- dataBlock %>%
      # starts_with("N"),starts_with("mean"),starts_with("OPcov_"),
      # starts_with("OPcovNoWeights"), starts_with("indAlloc")
      select(starts_with("median"),
             starts_with("sd"),starts_with("iqr")) %>%
      summarise_each(funs(min(., na.rm = TRUE),max(., na.rm = TRUE),mean(., na.rm = TRUE),
                          median(., na.rm = TRUE),sd(., na.rm = TRUE),iqr(., na.rm = TRUE)))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    #ncolsumStats <- ncol(sumStats)
    # transpose every lenVar number of rows into columns
    sumStats2 <- data.frame()
    for (i in 1:length(statsNames)){
      for (j in 1:lenVar){
        for (k in 1:ncol(sumStats)){
          sumStats2[i,k+(j-1)*ncol(sumStats)] <- sumStats[(i-1)*lenVar + j,k] 
        }
      }
    }
    sumStats <- sumStats2  
    row.names(sumStats) <- statsNames
    if (lenVar == 3) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15,21:23)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))),
                         contains(paste0("_",substr(thisList[3],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    } else if (lenVar == 2) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    }
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)
    
    # reorder columns
    incomeStats <- incomeStats[,reorder]
    regionStats <- regionStats[,reorder]
    
  } else {
    
    sumStatsAux <- dataBlock %>%
      select(#N,mean,
        median,sd,iqr#,OPcov,OPcovNoWeights,indAlloc
      ) %>%
      summarise_each(funs(min,max,mean,median,sd,iqr))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov_"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    row.names(sumStats) <- statsNames
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)  
    
  }
  
  # -------------------------------------------
  # Prepare the output tables
  # -------------------------
  
  incomeStats <- filter(incomeStats, !is.na(incomeLevel))
  incomeRowNames <- as.character(incomeStats$incomeLevel)
  incomeStats <- select(incomeStats, -incomeLevel)
  incomeStats <- mutate_each(incomeStats, funs(as.numeric))
  row.names(incomeStats) <- incomeRowNames
  incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
  
  regionStats <- filter(regionStats, !is.na(region))
  regionRowNames <- as.character(regionStats$region)
  regionStats <- select(regionStats, -region)
  regionStats <- mutate_each(regionStats, funs(as.numeric))
  row.names(regionStats) <- regionRowNames
  
  # final reordering of columns
  if (lenVar == 3) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                       contains(paste0("_",substr(thisList[2],1,3))),
                       contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))),
                          contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  } else if (lenVar == 2) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                       contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  }
  
  # whichTable: "Countries"=1,"Summary Stats"=2,"Income level medians"=3,"Region medians"=4
  
  if (whichTable == 1){
    summaryStats <- dataBlock
    summaryStats <- select(summaryStats, -countryOnly, -yearOnly,-countryDes)
    summaryStats[,2:(ncol(summaryStats)-2)] <- round(summaryStats[,2:(ncol(summaryStats)-2)],2)
  }
  if (whichTable == 2){
    summaryStats <- round(sumStats,2)
  }
  if (whichTable == 3){
    summaryStats <- round(incomeStats,2)
  }
  if (whichTable == 4){
    summaryStats <- round(regionStats,2)
  }
  # NAs to "---"
  summaryStats[is.na(summaryStats)] <- "---"
  
  # I have to add a dummy column so the alignment works (align)
  summaryStats$dummy <- rep("",nrow(summaryStats))
  # modify column names
  names(summaryStats) <- c(names(summaryStats)[1:(ncol(summaryStats)-1)],"")

  if (!(firmType == "All firms")) {
    # add an extra header. Push current header to row1
    data_aux <- summaryStats
    data_aux[1,] <- names(summaryStats)
    for (i in 1:nrow(summaryStats)){
      data_aux[i+1,] <- summaryStats[i,]
    }
    row.names(data_aux) <- c(" ",row.names(summaryStats))
    summaryStats <- data_aux
    
    if (lenVar==3){
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2],rep(" ",2),thisList[3]," ")
      
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('>{\\raggedright}p{0.6in}',rep('>{\\raggedleft}p{0.6in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            size="\\footnotesize",
            booktabs = FALSE, table.placement="", hline.after = c(1) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    } else {
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2]," ")
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('l',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    }
  } else {
    data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
    align(data.table) <- c('l',rep('r',ncol(data.table)-1),'l')
    print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
    )#sanitize.text.function = function(x){x}) # include sanitize to control formats  
  }
  
  
}  
summaryStats(sect,ind,type,2)

## ---- table3 ----
summaryStats <- function(sector,indicatorDesc,firmType,whichTable){
  #ageRange,sizeRange,expRange,ownRange,firmType){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By size"
  # allocEff <- "All countries"
  # whichTable <- 2
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # Filter by allocation has to be done from the all firms dataBlock for Manufacturing
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  # If sector is Manufacturing then group by groupByVar
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    
    reorder <- .reorderColumns(lenVar,col_per_block = 2) # call the reorder function to arrange columns
    
    sumStatsAux <- dataBlock %>%
      # starts_with("N"),starts_with("mean"),starts_with("OPcov_"),
      # starts_with("OPcovNoWeights"), starts_with("indAlloc")
      select(starts_with("median"),
             starts_with("sd"),starts_with("iqr")) %>%
      summarise_each(funs(min(., na.rm = TRUE),max(., na.rm = TRUE),mean(., na.rm = TRUE),
                          median(., na.rm = TRUE),sd(., na.rm = TRUE),iqr(., na.rm = TRUE)))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    #ncolsumStats <- ncol(sumStats)
    # transpose every lenVar number of rows into columns
    sumStats2 <- data.frame()
    for (i in 1:length(statsNames)){
      for (j in 1:lenVar){
        for (k in 1:ncol(sumStats)){
          sumStats2[i,k+(j-1)*ncol(sumStats)] <- sumStats[(i-1)*lenVar + j,k] 
        }
      }
    }
    sumStats <- sumStats2  
    row.names(sumStats) <- statsNames
    if (lenVar == 3) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15,21:23)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))),
                         contains(paste0("_",substr(thisList[3],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    } else if (lenVar == 2) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    }
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)
    
    # reorder columns
    incomeStats <- incomeStats[,reorder]
    regionStats <- regionStats[,reorder]
    
  } else {
    
    sumStatsAux <- dataBlock %>%
      select(#N,mean,
        median,sd,iqr#,OPcov,OPcovNoWeights,indAlloc
      ) %>%
      summarise_each(funs(min,max,mean,median,sd,iqr))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov_"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    row.names(sumStats) <- statsNames
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)  
    
  }
  
  # -------------------------------------------
  # Prepare the output tables
  # -------------------------
  
  incomeStats <- filter(incomeStats, !is.na(incomeLevel))
  incomeRowNames <- as.character(incomeStats$incomeLevel)
  incomeStats <- select(incomeStats, -incomeLevel)
  incomeStats <- mutate_each(incomeStats, funs(as.numeric))
  row.names(incomeStats) <- incomeRowNames
  incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
  
  regionStats <- filter(regionStats, !is.na(region))
  regionRowNames <- as.character(regionStats$region)
  regionStats <- select(regionStats, -region)
  regionStats <- mutate_each(regionStats, funs(as.numeric))
  row.names(regionStats) <- regionRowNames
  
  # final reordering of columns
  if (lenVar == 3) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))),
                          contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))),
                          contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  } else if (lenVar == 2) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  }
  
  # whichTable: "Countries"=1,"Summary Stats"=2,"Income level medians"=3,"Region medians"=4
  
  if (whichTable == 1){
    summaryStats <- dataBlock
    summaryStats <- select(summaryStats, -countryOnly, -yearOnly,-countryDes)
    summaryStats[,2:(ncol(summaryStats)-2)] <- round(summaryStats[,2:(ncol(summaryStats)-2)],2)
  }
  if (whichTable == 2){
    summaryStats <- round(sumStats,2)
  }
  if (whichTable == 3){
    summaryStats <- round(incomeStats,2)
  }
  if (whichTable == 4){
    summaryStats <- round(regionStats,2)
  }
  # NAs to "---"
  summaryStats[is.na(summaryStats)] <- "---"
  
  # I have to add a dummy column so the alignment works (align)
  summaryStats$dummy <- rep("",nrow(summaryStats))
  # modify column names
  names(summaryStats) <- c(names(summaryStats)[1:(ncol(summaryStats)-1)],"")
  
  if (!(firmType == "All firms")) {
    # add an extra header. Push current header to row1
    data_aux <- summaryStats
    data_aux[1,] <- names(summaryStats)
    for (i in 1:nrow(summaryStats)){
      data_aux[i+1,] <- summaryStats[i,]
    }
    row.names(data_aux) <- c(" ",row.names(summaryStats))
    summaryStats <- data_aux
    
    if (lenVar==3){
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2],rep(" ",2),thisList[3]," ")
      
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('>{\\raggedright}p{0.6in}',rep('>{\\raggedleft}p{0.6in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            size="\\footnotesize",
            booktabs = FALSE, table.placement="", hline.after = c(1) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    } else {
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2]," ")
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('l',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    }
  } else {
    data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
    align(data.table) <- c('l',rep('r',ncol(data.table)-1),'l')
    print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
    )#sanitize.text.function = function(x){x}) # include sanitize to control formats  
  }
  
  
}  

summaryStats(sect,ind,type,3)

## ---- table4 ----
summaryStats <- function(sector,indicatorDesc,firmType,whichTable){
  #ageRange,sizeRange,expRange,ownRange,firmType){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By size"
  # allocEff <- "All countries"
  # whichTable <- 2
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # Filter by allocation has to be done from the all firms dataBlock for Manufacturing
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  # If sector is Manufacturing then group by groupByVar
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    
    reorder <- .reorderColumns(lenVar,col_per_block = 2) # call the reorder function to arrange columns
    
    sumStatsAux <- dataBlock %>%
      # starts_with("N"),starts_with("mean"),starts_with("OPcov_"),
      # starts_with("OPcovNoWeights"), starts_with("indAlloc")
      select(starts_with("median"),
             starts_with("sd"),starts_with("iqr")) %>%
      summarise_each(funs(min(., na.rm = TRUE),max(., na.rm = TRUE),mean(., na.rm = TRUE),
                          median(., na.rm = TRUE),sd(., na.rm = TRUE),iqr(., na.rm = TRUE)))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    #ncolsumStats <- ncol(sumStats)
    # transpose every lenVar number of rows into columns
    sumStats2 <- data.frame()
    for (i in 1:length(statsNames)){
      for (j in 1:lenVar){
        for (k in 1:ncol(sumStats)){
          sumStats2[i,k+(j-1)*ncol(sumStats)] <- sumStats[(i-1)*lenVar + j,k] 
        }
      }
    }
    sumStats <- sumStats2  
    row.names(sumStats) <- statsNames
    if (lenVar == 3) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15,21:23)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))),
                         contains(paste0("_",substr(thisList[3],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    } else if (lenVar == 2) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15)]
      # reorder columns
      sumStats <- select(sumStats, contains(paste0("_",substr(thisList[1],1,3))),
                         contains(paste0("_",substr(thisList[2],1,3))))
      # rename columns
      names(sumStats) <- rep(c("median","sd","IQR"),lenVar)
    }
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)
    
    # reorder columns
    incomeStats <- incomeStats[,reorder]
    regionStats <- regionStats[,reorder]
    
  } else {
    
    sumStatsAux <- dataBlock %>%
      select(#N,mean,
        median,sd,iqr#,OPcov,OPcovNoWeights,indAlloc
      ) %>%
      summarise_each(funs(min,max,mean,median,sd,iqr))
    
    sumStats <- data.frame(#N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
      #mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
      median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
      sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
      iqr_sum = as.numeric(select(sumStatsAux, starts_with("iqr"))[1,]),
      #OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov_"))[1,]),
      #OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
      #indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
      stringsAsFactors = FALSE)
    row.names(sumStats) <- statsNames
    
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    # Calculate region level medians  ----------
    regionStats <- dataBlock %>%
      select(region,#starts_with("N"),starts_with("mean"),
             starts_with("median"),starts_with("sd"),starts_with("iqr")
             #,OPcov,starts_with("OPcovNoWeights"),starts_with("indAlloc")
      ) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)  
    
  }
  
  # -------------------------------------------
  # Prepare the output tables
  # -------------------------
  
  incomeStats <- filter(incomeStats, !is.na(incomeLevel))
  incomeRowNames <- as.character(incomeStats$incomeLevel)
  incomeStats <- select(incomeStats, -incomeLevel)
  incomeStats <- mutate_each(incomeStats, funs(as.numeric))
  row.names(incomeStats) <- incomeRowNames
  incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
  
  regionStats <- filter(regionStats, !is.na(region))
  regionRowNames <- as.character(regionStats$region)
  regionStats <- select(regionStats, -region)
  regionStats <- mutate_each(regionStats, funs(as.numeric))
  row.names(regionStats) <- regionRowNames
  
  # final reordering of columns
  if (lenVar == 3) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))),
                          contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))),
                          contains(paste0("_",substr(thisList[3],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  } else if (lenVar == 2) {
    # reorder columns
    incomeStats <- select(incomeStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(incomeStats) <- rep(c("median","sd","IQR"),lenVar)
    regionStats <- select(regionStats, contains(paste0("_",substr(thisList[1],1,3))),
                          contains(paste0("_",substr(thisList[2],1,3))))
    # rename columns
    names(regionStats) <- rep(c("median","sd","IQR"),lenVar)
  }
  
  # whichTable: "Countries"=1,"Summary Stats"=2,"Income level medians"=3,"Region medians"=4
  
  if (whichTable == 1){
    summaryStats <- dataBlock
    summaryStats <- select(summaryStats, -countryOnly, -yearOnly,-countryDes)
    summaryStats[,2:(ncol(summaryStats)-2)] <- round(summaryStats[,2:(ncol(summaryStats)-2)],2)
  }
  if (whichTable == 2){
    summaryStats <- round(sumStats,2)
  }
  if (whichTable == 3){
    summaryStats <- round(incomeStats,2)
  }
  if (whichTable == 4){
    summaryStats <- round(regionStats,2)
  }
  # NAs to "---"
  summaryStats[is.na(summaryStats)] <- "---"
  
  # I have to add a dummy column so the alignment works (align)
  summaryStats$dummy <- rep("",nrow(summaryStats))
  # modify column names
  names(summaryStats) <- c(names(summaryStats)[1:(ncol(summaryStats)-1)],"")
  
  if (!(firmType == "All firms")) {
    # add an extra header. Push current header to row1
    data_aux <- summaryStats
    data_aux[1,] <- names(summaryStats)
    for (i in 1:nrow(summaryStats)){
      data_aux[i+1,] <- summaryStats[i,]
    }
    row.names(data_aux) <- c(" ",row.names(summaryStats))
    summaryStats <- data_aux
    
    if (lenVar==3){
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2],rep(" ",2),thisList[3]," ")
      
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('>{\\raggedright}p{0.6in}',rep('>{\\raggedleft}p{0.6in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            size="\\footnotesize",
            booktabs = FALSE, table.placement="", hline.after = c(1) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    } else {
      names(summaryStats) <- c(" ",thisList[1],rep(" ",2),thisList[2]," ")
      data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
      align(data.table) <- c('l',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-1),'l')
      print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
            booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
      )#sanitize.text.function = function(x){x}) # include sanitize to control formats
    }
  } else {
    data.table <- xtable(summaryStats, digits=rep(2,ncol(summaryStats)+1)) #control decimals
    align(data.table) <- c('l',rep('r',ncol(data.table)-1),'l')
    print(data.table, include.rownames=TRUE,include.colnames=TRUE, floating=FALSE, 
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center"
    )#sanitize.text.function = function(x){x}) # include sanitize to control formats  
  }
  
  
}  

summaryStats(sect,ind,type,4)

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

## ---- plot2 ---- 
summaryPlots <- function(sector,indicatorDesc,firmType,whichTable){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By age"
  # allocEff <- "All countries"
  # whichTable <- 2
  # varPlot <- "indAlloc"
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      colOrder <- c(1,2,3)
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      colOrder <- c(2,1,3)
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # call the reorder function to arrange columns
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    reorder <- .reorderColumns(lenVar,col_per_block=2)
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  
  # ----------------------------------------------------
  # Plotting part --------------------------------------
  
  # Plot faceted charts/histograms by types of firms
  if (!(firmType == "All firms") & (sector=="Manufacturing")){ # for now only plot Manufacturing
    if (whichTable==2){
      dataPlot <- select(dataBlock, starts_with("median"))
      dataPlot <- dataPlot[,colOrder]
      names(dataPlot) <- thisList
      dataPlot <- gather(dataPlot, typeList, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~typeList) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){
      # Calculate income level medians  ----------
      incomeStats <- dataBlock %>%
        select(incomeLevel,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
               #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
        ) %>%
        #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(incomeLevel) %>%
        summarise_each(funs(median(as.numeric(.))))
      incomeStats <- as.data.frame(incomeStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        incomeStats <- incomeStats[,reorder]
      }
      
      incomeStats <- filter(incomeStats, !is.na(incomeLevel))
      incomeRowNames <- as.character(incomeStats$incomeLevel)
      incomeStats <- select(incomeStats, -incomeLevel)
      incomeStats <- mutate_each(incomeStats, funs(as.numeric))
      row.names(incomeStats) <- incomeRowNames
      incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
      
      dataPlot1 <- select(incomeStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, income = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -income)
      
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~income) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
      
    } else if (whichTable==4){
      # Calculate income level medians  ----------
      regionStats <- dataBlock %>%
        select(region,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
        ) %>%
        #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(region) %>%
        summarise_each(funs(median(as.numeric(.))))
      regionStats <- as.data.frame(regionStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        regionStats <- regionStats[,reorder]
      }
      
      regionStats <- filter(regionStats, !is.na(region))
      regionRowNames <- as.character(regionStats$region)
      regionStats <- select(regionStats, -region)
      regionStats <- mutate_each(regionStats, funs(as.numeric))
      row.names(regionStats) <- regionRowNames
      
      dataPlot1 <- select(regionStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, region = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -region)
      #       
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
    }
    # Plot individual charts/histograms   
  } else if (firmType == "All firms"){ 
    
    if (whichTable==2){ # summary stats
      dataPlot <- select(dataBlock, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #               binwidth=bw,
        #               colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        #geom_vline(aes(xintercept=mean(median, na.rm=T)),   # Ignore NA values for mean
        #           color="red", linetype="dashed", size=1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){ # summary stats
      dataPlot <- select(dataBlock, median, incomeLevel)
      dataPlot <- filter(dataPlot, !is.na(incomeLevel))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~incomeLevel) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==4){ # summary stats
      dataPlot <- select(dataBlock, median, region)
      dataPlot <- filter(dataPlot, !is.na(region))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Chart not yet available", cex=2)
  }
  
}  
summaryPlots(sect,ind,type,2)

## ---- plot3 ---- 
summaryPlots <- function(sector,indicatorDesc,firmType,whichTable){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By age"
  # allocEff <- "All countries"
  # whichTable <- 2
  # varPlot <- "indAlloc"
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      colOrder <- c(1,2,3)
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      colOrder <- c(2,1,3)
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # call the reorder function to arrange columns
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    reorder <- .reorderColumns(lenVar,col_per_block=2)
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  
  # ----------------------------------------------------
  # Plotting part --------------------------------------
  
  # Plot faceted charts/histograms by types of firms
  if (!(firmType == "All firms") & (sector=="Manufacturing")){ # for now only plot Manufacturing
    if (whichTable==2){
      dataPlot <- select(dataBlock, starts_with("median"))
      dataPlot <- dataPlot[,colOrder]
      names(dataPlot) <- thisList
      dataPlot <- gather(dataPlot, typeList, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~typeList) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){
      # Calculate income level medians  ----------
      incomeStats <- dataBlock %>%
        select(incomeLevel,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
               #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
        ) %>%
        #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(incomeLevel) %>%
        summarise_each(funs(median(as.numeric(.))))
      incomeStats <- as.data.frame(incomeStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        incomeStats <- incomeStats[,reorder]
      }
      
      incomeStats <- filter(incomeStats, !is.na(incomeLevel))
      incomeRowNames <- as.character(incomeStats$incomeLevel)
      incomeStats <- select(incomeStats, -incomeLevel)
      incomeStats <- mutate_each(incomeStats, funs(as.numeric))
      row.names(incomeStats) <- incomeRowNames
      incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
      
      dataPlot1 <- select(incomeStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, income = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -income)
      
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~income) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
      
    } else if (whichTable==4){
      # Calculate income level medians  ----------
      regionStats <- dataBlock %>%
        select(region,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
        ) %>%
        #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(region) %>%
        summarise_each(funs(median(as.numeric(.))))
      regionStats <- as.data.frame(regionStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        regionStats <- regionStats[,reorder]
      }
      
      regionStats <- filter(regionStats, !is.na(region))
      regionRowNames <- as.character(regionStats$region)
      regionStats <- select(regionStats, -region)
      regionStats <- mutate_each(regionStats, funs(as.numeric))
      row.names(regionStats) <- regionRowNames
      
      dataPlot1 <- select(regionStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, region = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -region)
      #       
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
    }
    # Plot individual charts/histograms   
  } else if (firmType == "All firms"){ 
    
    if (whichTable==2){ # summary stats
      dataPlot <- select(dataBlock, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        #geom_vline(aes(xintercept=mean(median, na.rm=T)),   # Ignore NA values for mean
        #           color="red", linetype="dashed", size=1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){ # summary stats
      dataPlot <- select(dataBlock, median, incomeLevel)
      dataPlot <- filter(dataPlot, !is.na(incomeLevel))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~incomeLevel) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==4){ # summary stats
      dataPlot <- select(dataBlock, median, region)
      dataPlot <- filter(dataPlot, !is.na(region))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Chart not yet available", cex=2)
  }
  
}  
summaryPlots(sect,ind,type,3)

## ---- plot4 ---- 
summaryPlots <- function(sector,indicatorDesc,firmType,whichTable){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By age"
  # allocEff <- "All countries"
  # whichTable <- 2
  # varPlot <- "indAlloc"
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      colOrder <- c(1,2,3)
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      thisList <- firmSizeList[-1]
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      colOrder <- c(2,1,3)
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      thisList <- firmExpStatusList[-1]
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,1)
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    firmType == "All firms"
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev","IQR")
  
  # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # call the reorder function to arrange columns
  if (!(firmType == "All firms") & (sector=="Manufacturing")){
    reorder <- .reorderColumns(lenVar,col_per_block=2)
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  
  # ----------------------------------------------------
  # Plotting part --------------------------------------
  
  # Plot faceted charts/histograms by types of firms
  if (!(firmType == "All firms") & (sector=="Manufacturing")){ # for now only plot Manufacturing
    if (whichTable==2){
      dataPlot <- select(dataBlock, starts_with("median"))
      dataPlot <- dataPlot[,colOrder]
      names(dataPlot) <- thisList
      dataPlot <- gather(dataPlot, typeList, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~typeList) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){
      # Calculate income level medians  ----------
      incomeStats <- dataBlock %>%
        select(incomeLevel,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
               #,starts_with("OPcov_"),starts_with("OPcovNoWeights"),starts_with("indAlloc")
        ) %>%
        #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(incomeLevel) %>%
        summarise_each(funs(median(as.numeric(.))))
      incomeStats <- as.data.frame(incomeStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        incomeStats <- incomeStats[,reorder]
      }
      
      incomeStats <- filter(incomeStats, !is.na(incomeLevel))
      incomeRowNames <- as.character(incomeStats$incomeLevel)
      incomeStats <- select(incomeStats, -incomeLevel)
      incomeStats <- mutate_each(incomeStats, funs(as.numeric))
      row.names(incomeStats) <- incomeRowNames
      incomeStats <- incomeStats[c(2,3,4,1),]# order income rows
      
      dataPlot1 <- select(incomeStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, income = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -income)
      
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~income) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
      
    } else if (whichTable==4){
      # Calculate income level medians  ----------
      regionStats <- dataBlock %>%
        select(region,#starts_with("N"),starts_with("mean"),
               starts_with("median"),starts_with("sd"),starts_with("iqr")
        ) %>%
        #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
        group_by(region) %>%
        summarise_each(funs(median(as.numeric(.))))
      regionStats <- as.data.frame(regionStats)
      
      if (!(firmType == "All firms") & (sector=="Manufacturing")){
        # reorder columns
        regionStats <- regionStats[,reorder]
      }
      
      regionStats <- filter(regionStats, !is.na(region))
      regionRowNames <- as.character(regionStats$region)
      regionStats <- select(regionStats, -region)
      regionStats <- mutate_each(regionStats, funs(as.numeric))
      row.names(regionStats) <- regionRowNames
      
      dataPlot1 <- select(regionStats, starts_with("median"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot1 <- mutate(dataPlot1, region = row.names(dataPlot1))
      dataPlot1 <- gather(dataPlot1, type, value, -region)
      #       
      ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
        geom_bar(position="dodge",stat="identity") +
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + 
        labs(x="",y="",title=paste("Median",firmType))+
        scale_fill_manual(values = rainbow(lenVar),labels = thisList)
      
      # Plot side by side
      #multiplot(p1,p2,cols=2)
    }
    # Plot individual charts/histograms   
  } else if (firmType == "All firms"){ 
    
    if (whichTable==2){ # summary stats
      dataPlot <- select(dataBlock, median)
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        #geom_vline(aes(xintercept=mean(median, na.rm=T)),   # Ignore NA values for mean
        #           color="red", linetype="dashed", size=1) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==3){ # summary stats
      dataPlot <- select(dataBlock, median, incomeLevel)
      dataPlot <- filter(dataPlot, !is.na(incomeLevel))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~incomeLevel) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    } else if (whichTable==4){ # summary stats
      dataPlot <- select(dataBlock, median, region)
      dataPlot <- filter(dataPlot, !is.na(region))
      #bw <- diff(range(dataPlot$median)) / (2 * IQR(dataPlot$median) / length(dataPlot$median)^(1/3))
      ggplot(dataPlot,aes(median)) + 
        #geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis
        #                  binwidth=bw,
        #                  colour="black", fill="white") +
        geom_density(aes(y=..density..),alpha=.4, fill="green") +  
        facet_wrap(~region) +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),plot.title = element_text(lineheight=.5)
        ) # Overlay with transparent density plot
      
    }
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Chart not yet available", cex=2)
  }
  
}  
summaryPlots(sect,ind,type,4)

