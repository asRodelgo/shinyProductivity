# Calculate summary plots -----------------------------------
.summaryPlots <- function(sector,indicatorDesc,firmType,allocEff,whichTable){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By size"
  # allocEff <- "All countries"
  # whichTable <- 1
  # varPlot <- "indAlloc"
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      thisList <- firmAgeList[-1]
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      colOrder <- c(1,2,3,4)
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
      colOrder <- c(1,3,4,2)
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,3,1)
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      thisList <- firmForeignOwnerList[-1]
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      colOrder <- c(2,3,1)
      #dataBlock <- dataBlock_forOwner
    }
    thisDataBlock <- dataBlock[[paste(sectCode,groupByVar,indicatorCode,sep="_")]]
    # use this DataBlock to filter Manufacturing sector indicators by allocation efficiency 
    manufDataBlock <- dataBlock[[paste("Manuf","all",indicatorCode,sep="_")]]
    refDataBlock <- manufDataBlock
    
  } else if (sector == "Services"){
    thisDataBlock <- dataBlock[[paste("Serv","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  } else {
    thisDataBlock <- dataBlock[[paste("AllSect","all",indicatorCode,sep="_")]]
    refDataBlock <- thisDataBlock
  }
  dataBlock <- as.data.frame(thisDataBlock)
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev")
  
    # remove columns generated from NA adCountry to avoid errors
  dataBlock <- select(dataBlock, everything(), -ends_with("_NA"))
  
  # Filter dataBlock according to the desired allocation Efficiency
  if (allocEff == "Direct and Indirect Allocation Efficient"){
    refDataBlock <- filter(refDataBlock, (OPcov > 0) & (indAlloc > 1)) 
  } else if (allocEff == "Direct Allocation Efficient"){
    refDataBlock <- filter(refDataBlock, OPcov > 0) 
  } else if (allocEff == "Indirect Allocation Efficient"){
    refDataBlock <- filter(refDataBlock, indAlloc > 1) 
  } else if (allocEff == "Allocation Inefficient"){
    refDataBlock <- filter(refDataBlock, (OPcov < 0) & (indAlloc < 1)) 
  }
  # Filter by allocation has to be done from the all firms dataBlock for Manufacturing
  if (!(firmType == "All firms")){
    refCountries <- refDataBlock$country
    dataBlock <- filter(dataBlock, country %in% refCountries)
  } else {
    dataBlock <- refDataBlock
  } 
  
  
  # ----------------------------------------------------
  
  # Plotting part
  if (whichTable==2){
      dataPlot1 <- select(dataBlock, starts_with("OPcov_"))
      dataPlot1 <- dataPlot1[,colOrder]
      dataPlot2 <- select(dataBlock, starts_with("indAlloc"))
      dataPlot2 <- dataPlot2[,colOrder]
      
      par(mfrow = c(1,2))
      boxplot(dataPlot1,names=thisList)
      title("O-P covariance (unweighted)")
      boxplot(dataPlot2,names=thisList)
      title("Indirect Allocation")
      mtext(paste(indicatorDesc,firmType), outer = TRUE, cex = 1.5)
  } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Chart not yet available", cex=2)
  }
}  

#write.csv(summaryStats,"summaryStats.csv")
