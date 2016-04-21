# function to create side by side ggplots -----------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Calculate summary plots -----------------------------------
.summaryPlots <- function(sector,indicatorDesc,firmType,allocEff,whichTable){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By size"
  # allocEff <- "All countries"
  # whichTable <- 3
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
    reorder <- .reorderColumns(lenVar) # call the reorder function to arrange columns
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
      
  } else if (whichTable==3){
    # Calculate income level medians  ----------
    incomeStats <- dataBlock %>%
      select(incomeLevel,starts_with("N"),starts_with("mean"),starts_with("median"),
             starts_with("sd"),starts_with("iqr"),starts_with("OPcov_"),starts_with("OPcovNoWeights"),
             starts_with("indAlloc")) %>%
      #select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(incomeLevel) %>%
      summarise_each(funs(median(as.numeric(.))))
    incomeStats <- as.data.frame(incomeStats)
    
    if (!(firmType == "All firms")){
      # reorder columns
      incomeStats <- incomeStats[,reorder]
    }
    
    incomeStats <- filter(incomeStats, !is.na(incomeLevel))
    row.names(incomeStats) <- as.character(incomeStats$incomeLevel)
    incomeStats <- select(incomeStats, -incomeLevel)
    
    
    dataPlot1 <- select(incomeStats, starts_with("OPcov_"))
    dataPlot1 <- dataPlot1[,colOrder]
    dataPlot1 <- mutate(dataPlot1, income = row.names(dataPlot1))
    dataPlot1 <- gather(dataPlot1, type, value, -income)
    dataPlot2 <- select(incomeStats, starts_with("indAlloc"))
    dataPlot2 <- dataPlot2[,colOrder]
    dataPlot2 <- mutate(dataPlot2, income = row.names(dataPlot2))
    dataPlot2 <- gather(dataPlot2, type, value, -income)
    
    p1 <- ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
      geom_bar(position="dodge",stat="identity") +
      facet_wrap(~income) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) + 
      labs(x="",y="",title=paste("OP covariances",firmType))+
      scale_fill_manual(values = rainbow(lenVar),labels = thisList)
    
    p2 <- ggplot(dataPlot2, aes(x=type,y=value,fill=type)) +
      geom_bar(position="dodge",stat="identity") +
      facet_wrap(~income) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) + 
      labs(x="",y="",title=paste("Indirect Allocation Ratio",firmType))+
      scale_fill_manual(values = rainbow(lenVar),labels = thisList)
    
    # Plot side by side
    multiplot(p1,p2,cols=2)
    
  } else if (whichTable==4){
    # Calculate income level medians  ----------
    regionStats <- dataBlock %>%
      select(region,starts_with("N"),starts_with("mean"),starts_with("median"),
             starts_with("sd"),starts_with("iqr"),starts_with("OPcov_"),starts_with("OPcovNoWeights"),
             starts_with("indAlloc")) %>%
      #select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      group_by(region) %>%
      summarise_each(funs(median(as.numeric(.))))
    regionStats <- as.data.frame(regionStats)
    
    if (!(firmType == "All firms")){
      # reorder columns
      regionStats <- regionStats[,reorder]
    }
    
    regionStats <- filter(regionStats, !is.na(region))
    row.names(regionStats) <- as.character(regionStats$region)
    regionStats <- select(regionStats, -region)
    
    
    dataPlot1 <- select(regionStats, starts_with("OPcov_"))
    dataPlot1 <- dataPlot1[,colOrder]
    dataPlot1 <- mutate(dataPlot1, region = row.names(dataPlot1))
    dataPlot1 <- gather(dataPlot1, type, value, -region)
    dataPlot2 <- select(regionStats, starts_with("indAlloc"))
    dataPlot2 <- dataPlot2[,colOrder]
    dataPlot2 <- mutate(dataPlot2, region = row.names(dataPlot2))
    dataPlot2 <- gather(dataPlot2, type, value, -region)
    
    p1 <- ggplot(dataPlot1, aes(x=type,y=value,fill=type)) +
      geom_bar(position="dodge",stat="identity") +
      facet_wrap(~region) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) + 
      labs(x="",y="",title=paste("OP covariances",firmType))+
      scale_fill_manual(values = rainbow(lenVar),labels = thisList)
    
    p2 <- ggplot(dataPlot2, aes(x=type,y=value,fill=type)) +
      geom_bar(position="dodge",stat="identity") +
      facet_wrap(~region) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) + 
      labs(x="",y="",title=paste("Indirect Allocation Ratio",firmType))+
      scale_fill_manual(values = rainbow(lenVar),labels = thisList)
    
    # Plot side by side
    multiplot(p1,p2,cols=2)
    
  } else {
      plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
      graphics::text(1.5, 1,"Chart not yet available", cex=2)
  }
}  

#write.csv(summaryStats,"summaryStats.csv")
