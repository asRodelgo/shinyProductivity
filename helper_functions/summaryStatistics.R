# -------------------------------------
# Functions to calculate final tables with summary statistics by firm characteristics,
# regions and income levels
# -------------------------------------

.summaryStatsByCountry <- function(data,countryYear,groupByVar,sector){
  
  # indicatorCode <- "n2a_d2"
  # indicatorQuantileCode <- "d2_n2a"
  # removeOutliers <- 0
  # countryYear <- "Afghanistan2014"  
  #   ageB <- 0
  #   ageU <- 200
  #   expStatusB <- 0
  #   expStatusU <- 100
  #   forOwnerB <- 0
  #   forOwnerU <- 100
  #   sector <- "All sectors"
  #   sizeRange <- "All firms"
  #   ageRange <- "All firms"
  #   expRange <- "All firms"
  #   ownRange <- "All firms"
  
  # some mappings
#   indicatorCode <- .indicatorToCode(indicatorDesc)
#   indicatorQuantileCode <- summaryMaps[summaryMaps$code==indicatorCode,]$indicatorQuant
#   N_indicatorCode <- paste0("N_",indicatorCode)
#   outlierIQRfactor <- 3
#   removeOutliers <- 1
#   ageB <- ageRange[1]
#   ageU <- ageRange[2]
#   expStatusB <- expStatus[1]
#   expStatusU <- expStatus[2]
#   forOwnerB <- forOwner[1]
#   forOwnerU <- forOwner[2]
  
  # Filter original data by sector
  if (sector == "Manufacturing") {
    data <- filter(data, sector_MS %in% sector)
  } 
  if (sector == "Services") {
    data <- filter(data, sector_MS %in% sector)
  }
  
  # Start calculations ------------------------------------
  if (nrow(data[data$country==countryYear,])>=5){ # If sample too small, not worth it
    # Calculate number of outliers left out. Default rule is +-3*IQR  
    data_aux <- data %>%
      select(country,N_indicator = one_of(N_indicatorCode)) %>%
      filter(country == countryYear) %>%
      group_by(country) %>%
      mutate(sampleSizeBefore=sum(N_indicator,na.rm=TRUE))
    sampleSizeBefore = data_aux$sampleSizeBefore[1]
    
    # default weight type: "sampling"
     
    # if indicator and indicatorQuantile are the same, keep just one
    if (indicatorCode == indicatorQuantileCode){ 
      data2 <- data %>%
        filter(country == countryYear) %>%
        select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
               N_indicator = one_of(N_indicatorCode),age,size,expStatus,forOwner) %>%
        filter(!is.na(indicator)) %>%
        mutate(indicatorQuantile = indicator)
    } else {
      data2 <- data %>%
        filter(country == countryYear) %>%
        select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
               indicatorQuantile = one_of(indicatorQuantileCode),N_indicator = one_of(N_indicatorCode),
               age,size,expStatus,forOwner) %>%
        filter(!is.na(indicator)) # remove NAs
    }
    # remove NAs on l1 indicator
    data2 <- filter(data2, !is.na(l1))
    data2 <- as.data.frame(data2)
    
    
    # The actual calculations (Improve this in the future)
    if (groupByVar=="age") {
      data2 <- data2 %>%
        group_by(age) %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) %>% # remove outliers
        mutate(N = sum(N_indicator,na.rm=TRUE),
               #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
               mean = weighted.mean(indicator,wt,na.rm=TRUE),
               median = weightedMedian(indicator,wt,na.rm=TRUE),
               sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)*(sum(wt)/(sum(wt)^2-sum(wt^2)))),
               se = sd/sqrt(sum(wt)),
               iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
               iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
               tot_emp10_50 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               tot_emp50_90 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               emp10 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA)),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA)),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA)),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE),
               indAlloc = ratio_median_emp90_50/ratio_median_emp10_50
        ) %>%
        select(country,N, mean, median, sd, iqr, OPcov, OPcovNoWeights, indAlloc)
      
    } else if (groupByVar == "size") {
      data2 <- data2 %>%
        group_by(size) %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) %>% # remove outliers
        mutate(N = sum(N_indicator,na.rm=TRUE),
               #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
               mean = weighted.mean(indicator,wt,na.rm=TRUE),
               median = weightedMedian(indicator,wt,na.rm=TRUE),
               sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)*(sum(wt)/(sum(wt)^2-sum(wt^2)))),
               se = sd/sqrt(sum(wt)),
               iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
               iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
               tot_emp10_50 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               tot_emp50_90 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               emp10 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA)),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA)),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA)),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE),
               indAlloc = ratio_median_emp90_50/ratio_median_emp10_50
        ) %>%
        select(country,N, mean, median, sd, iqr, OPcov, OPcovNoWeights, indAlloc)
      
    } else if (groupByVar == "expStatus") {
      data2 <- data2 %>%
        group_by(expStatus) %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) %>% # remove outliers
        mutate(N = sum(N_indicator,na.rm=TRUE),
               #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
               mean = weighted.mean(indicator,wt,na.rm=TRUE),
               median = weightedMedian(indicator,wt,na.rm=TRUE),
               sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)*(sum(wt)/(sum(wt)^2-sum(wt^2)))),
               se = sd/sqrt(sum(wt)),
               iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
               iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
               tot_emp10_50 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               tot_emp50_90 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               emp10 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA)),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA)),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA)),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE),
               indAlloc = ratio_median_emp90_50/ratio_median_emp10_50
        ) %>%
        select(country,N, mean, median, sd, iqr, OPcov, OPcovNoWeights, indAlloc)
      
    } else if (groupByVar == "forOwner") {
      data2 <- data2 %>%
        group_by(forOwner) %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) %>% # remove outliers
        mutate(N = sum(N_indicator,na.rm=TRUE),
               #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
               mean = weighted.mean(indicator,wt,na.rm=TRUE),
               median = weightedMedian(indicator,wt,na.rm=TRUE),
               sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)*(sum(wt)/(sum(wt)^2-sum(wt^2)))),
               se = sd/sqrt(sum(wt)),
               iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
               iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
               tot_emp10_50 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               tot_emp50_90 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               emp10 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA)),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA)),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA)),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE),
               indAlloc = ratio_median_emp90_50/ratio_median_emp10_50
        ) %>%
        select(country,N, mean, median, sd, iqr, OPcov, OPcovNoWeights, indAlloc)
      
    } else { # no specific filter selection
      data2 <- data2 %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) %>% # remove outliers
        mutate(N = sum(N_indicator,na.rm=TRUE),
               #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
               mean = weighted.mean(indicator,wt,na.rm=TRUE),
               median = weightedMedian(indicator,wt,na.rm=TRUE),
               sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)*(sum(wt)/(sum(wt)^2-sum(wt^2)))),
               se = sd/sqrt(sum(wt)),
               iqr = wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE),
               iqratio = wtd.quantile(indicator,100*round(wt,1),0.75)/wtd.quantile(indicator,100*round(wt,1),0.25),
               tot_emp10_50 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               tot_emp50_90 = sum(ifelse((indicatorQuantile>=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE)) & (indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE)),wt*l1,0),na.rm=TRUE),
               emp10 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA)),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA)),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = as.numeric(ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA)),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE),
               indAlloc = ratio_median_emp90_50/ratio_median_emp10_50
        ) %>%
        select(country,N, mean, median, sd, iqr, OPcov, OPcovNoWeights, indAlloc)
    }         
  
    # Remove duplicates, I only need 1 row per group_by. In case of non-Manufacturing
    # firms, this will yield 1 row in total per country
    data2 <- data2[!(duplicated(data2)),]
    data2 <- as.data.frame(data2)
    # compute number of ouliers left out
    sampleSizeAfter <- sum(data2$N)
    # spread the rows into columns for presentation purposes
    # and differentiate column names by firm type if applicable
    if (!(groupByVar=="all")){ 
      dataBind <- data2[1,-c(1)]
      listNames <- names(dataBind)[2:ncol(dataBind)]
      names(dataBind)[2:ncol(dataBind)] <- paste0(listNames,"_",data2[1,1])
      i <- 2
      j <- ncol(dataBind)
      while (i <= nrow(data2)){
        dataBind <- cbind(dataBind,data2[i,-c(1:2)])
        names(dataBind)[(j+1):ncol(dataBind)] <- paste0(listNames,"_",data2[i,1])
        i <- i + 1
        j <- ncol(dataBind)
      }
      data2 <- dataBind
    }
    data2 <- mutate(data2, outliersOut = sampleSizeBefore - sampleSizeAfter)
  
  # if number of firms is less than 5, return an empty dataset  
  } else {
    data2 <- data.frame(country,income=NA,OPcov=NA,OPcovNoWeights=NA,
                        ratio_median_emp90_50=NA,ratio_median_emp10_50=NA,outliersOut=NA,None=NA)
  }
  # output Indirect Allocative Efficiency
  #data2 <- mutate(data2, indAllocEff = ratio_median_emp90_50/ratio_median_emp10_50)
  return(data2)
}


# calculate indicators by country according to the filters ----------------
.calculateDataBlock <- function(data,groupByVar,sector) {
  
  dataBlock <- data.frame()
  
  #for (cou in countryList) {
  for (cou in c("Afghanistan2014","Albania2013","Angola2010")) {  
    addCountry <- .summaryStatsByCountry(data,cou,groupByVar,sector)
    # rbind only if returned data is not empty to avoid errors
    if (nrow(dataBlock)>0){ 
      if (!is.na(addCountry[1,ncol(addCountry)])) {
        dataBlock <- bind_rows(dataBlock,addCountry)
      }
    } else if (!is.na(addCountry[1,ncol(addCountry)])){
      dataBlock <- bind_rows(dataBlock,addCountry)
    }
  }
  
  # Add country regions
  countryRegions <- select(countryRegions, country,region,countryDes,incomeLevel)
  dataBlock <- mutate(dataBlock, countryOnly = substr(country,1,nchar(country)-4),
                      yearOnly = substr(country,nchar(country)-3,nchar(country)))
  dataBlock <- merge(dataBlock, countryRegions, by.x="countryOnly",by.y="country", all.x = TRUE)
  
  return(dataBlock)

}


# Calculate summary stats -----------------------------------
.summaryStats <- function(sector,indicatorDesc,firmType){
                                   #ageRange,sizeRange,expRange,ownRange,firmType){
  
  # Initial parameters ---------------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  #indicatorCode <- "n2a_d2"
  indicatorQuantileCode <- summaryMaps[summaryMaps$code==indicatorCode,]$indicatorQuant
  N_indicatorCode <- paste0("N_",indicatorCode)
  outlierIQRfactor <- 3
  removeOutliers <- 1
  
  # filter data by sector. Only drill down for manufacturing -------------------
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      dataBlock <- dataBlock_forOwner
    }
    
    # calculate age, size, export status, foreign ownership and tech innov status and filter
#     if (sizeRange == "All firms") {
#       sizeRange <- firmSizeList
#     }
#     if (ageRange == "All firms") {
#       ageRange <- firmAgeList
#     }
#     if (expRange == "All firms") {
#       expRange <- firmExpStatusList
#     }
#     if (ownRange == "All firms") {
#       ownRange <- firmForeignOwnerList
#     }
    
#     data <- data %>%
#       group_by(country,idstd) %>%
#       filter(age %in% ageRange & size %in% sizeRange &
#                expStatus %in% expRange & 
#                forOwner %in% ownRange)
#     
#     data <- as.data.frame(data)
  
  } else if (sector == "Services"){
    dataBlock <- dataBlockServices
  }
  
  # Calculate summary statistics for the selected countries ----------
  statsNames <- c("Min", "Max", "Mean", "Median", "Stdev")
  
  # If sector is Manufacturing then group by groupByVar
  if (sector == "Manufacturing"){
    
    sumStatsAux <- dataBlock %>%
      select(starts_with("N"),starts_with("mean"),starts_with("median"),
             starts_with("sd"),starts_with("OPcov"),starts_with("OPcovNoWeights"),
             starts_with("indAlloc")) %>%
      summarise_each(funs(min,max,mean,median,sd))
    
    sumStats <- data.frame(N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
                           mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
                           median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
                           sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
                           OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov_"))[1,]),
                           OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
                           indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
                           stringsAsFactors = FALSE)
    
    sumStats2 <- data.frame()
    for (i in 1:length(statsNames)){
      for (j in 1:lenVar){
        for (k in 1:ncol(sumStats)){
          sumStats2[i,k+(j-1)*ncol(sumStats)] <- sumStats[i+j-1,k] 
        }
      }
    }
    sumStats <- sumStats2  
    row.names(sumStats) <- statsNames
    
  } else {
    
    sumStatsAux <- dataBlock %>%
      select(N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
      summarise_each(funs(min,max,mean,median,sd))
    
    sumStats <- data.frame(N_sum = as.numeric(select(sumStatsAux, starts_with("N"))[1,]),
                            mean_sum = as.numeric(select(sumStatsAux, starts_with("mean"))[1,]),
                            median_sum = as.numeric(select(sumStatsAux, starts_with("median"))[1,]),
                            sd_sum = as.numeric(select(sumStatsAux, starts_with("sd"))[1,]),
                            OPcov_sum = as.numeric(select(sumStatsAux, starts_with("OPcov_"))[1,]),
                            OPcovNoW_sum = as.numeric(select(sumStatsAux, starts_with("OPcovNo"))[1,]),
                            indAlloc_sum = as.numeric(select(sumStatsAux, starts_with("indA"))[1,]),
    stringsAsFactors = FALSE)
    row.names(sumStats) <- statsNames
  }
  # Calculate income level summary  ----------
  incomeStats <- dataBlock %>%
    select(incomeLevel,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
    group_by(incomeLevel) %>%
    summarise_each(funs(median))
  
  # Calculate region level summary  ----------
  regionStats <- dataBlock %>%
    select(region,N,mean,median,sd,OPcov,OPcovNoWeights,indAlloc) %>%
    group_by(region) %>%
    summarise_each(funs(median))

  
}  