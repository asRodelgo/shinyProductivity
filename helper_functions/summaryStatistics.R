# -------------------------------------
# Functions to calculate final tables with summary statistics by firm characteristics,
# regions and income levels
# -------------------------------------


.summaryStatsByCountry <- function(countryYear,groupByVar,sector,indicatorDesc,outlierIQRfactor){
  
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
  
  # Initial parameters ---------------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  #indicatorCode <- "n2a_d2"
  indicatorQuantileCode <- summaryMaps[summaryMaps$code==indicatorCode,]$indicatorQuant
  N_indicatorCode <- paste0("N_",indicatorCode)
  #outlierIQRfactor <- 3
  removeOutliers <- 1
  
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
      filter(country == countryYear) %>%
      group_by(country) %>%
      select(country,indicatorCode = one_of(indicatorCode)) %>%
      mutate(N_indicator = ifelse(!(is.na(indicatorCode)),1,NA)) %>%
      mutate(sampleSizeBefore=sum(N_indicator,na.rm=TRUE))
    sampleSizeBefore = data_aux$sampleSizeBefore[1]
    
    # default weight type: "sampling"
     
    # if indicator and indicatorQuantile are the same, keep just one
    if (indicatorCode == indicatorQuantileCode){ 
      data2 <- data %>%
        filter(country == countryYear) %>%
        mutate(N_indicator = ifelse(!(is.na(indicatorCode)),1,NA)) %>%
        select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
               N_indicator,age,size,expStatus,forOwner) %>% #N_indicator = one_of(N_indicatorCode),
        filter(!is.na(indicator)) %>%
        mutate(indicatorQuantile = indicator)
    } else {
      data2 <- data %>%
        filter(country == countryYear) %>%
        mutate(N_indicator = ifelse(!(is.na(indicatorCode)),1,NA)) %>%
        select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
               indicatorQuantile = one_of(indicatorQuantileCode),N_indicator,#N_indicator = one_of(N_indicatorCode),
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
.calculateDataBlock <- function(groupByVar,sector,indicatorDesc) {
  
  dataBlock <- data.frame()
  
  for (cou in countryList) {
  #for (cou in c("Afghanistan2014","Albania2013","Angola2010")) {  
    addCountry <- .summaryStatsByCountry(cou,groupByVar,sector,indicatorDesc,outlierIQRfactor = 3)
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

.reorderColumns <- function(lenVar,col_per_block){
  
  #col_per_block <- 7
  if (lenVar == 2){
    reorder <- c(1,seq(2,col_per_block*lenVar+2,lenVar),seq(3,col_per_block*lenVar+3,lenVar))
  }
  if (lenVar == 3){
    reorder <- c(1,seq(2,col_per_block*lenVar+2,lenVar),seq(3,col_per_block*lenVar+3,lenVar),
                 seq(4,col_per_block*lenVar+4,lenVar))
  }
  if (lenVar == 4){
    reorder <- c(1,seq(2,col_per_block*lenVar+2,lenVar),seq(3,col_per_block*lenVar+3,lenVar),
                 seq(4,col_per_block*lenVar+4,lenVar),seq(5,col_per_block*lenVar+5,lenVar))
  }
  if (lenVar == 5){
    reorder <- c(1,seq(2,col_per_block*lenVar+2,lenVar),seq(3,col_per_block*lenVar+3,lenVar),
                 seq(4,col_per_block*lenVar+4,lenVar),seq(5,col_per_block*lenVar+5,lenVar)
                 ,seq(6,col_per_block*lenVar+6,lenVar))
  }
  if (lenVar == 6){
    reorder <- c(1,seq(2,col_per_block*lenVar+2,lenVar),seq(3,col_per_block*lenVar+3,lenVar),
                 seq(4,col_per_block*lenVar+4,lenVar),seq(5,col_per_block*lenVar+5,lenVar)
                 ,seq(6,col_per_block*lenVar+6,lenVar),seq(7,col_per_block*lenVar+7,lenVar))
  }

  return(reorder)
}

# Calculate summary stats -----------------------------------
.summaryStats <- function(sector,indicatorDesc,firmType,allocEff,whichTable){
                                   #ageRange,sizeRange,expRange,ownRange,firmType){
  
  # sector <- "Manufacturing"
  # indicatorDesc <- "labor cost (n2a) over sales (d2)"
  # firmType <- "By exports status"
  # allocEff <- "All countries"
  # whichTable <- 2
  
  # filter data by sector. Only drill down for manufacturing -------------------
  indicatorCode <- .indicatorToCode(indicatorDesc)
  sectCode <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  
  groupByVar <- "all"
  if (sector == "Manufacturing") {
    #data <- filter(data, sector_MS %in% sector)
    if (firmType == "By age") {
      groupByVar <- "age"
      lenVar <- length(firmAgeList)-1
      #dataBlock <- dataBlock_age
    }
    if (firmType == "By size") {
      groupByVar <- "size"
      lenVar <- length(firmSizeList)-1
      #dataBlock <- dataBlock_size
    }
    if (firmType == "By exports status") {
      groupByVar <- "expStatus"
      lenVar <- length(firmExpStatusList)-1
      #dataBlock <- dataBlock_expStatus
    }
    if (firmType == "By foreign ownership") {
      groupByVar <- "forOwner"
      lenVar <- length(firmForeignOwnerList)-1
      #dataBlock <- dataBlock_forOwner
    }
    if (firmType == "By tech. innovation") {
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
    } else if (lenVar == 2) {
      names(sumStats) <- names(dataBlock)[c(5:7,13:15)]
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
  incomeStats <- mutate(incomeStats, Group = row.names(incomeStats))
  incomeStats <- select(incomeStats, Group,everything())
  
  regionStats <- filter(regionStats, !is.na(region))
  regionRowNames <- as.character(regionStats$region)
  regionStats <- select(regionStats, -region)
  regionStats <- mutate_each(regionStats, funs(as.numeric))
  row.names(regionStats) <- regionRowNames
  regionStats <- mutate(regionStats, Group = row.names(regionStats))
  regionStats <- select(regionStats, Group,everything())
  # whichTable: "Countries"=1,"Summary Stats"=2,"Income level medians"=3,"Region medians"=4
  
  if (whichTable == 1){
    summaryStats <- dataBlock
    summaryStats <- select(summaryStats, country=countryOnly,year=yearOnly,N,mean,
                           median,sd,iqr,OPcov,indAlloc,outliersOut)
    summaryStats[,3:ncol(summaryStats)] <- round(summaryStats[,3:ncol(summaryStats)],2)
  }
  if (whichTable == 2){
    summaryStats <- round(sumStats,2)
  }
  if (whichTable == 3){
    summaryStats <- incomeStats
    summaryStats[,2:ncol(summaryStats)] <- round(summaryStats[,2:ncol(summaryStats)],2)
  }
  if (whichTable == 4){
    summaryStats <- regionStats
    summaryStats[,2:ncol(summaryStats)] <- round(summaryStats[,2:ncol(summaryStats)],2)
  }
  # NAs to "---"
  summaryStats[is.na(summaryStats)] <- "---"
  
  return(summaryStats)
}  

#write.csv(summaryStats,"summaryStats.csv")
