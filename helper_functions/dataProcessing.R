###
# 1. Calculate all summary stats as in Nona's excel file
###


.statsTable <- function(sector,countryYear,removeOutliers,outlierIQRfactor,indicatorDesc,indicatorQuantileDesc,weightType,
                        ageRange,sizeRange,expStatus,forOwner){

# removeOutliers <- FALSE
# outlierIQRfactor <- 10
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
  
  # some mappings
  indicatorCode <- .indicatorToCode(indicatorDesc)
  indicatorQuantileCode <- .indicatorToCode(indicatorQuantileDesc)
  N_indicatorCode <- paste0("N_",indicatorCode)
  outlierIQRfactor <- as.numeric(outlierIQRfactor)
  ageB <- ageRange[1]
  ageU <- ageRange[2]
  expStatusB <- expStatus[1]
  expStatusU <- expStatus[2]
  forOwnerB <- forOwner[1]
  forOwnerU <- forOwner[2]

  # filter data by sector. Only drill down for manufacturing 
  if (sector == "Manufacturing") {
    data <- filter(data, sector_MS %in% sector)
    # calculate age, size, export status, foreign ownership and tech innov status and filter
    if (sizeRange == "All firms") {
      sizeRange <- c("Small firm","Medium firm","Large firm")
    }
    data <- data %>%
      group_by(country,idstd) %>%
      mutate(age = as.numeric(thisYear) - b5, 
             size = as.character(ifelse(l1 < 20,"Small firm",ifelse(l1 < 100,"Medium firm","Large firm"))), 
             expStatus = d3b + d3c, forOwner = b2a) %>% # filter by age, size, etc...
      filter(age >= ageB & age <= ageU & size %in% sizeRange &
               expStatus >= expStatusB & expStatus <= expStatusU & 
               forOwner >= forOwnerB & forOwner <= forOwnerU)
    
    data <- as.data.frame(data)
  } else if (sector == "Services") {
    data <- filter(data, sector_MS %in% sector)
  }
  
  # If sample too small, not worth it
  if (nrow(data[data$country==countryYear,])>=5){
    # Calculate number of outliers left out  
    data_aux <- data %>%
      select(country,N_indicator = one_of(N_indicatorCode)) %>%
      filter(country == countryYear) %>%
      group_by(country) %>%
      mutate(sampleSizeBefore=sum(N_indicator,na.rm=TRUE))
    sampleSizeBefore = data_aux$sampleSizeBefore[1]
    
    # selected weight type
    if (weightType==2){ # market share
      data <- data %>%
        group_by(country) %>%
        mutate(wt = wt*d2_gdp09/sum(wt*d2_gdp09,na.rm=TRUE))
    } else if (weightType==3){ # employment share
      data <- data %>%
        group_by(country) %>%
        mutate(wt = wt*l1/sum(wt*l1,na.rm=TRUE))
    } 
    
    # calculate statistics
    if (removeOutliers==1){
      
      if (indicatorCode == indicatorQuantileCode){ # if indicator and indicatorQuantile are the same, keep just one
        data2 <- data %>%
          filter(country == countryYear) %>%
          select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
                 N_indicator = one_of(N_indicatorCode)) %>%
          filter(!is.na(indicator)) %>%
          mutate(indicatorQuantile = indicator)
      } else {
        data2 <- data %>%
          filter(country == countryYear) %>%
          select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
                 indicatorQuantile = one_of(indicatorQuantileCode),N_indicator = one_of(N_indicatorCode)) %>%
          filter(!is.na(indicator)) # remove NAs
      }
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
               emp10 = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.1,na.rm=TRUE),l1,NA),
               median_emp10 = weightedMedian(emp10,wt,na.rm=TRUE),
               emp50 = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.5,na.rm=TRUE),l1,NA),
               median_emp50 = weightedMedian(emp50,wt,na.rm=TRUE),
               emp90 = ifelse(indicatorQuantile<=wtd.quantile(indicatorQuantile,100*round(wt,1),0.9,na.rm=TRUE),l1,NA),
               median_emp90 = weightedMedian(emp90,wt,na.rm=TRUE),
               ratio_median_emp10_50 = median_emp10/median_emp50,
               ratio_median_emp90_50 = median_emp90/median_emp50,
               emp_weighted = l1/sum(wt*l1,na.rm=TRUE),
               emp_unweighted = l1/sum(l1,na.rm=TRUE),
               OPcov = sum(wt*(indicator-mean)*(emp_weighted-(1/sum(wt,na.rm=TRUE))),na.rm=TRUE),
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
                        ) %>%
        select(-idstd,-wt,-sector_MS,-l1,-indicator,-indicatorQuantile,-emp10,-emp50,-emp90,
               -emp_weighted,-emp_unweighted,-N_indicator)
    
      
    } else {
      
      if (indicatorCode == indicatorQuantileCode){ # if indicator and indicatorQuantile are the same, keep just one
        data2 <- data %>%
          filter(country == countryYear) %>%
          select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
                 N_indicator = one_of(N_indicatorCode)) %>%
          filter(!is.na(indicator)) %>%
          mutate(indicatorQuantile = indicator)
      } else {
        data2 <- data %>%
          filter(country == countryYear) %>%
          select(idstd,country,wt,sector_MS,income, l1, indicator = one_of(indicatorCode),
                 indicatorQuantile = one_of(indicatorQuantileCode),N_indicator = one_of(N_indicatorCode)) %>%
          filter(!is.na(indicator)) # remove NAs
      }
      data2 <- as.data.frame(data2)
      data2 <- data2 %>%  
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
               OPcovNoWeights = sum((indicator-mean(indicator,na.rm=TRUE))*(emp_unweighted-mean(emp_unweighted,na.rm=TRUE)),na.rm=TRUE)
        ) %>%
        select(-idstd,-wt,-sector_MS,-l1,-indicator,-indicatorQuantile,-emp10,-emp50,-emp90,
               -emp_weighted,-emp_unweighted,-N_indicator)
      
    }
    data2 <- data2[!(duplicated(data2)),]
    
    sampleSizeAfter <- data2$N
    data2 <- mutate(data2, outliersOut = sampleSizeBefore - sampleSizeAfter)
  
  } else {
    data2 <- data.frame(country,income=NA,OPcov=NA,OPcovNoWeights=NA,
    ratio_median_emp90_50=NA,ratio_median_emp10_50=NA,outliersOut=NA,None=NA)
  }
  # output Indirect Allocative Efficiency
  #data2 <- mutate(data2, indAllocEff = ratio_median_emp90_50/ratio_median_emp10_50)
  return(data2)
}

# plot boxplots
.statsPlots <- function(sector,countryYear,removeOutliers,outlierIQRfactor,indicatorDesc,indicatorQuantileDesc,
                        ageRange,sizeRange,expStatus,forOwner) {

  # some mappings
  indicatorCode <- .indicatorToCode(indicatorDesc)
  indicatorQuantileCode <- .indicatorToCode(indicatorQuantileDesc)
  outlierIQRfactor <- as.numeric(outlierIQRfactor)
  ageB <- ageRange[1]
  ageU <- ageRange[2]
  expStatusB <- expStatus[1]
  expStatusU <- expStatus[2]
  forOwnerB <- forOwner[1]
  forOwnerU <- forOwner[2]
  
  # filter data by sector. Only drill down for manufacturing 
  if (sector == "Manufacturing") {
    data <- filter(data, sector_MS %in% sector)
    # calculate age, size, export status, foreign ownership and tech innov status and filter
    if (sizeRange == "All firms") {
      sizeRange <- c("Small firm","Medium firm","Large firm")
    }
    data <- data %>%
      group_by(country,idstd) %>%
      mutate(age = as.numeric(thisYear) - b5, 
             size = as.character(ifelse(l1 < 20,"Small firm",ifelse(l1 < 100,"Medium firm","Large firm"))), 
             expStatus = d3b + d3c, forOwner = b2a) %>% # filter by age, size, etc...
      filter(age >= ageB & age <= ageU & size %in% sizeRange &
               expStatus >= expStatusB & expStatus <= expStatusU & 
               forOwner >= forOwnerB & forOwner <= forOwnerU)
    
    data <- as.data.frame(data)
  } else if (sector == "Services") {
    data <- filter(data, sector_MS %in% sector)
  }
  
  # If sample too small, not worth it
  if (nrow(data[data$country==countryYear,])>=5){
    # prepare plots
    if (removeOutliers==1){
      data2 <- data %>%
        filter(country==countryYear) %>%
        select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicatorCode),
               indicatorQuantile = one_of(indicatorQuantileCode)) %>%
        filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
               & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
        ) # remove outliers
    } else{
      data2 <- data %>%
        filter(country==countryYear) %>%
        select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicatorCode),
               indicatorQuantile = one_of(indicatorQuantileCode))
    }
    par(mfrow = c(2,2))
    hist(data2$indicator, main = indicatorCode,xlab=indicatorCode,col="lightgreen")
    boxplot(data2$indicator,xlab=indicatorCode)
    plot(data2$indicator,data2$indicatorQuantile,xlab=indicatorCode,ylab=indicatorQuantileCode)
    boxplot(data2$indicator,data2$indicatorQuantile,names=c(indicatorCode,indicatorQuantileCode))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"", col="red", cex=2)
  }
}
