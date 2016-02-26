###
# 1. Calculate all summary stats as in Nona's excel file
###
.indicatorToCode <- function(indicatorDesc){
  
  indicator <- filter(dataMaps,indicator == indicatorDesc)$code
}


.statsTable <- function(countryYear,removeOutliers,outlierIQRfactor,indicatorDesc,indicatorQuantileDesc){

# removeOutliers <- TRUE
# outlierIQRfactor <- 10
# indicator <- "n2a_d2"
# indicatorQuantile <- "d2_n2a"
# removeOutliers <- 1
# countryYear <- "Brazil2009"  
  indicator <- .indicatorToCode(indicatorDesc)
  indicatorQuantile <- .indicatorToCode(indicatorQuantileDesc)
  N_indicator <- paste0("N_",indicator)
  outlierIQRfactor <- as.numeric(outlierIQRfactor)

  # to calculate number of outliers left out
  data_aux <- data %>%
    select(country,N_indicator = one_of(N_indicator)) %>%
    filter(country == countryYear) %>%
    mutate(sampleSizeBefore=sum(N_indicator,na.rm=TRUE))
  sampleSizeBefore = data_aux$sampleSizeBefore[1]
#  
  if (removeOutliers==1){
    
    data2 <- data %>%
      filter(country == countryYear) %>%
      select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
              indicatorQuantile = one_of(indicatorQuantile),N_indicator = one_of(N_indicator)) %>%
      #group_by(country) %>% 
      filter(!is.na(indicator)) %>% # remove NAs
      filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
             & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
             ) %>% # remove outliers
      mutate(N = sum(N_indicator,na.rm=TRUE),
             #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
             mean = weighted.mean(indicator,wt,na.rm=TRUE),
             median = weightedMedian(indicator,wt,na.rm=TRUE),
             sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)/sum(wt)),
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
    
    data2 <- data %>%
      filter(country == countryYear) %>%
      select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
             indicatorQuantile = one_of(indicatorQuantile),N_indicator = one_of(N_indicator)) %>%
      #group_by(country) %>% 
      filter(!is.na(indicator)) %>% # remove NAs
      mutate(N = sum(N_indicator,na.rm=TRUE),
             #N_effective = sum(ifelse(!(is.na(indicator)),1,0),na.rm=TRUE),
             mean = weighted.mean(indicator,wt,na.rm=TRUE),
             median = weightedMedian(indicator,wt,na.rm=TRUE),
             sd = sqrt(sum(wt*(indicator-mean)^2,na.rm=TRUE)/sum(wt)),
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
    
  }
  data2 <- data2[!(duplicated(data2)),]
  
  sampleSizeAfter <- data2$N
  data2 <- mutate(data2, outliersOut = sampleSizeBefore - sampleSizeAfter)
  # output Indirect Allocative Efficiency
  #data2 <- mutate(data2, indAllocEff = ratio_median_emp90_50/ratio_median_emp10_50)
  return(data2)
}

# plot boxplots
.statsPlots <- function(countryYear,removeOutliers,outlierIQRfactor,indicatorDesc,indicatorQuantileDesc) {

  indicator <- .indicatorToCode(indicatorDesc)
  indicatorQuantile <- .indicatorToCode(indicatorQuantileDesc)
  outlierIQRfactor <- as.numeric(outlierIQRfactor)
  if (removeOutliers==1){
    data2 <- data %>%
      filter(country==countryYear) %>%
      select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
             indicatorQuantile = one_of(indicatorQuantile)) %>%
      filter((indicator < wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)+outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
             & (indicator > wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)-outlierIQRfactor*(wtd.quantile(indicator,100*round(wt,1),0.75,na.rm=TRUE)-wtd.quantile(indicator,100*round(wt,1),0.25,na.rm=TRUE)))
      ) # remove outliers
  } else{
    data2 <- data %>%
      filter(country==countryYear) %>%
      select(idstd,country,wt,sector_MS,income,l1,indicator = one_of(indicator),
             indicatorQuantile = one_of(indicatorQuantile))
  }
  par(mfrow = c(2,2))
  hist(data2$indicator)
  boxplot(data2$indicator)
  plot(data2$indicator,data2$indicatorQuantile)
  boxplot(data2$indicator,data2$indicatorQuantile)
}
