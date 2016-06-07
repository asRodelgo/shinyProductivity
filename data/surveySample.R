library(foreign) # read/write Stata (.dta) files
library(readstata13) # read data in version 13 of Stata
library(survey)
library(dplyr)
data <- read.dta13("data/TFPR_and_ratios.dta")

# stratified survey for non-tfp indicators (id=~1 as this is stratified sampling with no clusters)
# At country level, strata don't play any role and the stratified sample mean is equivalent
# to weighted mean. 

cou <- "Argentina2010"
sect <- "Manufacturing"
variable <- "n2a_d2"#"d2_l1"
isic_code <- 19

dataForSampling <- data %>%
  filter(country == cou & sector_MS == sect & isic == isic_code) %>%
  select(idstd,wt,strata,var = one_of(variable))

# dataForSampling <- dataForSampling %>%
#   group_by(isic) %>%
#   mutate(meanS = mean(var,na.rm=TRUE)) 
# no clusters (id=~1)
myDesign <- svydesign(id=~1,data=dataForSampling,weights=~wt,strata=~strata)
# avoid errors as some strata contain only 1 PSU (primary sample unit)  
options(survey.lonely.psu = "certainty")
# calculate means by the interaction of sector and isic
svymean(~var,myDesign,na.rm=TRUE)
# compare with weighted mean
weighted.mean(dataForSampling$var,dataForSampling$wt,na.rm=TRUE)

# data(api)
# 
# ## one-stage cluster sample
# dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
# 
# svymean(~api00, dclus1, deff=TRUE)
# svymean(~factor(stype),dclus1)
# svymean(~interaction(stype, comp.imp), dclus1)
# 

