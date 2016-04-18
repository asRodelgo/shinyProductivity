# load global packages ----------------------------------------------
library(plyr) # manipulate data 
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(data.table) # fast operations
library(tidyr) # transform data
library(reshape2) # manipulate data
library(foreign) # read/write Stata (.dta) files
library(readstata13) # read data in version 13 of Stata
library(matrixStats) # calculate weighted median
# library(reldist) # calculate weighted quantiles
library(Hmisc) # calculate weighted quantiles
library(DT) # customize dataTable javascript library

# --------------------------------------------------------
# Object: load the raw questionnaire data and perform:
# 1. Calculate summary statistics with/without weights.Check with Nona's
# 2. Robustness checks of 10-50 percentile vs. 50-90 percentile for employment. 
#    Check the signs of the covariances
# 3. Aggregate indicators by: 
#     Country
#     Type of firm: age, size, export status, tech. innovation status, foreign ownership
#     Sector: Aggregated, Manufacture and Services
# 4. Look at the ES country profile report for more visualization ideas
# --------------------------------------------------------
# this year
thisYear <- substr(Sys.Date(),1,4)
# Read the global data available for the whole session. Will be loaded only once
source("data/read_data.R", local = TRUE)
# These functions are called before the actual server work. They will be loaded for the
# session
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)


