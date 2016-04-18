# Read data
#data <- read.dta13("data/TFPR_and_ratios.dta")
# Read data complete
data <- read.dta13("data/TFPR_and_ratios_Complete.dta")
# Read country regions and income levels
countryRegions <- read.csv("data/countryMappingProductivity.csv")
# Read data mappings
dataMaps <- read.csv("data/codeMappings.csv",stringsAsFactors = FALSE)
# Read data mappings for the summary tables
summaryMaps <- read.csv("data/codeMappings_summaryStats.csv",stringsAsFactors = FALSE)
# country List
countryList <- sort(unique(data$country))
#indicators List
indicatorList <- sort(unique(filter(summaryMaps, allSectors == 1)$indicator))
#
#indicatorListOrig <- sort(unique(filter(dataMaps, allSectors == 1)$indicator))
#
sectorList <- c("All sectors", "Manufacturing", "Services")
#
firmTypeList <- c("All firms","By age","By size","By exports status","By foreign ownership") #,"By tech. innovation"
#
firmAgeList <- c("All firms","0-5","6-15","16-30","+30")
firmSizeList <- c("All firms","Small firms","Medium firms","Large firms")
firmExpStatusList <- c("All firms","0-25%","26-50%","51-75%","+75%")
#firmTechInnovList
firmForeignOwnerList <- c("All firms","0-50%","50-75%","+75%")

.indicatorList <- function(sector){
  
  if (sector == "Manufacturing"){
    indList <- filter(summaryMaps, manufacturing == 1)$indicator
  } else if (sector == "Services"){
    indList <- filter(summaryMaps, services == 1)$indicator
  } else {
    indList <- filter(summaryMaps, allSectors == 1)$indicator
  }
  return(indList)   
}

.firmTypeList <- function(sector){
  
  if (sector == "Manufacturing"){
    firmList <- c("all","age","size","expStatus","forOwner")
  } else {
    firmList <- c("all")
  }
  return(firmList)   
}

.indicatorToCode <- function(indicatorDesc){
  
  indicatorCode <- filter(summaryMaps,indicator == indicatorDesc)$code
}

# pre-process data (execute once at start up) -----------------------
# segment data according to firm types values
data <- data %>%
  group_by(country,idstd) %>%
  mutate(ageVal = as.numeric(thisYear) - b5,
         age = as.character(ifelse(ageVal < 6,firmAgeList[2],ifelse(ageVal < 16,firmAgeList[3],ifelse(ageVal < 31,firmAgeList[4],firmAgeList[5])))),
         size = as.character(ifelse(l1 < 20,firmSizeList[2],ifelse(l1 < 100,firmSizeList[3],firmSizeList[4]))), 
         expVal = d3b + d3c,
         expStatus = as.character(ifelse(expVal < 26,firmExpStatusList[2],ifelse(expVal < 51,firmExpStatusList[3],ifelse(expVal < 75,firmExpStatusList[4],firmExpStatusList[5])))),
         forOwner = as.character(ifelse(b2a < 51,firmForeignOwnerList[2],ifelse(b2a < 76,firmForeignOwnerList[3],firmForeignOwnerList[4])))) %>% # filter by age, size, etc...
  select(-ageVal,-expVal)

# prepare data for summary statistics. Read dataBlocks --------------------

# Calculate datablocks (for the actual UI, precalculate these and read.csv)
# takes about 15 min. Creates list with 35 elements and saves it in disk
# dataBlock <- list()
# for (sect in sectorList){
#   for (type in .firmTypeList(sect)){
#     for (ind in .indicatorList(sect)) {
#       indCode <- .indicatorToCode(ind)
#       sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
#       dataBlock[[paste(sectCode,type,indCode,sep="_")]] <- .calculateDataBlock(type,sect,ind)
#       write.csv(dataBlock[[paste(sectCode,type,indCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),row.names = FALSE)
#     }
#   }
# }
# read dataBlocks
dataBlock <- list()
for (sect in sectorList){
  for (type in .firmTypeList(sect)){
    for (ind in .indicatorList(sect)) {
      indCode <- .indicatorToCode(ind)
      sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
      dataBlock[[paste(sectCode,type,indCode,sep="_")]] <- read.csv(paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),stringsAsFactors = FALSE)
    }
  }
}


# dataBlockServices <- .calculateDataBlock(data,"all","Services")
# dataBlockManufacturing <- .calculateDataBlock(data,"all","Manufacturing")
# dataBlock_age <- .calculateDataBlock(data,"age","Manufacturing")
# dataBlock_size <- .calculateDataBlock(data,"size","Manufacturing")
# dataBlock_expStatus <- .calculateDataBlock(data,"expStatus","Manufacturing")
# dataBlock_forOwner <- .calculateDataBlock(data,"forOwner","Manufacturing")

# write dataBlocks
# for (sect in sectorList){
#   for (type in .firmTypeList(sect)){
#     for (ind in .indicatorList(sect)) {
#       indCode <- .indicatorToCode(ind)
#       sectCode <- ifelse(sect=="All sectors","AllSect",ifelse(sect=="Manufacturing","Manuf","Serv"))
#       write.csv(dataBlock[[paste(sectCode,type,indCode,sep="_")]],paste0("data/dataBlock_",paste(sectCode,type,indCode,sep="_"),".csv"),row.names = FALSE)
#     }
#   }
# }

# write.csv(dataBlock,"data/dataBlock.csv",row.names = FALSE)
# write.csv(dataBlockServices,"data/dataBlockServices.csv",row.names = FALSE)
# write.csv(dataBlockManufacturing,"data/dataBlockManufacturing.csv",row.names = FALSE)
# write.csv(dataBlock_age,"data/dataBlock_age.csv",row.names = FALSE)
# write.csv(dataBlock_size,"data/dataBlock_size.csv",row.names = FALSE)
# write.csv(dataBlock_expStatus,"data/dataBlock_expStatus.csv",row.names = FALSE)
# write.csv(dataBlock_forOwner,"data/dataBlock_forOwner.csv",row.names = FALSE)

# read dataBlocks
# dataBlock <- read.csv("data/dataBlock.csv",stringsAsFactors = FALSE)
# dataBlockServices <- read.csv("data/dataBlockServices.csv",stringsAsFactors = FALSE)
# dataBlockManufacturing <- read.csv("data/dataBlockManufacturing.csv",stringsAsFactors = FALSE)
# dataBlock_age <- read.csv("data/dataBlock_age.csv",stringsAsFactors = FALSE)
# dataBlock_size <- read.csv("data/dataBlock_size.csv",stringsAsFactors = FALSE)
# dataBlock_expStatus <- read.csv("data/dataBlock_expStatus.csv",stringsAsFactors = FALSE)
# dataBlock_forOwner <- read.csv("data/dataBlock_forOwner.csv",stringsAsFactors = FALSE)

# noResponse <- data.frame(code = c(-5,-6,-7,-8,-9), 
#                          desc = c("Application denied","Still in process","Does not apply",
#                                   "refusal (spontaneous)","Don't know (spontaneous)"))

