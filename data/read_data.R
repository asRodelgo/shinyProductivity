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
firmAgeList <- c("All firms","0-10","11-20","+20")
firmSizeList <- c("All firms","Small firms","Medium firms","Large firms")
firmExpStatusList <- c("All firms","Do not export","Export")
#firmTechInnovList
firmForeignOwnerList <- c("All firms","Local Ownership","Foreign Ownership")

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

.firmTypeListDesc <- function(sector){
  
  if (sector == "Manufacturing"){
    firmList <- firmTypeList
  } else {
    firmList <- c("All firms")
  }
  return(firmList)   
}
.sectorToCode <- function(sector){
  
  sect <- ifelse(sector=="All sectors","AllSect",ifelse(sector=="Manufacturing","Manuf","Serv"))
  return(sect)
}

.firmTypeCode <- function(type){
  if (type=="All firms"){
    typeCode <- "all"
  } else if (type=="By age"){
    typeCode <- "age"
  } else if (type=="By size"){
    typeCode <- "size"
  } else if (type=="By export status"){
    typeCode <- "expStatus"
  } else {
    typeCode <- "forOwner"
  }
  return(typeCode)
}

.indicatorToCode <- function(indicatorDesc){
  
  indicatorCode <- filter(summaryMaps,indicator == indicatorDesc)$code
  return(indicatorCode)
}
.indicatorToCodeOld <- function(indicatorDesc){
  
  indicatorCode <- filter(dataMaps,indicator == indicatorDesc)$code
}

# pre-process data (execute once at start up) -----------------------
# segment data according to firm types values
data <- data %>%
  group_by(country,idstd) %>%
  mutate(ageVal = as.numeric(thisYear) - b5,
         age = as.character(ifelse(ageVal < 11,firmAgeList[2],ifelse(ageVal < 21,firmAgeList[3],firmAgeList[4]))),
         size = as.character(ifelse(l1 < 20,firmSizeList[2],ifelse(l1 < 100,firmSizeList[3],firmSizeList[4]))), 
         expVal = d3b + d3c,
         expStatus = as.character(ifelse(expVal > 0,firmExpStatusList[3],firmExpStatusList[2])),
         forOwner = as.character(ifelse(b2a > 0,firmForeignOwnerList[3],firmForeignOwnerList[2]))) %>% # filter by age, size, etc...
  select(-ageVal,-expVal)

# prepare data for summary statistics. Read dataBlocks --------------------

#Calculate datablocks (for the actual UI, precalculate these and read.csv)
#takes about 1/2 min per dataBlock. Creates list with all dataBlocks and saves it in disk
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

