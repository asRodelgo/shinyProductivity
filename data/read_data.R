# Read data
#data <- read.dta13("data/TFPR_and_ratios.dta")
# read some generic functions
source("helper_functions/generic_functions.R", local = TRUE)
# Read data complete
data <- read.dta13("data/TFPR_and_ratios.dta")
# Read country regions and income levels
countryRegions <- read.csv("data/countryMappingProductivity.csv",stringsAsFactors = FALSE)
# Read data mappings
dataMaps <- read.csv("data/codeMappings.csv",stringsAsFactors = FALSE)
# Read data mappings for the summary tables
summaryMaps <- read.csv("data/codeMappings_summaryStats.csv",stringsAsFactors = FALSE)
# country List
countryList <- sort(unique(data$country))
countryOnlyList <- sort(unique(countryRegions$country))

#indicators List
indicatorList <- sort(unique(filter(summaryMaps, allSectors == 1)$indicator))
indicatorTFPList <- .indicatorList("Manufacturing")[which(substr(.indicatorList("Manufacturing"),1,5)=="Total")]
indicatorOnlyTFP <- sort(unique(filter(summaryMaps, substr(code,1,4)=="tfpr" & sectorLevel == 1)$indicator))
#
#indicatorListOrig <- sort(unique(filter(dataMaps, allSectors == 1)$indicator))
#
sectorList <- c("All sectors", "Manufacturing", "Services")
#
firmTypeList <- c("All firms","By age","By size","By exports status","By imports status","By foreign ownership") #,"By tech. innovation"
#
firmAgeList <- c("All firms","Young","Mature","Old")
firmSizeList <- c("All firms","Small","Medium","Large")
firmExpStatusList <- c("All firms","NonExporter","Exporter")
firmImpStatusList <- c("All firms","NonImporter","Importer")
#firmTechInnovList
firmForeignOwnerList <- c("All firms","Domestic","Foreign")
industryMaps <- read.csv("data/industryMapping.csv",stringsAsFactors = FALSE)
industryList <- industryMaps$industry

# pre-process data (execute once at start up) -----------------------
# segment data according to firm types values
data <- data %>%
  group_by(country,idstd) %>%
  mutate(ageVal = as.numeric(thisYear) - b5,
         age = as.character(ifelse(ageVal < 11,firmAgeList[2],ifelse(ageVal < 21,firmAgeList[3],firmAgeList[4]))),
         size = as.character(ifelse(l1 < 20,firmSizeList[2],ifelse(l1 < 100,firmSizeList[3],firmSizeList[4]))), 
         expVal = d3b + d3c,
         expStatus = as.character(ifelse(expVal > 0,firmExpStatusList[3],firmExpStatusList[2])),
         impStatus = as.character(ifelse(d13 == 1,firmImpStatusList[3],ifelse(d13 == 2,firmImpStatusList[2],NA))),
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
      indCode <- .indicatorToCodeAllIndustries(ind)
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

