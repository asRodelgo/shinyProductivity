# Read data
#data <- read.dta13("data/TFPR_and_ratios.dta")
# Read data complete
data <- read.dta13("data/TFPR_and_ratios_Complete.dta")
# Read data mappings
dataMaps <- read.csv("data/codeMappings.csv",stringsAsFactors = FALSE)
# country List
countryList <- sort(unique(data$country))
#indicators List
indicatorList <- sort(unique(filter(dataMaps, allSectors == 1)$indicator))
#
sectorList <- c("All sectors", "Manufacturing", "Services")
#