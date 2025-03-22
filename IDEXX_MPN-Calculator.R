### LOADING-------------------------------------------------
library("readxl")
library("ggplot2")
library("dplyr")
library("fitdistrplus")


###FILE SETUP--------------------------------------------------------------------
##Data
IDEXXData <- paste0(FolderPath,"/Data/final datasheets")

##MPN Table for IDEXX
MPNTable <- read_excel(paste0(FolderPath,"/Data/MPNTable.xlsx"))
MPNTable[] <- lapply(MPNTable, function(x) gsub("[<1]", "0.5", x))
MPNTable[] <- lapply(MPNTable, function(x) gsub("[>2419.6]", "2420", x))


###DATA SETUP------------------------------------------------------------------------
##Calculate concentrations for IDEXX
#MPN Function
calculate_mpn <- function(large, small) {
  return(MPNTable[(large) + 1, (small) + 2])
}
#MPN, adjusted for volume used
IDEXXData$MPNColiform <- mapply(calculate_mpn, IDEXXData$`# LARGE YELLOW WELLS`, IDEXXData$`# SMALL YELLOW WELLS`) 
IDEXXData$MPNColiform_adj <- as.numeric(IDEXXData$MPNColiform)* (100/IDEXXData$`Volume (mL)`)
IDEXXData$MPNEcoli <- mapply(calculate_mpn, IDEXXData$`# LARGE FLUOR WELLS`, IDEXXData$`# SMALL FLUOR WELLS`) 
IDEXXData$MPNEcoli_adj <- as.numeric(IDEXXData$MPNEcoli)* (100/IDEXXData$`Volume (mL)`)
