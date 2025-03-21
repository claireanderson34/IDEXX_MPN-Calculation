### LOADING-------------------------------------------------
library("readxl")
library("ggplot2")
library("dplyr")
library("fitdistrplus")


###FILE SETUP--------------------------------------------------------------------
##Box files 
#(Downloaded from Box: All files -> CRADLE-Data -> Env-lab -> 1. Raw -> F1 -> final datasheets)
boxDataPath <- paste0(FolderPath,"/Data/final datasheets")
#(Downloaded from Box: All Files -> CRADLE-Data -> Baseline)
SurveyBaseline <- read.csv(paste0(FolderPath,"/Data/Baseline_data.csv"))

##MPN Table for IDEXX
MPNTable <- read_excel(paste0(FolderPath,"/Data/MPNTable.xlsx"))
MPNTable[] <- lapply(MPNTable, function(x) gsub("[<>]", "", x))


###DATA SETUP------------------------------------------------------------------------
##IDEXX Data
IDEXXFileList <- list.files(boxDataPath, pattern = "^CRADLE-IDEXX.*\\.xlsx$", full.names = TRUE)
IDEXXFList <- lapply(IDEXXFileList, function(file) {
  df <- read_excel(file)
  df$`Lab ID` <- as.numeric(df$`Lab ID`)
  return(df)
})
IDEXXData <- bind_rows(IDEXXFList)

##Add treatment arms to data sets
Arm <- data.frame(SurveyBaseline$dataid, SurveyBaseline$arm)
names(Arm) <- c("dataid", "arm")
IDEXXData$dataid <- sapply(strsplit(as.character(IDEXXData$Dataid), "[^0-9]"), `[`, 1)
IDEXXData <- merge(IDEXXData, Arm, by = "dataid")

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