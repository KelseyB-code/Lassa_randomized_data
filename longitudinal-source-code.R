library(purrr)
library(dplyr)
setwd("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data")
filename = "commcare-cleaned-data-2018-2021.csv"
lassa <- read.csv(filename)
lassa$ID <- seq.int(nrow(lassa))
lassa <- lassa %>%
  filter(lassa$clinical_outcome.outcome == "recovered" | lassa$clinical_outcome.outcome == "died")


################################################################################
# Blood chemistry over time
# 
# namelist <- c("aspartate", "urea", "creatinine", "alanine", "potassium")
# ndays <- 20
# 
# bc <- lassa %>%
#   dplyr::select(starts_with("blood_chemistry")) %>%
#   dplyr::select(contains(namelist))
# 
# list <- lapply(1:length(namelist), function(i) bc[,grepl(namelist[i], names(bc))])
# 
# days <- lassa %>%
#   dplyr::select(contains("blood_chemistry_list.days_since_admission"))

################################################################################
#Ct over time
#
namelist <- c("Ct")

bc <- lassa %>%
  select(contains("pcr_list.pcr_cycle_threshold"))
bc <- map(1:7, function(x) ifelse(bc[[x]] > 60, NA, bc[[x]]))

list <- list(bc)

days <- lassa %>%
  select(contains("pcr_list.days"))

################################################################################
# Vitals over time
# 
# namelist <- c("temperature", 
#               "systolic_blood_pressure", 
#               "diastolic_blood_pressure", 
#               "pulse_rate", 
#               "respiratory_rate")
# 
# bc <- lassa %>%
#   dplyr::select(starts_with("clinical_factors.vital_signs.max_")) %>%
#   dplyr::select(contains(namelist))
# 
# list <- lapply(1:length(namelist), function(i) bc[,grepl(namelist[i], names(bc))])