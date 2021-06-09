# make a separate table for treatments to show difference between survived and died
# figure out if there is an effect on mortality of using different antibiotics 
# report overall frequency for prescribed antibiotics, turn into binary vars and report mortality for each
# all vars that start with special_interventions
# tweak plots, data cleaning and remove outliers (do this in the graphs by ignoring outliers and fixing axes)

# Final report should be Table 1 symptoms (w/o spec. int.), table 2 special interventions, table 3 antibiotics 

# next figure out how to run the Ebola code from the cluster 

rm(list=ls())


library(purrr)
library(dplyr)
library(gridExtra)

setwd("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data")
filename = "commcare-cleaned-data-2018-2021.csv"

## load data
lassa <- read.csv(filename)
lassa$ID <- seq.int(nrow(lassa))
lassa <- lassa %>%
  filter(lassa$clinical_outcome.outcome == "recovered" | lassa$clinical_outcome.outcome == "died")

bloodchem <- c("aspartate", "urea", "creatinine", "alanine", "potassium")
vitals <- c("temperature",
            "systolic_blood_pressure",
            "diastolic_blood_pressure",
            "pulse_rate",
            "respiratory_rate")
ct <- c("ct")
namelist <- c(bloodchem, vitals, ct)

setwd("/Users/kelsey/Lassa_randomized_data")
source("5b_values-over-time-function.R")

pdf("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data/all-over-time.pdf")
n <- length(gglist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(gglist, ncol=nCol))
dev.off()