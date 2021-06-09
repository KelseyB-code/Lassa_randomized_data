# 
library(purrr)
library(dplyr)
library(gridExtra)
library(ggplot2)
# 
# setwd("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data")
# filename = "commcare-cleaned-data-2018-2021.csv"
# 
# ## load data
# lassa <- read.csv(filename)
# lassa$ID <- seq.int(nrow(lassa))
# lassa <- lassa %>%
#   filter(lassa$clinical_outcome.outcome == "recovered" | lassa$clinical_outcome.outcome == "died")
# 
# bloodchem <- c("aspartate", "urea", "creatinine", "alanine", "potassium")
# vitals <- c("temperature",
#               "systolic_blood_pressure",
#               "diastolic_blood_pressure",
#               "pulse_rate",
#               "respiratory_rate")
# ct <- c("ct")
# namelist <- c(bloodchem, vitals, ct)
# 

################################################################################
# write custom ggplot function

ggfunc <- function(df, names) {ggplot(data = df, aes(x = day, y = val, group = ID, colour = outcome)) +
    #geom_line(aes(color = as.factor(outcome), linetype = as.factor(outcome)), size = 0.7) +
    #scale_linetype_manual(values = c("longdash", "solid")) + 
    geom_point(aes(color = as.factor(outcome)), alpha = 1/15) +
    scale_color_manual(values = c("black", "red")) + theme_bw() +
    theme(legend.position = "", panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    labs(x = "day", y = names, colour = "outcome ") +
    xlim(1, max(subset(df$day, df$outcome == 1), na.rm=T)) + 
    ggtitle(names) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #ylim(0,100) + 
    geom_smooth(aes(group = outcome))}

# returns a days data frame for each variable
days_func <- function(variable){
  if(variable %in% bloodchem){
    days <- lassa %>%
    dplyr::select(contains("blood_chemistry_list.days_since_admission"))
  }else if(variable %in% vitals){
    days <- data.frame(lapply(1:10, function(x) rep(x, nrow(lassa))))
  }else{
    days <- lassa %>%
      dplyr::select(contains("pcr_list.days"))
  }
    return(days)
}
  
# return a values data frame for each variable
bc_func <- function(variable){
  if(variable %in% bloodchem){
    bc <- lassa %>%
      dplyr::select(starts_with("blood_chemistry")) %>%
      dplyr::select(contains(variable))
  }else if(variable %in% vitals){
    bc <- lassa %>%
      dplyr::select(starts_with("clinical_factors.vital_signs.max_")) %>%
      dplyr::select(contains(variable))
  }else{
    bc <- lassa %>%
      dplyr::select(contains("pcr_list.pcr_cycle_threshold")) %>%
      dplyr::mutate(across(everything(), ~ ifelse(. > 60, NA, .)))
  }
  return(bc)
}

bl_func <- function(variable) {
  biglist <- lapply(1:dim(bc_func(variable))[2], 
                    function(x) data.frame(ID = lassa$ID, 
                                           outcome = ifelse(lassa$clinical_outcome.outcome == "died", 1, 0), 
                                           age = lassa$basic_information.age, 
                                           day = days_func(variable)[,x],  
                                           val = bc_func(variable)[[x]]))
return(biglist)
}

list <- lapply(namelist, bl_func)

list <- lapply(1:length(namelist), function(x) bind_rows(list[[x]]))

list <- lapply(1:length(namelist), function(x)
  list[[x]] %>%
    mutate(outcome=as.factor(outcome), 
           ID = as.factor(ID),
           day = as.numeric(gsub(" days", "",  list[[x]]$day))) %>%
    group_by(outcome) %>%
    filter(!(abs(day - median(day, na.rm=T)) > 2*sd(day, na.rm=T)) & day>0) %>%
    filter(!(abs(val - median(val, na.rm=T)) > 2*sd(val, na.rm=T)) & val>0))

list <- lapply(1:length(namelist), function(x) distinct(list[[x]]) %>% na.omit(list[[x]]))

names(list) <- namelist

gglist <- lapply(1:length(namelist), function(x) ggfunc(list[[x]], namelist[[x]]))

