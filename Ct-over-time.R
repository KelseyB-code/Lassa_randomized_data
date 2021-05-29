rm(list=ls())
# look at missing data
setwd("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data")
lassa <- read.csv("commcare-cleaned-data-2018-2020.csv")

ct <- lassa %>%
  select(contains("pcr_list.pcr_cycle"))

ct <- cbind(ct, lassa$clinical_outcome.outcome)

names <- c("day0", "day1", "day2", "day3", "day4", "day5", "day6", "out")
names(ct) <- names

ct <- ct %>%
  filter(out == "recovered" | out == "died")

ct %>%
  group_by(out) %>%
  summarise_all(funs(sum(!is.na(.))))

#################################################################

library(ggplot2)
library(dplyr)

var = "Ct"
search_term = "pcr_list.pcr_cycle_threshold" #variable name in blood_chemistry_list
filename = "Data/commcare-cleaned-data-2018-2020.csv"
lassa <- read.csv(filename)

lassa <- lassa %>%
  filter(lassa$clinical_outcome.outcome == "recovered" | lassa$clinical_outcome.outcome == "died")

lassa$ID <- seq.int(nrow(lassa))
age <- as.numeric(lassa$basic_information.age)

outcome <- lassa %>%
  select(clinical_outcome.outcome)
outcome <- ifelse(outcome == "recovered", 0, 1)

val <- lassa %>%
  select(contains(search_term))
days <- lassa %>%
  select(contains("pcr_list.days_since_admission_of_test")) 

day0 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,1], "val" = val[,1]))
day1 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,2], "val" = val[,2]))
day2 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,3], "val" = val[,3]))
day3 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,4], "val" = val[,4]))
day4 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,5], "val" = val[,5]))
day5 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,6], "val" = val[,6]))
day6 = data.frame(cbind("ID" = lassa$ID, "outcome" = outcome, "age" = age, "day" = days[,7], "val" = val[,7]))

long_data <- rbind(day0, day1, day2, day3, day4, day5, day6)
long_data$ID <- as.numeric(as.character(long_data$ID))
long_data$outcome <- as.factor(long_data$clinical_outcome.outcome)

long_data$day <- gsub(" days", "",  long_data$day)
long_data$day <- as.numeric(long_data$day)

long_data <- arrange(long_data[c(1,6,4,5,3)], ID)
long_data$ID <- as.factor(long_data$ID)
long_data_raw <- long_data

long_data <- distinct(long_data) 
long_data <- long_data[complete.cases(long_data), ] 

long_data <-long_data %>% 
  group_by(ID) 

write.csv(long_data, paste("Data/", var, "_over_time.csv", sep=""), row.names = F)

###################################################################################
long_data$val = as.numeric(long_data$val)

ggplot(data = long_data, aes(x = day, y = val, group = ID, colour = outcome)) +
  geom_point(aes(color = as.factor(outcome)), alpha = 1/15) + 
  scale_color_manual(values = c("black", "red")) + theme_bw() +
  theme(legend.position = "", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x = "day", y = var, colour = "outcome ") +
  xlim(0, 15) + 
  ggtitle(var) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(18,50) +
  geom_smooth(aes(group = outcome))