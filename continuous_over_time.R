library(ggplot2)
library(dplyr)

var = "AST"
search_term = "creatinine" #variable name in blood_chemistry_list
filename = "commcare-cleaned-data-2018-2020.csv"
lassa <- read.csv(filename)

lassa <- lassa %>%
  filter(lassa$clinical_outcome.outcome == "recovered" | lassa$clinical_outcome.outcome == "died")

lassa$ID <- seq.int(nrow(lassa))
age <- as.numeric(lassa$basic_information.age)

outcome <- lassa$clinical_outcome.outcome
outcome <- ifelse(outcome == "recovered", 0, 1)

val <- lassa %>%
  select(starts_with("blood_chemistry")) %>%
  select(contains(search_term))

days <- lassa %>%
  select(contains("blood_chemistry_list.days_since_admission")) 

df_list <- lapply(1:7, function(i) 
  data.frame(ID = lassa$ID, outcome = outcome, age = age, day = days[,i], val = val[,i]))

long_data <- bind_rows(df_list)

long_data <- long_data %>%
  mutate(outcome=as.factor(outcome), 
         ID = as.factor(ID),
         day = as.numeric(gsub(" days", "",  long_data$day)))

long_data <- arrange(long_data, ID)

long_data <- distinct(long_data) 
long_data <- long_data[complete.cases(long_data), ] 
str(long_data)

#write.csv(long_data, paste("Data/", var, "_over_time.csv", sep=""), row.names = F)

####################################################################################
# Graph BUN to cre ratio

# cre <- read.csv("Cre_over_time.csv")
# bun <- read.csv("BUN_over_time.csv")
# 
# cre_bun <- merge(cre, bun,by=c("ID","day", "age", "outcome"))
# 
# colnames(cre_bun)[5] <- "cre"
# colnames(cre_bun)[6] <- "bun"
# 
# cre_bun$val <- cre_bun$bun/cre_bun$cre
# head(cre_bun)
# long_data <- cre_bun
#write.csv(cre_bun, paste("Data/BUN_CRE_ratio_over_time.csv", sep=""), row.names = F)

#pdf(file = paste("Data/lassa_fever_analysis/", var, "_over_time.pdf", sep=""),   # The directory you want to save the file in
#width = 7.4, # The width of the plot in inches
# height = 5.85) # The height of the plot in inches

ggplot(data = long_data, aes(x = day, y = val, group = ID, colour = outcome)) +
  #geom_line(aes(color = as.factor(outcome), linetype = as.factor(outcome)), size = 0.7) +
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  scale_color_manual(values = c("black", "red")) + theme_bw() +
  theme(legend.position = "", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x = "day", y = var, colour = "outcome ") +
  xlim(0, 25) + 
  ggtitle(var) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,100) + 
  geom_smooth(aes(group = outcome))

#dev.off()

##### count values by day for patients who died
# out1 <- subset(long_data, outcome == 1)
# out1 %>%
#   group_by(day) %>%
#   count()
