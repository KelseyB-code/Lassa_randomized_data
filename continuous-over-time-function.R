
rm(list=ls())
library(ggplot2)
library(purrr)
library(dplyr)

## preprocess data 
################################################################################

setwd("/Users/kelsey/Lassa_randomized_data")
source('longitudinal-source-code.R')

################################################################################

biglist <- lapply(1:length(namelist), 
                  function(x) lapply(1:20, 
                                     function(i) data.frame(ID = lassa$ID, 
                                                            outcome = ifelse(lassa$clinical_outcome.outcome == "died", 1, 0), 
                                                            age = lassa$basic_information.age, 
                                                            day = days[,i], 
                                                            val = list[[x]][[i]])))

biglist <- lapply(1:length(namelist), function(x) bind_rows(biglist[[x]]))

biglist <- lapply(1:length(namelist), function(x)
  biglist[[x]] %>%
    mutate(outcome=as.factor(outcome), 
           ID = as.factor(ID),
           day = as.numeric(gsub(" days", "",  biglist[[x]]$day))))

biglist <- lapply(1:length(namelist), function(x) distinct(biglist[[x]]) %>% na.omit(biglist[[x]]))

names(biglist) <- namelist

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

ggfunc <- function(df, names) {ggplot(data = df, aes(x = day, y = val, group = ID, colour = outcome)) +
    #geom_line(aes(color = as.factor(outcome), linetype = as.factor(outcome)), size = 0.7) +
    #scale_linetype_manual(values = c("longdash", "solid")) + 
    geom_point(aes(color = as.factor(outcome)), alpha = 1/15) +
    scale_color_manual(values = c("black", "red")) + theme_bw() +
    theme(legend.position = "", panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    labs(x = "day", y = names, colour = "outcome ") +
    xlim(0, 25) + 
    ggtitle(names) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #ylim(0,100) + 
    geom_smooth(aes(group = outcome))}

gglist <- lapply(1:length(namelist), function(x) ggfunc(biglist[[x]], namelist[[x]]))

g1 <- gglist[[1]] +
  ylim(0,1000)

g2 <- gglist[[2]] +
  ylim(0,100) +
  ggtitle("BUN") +
  labs(y = "BUN")

g3 <- gglist[[3]] +
  ylim(0,6)

g4 <- gglist[[4]] +
  ylim(0,400)

g5 <- gglist[[5]] +
  ylim(0,8)

library(gridExtra)
df("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data/plots-over-time.pdf", 
    onefile = TRUE)
grid.arrange(g1, g2, g3, g4, g5, nrow = 2, ncol = 3)
dev.off()

##### count values by day for patients who died
# library(gtsummary)
# out1 <- subset(biglist[[1]], outcome == 1)
# out1 <- subset(out1, day >0)
# 
# barplot(table(out1$day), ylim=c(0,4), xlim=c(0,12), main = "Frequency of reported Ct values \n for patients who died")
