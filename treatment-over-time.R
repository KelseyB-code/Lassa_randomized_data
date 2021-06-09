
library(purrr)
library(dplyr)
library(gridExtra)

setwd("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data")
filename = "commcare-cleaned-data-2018-2021.csv"
lassa <- read.csv(filename)
lassa$ID <- seq.int(nrow(lassa))
lassa <- lassa %>%
  filter(lassa$special_interventions.dialysis %in% c("no", "yes"))

ggfunc <- function(df, names) {ggplot(data = df, aes(x = day, y = val, group = ID, colour = outcome)) +
    #geom_line(aes(color = as.factor(outcome), linetype = as.factor(outcome)), size = 0.7) +
    #scale_linetype_manual(values = c("longdash", "solid")) + 
    geom_point(aes(color = as.factor(outcome)), alpha = 1/15) +
    scale_color_manual(values = c("black", "red")) + theme_bw() +
    theme(legend.position = "", panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    labs(x = "day", y = names, colour = "outcome ") +
    ggtitle(names) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #ylim(0,100) + 
    geom_smooth(aes(group = outcome))}

 ###############################################################################
##### Vitals over time
group_var <- "lassa$special_interventions.dialysis"

func <- function() {namelist <- c("temperature",
              "systolic_blood_pressure",
              "diastolic_blood_pressure",
              "pulse_rate",
              "respiratory_rate")
bc <- lassa %>%
  dplyr::select(starts_with("clinical_factors.vital_signs.max_")) %>%
  dplyr::select(contains(namelist))
}

list <- lapply(1:length(namelist), function(i) bc[,grepl(namelist[i], names(bc))])

days <- data.frame(lapply(1:10, function(x) rep(x, nrow(lassa))))

biglist <- lapply(1:length(namelist), 
                  function(x) lapply(1:10, 
                                     function(i) data.frame(ID = lassa$ID, 
                                                            outcome = ifelse(group_var == "yes", 1, 0), 
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
}

gglist <- lapply(1:length(namelist), function(x) ggfunc(biglist[[x]], namelist[[x]]))
g1 <- gglist[[1]] +
  xlim(1,10) +
  ylim(35,40)

g2 <- gglist[[2]] +
  xlim(1,10) +
  ylim(100,200)

g3 <- gglist[[3]] +
  xlim(1,10) +
  ylim(50,120)

g4 <- gglist[[4]] +
  xlim(1,10) +
  ylim(50,150)

g5 <- gglist[[5]] +
  xlim(1,10) +
  ylim(15,35)

pdf("/Users/kelsey/Dropbox (UMass Medical School)/Kelsey/Lassa Fever/Data/vitals-over-time_dialysis.pdf", 
    onefile = TRUE)
grid.arrange(g1, g2, g3, g4, g5, nrow = 2, ncol = 3)
dev.off()

