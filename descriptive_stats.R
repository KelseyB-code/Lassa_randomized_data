# load data
rm(list=ls())
filename = "C:/Users/Matt/OneDrive/Desktop/Kelsey/Data/2018-cleaned.csv"
lassa <- read.csv(filename)

#names(lassa)
#dim(lassa) 

cat.vars.yes.no <- c("scns",
                     "blding",
                     "sthr",
                     "coug",
                     "vomit",
                     "diarr",
                     "head", 
                     "abdp",
                     "chesp",
                     "weak",
                     "jaun",
                     "reyes",
                     "swell",
                     "ribav",
                     "dial",
                     "prot",
                     "hema")

cat.vars.other <- c("sex", 
                    "out")

cat.vars.all <- c(cat.vars.yes.no, cat.vars.other)

lassa$sex <- ifelse(lassa$sex == 0, "Female", "Male")
lassa$out <- ifelse(lassa$out == 0, "Survived", "Died")

for (i in 1: length(cat.vars.yes.no)) {
  lassa[, cat.vars.yes.no[i]] <- ifelse(lassa[, cat.vars.yes.no[i]] == 1, "Yes", "No")
}

for (i in 1: length(cat.vars.all)){
  tb <- table(lassa[, cat.vars.all[i]])
  lassa[, cat.vars.all[i]] <- factor(lassa[, cat.vars.all[i]],
                                 levels = rownames(tb))
}

############################################################################################

ll <- function(x) t.test(x)$conf.int[1]
ul <- function(x) t.test(x)$conf.int[2]

odds <- function(x) glm(out ~ lassa[,x], data = lassa, family = binomial)$coefficients[2]

t1 <-
  lassa %>%
  select(age_scaled, out) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({ll}-{ul})", missing = "no") %>%
  modify_header(stat_0 ~ "**Mean (95% CI)**")

t2 <-
  lassa %>%
  select(age_scaled, out) %>%
  tbl_summary(statistic = all_continuous() ~ "{odds}", missing = "no") %>%
  modify_header(stat_0 ~ "OR")
t2

tbl_merge(list(t1, t2)) %>%
  modify_footnote(everything() ~ NA_character_) %>%
  modify_spanning_header(everything() ~ NA_character_)

mean(lassa$age_scaled)

t1 <- lassa %>%
  select(age_scaled, out) %>%
  tbl_summary(by = out, 
              statistic = list(all_categorical() ~ c("{n} ({p}%)"),
                               all_continuous()  ~ c("{mean} ({ll}-{ul})")),
              missing = "no") %>%
  modify_header(label ~ "**Variables**") %>% 
  add_overall() %>%
  bold_labels()  %>%
  as_flex_table()

################
# OR
t1
lassa$age_scaled <- lassa$age/10
lassa$out <- ordered(lassa$out, levels = c("Survived", "Died"))

t2 <- lassa %>%
  select(age_scaled, out) %>%
  tbl_uvregression(
    method = glm,
    y = out, 
    method.args = list(family = binomial, na.action=na.omit),
    exponentiate = TRUE) 

merge_at()