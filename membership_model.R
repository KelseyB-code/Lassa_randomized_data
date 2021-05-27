library(dplyr)
dev_filename = "Data/data-2011-15.csv"
val_filename = "Data/2018-cleaned.csv"
dev <- read.csv(dev_filename)
val <- read.csv(val_filename)

dev <- dev %>% select(AGE, BLDING, SCNS, CRE, AST, K)


dev2 <- data.frame(dev2)
sum(complete.cases(dev2))
dev2[dev2 == "\\N"] <- NA
dev2


val <- val %>% select(age, blding, scns, cre, ast, k)
sum(complete.cases(val))
val[val == "NA"] <- NA
val

class(val$cre)
class(dev2$CRE)
table(dev2$CRE)
as.numeric(dev2$CRE)
dev2$CRE
