
## Data Analysis Final Project

library(plyr)
library(psych)
library(doBy)
library(gmodels)
library(plm)
library(visreg)
library(stargazer)
library(QMSS)
library(ggplot2)
library(pscl)
library(pastecs)

d <- read.csv("GSS.2006.csv.xls")
vars <- c("closeblk", "acqblack", "degree", "educ", "wordsum", "rincome", "age", "sex", "race", "relpersn", "polviews", "intrace1", "librac", "affrmact", "workwhts", "workblks")
dsub <- d[, vars]

summary(dsub$educ)
summary(dsub$affrmact)
summary(dsub$white)
summary(dsub$intwhite)
summary(dsub$polviews)
summary(dsub$male)
summary(dsub$rincome)
summary(dsub$religion)
summary(dsub$age)

describe(dsub$educ)
describe(dsub$affrmact)
describe(dsub$white)
describe(dsub$intwhite)
describe(dsub$polviews)
describe(dsub$male)
describe(dsub$rincome)
describe(dsub$religion)
describe(dsub$age)

## RECODING OF VARIABLES ##

## level of education (educ)
## 0 = no formal schooling
## 1 = first grade
## ...
## 20 = 8 years of college
##_______________________________________________________________________________________

## race of respondent (race)
## 1 if white
## 2 if black
## 3 if other

## (white)
dsub$white <- ifelse(dsub$race == 1, 1, 0)

## (black)
dsub$black <- ifelse(dsub$race == 2, 1, 0)
##_______________________________________________________________________________________

## gender of respondent
dsub$male <- ifelse(dsub$sex == 1, 1, 0)
## 1 if male    (2003)
## 0 if female  (2507)

## income (rincome)
## 1 = under $1000
## 2 = $1,000 to $2,999, etc.

## religiosity (religion)
dsub$religion <- 5 - dsub$relpersn
## 1 = not religious at all
## 2 = slightly religious
## 3 = moderately religious
## 4 = very religious

#political views (polviews)
## 1 = extremely liberal
## ...
## 7 = extremely conservative
##_______________________________________________________________________________________

## race of interviewer (intrace1)
## 1 if white
## 2 if black

## (intwhite)
dsub$intwhite <- ifelse(dsub$intrace1 == 1, 1, 0)

## (intblack)
dsub$intblack <- ifelse(dsub$intrace == 2, 1, 0)
##_______________________________________________________________________________________

## affirmative action question (affrmact)
## strongly opposes (racist)      = 4
## ...
## strongly favors  (not racist)  = 1

##binary of affrmact2
dsub$affrmact2[dsub$affrmact==1 ] <- 0
dsub$affrmact2[dsub$affrmact==2 ] <- 0
dsub$affrmact2[dsub$affrmact==3 ] <- 1
dsub$affrmact2[dsub$affrmact==4 ] <- 1
##_______________________________________________________________________________________

## FIRST ANALYSIS ##

dsub$white_inblack <- ifelse(dsub$race==1 & dsub$intrace1==2, 1, 0)
table(dsub$white_inblack)

dsub$white_inwhite <- ifelse(dsub$race==1 & dsub$intrace1==1, 1, 0)
table(dsub$white_inwhite)
dsub$white_innotwhite <- ifelse(dsub$race==1 & dsub$intrace1!=1, 1, 0)
table(dsub$white_innotwhite)
dsub$notwhite_inwhite <- ifelse(dsub$race!=1 & dsub$intrace1==1, 1, 0)
table(dsub$notwhite_inwhite)
dsub$notwhite_innotwhite <- ifelse(dsub$race!=1 & dsub$intrace1!=1, 1, 0)
table(dsub$notwhite_innotwhite)


## COLLEGE AND WHITENESS
dsub$white_college <- ifelse(dsub$race==1 & dsub$educ>12, 1, 0)
table(dsub$white_college)
dsub$white_nocollege <- ifelse(dsub$race==1 & dsub$educ<=12, 1, 0)
table(dsub$white_nocollege)
dsub$notwhite_nocollege <- ifelse(dsub$race!=1 & dsub$educ<=12, 1, 0)
table(dsub$notwhite_nocollege)
dsub$notwhite_college <- ifelse(dsub$race!=1 & dsub$educ>12, 1, 0)
table(dsub$notwhite_college)

lm66 <- lm(affrmact ~ white_college + white_nocollege + notwhite_college + polviews + sex2 + rincome + religion + age, data=dsub)
summary(lm66)

## basegroup is white and white

lm <- lm(affrmact ~ white_inwhite + whiteinnotwhite + notwhite_inwhite + educ + polviews + sex2 + rincome + religion + age, data=dsub)
summary(lm)

##_______________________________________________________________________________________
## SIMPLE AGGREEMENT WITH AFFIRMATIVE ACTION

lm1 <- lm(affrmact ~ white + intwhite + educ + polviews + male + rincome + religion + age, data=dsub)
summary(lm1)

lm1.1 <- lm(affrmact ~ white + closeblk + educ + polviews + sex2 + rincome + religion + age, data=dsub, subset = intrace1==1)
summary(lm1.1)

anova(lm1, lm1.1)

## SO FAR THIS IS THE BEST ONE
##______________________________

lm2 <- lm(affrmact ~ black + intblack + educ + polviews + sex2 + rincome + religion + age, data=dsub)
summary(lm2)

## OKAY THIS IS BY FAR THE BEST ONE

##_________SIMPLE REGRESSION WITH WHITE INTERVIEWER_________
lm3 <- lm(affrmact ~ white + educ + polviews + male + rincome + religion + age, data=dsub, subset = intrace1==1)
summary(lm3)
##_________SIMPLE REGRESSION WITH NON-WHITE INTERVIEWER_________
lm4 <- lm(affrmact ~ white + educ + polviews + male + rincome + religion + age, data=dsub, subset = intwhite==0)
summary(lm4)
##_________SIMPLE REGRESSION WITH WHITE RESPONDENTS ONLY_________
lm5 <- lm(affrmact ~ intwhite + educ + polviews + male + rincome + religion + age, data=dsub, subset = (race==1))
summary(lm5)

##_________SIMPLE REGRESSION WITH Non WHITE RESPONDENTS ONLY_________
lm5 <- lm(affrmact ~ intwhite + educ + polviews + male + rincome + religion + age, data=dsub, subset = (race!=1))
summary(lm5)
####
##______________________________

## LOGIT VERSION
##_______________________________________________________________________________________
## prob model
lm9 = lm(affrmact2 ~ intblack + educ + polviews + sex2 + rincome + religion + age, data=dsub)
summary(lm9)

lm9 = lm(affrmact2 ~ white_inwhite + whiteinnotwhite + notwhite_inwhite + educ + polviews + sex2 + rincome + religion + age, data=dsub, subset = (race==1), family=binomial)
summary(lm9)

logit11 = glm(affrmact2 ~ white_inwhite + whiteinnotwhite + notwhite_inwhite + educ + polviews + sex2 + rincome + religion + age, data=dsub, subset = (race==1), family=binomial)
summary(logit11)
exp(coef(logit11))

## logit model
logit1 = glm(affrmact2 ~ intblack + educ + polviews + sex2 + rincome + religion + age, data=dsub, subset = (race==1), family=binomial)
summary(logit1)
exp(coef(logit1))

## Probability model first
lm10 = lm(affrmact2 ~ white + intblack + educ + polviews + sex2 + rincome + religion +age, data=dsub, subset = intrace1==1)
summary(lm10)

## Then do logit model

logit2 = glm(affrmact2 ~ white + intwhite + educ + polviews + male + rincome + religion + age, dsub, family=binomial)
summary(logit2)
exp(coef(logit2))

pR2(logit2)

logit3 = glm(affrmact2 ~ intwhite + educ + polviews + male + rincome + religion + age, dsub, subset = race==1, family=binomial)
summary(logit3)
exp(coef(logit3))



logit3 = glm(affrmact2 ~ white + educ + polviews + male + rincome + religion + age, data=dsub, subset = intwhite==0, family=binomial)
summary(logit3)
exp(coef(logit3))

predict(logit2, type = "response", newdata = data.frame(white = c(1,1), intwhite = c(1,1), educ = c(12, 20), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))
predict(logit2, type = "response", newdata = data.frame(white = c(1,0), intwhite = c(1,1), educ = c(12, 12), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))
predict(logit2, type = "response", newdata = data.frame(white = c(1,0), intwhite = c(1,1), educ = c(20, 20), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))

predict(logit2, type = "response", newdata = data.frame(white = c(1,1), intwhite = c(1,0), educ = c(12, 12), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))

predict(logit3, type = "response", newdata = data.frame(intwhite = c(1,1), educ = c(0, 20), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))
predict(logit3, type = "response", newdata = data.frame(intwhite = c(1,0), educ = c(0, 20), polviews = c(4,4), male = c(1,1), rincome = c(10,10), religion = c(3,3), age = c(50,50)))

## Longitudinal Analysis
##_______________________________________________________________________________________

l <- read.csv("panel-for-R.csv")
vars2 <- c("idnum", "panelwave", "closeblk", "degree", "educ", "wordsum", "rincome", "sex", "race", "relpersn", "polviews", "intrace1", "librac", "affrmact", "workwhts", "workblks", "age")
lsub <- l[, vars2]
lsub <- na.omit(lsub)

lsub$degree2[lsub$degree==0 ] <- 0
lsub$degree2[lsub$degree==1 ] <- 1
lsub$degree2[lsub$degree==2 ] <- 2
lsub$degree2[lsub$degree==3 ] <- 2
lsub$degree2[lsub$degree==4 ] <- 2

lsub$white <- ifelse(lsub$race == 1, 1, 0)
lsub$black <- ifelse(lsub$race == 2, 1, 0)
lsub$male <- ifelse(lsub$sex == 1, 1, 0)
lsub$religion <- 5 - lsub$relpersn
lsub$inwhite <- ifelse(lsub$intrace1 == 1, 1, 0)
lsub$intblack <- ifelse(lsub$intrace == 2, 1, 0)
lsub$book <- ifelse(lsub$librac == 2, 1, 0)

lsub$year = ifelse(lsub$panelwave==3, 1, 0)

## Run analysis

clusterSE <- function(fit, cluster.var, data){ 
  require(plm); require(lmtest)
  
  if (missing(data) & cluster.var %in% colnames(index(fit))){
    cvar <- index(fit, cluster.var)
    n <- length(unique(cvar))
    N <- length(cvar)
  }
  else{
    row.ids <- as.numeric(rownames(model.frame(fit)))
    n <- length(unique(data[row.ids, cluster.var]))
    N <- length(row.ids) 
  }
  
  df <- (n/(n - 1)) * (N - 1)/fit$df.residual
  vcov <- df*vcovHC(fit, type = "HC0", cluster = "group")
  coeftest(fit, vcov = vcov)
}

## Running a pooled vs paneled analysis <- including the race of interviewer

lsub$sexdiff <- firstD(male, idnum, lsub)
lsub$whitediff <- firstD(white, idnum, lsub)
lsub$blackdiff <- firstD(black, idnum, lsub)
lsub$educdiff <- firstD(educ, idnum, lsub)
lsub$wordsumdiff <- firstD(wordsum, idnum, lsub)
lsub$agediff <- firstD(age, idnum, lsub)

summary(lsub$educdiff)
summary(lsub$wordsumdiff)
summary(lsub$agediff)

constants <- with(lsub, sexdiff==0 & whitediff==0 & blackdiff==0 & educdiff>=0 & agediff>0)
constants2 <- with(lsub, sexdiff==0 & whitediff==0 & blackdiff==0 & educdiff>=0 & agediff>0 & race==1)


## ___________POOLED___________CONSTANTS AND WHITE PPL ONLY___________

lma.pooled <- lm(affrmact ~ intblack + educ + polviews + sex2 + rincome + religion + polviews + age + as.factor(panelwave),  data = lsub, subset = constants2)
summary(lma.pooled)
clusterSE(fit = lma.pooled, cluster.var = "idnum", data=lsub)

## ___________PANEL___________CONSTANTS AND WHITE PPL ONLY___________

plm1 <- plm(affrmact ~ white + inwhite + educ + polviews + male + rincome + religion + polviews + year + age, index = c("idnum", "panelwave"), model = "fd", data = lsub, subset = constants2)
summary(plm1)
clusterSE(fit = plm1, cluster.var = "idnum", data=lsub)

## ___________POOLED___________CONSTANTS ONLY___________

lma.pooled1 <- lm(affrmact ~ white + intblack + educ + polviews + sex2 + rincome + religion + polviews + age + as.factor(panelwave),  data = lsub, subset = constants)
summary(lma.pooled1)
clusterSE(fit = lma.pooled1, cluster.var = "idnum", data=lsub)

## ___________PANEL___________NO CONSTANTS___________

plm2 <- plm(affrmact ~ white + intblack + educ + polviews + sex2 + rincome + religion + polviews + age + year, index = c("idnum", "panelwave"), model = "fd", data = lsub)
summary(plm2)
clusterSE(fit = plm2, cluster.var = "idnum", data=lsub)

## ___________PANEL___________ONLY CONSTANTS___________

plm3 <- plm(affrmact ~ white + inwhite + educ + polviews + male + rincome + religion + polviews + age + year, index = c("idnum", "panelwave"), model = "fd", data = lsub, subset = constants)
summary(plm3)
clusterSE(fit = plm3, cluster.var = "idnum", data=lsub)


## CRAZY STUFF
lsub$whiteinblack <- ifelse(lsub$race==1 & lsub$intrace1==2, 1, 0)
table(lsub$whiteinblack)
lsub$whiteinwhite <- ifelse(lsub$race==1 & lsub$intrace1==1, 1, 0)
table(lsub$whiteinwhite)
lsub$whiteinnotwhite <- ifelse(lsub$race==1 & lsub$intrace1!=1, 1, 0)
table(lsub$whiteinnotwhite)
lsub$notwhiteinwhite <- ifelse(lsub$race!=1 & lsub$intrace1==1, 1, 0)
table(lsub$notwhiteinwhite)
lsub$notwhiteinnotwhite <- ifelse(lsub$race!=1 & lsub$intrace1!=1, 1, 0)
table(lsub$notwhiteinnotwhite)

## ___________POOLED___________CONSTANTS AND WHITE PPL ONLY___________

lma.pooled2 <- lm(affrmact ~ notwhiteinwhite + notwhiteinnotwhite + whiteinnotwhite + educ + polviews + sex2 + rincome + religion + polviews + age + as.factor(panelwave),  data = lsub, subset = constants)
summary(lma.pooled2)
clusterSE(fit = lma.pooled2, cluster.var = "idnum", data=lsub)

## ___________PANEL___________CONSTANTS AND WHITE PPL ONLY___________

plm1.2 <- plm(affrmact ~ notwhiteinwhite + notwhiteinnotwhite + whiteinnotwhite + educ + polviews + sex2 + rincome + religion + polviews + year + age, index = c("idnum", "panelwave"), model = "fd", data = lsub, subset = constants)
summary(plm1.2)
clusterSE(fit = plm1.2, cluster.var = "idnum", data=lsub)


