## ---- R Lab Assignment #2 -----
## (due Oct. 4th)
## based on data from here:  http://www.thearda.com/Archive/Files/Codebooks/GSS2006_CB.asp

## Answer these six questions as best you can:

## 1. Recode 1 *sort of* continuous variable into categories.  Tell me what you did and explain the variable(s).

## 2. Recode 1 another variable and attach value labels.  Tell me what you did and explain the variable(s).

## 3. Use one (or both) of your recoded variables to do a cross-tabulation (as you had done last week, with prop.table, doBy, or ddply). Explain your results.

## 4. Run a linear regression with 1 independent and 1 dependent variable.  Make all of the recodes necessary to make the model as easy to interpret as possible.  And explain your results.

## 5. Plot two variables, either as a scatter plot or boxplot.  Explain your results.

## 6. Tell me two theories/ideas you might want to test in this course. Do you have a dataset for these ideas/theories already? Do you have it in R-readable format already? What is your main independent variable? What is your main dependent variable?  Send me an email with the subject "Independent Project Ideas - [your name]" to gme2101@columbia.edu

####################################### 

## You can start by setting a working directory for all your files and programs 
## If you are using a PC, you have to replace each / with two //

##setwd("D:/Documents/gme2101")

##d = read.csv("GSS2006.csv")

d = read.csv(file.choose()) ## if it doesn't work setting a directory

## 1. Recode 1 *sort of* continuous variable into categories.  Tell me what you did and explain the variable(s).

## A. The simplest way to make a dummy variable:

d$hi.attend = ifelse((d$attend>4), 1, 0) ## binary recode, where we make it 1 if "How often do you attend religious services?" is everything as frequent as 4 or more, which is About once a month or more, 0 otherwise

table(d$hi.attend, d$attend) ## check the recoding:  It is 0 for all categories less than 4 and 1 for everything greater than or equal to 4


## B. Breaking a variable into categories:

d$attend.cat = cut(d$attend, breaks = c(-1, 2, 5, 8), label=c("weak","moderate","strong"), ordered=TRUE) ## create a number of categories for religious attendance ##

table(d$attend.cat, d$attend) ## check the recoding, and see that 


## C. Coding multiple conditions:

d$bothftw = ifelse((d$wrkstat==1 & d$sex==1), 1, 0) ## an example with multiple conditions at once, both male and full-time working ##

table(d$bothftw, d$wrkstat, d$sex)


## D. Another way to apply multiple labels:  make it 1 if you've once married, 0 if you aren't once married

d$oncemarried[d$marital==1 ] <- 0
d$oncemarried[d$marital==2 ] <- 1
d$oncemarried[d$marital==3 ] <- 1
d$oncemarried[d$marital==4 ] <- 1
d$oncemarried[d$marital==5 ] <- 0

table(d$oncemarried, d$marital) ## check against the original


## E. Changing to missing values 
## The answers to "How scientific are each of the following fields? If you have not heard of a particular field, just say you haven't heard of it. F. Economics. Is economics very scientific, pretty scientific, not too scientific, or not scientific at all?" and 5="Haven't heard of it (Vol.)", so we want to make that a missing answer. 

d$econ.new = d$econsci
d$econ.new[d$econsci==5] <- NA

table(d$econsci, d$econ.new)


## 2. Recode 1 other variable and attach value labels.  Tell me what you did and explain the variable(s).

## A. Add labels to existing variables:

d$hi.attend.lab <- ordered(d$hi.attend, levels = c(0,1), labels = c("low", "high")) ## using the hi.attend variable from above ##

table(d$hi.attend.lab, d$hi.attend)

## B. Reverse code a variable and then add labels and make it ordered:

d$rhappy = 4-d$happy ## to reverse code a variable, do this ... (highest category + 1) - orginal_variable ##

d$rhappy.fact = as.factor(d$rhappy) ## make this new numeric variable into a factor ##

d$lab.rhappy <- ordered(d$rhappy, levels = c(1,2,3), labels = c("unhappy", "so-so", "happy")) ## make that factor variable into an ORDERED factor, with value labels ##

table(d$lab.rhappy, d$rhappy)


mean(d$happy, na.rm=T) ## the original variable, happy, was numeric, so we can get the mean ##

mean(as.numeric(d$lab.rhappy), na.rm=T) ## the new variable, lab.rhappy, is an ordered factor -- and we need to tell R to treat it like a number, hence, the as.numeric) ##


## 3. Use one (or both) of your recoded variables to do a cross-tabulation (like last week, with prop.table, doBy, or ddply). Explain your results.

d$hi.attend = ifelse((d$attend>4), 1, 0) ## as before ##

d$hi.thorough = ifelse((d$big5c1<2), 1, 0) ## this is a question about "To what extent do you agree or disagree with the following statements? I see myself as someone who... c. Does a thorough job" and people who say they strongly agree are coded 1, otherwise 0.

## install.packages("gmodels")

library(gmodels)

CrossTable(d$hi.attend, d$hi.thorough, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")  ## the results indicate that being more thorough is not associated with going to religious services more, which is what some psychological theories would have predicted



## 4. Run a linear regression with 1 independent and 1 dependent variable; make all of the recodes necessary to make the model as easy to interpret as possible; and explain your results.

## install.packages("psych")

library(psych)

describe(d$hrs1) ## respondent's work hours ##

describe(d$sphrs1) ## respondent's spouse's work hours ##

## How do you think one's hours should be related to one's spouse's hours?


lm1 = lm(hrs1 ~ sphrs1, data=d) ## predicting one's hours from their spouse's ##
summary(lm1) ## examine the results: a coefficient of 0.04662 indicates no obvious relationship between my hours and my spouse's hours ##

##  -- or --

## Do people who are hispanic approve more of their friends and family marrying people who are hispanic?

d$hisp = ifelse(d$hispanic!=1, 1, 0) ## recode people to hispanic or not ##

d$rmarhisp = 6-d$marhisp ## reverse code acceptance of marriage to a hispanic person ##
d$rmarhisp.lab = ordered(d$rmarhisp, levels = c(1,2,3,4,5), labels = c("strongly disapprove", "somewhat disapprove", "neutral", "somewhat approve", "strongly approve" )) ## add labels ##

lm2 = lm(as.numeric(rmarhisp.lab) ~ hisp , data=d) 
summary(lm2) 

## As you can see, hispanics score 0.6391 higher on happiness with someone in their family marrying a hispanic person


## 5. Plot two variables, either as a scatter plot or boxplot; add in trend/regression lines; and explain your results.


plot(d$sphrs1, d$hrs1, main="Scatterplot Example", 
     xlab="Spouse Hours", ylab="Hours", pch=19) ## scatter X and Y ##

abline(lm(hrs1 ~ sphrs1, data=d), col="blue") ## add in a regression line ##

## -- or ---

plot(jitter(d$sphrs1), jitter(d$hrs1), main="Scatterplot Example", 
     xlab="Spouse Hours", ylab="Hours", pch=19) ## same as above, but jitter the points ##

## -- or (for a boxplot) ---

plot(d$sex, d$hrs1, main="Scatterplot Example", 
     xlab="Sex", ylab="Hours", pch=19) ## this creates a scatter ##

plot(as.factor(d$sex), d$hrs1, main="Scatterplot Example", 
     xlab="Sex", ylab="Hours", pch=19) ## this creates a box plot ##

mean(d[d$sex == 1, 'hrs1'], na.rm=T)
describe(d[d$sex == 1, 'hrs1']) ## respondent's work hours ##

## ----- other useful graphing codes ----

hist(d$hrs1) ## draws a histogram ##

dense <- density(d$hrs1, na.rm=T) # returns the density data 
plot(dense) # plots the results as a kernel density plot

## install.packages("ggplot2")
library(ggplot2)
ggplot(d, aes(x=d$sphrs1, y=d$hrs1)) + ## Another scatter plot
  geom_point(shape=1)      +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line


## 6. Tell me two theories/ideas you might want to test in this course. Do you have a dataset for these ideas/theories already? Do you have it in R-readable format already? What is your main independent variable? What is your main dependent variable?  Send me an email with the subject "Independent Project Ideas - [your name]" to gme2101@columbia.edu
