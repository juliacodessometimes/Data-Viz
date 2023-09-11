# Lab 3 Examples

## Multiple Regression Examples


## 1. Run a simple bivariate regression, and interpret your results.  (Did the results fit your expectations?  Why?  Why not?)  

## 2. Add an additional variable that might mediate or partly "explain" the initial association from that simple regression above -- and explain your results.  Did it work out?  Yes?  No? 

## 3. Run another multiple regression.  Tell me how you expect your dependent variable to be affected by the independent variables.  Interpret your results.

## 4. Now add another independent variable to that model in Question 3, preferably a set of dummy variables.  Tell me why you added that new set of variables and what effect you expected them to have.  Did they have an effect?  Interpret that new model.  ##

## 5. Now run a partial F test comparing the model in Question 3 to the model in Question 4.  Does the F test support the idea of adding those new variables?  Why?  Why not? ##



g = read.csv(file.choose()) ## use GSS.2006.csv

## 1. Run a simple bivariate regression, and interpret your results.  (Did the results fit your expectations?  Why?  Why not?)  

g$realrinc1000s = (g$realrinc)/1000 ## turn income into 1000s of dollars for ease of interpretation

plot(as.factor(g$replaceu), g$realrinc1000s) ##  replaceu asked "How difficult or easy do you think it would be for your firm or organization to replace you if you left?" with 1 being easily and 5 being very difficultly

lm1 = lm(replaceu ~ realrinc1000s , data=g,  subset = !is.na(big5d2) )
summary(lm1)

## For every 1000 dollars more, a person believes it is 0.008 points harder to find someone to replace them, on average


## 2. Add an additional variable that might mediate or partly "explain" the initial association from that simple regression above -- and explain your results.  Did it work out?  Yes?  No? 

## Perhaps people who make more money are just more confident, so I looked at big5d2, To what extent do you agree or disagree with the following statements? I see myself as someone who... d. Gets nervous easily" and the higher the score, the more confident people are

plot(as.factor(g$big5d2), g$realrinc1000s)

lm2 = lm(replaceu ~ realrinc1000s + big5d2, data=g)
summary(lm2)

## Net of how much money someone makes, for each category more confident they feel, they are .13 points higher on thinking they cannot be easily replaced
## But you will see that this second variable hardly changes the income variable, so it is not mediating the effect really

## install.packages("stargazer")
library(stargazer)
stargazer(lm1, lm2, type = "text")


## You can use other files than just the .csv I have given you.  Look at this STATA file ...

## 3. Run another multiple regression.  Tell me how you expect your dependent variable to be affected by the independent variables.  Interpret your results.

##install.packages("readstata13")

library(readstata13)

## Go to http://gss.norc.org/get-the-data/stata and download GSS2008 in Stata format and unzip it

gss08 =    read.dta13("C:\\Users\\gme2101\\Downloads\\2008_stata\\GSS2008.dta", convert.factors = F, generate.factors = FALSE,
                      encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                      missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE, add.rownames = FALSE, nonint.factors = FALSE)


lm3 = lm(visart ~ visnhist + size, gss08)
summary(lm3)

## this regression shows that if you live in a bigger city (size), that is positively related to the number of times you visited an art museum (visart), net of how many times you visited an historical museum (visnhist)


####################################

## 3. Run another multiple regression.  Tell me how you expect your dependent variable to be affected by the independent variables.  Interpret your results.

## install.packages("plyr")
library(plyr)

d = read.csv(file.choose()) ## choose the WVS.csv from Lab 3 ##

## What variables are here?  Look here to find out: http://www.worldvaluessurvey.org/WVSOnline.jsp
## Or look here: http://www.thearda.com/Archive/Files/Codebooks/WVS2010_CB.asp

## Here is a question about "Using this card, would you please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you? It is important to this person to have a good time; to 'spoil' oneself (V73)" with higher scores meaning less like me 

d = rename(d, c("V73"="spoil"))
d$rspoil = 7-d$spoil
d$rspoil.lab <- ordered(d$rspoil, levels = c(1,2,3,4,5,6), labels = c("not at all like me", "2", "3", "4", "5", "very much like me"))
table(d$rspoil.lab)

d = rename(d, c("V242"="age"))
d$married=ifelse(d$V57==1, 1,0)
d$female = ifelse(d$V240==2, 1, 0)
d = rename(d, c("V239"="ses")) ## this asks where you place yourself in the income distribution in your country

## Here is a regression predicting if you want to spoil yourself as a function of age, sex, and marital status
## We did this for Australia and only if people also answered about ses

lm1 = lm(as.numeric(rspoil.lab) ~ age + female + married, d, subset=V2==36 & !is.na(ses))  ## This is for Australia ##
summary(lm1)

## results show that these things matter, perhaps in predictable ways


## 4. Now add another independent variable to that model in Question 3, preferably a set of dummy variables.  Tell me why you added that new set of variables and what effect you expected them to have.  Did they have an effect?  Interpret that new model.  ##

lm2 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses, d, subset=V2==36)
summary(lm2)

## We have added in ses, and 

## 5. Now run a partial F test comparing the model in Question 3 to the model in Question 4.  Does the F test support the idea of adding those new variables?  Why?  Why not? ##

anova(lm1, lm2)

## hopefully, you recognize that the partial F is just the square of the t-statistic on ses

## or ##

lm3 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.factor(ses), d, subset=V2==36)
summary(lm3)

anova(lm1, lm3)

## hopefully, you recognize that the partial F is now for each category of ses.  As a whole, do the ses categories add to the predictive power of the model?


## or ... I could break ses into high, medium and low ##

d$ses.cat = cut(d$ses, breaks = c(-1, 4, 7, 10), label=c("poor","middle","rich")) 

lm4 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses.cat, d, subset=V2==36)
summary(lm4)

anova(lm1, lm4)

## or, I could treat these categories as numeric (not recommended!) ##

lm5 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.numeric(ses.cat), d, subset=V2==36)
summary(lm5)



