## Lab 1
## due Sept 27th
## based on data from here:  http://www.thearda.com/Archive/Files/Codebooks/GSS2006_CB.asp

## go line-by-line through this code; otherwise you might miss something

gss = read.csv(file.choose()) ## look for a pop up window and choose GSS.2006.csv ## this loads the data into R

names(gss) ## what variables are in here? "vpsu" and "vstrat" etc.

View(gss) ## look at it as a spreadsheet


##1-  Use the table command, summary or describe commands, to tabulate one variable; and explain your results.

## How about I investigate something with work-life conflict?

table(gss$jbintfam) ## tabulates "How often do you feel that the demands of your job interfere with your family life?" 

## if I look up JBINTFAM at www.thearda.com above, I see that 1 = Always, and 5 = Never; there are 206 5's and only 51 1's

summary(gss$jbintfam) ## calculates this variable on a 1-5 scale; not as helpful

prop.table(table(gss$jbintfam)) ## this gives us the proportions for this variable

options(digits=2) ## what if I think those proportions are ugly?  Shorten them.

prop.table(table(gss$jbintfam)) ## this gives us the proportions for this variable

## seeing this as a proportion helps a bit: ony 5% are Always feeling interfered with, while 20% are Never feeling interfered with

options(digits=7) ## what if I want to get back to my original level.  Default is 7.


install.packages("psych")  ## install this package, if you haven't already  

library(psych) ## this brings into your R session

describe(gss$wrkstat) ## this summarizes "Last week were you working full-time, part-time, going to school, keeping house, or what?" as if it were a number; this is not something you'd normally do

table(gss$wrkstat) ## Ahh, that's better: a table of categories: we see that 2322 people work full-time, for instance. 

describe(gss$hrs1) ## this works better for a numeric variable like "IF WORKING, FULL OR PART TIME: How many hours did you work last week, at all jobs?".  The mean is 42.08.


## 2. Using either the plyr or doBy commands, compare the means and standard deviations for one variable, for at least two subgroups; and explain your results

install.packages("plyr")  ## install this package, if you haven't already

library(plyr)

ddply(gss, "jbintfam", summarise, Mean = mean(hrs1, na.rm = T), SD = sd(hrs1, na.rm = T))

## ddply's results show that Always work 49.95 hours, while Nevers work 38.34 hours, with everyone else monotonically related.

install.packages("doBy")  ## install this package, this is going to do the same thing as using plyr above

library(doBy)

summaryBy(hrs1~jbintfam, data=gss, FUN=c(mean, sd), na.rm=T) ## same result as above when using plyr; just another way

boxplot(hrs1~jbintfam, data=gss) ## graphically displays the relationship in boxplot form

install.packages("ggplot2") ## this program will make a fancy boxplot instead; same results as above

library(ggplot2)

p = ggplot(gss, aes(factor(jbintfam), hrs1))

p + geom_boxplot() + geom_jitter()

p + geom_boxplot(aes(fill = factor(jbintfam))) ## see how they include the NAs, which are the missings


##3. Using the gmodels or another command, crosstabulate two categorical or ordinal variables (getting proportions); and explain your results

install.packages("gmodels")  ## install this package, if you haven't already

library(gmodels)

CrossTable(gss$jbintfam, gss$wrkstat, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS") ## this generates how frequently different types of workers feel interference between family and work

## Interesting: full-timers experience 18% always conflicted, vs. part-timers experience 31% always conflicted.  Is that a statistically significant difference?  Stay tuned ...


## no need to save your session, but do save your R Script
