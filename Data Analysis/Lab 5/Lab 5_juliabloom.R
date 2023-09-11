##Lab 5

## setwd("C:/Users/pantalaimon/Desktop/Data Analysis/Lab 5")
## d = read.csv(file.choose())

## Question 1

sub <- d[, c("vote04", "age", "poleff3", "news")]
sub <- na.omit(sub)
sub$readnews = 6 - sub$news
sub$politic = 6 - sub$poleff3
sub$vote = ifelse(sub$vote04==1, 1, 0)
lm1 = lm(vote ~ age + politic + readnews, sub)
summary(lm1)

## Question 2

logit1 = glm(vote ~ age + politic + readnews, sub, family=binomial)
summary(logit1)

## Question 3

exp(coef(logit1))

## Question 4

predict(logit1, type = "response", newdata = data.frame( age = c(18,89), politic = c(1, 1), readnews = c(5,5)))

predict(logit1,  type = "response", newdata = data.frame( age = c(18,89), politic = c(5, 5), readnews = c(5,5)))

predict(logit1,  type = "response", newdata = data.frame( age = c(18,89), politic = c(3, 3), readnews = c(3,3)))
