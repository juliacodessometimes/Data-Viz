## ---- R Lab #5 -----


## 1. Run a multiple linear probability model.  Tell me how you think your independent variables will affect your dependent variable.  Interpret your results.  Were your expectations correct?  Why or why not?

## 2. Run a multiple (binary) logistic model.  (It can be the same as the above LPM or -- even better -- a new model.)  Tell me how you think your independent variables will affect your dependent variable.  Interpret your results in the logit scale.  Were your expectations correct?  Why or why not?

## 3. Get odds ratios from your logit model in Question 2 and interpret some of them.  

## 4. Get predicted probabilities from your logit model in Question 2 for some constellations of X values and interpret the results.  


d = read.csv(file.choose()) ## choose the 2006 GSS ##

## The question is what are some things that predict "How often do you come home from work exhausted?" ranging from always to never.

sub <- d[, c("xhaustn", "hrs1", "age", "prestg80", "babies", "wrkstat")]

sub <- na.omit(sub) ## get rid of all missings; necessary for predictions later ##

sub$exh = ifelse(sub$xhaustn==1, 1, 0) ## only look at "always exhausted" vs. everything else ##

table(sub$exh)

## 1. Run a multiple linear probability model.  Tell me how you think your independent variables will affect your dependent variable.  Interpret your results.  Were your expectations correct?  Why or why not?

lm1 = lm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1)
summary(lm1)

## 2. Run a multiple (binary) logistic model.  (It can be the same as the above LPM or -- even better -- a new model.)  Tell me how you think your independent variables will affect your dependent variable.  Interpret your results in the logit scale.  Were your expectations correct?  Why or why not?

logit1 = glm(exh ~ hrs1 + age + prestg80 + babies, sub, subset= wrkstat==1, family=binomial)
summary(logit1)

## 3. Get odds ratios from your logit model in Question 2 and interpret some of them.  

exp(coef(logit1))

## 4. Get predicted probabilities from your logit model in Question 2 for some constellations of X values and interpret the results.  

predict(logit1, type = "response", newdata = data.frame(hrs1 = c(35,80), age = c(35, 35), prestg80 = c(50, 50), babies = c(0,0)))

predict(logit1, type = "response", newdata = data.frame( white = c(35,35), politic = c(1, 1), readnews = c(1,5)))

predict(logit1,  type = "response", newdata = data.frame( hrs1 = c(35,60), age = c(20, 50), prestg80 = c(80, 40), babies = c(0,2)))

## the below will get it for any combination of variables and everything else set to means ##

pred.dat <- with(sub, expand.grid( 
  hrs1 = sort(unique(hrs1)),
  age = mean(age),
  prestg80 = mean(prestg80),
  babies = sort(unique(babies))))


### This function is from QMSS package 

#' Predicted probabilities and confidence intervals from logit or probit model
#'
#' @param model A \code{\link[stats]{glm}} model fit with \code{binomial} family and 
#' either a \code{logit} or \code{probit} link function.
#' @param predData A data frame to pass to \code{\link[stats]{predict.glm}} in which to look 
#' for variables with which to predict. 
#' @param ci Logical value indicating whether to compute confidence intervals. 
#' @param level The confidence level to use if \code{ci} is \code{TRUE}. 
#' @return A data frame with \code{predData} and the associated predicted probabilities. 
#' Confidence intervals are included if argument \code{ci} is \code{TRUE}. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' GSS_2010$Y <- with(GSS_2010, 
#'                    cut(realinc, 
#'                    breaks=c(-Inf, median(realinc, na.rm = T), Inf), 
#'                    labels=c("Low", "High")))
#' logitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial)
#' probitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial(link = "probit"))
#' predData <- data.frame(age = 20, educ = 15)
#' predProb(logitmodel, predData, ci = F)
#' predProb(probitmodel, predData, ci= F)
#' predData <- expand.grid(age = c(20, 50, 80), educ = c(5, 10, 15))
#' predProb(logitmodel, predData, ci = T)
#' predProb(probitmodel, predData, ci= T)

predProb <- function(model, predData, ci = TRUE, level = 0.95){
  
  link <- model$family$link
  bad_link <- !(link %in% c("logit", "probit"))
  
  if (bad_link) {
    stop("Link function should be 'logit' or 'probit'")
  }
  
  fun <- ifelse(link == "probit", "pnorm", "plogis")
  
  if (ci == FALSE){
    preds <- predict(model, type = "response", newdata = predData)
    preds <- cbind(predData, PredictedProb = preds)
    return(preds)
  }
  else {
    temp <- predict(model, type = "link", se = TRUE, newdata = predData)
    fit <- temp$fit
    se <- temp$se.fit
    p <- (1 - level)/2
    p <- c(p, 1-p)
    PredictedProb <- do.call(fun, args = list(q = fit))
    ci1 <- do.call(fun, args = list(q = fit + qnorm(p[1])*se))
    ci2 <- do.call(fun, args = list(q = fit + qnorm(p[2])*se))
    CI <- cbind(ci1, ci2)
    colnames(CI) <- paste0(paste(100*p), "%")
    preds <- cbind(predData, PredictedProb, CI)
    return(preds)
  }
}


predProb(logit1, predData = pred.dat, ci = F)

## visualize your probabilities ##

sub$b = as.factor(sub$babies)
logit2 = glm(exh ~ hrs1 + age + prestg80 + b, sub, subset= wrkstat==1 & babies<3, family=binomial)
summary(logit2)

##install.packages("visreg")
library(visreg)
visreg(logit2, "hrs1", by = "b", 
       partial = F, overlay = T, 
       xlab = "Hours", 
       ylab = "Predicted probability", 
       scale= "response",
       type="conditional",
       alpha=.05) ## 