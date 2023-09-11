
## LAB 6

library(plm)
library(plyr)
library(psych)
library(VGAM)

d=read.csv(file.choose())

pd <- arrange(d,idnum,panelwave)

vars <- c("idnum","panelwave","fechld", "wrkstat", "sex")
pd.sub <- pd[, vars]

head(pd.sub)

pd.sub$female = ifelse(pd.sub$sex==2, 1, 0) ## if participant is female

pd.sub$working = ifelse(pd.sub$wrkstat==1, 1, 0) ## if participant is working

pd.sub$r.fechld = 5-pd.sub$fechld ## reverse code for how close a working mother can be to her child

pd.sub$year= ifelse(pd.sub$panelwave==3, 1, 0) ## lump years 2 and 1 together

## 1. Run a naive ("pooled") OLS regression on the panel data.

lm1 <- lm(r.fechld ~ female + working,  data = pd.sub)
summary(lm1)


## clusters

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

clusterSE(fit = lm1, cluster.var = "idnum", data=pd.sub)

## 2. Run a first differences regression on the same model in Question 1.

plm1 <- plm(r.fechld ~ female + working + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

summary(plm1)

clusterSE(fit = plm1, cluster.var = "idnum", data=pd.sub)
