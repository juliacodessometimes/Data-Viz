#--- R Lab # 6 ----


##install.packages("plm")
library(plm)


d=read.csv(file.choose()) ## choose "panel-for-R.csv" and more information on variables are here: http://sda.berkeley.edu/sdaweb/analysis/?dataset=gss06panelw3 ##

vars <- c("idnum","panelwave","affrmact","race", "intrace1")
pd.sub <- d[, vars]

pd.sub$black = ifelse(pd.sub$race==2, 1, 0) ## this is if the person is black or not

pd.sub$intblack = ifelse(pd.sub$intrace1==2, 1, 0) ## this is if the interviewer is black or not

pd.sub$r.affact = 5-pd.sub$affrmact ## this is how people feel about What about your opinion "Do you favor preference in hiring and promotion strongly or not strongly for blacks? If opposes: Do you oppose preference in hiring and promotion strongly or not strongly?" ranging from strong support to strong opposition

pd.sub$year= ifelse(pd.sub$panelwave==3, 1, 0)

## 1. Run a naive ("pooled") OLS regression on the panel data.  Tell we how you expect your Xs to affect your Y and why.  Apply clustered standard errors too.  Interpret your results.

lm1 <- lm(r.affact ~ black + intblack + as.factor(panelwave),  data = pd.sub)

summary(lm1)

## Here is teh clusterSE from the QMSS package ##

#' Compute clustered standard errors. 
#'
#' @param fit A model fit with \code{\link[plm]{plm}} (\pkg{plm}).
#' @param cluster.var A character string naming the grouping/cluster variable.
#' @param data A data frame containing \code{cluster.var} Only needed if
#' \code{cluster.var} is not included in \code{index}. See 'Examples' below. 
#' @return Output from \code{\link[lmtest]{coeftest}} (\pkg{lmtest}) but with clustered standard errors. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @note \code{clusterSE} does not work with models fit with \code{lm}, however a similar model
#' can be fit with \code{\link[plm]{plm}} using the option \code{model = "pooling"}. You can then 
#' use \code{clusterSE} to compute clustered standard errors and retest the coefficients. 
#' @seealso \code{\link[lmtest]{coeftest}}
#' @export
#' @examples
#' \dontrun{
#' # Model from plm help page:
#' data("Produc", package = "plm")
#' fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'            data = Produc, index = c("state","year"), model = "random")
#' 
#' clusterSE(fit, cluster.var = "state") # don't need data argument since "state" is included in index 
#' }
#' 

clusterSE <- function(fit, cluster.var, data){ # note: cluster.var should be entered as character string
  require(plm); require(lmtest)
  
  if (missing(data) & cluster.var %in% colnames(index(fit))){
    cvar <- index(fit, cluster.var)
    n <- length(unique(cvar))
    N <- length(cvar)
  }
  else{
    row.ids <- as.numeric(rownames(model.frame(fit)))
    # 1. get number of clusters (omitting individuals with missingness on "divorce.easier" and/or "divorced")
    n <- length(unique(data[row.ids, cluster.var]))
    # 2. get number of observations (again omitting the same individuals with missingness)
    N <- length(row.ids) 
  }
  
  #3. compute degrees of freedom
  df <- (n/(n - 1)) * (N - 1)/fit$df.residual
  # compute variance-covariance matrix
  vcov <- df*vcovHC(fit, type = "HC0", cluster = "group")
  # retest coefficients  
  coeftest(fit, vcov = vcov)
}


clusterSE(fit = lm1, cluster.var = "idnum", data=pd.sub)

## 2. Run a first differences regression on the same model in Question 1.  Interpret your results.  Do you draw a different conclusion than in Question 1?  Explain.

plm1 <- plm(r.affact ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

summary(plm1)

clusterSE(fit = plm1, cluster.var = "idnum", data=pd.sub)

##---- Extra stuff you could do ----
  
pd.sub$fourvsall= ifelse(pd.sub$r.affact==4, 1, 0)

pd.sub$fourthreevsall= ifelse(pd.sub$r.affact>=3, 1, 0)

pd.sub$fourthreetwovsone= ifelse(pd.sub$r.affact>=2, 1, 0)

plm2 <- plm(fourvsall ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

plm3 <- plm(fourthreevsall ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

plm4 <- plm(fourthreetwovsone ~ black + intblack + year,  index = c("idnum", "panelwave"),  model = "fd", data = pd.sub)

summary(plm4)

## install.packages("stargazer")
library(stargazer)
stargazer(plm1, plm2, plm3, plm4, type = "text")

## -- a few more checks -----

## Here is the firstD command in the QMSS package

#' Compute first differences 
#'
#' @param var Variable to be first-differenced.
#' @param group Optional grouping variable (see 'Details').
#' @param df Optional data frame containing \code{var} and \code{group} (see 'Details'). 
#' @details If \code{df} is specified then \code{group} must also be specified. So it is possible 
#' to specify all three parameters, \code{var} and \code{group} only, or \code{var} by itself. 
#' An example of when one might wish to omit both \code{group} and \code{df} is when using \code{firstD} 
#' in conjunction with  \pkg{plyr}'s \code{\link[plyr]{ddply}} (see 'Examples'). If \code{df} is specified then it 
#' should be sorted by \code{group} and, if necessary, a second variable (e.g. time) that orders the 
#' observations of \code{var} in the appropriate sequence. 
#' @return \code{firstD(var)} returns a first-differenced version of \code{var}. 
#' \code{firstD(var,group)} returns a first-differenced version of \code{var} by \code{group}. 
#' And \code{firstD(var,group,df)} returns a first-differenced version of \code{var} by \code{group}, 
#' where \code{var} and \code{group} are searched for in \code{df}. Note that the first value of 
#' \code{firstD(var)} will be \code{NA} since there is no difference to compute. Similarly, for
#' \code{firstD(var,group)} and \code{firstD(var,group,df)} the first value for each group 
#' will be \code{NA}.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' # Specifying both group and df
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10))
#' df$Xdiff <- firstD(X, id, df)
#' df
#' 
#' # Omitting df
#' id <- rep(1:3, each = 3)
#' X <- rpois(9, 10)
#' Xdiff <- firstD(X, id)
#' 
#' # Omitting group and df 
#' \dontrun{
#' library(plyr)
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10), Y = rpois(9, 5))
#' ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
#' }

firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}

pd.sub$d.intblack = firstD(intblack, idnum, pd.sub )
table(pd.sub$d.intblack)
pd.sub$bw=ifelse(pd.sub$d.intblack==-1,1,0)
pd.sub$wb=ifelse(pd.sub$d.intblack==1,1,0)
pd.sub$d.r.affact=firstD(r.affact, idnum, pd.sub )
summary(lm(intblack ~ black, pd.sub))

summary(lm(d.r.affact ~ bw, pd.sub, subset=black==0))