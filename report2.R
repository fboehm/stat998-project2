
## ----"setup", echo=FALSE-------------------------------------------------
rm(list=ls())

library(knitr)
opts_chunk$set(cache=FALSE, echo=FALSE, results= 'hide', message = FALSE, warning=FALSE, tidy=TRUE, highlight=TRUE, fig.show='asis')


## ----"readdat"-----------------------------------------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gbm)
wls<- tbl_df(read.csv("WLS2.csv"))


## ----HAC2011responders---------------------------------------------------
wls_ha2011responders <- wls[!is.na(wls$HAC2011),]



## ----logistic_framingham-------------------------------------------------

glm_wrap<- function(formula, data= wls, family="binomial"
                    )
    {
    mylogit <- glm(formula, data = data, family = family)
    # convert logits to probabilities
    pred_prob <- expit(predict(mylogit))
    df<- data.frame(mylogit$model[,1], pred_prob)
    names(df)<- c("outcome", "predicted_probability") 
    return(df)
    }
     
     expit <- function(l)
  {
  exp(l)/(1+exp(l))
  }
pp_df<- glm_wrap(HAC2011 ~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011)


## ----pROC----------------------------------------------------------------
library(pROC)

plot.roc_wrap <- function(out_df # a dataframe outputted from glm_wrap()
                          )
    {
    plot.roc(out_df$outcome, out_df$predicted_probability, print.auc=TRUE, ci=FALSE)
    }



## ----plot-hac2011, fig.cap= "Receiver operating characteristic curve for the logistic regression model with outcome HAC2011 and the six Framingham covariates."----
plot.roc_wrap(glm_wrap(HAC2011 ~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011))


## ----plot-doc2011, fig.show='hide'---------------------------------------
plot.roc_wrap(glm_wrap(doc2011 ~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011))


## ----plot-hac2004, fig.show='hide'---------------------------------------
plot.roc_wrap(glm_wrap(HAC2004 ~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011))


## ----plot-doc2004, fig.show='hide'---------------------------------------
plot.roc_wrap(glm_wrap(doc2004 ~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011))



## ----subdivide_sex-------------------------------------------------------
wls_fem<- wls[wls$sex == "Female",]
wls_mal<- wls[wls$sex == "Male",]


## ----plot-hac2011-males, fig.show='hide'---------------------------------
plot.roc_wrap(glm_wrap(HAC2011 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_mal))
plot.roc_wrap(glm_wrap(doc2011 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_mal))
plot.roc_wrap(glm_wrap(HAC2004 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_mal))
plot.roc_wrap(glm_wrap(doc2004 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_mal))



## ----plot-hac2011-females, fig.show='hide'-------------------------------
plot.roc_wrap(glm_wrap(HAC2011 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_fem))
plot.roc_wrap(glm_wrap(doc2011 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_fem))
plot.roc_wrap(glm_wrap(HAC2004 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_fem))
plot.roc_wrap(glm_wrap(doc2004 ~ age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, data=wls_fem))



## ----subdivide-diabetes2004, fig.show='hide'-----------------------------
wls_dm04<- wls[wls$diabetes2004 == "Yes",]
wls_nodm04<- wls[wls$diabetes2004 == "No",]
plot.roc_wrap(glm_wrap(HAC2011 ~ sex + age + highchol2011 + smokever2011 + highbp2011, data=wls_nodm04))


## ------------------------------------------------------------------------
# write a function that 1. partitions wls by a binary variable and outputs two data subsets as a list.
subdivide <- function(data = wls, indicator = sex)
    {
    out <- list(wls[indicator,], wls[!indicator,])
    return(out)
    }



## ----fivefoldcv----------------------------------------------------------
df_in <- with(wls, data.frame(sex, age, highchol2011, smokever2011, diabetes2011, highbp2011))
source("glmcv.R")
glmcv(x=df_in, y=wls$HAC2011)


