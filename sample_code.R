library(data.table)
library(stringr)
library(dplyr)
library(splines)
library(mgcv)
source("DGLM_function.R")

load ("sample_data.RData")
dat$month  = as.factor(dat$month)
dat$WEEKDAY = as.factor(dat$WEEKDAY)
dat$HOLIDAY_FO = as.factor(dat$HOLIDAY_FO)
dat$WARM_ATL = as.factor(dat$WARM_ATL)

variables <- c("avg_OZ_PWA", "ns(avg_temp,6)", "ns(avg_dew,6)", "ns(date, 12*11)", 
               "WEEKDAY", "HOLIDAY_FO", "WARM_ATL")

model.mean <- reformulate (variables, response = "rdas_any")
fit = DGLM_function(mean.formula = model.mean,
                    dispersion.pred = "month", data = dat, method = "gam")
#mean model
summary(fit$ED_glm)
#dispersion model
summary(fit$d_glm)
