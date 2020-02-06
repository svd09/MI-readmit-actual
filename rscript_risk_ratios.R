# Association between home health care use after myocardial infarction and 30-day readmissions.
# Analysis of the NRD 2012 - 2014 database.

# this rscript is going to look at risk ratio for various comorbidities that are associated 
# with increased 30-day readmissions.

# we will present the data as unadjusted risk differences for 30-day readmission

library(tidyverse);library(survey);library(tidylog);
library(MASS);library(sandwich);library(magrittr);
library(mosaic)

setwd("D:\\mi_readmit\\")

# get the data 


df = read_csv("dftouse.csv") # get the data to present risk difference for readmission

# going to do svytable and then get 2 x 2 table for each co-variate
# for those who have more than 2 groups, will need to create dummy variables 
# and keep 1 as indicator 

glimpse(df) # contains all the co-variates included in the model and important variables for analysis


# create a survey object

svy = survey::svydesign(data = df, ids = ~hosp_nrd,
                        strata = ~nrd_stratum, nest = T, weights = ~discwt)

# some hosp_nrd have only 1 subject, thus

options(survey.lonely.psu = "adjust")

# obtain results using risk ratio

# aweekend

weekend = svyglm(readmit ~ aweekend, design = svy,
                 family = "binomial"(link = "log"))

require(sandwich)

robSE <- sqrt(diag(vcovHC(weekend, type="HC")))[2]

rob <- coef(weekend)[2]

rob_weekend <- exp(c(rob, rob+qnorm(0.025)*robSE, rob-qnorm(0.025)*robSE))

names(rob) <- c("", "2.5 %", "97.5 %")

rob_weekend

# aweekend  aweekend  aweekend 
# 0.8053354 0.8045124 0.8061593 

# female

fem = svyglm(readmit ~ female, design = svy, 
             family = "binomial"(link = "log"))

require(sandwich)

robSE <- sqrt(diag(vcovHC(fem, type="HC")))[2]

rob <- coef(fem)[2]

rob_fem <- exp(c(rob, rob+qnorm(0.025)*robSE, rob-qnorm(0.025)*robSE))

names(rob) <- c("", "2.5 %", "97.5 %")

rob_fem

# female   female   female 
# 1.470837 1.469282 1.472394

# chf 

chf = svyglm(readmit ~ cm_chf, design = svy, family = "binomial"(link = "log"))

require(sandwich)

robSE <- sqrt(diag(vcovHC(chf, type="HC")))[2]

rob <- coef(chf)[2]

rob_chf <- exp(c(rob, rob+qnorm(0.025)*robSE, rob-qnorm(0.025)*robSE))

names(rob_chf) <- c("", "2.5 %", "97.5 %")

rob_chf

#     rr          2.5 %   97.5 % 
#   5.893052 5.882040 5.904084

# cm_chrnlung

lung = svyglm(readmit ~ cm_chrnlung, 
              design = svy, family = "binomial"(link = "log"))

require(sandwich)

robSE <- sqrt(diag(vcovHC(lung, type="HC")))[2]

rob <- coef(lung)[2]

rob_lung <- exp(c(rob, rob+qnorm(0.025)*robSE, rob-qnorm(0.025)*robSE))

names(rob_lung) <- c("rr", "2.5 %", "97.5 %")

rob_lung

