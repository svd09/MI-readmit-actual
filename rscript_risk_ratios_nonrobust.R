# Association between home health care use after myocardial infarction and 30-day readmissions.
# Analysis of the NRD 2012 - 2014 database.

# this rscript is going to look at risk ratio for various comorbidities that are associated 
# with increased 30-day readmissions.

# we will present the data as unadjusted risk differences for 30-day readmission

library(tidyverse);library(survey);library(tidylog);
library(MASS);library(sandwich);library(magrittr);
library(mosaic);library(jtools)

setwd("E:\\mi_readmit\\")

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

summ(weekend, confint = T, exp = T)

# aweekend  aweekend  aweekend 
# 0.8053354 0.8045124 0.8061593 

# female

fem = svyglm(readmit ~ female, design = svy, 
             family = "binomial"(link = "log"))

summ(fem, confint = T, exp = T)

# female   female   female 
# 1.470837 1.469282 1.472394

# chf 

chf = svyglm(readmit ~ cm_chf, design = svy, family = "binomial"(link = "log"))

summ(chf, confint = T, exp = T)

#     rr          2.5 %   97.5 % 
#   5.893052 5.882040 5.904084

# cm_chrnlung

lung = svyglm(readmit ~ cm_chrnlung, 
              design = svy, family = "binomial"(link = "log"))

summ(lung, confint = T, exp = T)

#       rr    2.5 %   97.5 % 
# 1.586485 1.584690 1.588282 

# diabetes

dm = svyglm(readmit ~ diabetes, 
              design = svy, family = "binomial"(link = "log"))

summ(dm, confint = T, exp = T)

#      rr    2.5 %   97.5 % 
# 1.424269 1.422839 1.425700 

# cm_renlfail

kidn = svyglm(readmit ~ cm_renlfail, 
            design = svy, family = "binomial"(link = "log"))

summ(kidn, confint = T, exp = T)

#       rr    2.5 %   97.5 % 
# 2.126626 2.123835 2.129421 

# cm_liver

livd = svyglm(readmit ~ cm_liver, 
              design = svy, family = "binomial"(link = "log"))

summ(livd, confint = T, exp = T)
#       rr    2.5 %   97.5 % 
# 1.388733 1.384249 1.393231

# cancer

can = svyglm(readmit ~ cancer, 
              design = svy, family = "binomial"(link = "log"))

summ(can, confint = T, exp = T)

#      rr    2.5 %   97.5 % 
# 1.810134 1.805837 1.814441 

insurance = svyglm(readmit ~ insu, 
          design = svy, family = "binomial"(link = "log"))

summ(insurance, confint = T, exp = T)

#        rr     2.5 %    97.5 % 
# 0.5178547 0.5169849 0.5187260 

# treatment

treat = svyglm(readmit ~ treatment, 
               design = svy, family = "binomial"(link = "log"))

summ(treat,confint = T,exp = T)

# this shows that when cabg indicator, medical > cabg > pci

# ----------------------------------------------------------
#  exp(Est.)   2.5%   97.5%   t val.
#---------------------- ----------- ------ ------- --------
#  (Intercept)                   0.08   0.07    0.08   -43.60
#treatmentmedical              5.18   4.61    5.82    27.66
#treatmentpci                  0.25   0.22    0.28   -21.06
#----------------------------------------------------------
  
#  -----------------------------
#  p
#---------------------- ------
#  (Intercept)              0.00
#treatmentmedical         0.00
#treatmentpci             0.00
#-----------------------------

# priomi

pmi = svyglm(readmit ~ priormi, design = svy,
             family = "binomial"(link = "log"))

summ(pmi, confint = T, exp = T)
  
  
# priorpci

prpci = svyglm(readmit ~ priorpci,
               design = svy, family = "binomial"(link = "log"))


summ(prpci,confint = T, exp = T)

# pstroke

pstr = svyglm(readmit ~ pstroke, design = svy,
              family = "binomial"(link = "log"))

summ(pstr, confint = T, exp = T)

# save as risk ratio with robust estimators
# however, we will plan use jtools to create estimates directly



# age

a = svyglm(readmit ~ age, svy, family = "binomial"(link = "log"))

exp(coef(a))

exp(confint(a))
  




