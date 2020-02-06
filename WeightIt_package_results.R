# using the WeightIt package with R and df6 to get direct ATT weights
# also using cobalt package to balance diagnostics

library(cobalt);library(WeightIt)

W <- weightit(hhc ~ hosp_nrd + age + aweekend + female
      + cm_chf + cm_chrnlung +  diabetes + 
    cm_htn_c + cm_hypothy + cm_liver + cm_obese + 
    cm_perivasc + cm_renlfail + cancer + hosp_loc + insu + smoke + year +
    dys + priormi + treatment + priorpci + car +
    pstroke, data = df6, 
    method = "ps", estimand = "ATT")


print(W)

t = bal.tab(W, un = T)

# get the ATT weights from W and attach
# them to the original dataframe
# create summary of weights

df6$wts = W$weights
df6$ps = W$ps

df6 %>% filter(hhc == 0) %$%
  summary(wts)

table(df6$hhc)

#- total patients after ATT weighting

total = 36979 + 37785

total

#- % of readmission with weights 

svywts = svydesign(ids = ~hosp_nrd, weights = ~discwt + wts, 
                   strata = ~nrd_stratum, nest = T, data = df6)

options(survey.lonely.psu = "adjust")

svymean(~readmit, svywts)

svytable(~readmit + hhc, svywts)

#- svytable results for weighted analysis

hhc.pos = 7814/(7814+29614)

hhc.no = 9101/(28578 + 9101)

hhc.pos
hhc.no


