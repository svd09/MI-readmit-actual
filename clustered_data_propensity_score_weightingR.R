# need libraries

library(tidyverse);library(tidylog);library(haven);library(survey);
library(srvyr);library(skimr);library(Matching);library(MatchIt);
library(cobalt);library(twang);library(MASS);library(car);library(lme4);library(nlme);
library(psych);library(magrittr)

# get data 

setwd("E:/mi_readmit")


df4 = read_csv("dftouse.csv")

df4$hhc[df4$discharge == "hhc"]<- 1
df4$hhc[df4$discharge == "home"]<- 0


df4 = data.frame(df4)

df4$year = as.factor(df4$year)

count <- df4 %>% count(hosp_nrd)

df5 = left_join(df4, count, by = "hosp_nrd")

df6 = df5 %>% filter(n > 1)





# use hosp_nrd as a fixed effect to obtain propensity score
# even initially using the hosp_nrd as a random effect, we do not get significant difference
# on the hosp_nrd variable 
# and then using fixed effects model, the hosp_nrd p = 0.05 and unadjust SD = -2.6%

psmodel = glm(hhc ~ hosp_nrd + age + aweekend + female
  + cm_chf + cm_chrnlung +  diabetes + 
  cm_htn_c + cm_hypothy + cm_liver + cm_obese + 
    cm_perivasc + cm_renlfail + cancer + hosp_loc + insu + smoke + year +
      dys + priormi + treatment + priorpci + car +
      pstroke, data = df6, family = "binomial"(link = "logit"))
              


df6$ps=fitted(psmodel) # get the propensity scores



# create mirror image histogram for the propensity scores

d = df6 %>% dplyr::select(ps, discharge)

dhome = d %>% dplyr::filter(discharge == 'home')

x1 = dhome$ps

dhhc = d %>% filter(discharge == "hhc")

x2 = dhhc$ps


#Make the plot
par(mar=c(0,5,3,3))
hist(x1 , main="" , xlim=c(0,1), ylab="Home Discharge", xlab="", ylim=c(0,70000) , xaxt="n", las=1 , col="tomato3", breaks=100)
par(mar=c(5,5,0,3))
hist(x2 , main="" , xlim=c(0,1), ylab="Home Health Care Discharge", xlab="Propensity Score derived from the logistic regression model", ylim=c(4000,0) , las=1 , col="lightblue"  , breaks=100)

#--------------------------------------------------------------------
#--------------------------------------------------------------------

# now to calculate the weights
# according to the textbook should use normalised weights with clustered data



df6$wts1 = ifelse(df6$hhc ==1, 1, df6$ps/(1-df6$ps)) # get ATT weights

df6 %>% group_by(discharge) %>% summarise(mean(wts1), sd(wts1), q25(wts1), q75(wts1))

# weights for ATT for home

df6_home = df6 %>% dplyr::filter(discharge == "home") 

summary(df6_home$wts1) 

# weights for ATT for hhc , confirm that all = 1


df6_hhc = df6 %>% filter(discharge == "hhc")

summary(df6_hhc$wts1)




plot = ggplot(df6, aes(x = ps, y = wts1, color = discharge)) + 
  geom_point() # plot the weights for the control and treat cohort



## balance using cobalt library



covlist = c("hosp_nrd",'age' , 'aweekend' , 'female' , 'cm_chf' , 'cm_chrnlung' ,
            'diabetes' , 'cm_htn_c' , 'cm_hypothy' , 'cm_liver' , 'cm_obese' , 'cm_perivasc',
            'cm_renlfail' , 'cancer' , 'hosp_loc' , 'insu' , 'smoke' , 'year' ,
            'dys' , 'priormi' , 'treatment' , 'priorpci' , 'car' ,
            'pstroke')




library(cobalt)



a2 = bal.tab(hhc ~  hosp_nrd + age + aweekend + female
            + cm_chf + cm_chrnlung +  diabetes + 
              cm_htn_c + cm_hypothy + cm_liver + cm_obese + 
              cm_perivasc + cm_renlfail + cancer + hosp_loc + insu + smoke + year +
              dys + priormi + treatment + priorpci + car +
              pstroke
            ,data = df6,
            weights = "wts1",distance = "p.score", method = "weighting", un = TRUE)

tbl = data.frame(a2$Balance)

tbl = tbl_df(tbl)

tbl

write_csv(tbl, "balance_table.csv")


plot(a2)



pre.szd = tbl$Diff.Un*100
post.szd = tbl$Diff.Adj*100


covnames = c("hosp_nrd", 'age' , 'aweekend' , 'female' , 'cm_chf' , 'cm_chrnlung' ,
             'diabetes' , 'cm_htn_c' , 'cm_hypothy' , 'cm_liver' , 'cm_obese' , 'cm_perivasc',
             'cm_renlfail' , 'cancer' , 'hosp_loc' , 'insu_medicaid', "insu_medicare" ,"insu_others","insu_private",  'smoke' , 'year' ,
             'dys' , 'priormi' , 'treatment_cabg' ,"treatment_medical", "treatment_pci", 'priorpci' , 'car' ,
             'pstroke', "ps")


# create loveplot after getting SD using cobalt

temp1 <- data.frame(pre.szd, post.szd, covnames)#combine the 3 variable into a data.base.
library(dplyr)
temp1<-temp1 %>% arrange(pre.szd)%>% mutate (covnames= factor(covnames,covnames)) #use dplyr mutate and arrange function to sort the covariates by pre.szd.
library(reshape2)
temp2<-melt(temp1,measure.vars=c("pre.szd","post.szd")) ##melt the database base to get a two level variable ("pre.szd","post.szd") that I will use in the plot.

p<-ggplot(temp2, aes(value, covnames, shape= variable,colour=variable)) ## the first step is tell ggplot that I want to use temp2 database to plot the (value) variable on x axis, covnames on y axis. Then I tell it to differentiate between the plotted geom based on (variable)  which is two level categorical variable ("pre.szd","post.szd").

p<-p +geom_point(size =3)+theme_bw()+labs(title="Standardized Difference Plot")+labs(x="Standardized Difference (%)")+labs(y="Covariates") # here, I tell ggplot that I want the values to be plotted as point (geom_point), I chose size 3, I change the background theme to black and white, then I label the axes.

p<-p+theme(legend.title=element_blank())+scale_colour_manual(values = c("pre.szd"="red", "post.szd"="green"),labels=c("Before Matching","After Matching"))+scale_shape_discrete(labels=c("Before Matching","After Matching")) ## here, I drop the legend title, I assign specific colors to the points, I rename the legend's labels.

p<-p + theme(legend.justification=c(1,0), legend.position=c(1,0)) + geom_vline(xintercept = 0,colour="blue") + geom_vline(xintercept = c(-10,10) ,colour="blue",linetype = "longdash") # I reposition the legend to right lower corner, I add the vertical lines.


p2 = p+ coord_cartesian(xlim = c(-60, 50)) + scale_x_continuous(breaks = seq(from = -200, to = 200, by = 10)) # Finally, I change x-axis limits and change the breaks to every 10.

#### adjusted results

adj = svydesign(ids = ~hosp_nrd, data = df6, 
                weights = ~discwt + wts1, nest = T, strata = ~hosp_nrd)

options(survey.lonely.psu = "adjust")

adjust = svyglm(readmit ~ hhc, design = adj, 
                family = "binomial"(link = "logit"))


summary(adjust)
exp(coef(adjust))
exp(confint(adjust))

