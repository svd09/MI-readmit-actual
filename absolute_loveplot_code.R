abs.pre.szd = abs(pre.szd)
abs.post.szd = abs(post.szd)

temp2 <- data.frame(abs.pre.szd,abs.post.szd, covnames)#combine the 3 variable into a data.base.
library(dplyr)
temp2 <-temp2 %>% arrange(abs.pre.szd)%>% mutate (covnames= factor(covnames,covnames)) #use dplyr mutate and arrange function to sort the covariates by pre.szd.
library(reshape2)
temp2<-melt(temp2,measure.vars=c("abs.pre.szd","abs.post.szd")) ##melt the database base to get a two level variable ("pre.szd","post.szd") that I will use in the plot.

a <-ggplot(temp2, aes(value, covnames, shape= variable,colour=variable)) ## the first step is tell ggplot that I want to use temp2 database to plot the (value) variable on x axis, covnames on y axis. Then I tell it to differentiate between the plotted geom based on (variable)  which is two level categorical variable ("pre.szd","post.szd").

a <- a +geom_point(size = 4)+theme_bw()+labs(title="Standardized Difference Plot")+labs(x="Standardized Difference (%)")+labs(y="Covariates") # here, I tell ggplot that I want the values to be plotted as point (geom_point), I chose size 3, I change the background theme to black and white, then I label the axes.

a <- a +theme(legend.title=element_blank())+scale_colour_manual(values = c("abs.pre.szd"="tomato3", "abs.post.szd"="lightblue"),labels=c("Before Matching","After Matching"))+scale_shape_discrete(labels=c("Before Matching","After Matching")) ## here, I drop the legend title, I assign specific colors to the points, I rename the legend's labels.

a <- a + theme(legend.justification=c(1,0), legend.position=c(1,0)) + geom_vline(xintercept = 0,colour="blue") + geom_vline(xintercept = c(-10,10) ,colour="blue",linetype = "longdash") # I reposition the legend to right lower corner, I add the vertical lines.


a2 = a + coord_cartesian(xlim = c(0, 150)) + scale_x_continuous(breaks = seq(from = -200, to = 200, by = 10)) # Finally, I change x-axis limits and change the breaks to every 10.


a3 = a2 + scale_y_discrete(limits = rev(levels(temp1$covnames)))
