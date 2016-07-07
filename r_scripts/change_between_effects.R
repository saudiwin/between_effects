require(stringr)
require(data.table)
require(ggplot2)
require(car)
require(nnet)

source("r_scripts/Ggplot2_theme.R")

lookat <- read.csv("data/Record of Papers Between Effects - Sheet1.csv")

lookat$N <- str_extract(lookat$N.T,"[0-9]+\\*")
lookat$T <- str_extract(lookat$N.T,"\\*[0-9]+")
lookat$N <- gsub("\\*",x=lookat$N,replacement = "")
lookat$T <- gsub("\\*",x=lookat$T,replacement = "")
lookat$N <- as.numeric(lookat$N)
lookat$T <- as.numeric(lookat$T)
lookat$journaltitle <- recode(lookat$journaltitle,"'The American Political Science Review'='American Political Science Review';
                              'The Journal of Politics'='Journal of Politics'")
lookat <- as.data.table(lookat)
date1 <- lookat$pubdate[1:292]
date2 <- lookat$pubdate[293:nrow(lookat)]
date1 <- as.Date(date1,format="%Y-%m-%d")
date2 <- as.Date(date2,format="%m/%d/%Y")
lookat$pubdate <- c(date1,date2)
lookat$pubdate[1:292] <- date1
lookat$pubdate[293:nrow(lookat)] <- date2
setkey(lookat,"pubdate","journaltitle")
lookat <- lookat[,year:=as.numeric(substr(as.character(pubdate),1,4))]
FE_values <- as.data.frame(t(table(lookat$FE.type,lookat$year,exclude=c("","One-way(Time), One-way(Within)","Unclear"))))


# Plot basic time series of counts
ggplot(FE_values,aes(y=Freq,x=as.numeric(as.character(Var1)),fill=Var2)) + geom_area() + theme_bw() + xlab("") + ylab("Articles Per Year") +
  scale_fill_brewer(type="seq",guide = guide_legend(title="Type of \nFixed Effect",reverse=TRUE)) + my_theme


# Break it down by journal
FE_values_mag <- as.data.frame(xtabs(~FE.type + journaltitle + year,data=lookat,exclude=c("","One-way(Time), One-way(Within)","Unclear")))

ggplot(FE_values_mag,aes(y=Freq,x=as.numeric(as.character(year)),fill=FE.type)) + geom_area() + facet_wrap(~journaltitle) + theme_bw()

#FX types are evenly distributed across journals, as evidenced by chi-sq statistic on contingency table (p=0.16)
#At least for all years...
xtabs(~FE.type + journaltitle,data=lookat,exclude=c("","One-way(Time), One-way(Within)","Unclear"))
summary(xtabs(~FE.type + journaltitle,data=lookat,exclude=c("","One-way(Time), One-way(Within)","Unclear")))

#But post-2010, it seems like APSR has more Two-way than other journals (p=0.07)
xtabs(~FE.type + journaltitle,data=lookat[year>2010,],exclude=c("","One-way(Time), One-way(Within)","Unclear"))
summary(xtabs(~FE.type + journaltitle,data=lookat[year>2010,],exclude=c("","One-way(Time), One-way(Within)","Unclear")))

#Try some modeling

lookat$FE.type_recode <- factor(lookat$FE.type,levels=c('One-way(Within)','One-way(Time)','Two-way'))

# Use one-way(Time) as base category

lookat$FE.type_recode <- relevel(lookat$FE.type_recode,ref='One-way(Time)')

model1 <- multinom(FE.type_recode ~ N + T,data=lookat)

# As N and T get larger, Two-way FX are more likely, although the effect is much more precise and strong for T.
# however, it is hard to separate this effect from the effect of time and increasing ability to collect data. So including a 
# linear time trend:

lookat$trend <- 2014 - lookat$year 

model2 <- multinom(FE.type_recode ~ N + T + trend,data=lookat)
summary(model2)

# Linear trend should control for technological progress (in a linear fashion). Unsurprisingly, as the count of years moves closer to 2014,
# one-way time FX become less likely compared to two-way FX. Furthermore, the number of time points in the model is still associated 
# with using 2-way and One-way FX over Time FX

# use google scholar to get citation counts for certain papers

DOIs <- c('10.2307/2111187','10.2307/2082979','10.1093/pan/mpm002','10.1111/0034-6527.00321')


