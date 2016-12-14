# Re-analysis of Acemoglu et al.

require(haven)
require(readxl)
require(dplyr)
require(magrittr)
require(tidyr)
require(ggplot2)
require(modelr)
require(broom)
require(purrr)
require(plotly)
require(pbapply)
require(parallel)
fiveyear <- read_dta("~/R_Projects/between_effects/Acemoglu one year panel.dta")
oneyear <- read_excel("~/R_Projects/between_effects/Income and Democracy Data AER adjustment.xls", 
                      sheet = "Annual Panel")

oneyear <- oneyear %>% group_by(country) %>% mutate(l1_fhpolrigaug=lag(fhpolrigaug,order_by=year),
                                                    l1_lrgdpch=lag(lrgdpch,order_by=year)) %>% 
  filter(!(is.na(fhpolrigaug)) & !(is.na(lrgdpch))) %>% mutate(panel_balance=n())

data1 <-  filter(oneyear,country %in% c('United States')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data1,formula=fhpolrigaug~ lrgdpch + factor(year)))['lrgdpch']
coef(lm(data=data1,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data2 <-  filter(oneyear,country %in% c('Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data2,formula=fhpolrigaug~ lrgdpch + factor(year)))['lrgdpch']
coef(lm(data=data2,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

tw_coef <- 1:100
f1_coef <- 1:100
c1_coef <- 1:100
p_coef <- 1:100
for(i in 1:100) {
data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
replacement <- runif(nrow(data3),0.99,.9999)
data3 <- ungroup(data3) %>% mutate(fhpolrigaug = ifelse(fhpolrigaug==1,replacement,fhpolrigaug))
tw_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
f1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year)))['lrgdpch']
c1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(country)))['lrgdpch']
p_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3 %>% group_by(country) %>% summarize(mean_fp=mean(fhpolrigaug))
}

data_frame(tw_coef,f1_coef,c1_coef,p_coef) %>% gather(model_type,estimate) %>% 
  ggplot(aes(x=estimate,fill=model_type,alpha=0.5)) + geom_density() + theme_minimal()

data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']



data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
data3$fhpolrigaug[1:7] <- data3$fhpolrigaug[1:7] + runif(length(1:7),0.4,0.45)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
data3$fhpolrigaug[34:41]  <- data3$fhpolrigaug[34:41] + runif(length(34:41),0.4,0.45)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

#Look at the mean differences in the panel and compare to 2-way FE
test_theory <- oneyear %>% group_by(country) %>% summarize(mean_gdp=mean(lrgdpch,na.rm=TRUE)) %>% 
  filter(!is.nan(mean_gdp) | !is.na(mean_gdp)) %>% mutate(mean_lag_gdp=lag(mean_gdp),diff=mean_gdp-mean_lag_gdp)
mean(test_theory$diff,na.rm=TRUE)
