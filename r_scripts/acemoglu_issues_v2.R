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
require(gridExtra)

fiveyear <- read_dta("Acemoglu one year panel.dta")
oneyear <- read_excel("Income and Democracy Data AER adjustment.xls", 
                      sheet = "Annual Panel")
oneyear <- oneyear %>% group_by(country) %>% mutate(l1_fhpolrigaug=lag(fhpolrigaug,order_by=year),
                                                    l1_lrgdpch=lag(lrgdpch,order_by=year)) %>% 
  filter(!(is.na(fhpolrigaug)) & !(is.na(lrgdpch))) %>% mutate(panel_balance=n())
fivey_cbalance <- fiveyear %>% group_by(country) %>% filter(!(is.nan(fhpolrigaug)) & !(is.nan(lrgdpch))) %>% 
  summarize(panel_balance=n())
fivey_tbalance <- fiveyear %>% group_by(year) %>% filter(!(is.nan(fhpolrigaug)) & !(is.nan(lrgdpch))) %>% 
  summarize(time_balance=n())
fiveyear <- fiveyear %>% group_by(country) %>% mutate(l1_fhpolrigaug=lag(fhpolrigaug,order_by=year),
                                                      l1_lrgdpch=lag(lrgdpch,order_by=year))
fiveyear <- left_join(fiveyear,fivey_cbalance) %>% left_join(fivey_tbalance,by='year') %>% filter(!is.na(panel_balance)) %>% 
  select(matches('fhpol|lrgd'),country,year,panel_balance,time_balance)


hm <- read_dta("C:/Users/bobku/Documents/R projects/between_effects/hm.dta")

plot1 <- ggplot(fiveyear,aes(x=fhpolrigaug)) + geom_bar() + theme_minimal() + ylab('') + xlab('Freedom House Scores') +
  theme(panel.grid = element_blank())
plot2 <- ggplot(hm,aes(x=D_pol_int)) + geom_bar(width=1) + theme_minimal() + xlab('Differenced Polity Scores') + ylab('') +
  theme(panel.grid = element_blank())
out_plot <- grid.arrange(plot1,plot2)
ggsave(out_plot,filename = 'acemoglu_haber_count.png',width=5,units='in')


# Iterate over A&R
tw_coef <- 1:1000
f1_coef <- 1:1000
c1_coef <- 1:1000
p_coef <- 1:1000
m_replace <- 1:1000
m_diff <- 1:1000
over_countries <- unique(fiveyear$countries)
for(i in 1:1000) {
  data3 <-  filter(fiveyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
  replacement <- runif(nrow(data3),0.99,.9999)
  data3 <- ungroup(data3) %>% mutate(fhpolrigaug2 = ifelse(fhpolrigaug==1,replacement,fhpolrigaug))
  tw_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug2~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
  f1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug2~ lrgdpch+ factor(year)))['lrgdpch']
  c1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug2~ lrgdpch+ factor(country)))['lrgdpch']
  p_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug2~ lrgdpch))['lrgdpch']
  m_replace[i] <- mean(ifelse(data3$fhpolrigaug==1,replacement,data3$fhpolrigaug),na.rm=TRUE)
  out_diff <- data3 %>% group_by(country) %>% summarize(mean_countries=mean(fhpolrigaug2))
  m_diff[i] <- (out_diff$mean_countries[1] - out_diff$mean_countries[2])^2
}

data_frame(tw_coef,f1_coef,c1_coef,p_coef) %>% gather(model_type,estimate) %>% 
  ggplot(aes(x=estimate,fill=model_type)) + geom_density(alpha=0.5) + theme_minimal() + 
  geom_vline(xintercept=c(min(tw_coef),max(tw_coef))) + theme(panel.grid=element_blank()) +
  xlab("Two-Way FE Coefficient for GDP on Freedom House Scores") + ylab('Iterations') +
  scale_fill_brewer(palette='Set1',guide=guide_legend(title=NULL),labels=c('Case FE','Time FE','Pooled OLS','Twoway FE'))

ggsave('ar_simulate.png',width=6,units='in')

#Iterate over H&M
# We use a simple lm estimator because the Driskol-Kraay errors won't matter for only examining the coefficient
tw_coef <- 1:1000
f1_coef <- 1:1000
c1_coef <- 1:1000
p_coef <- 1:1000
m_replace <- 1:1000
m_diff <- 1:1000
for(i in 1:1000) {
  #data3 <-  filter(fiveyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
  data3 <- hm
  replacement <- rnorm(nrow(data3),0,.2)
  data3 <- ungroup(data3) %>% mutate(polity_s2 = ifelse(D_pol_int==0,replacement + polity_s,polity_s)) %>% 
    group_by(hmccode) %>% mutate(L_pol_int2 = lag(polity_s2,order_by=year),D_pol_int2=polity_s2 - L_pol_int2)
  tw_coef[i] <- coef(lm(data=data3,formula=D_pol_int2~L_pol_int2 + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                          L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year) + factor(hmccode)))['L_oilcap_int']
  f1_coef[i] <-  coef(lm(data=data3,formula=D_pol_int2~L_pol_int2 + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                           L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year)))['L_oilcap_int']
  c1_coef[i] <-  coef(lm(data=data3,formula=D_pol_int2~L_pol_int2 + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                           L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(hmccode)))['L_oilcap_int']
  p_coef[i] <-  coef(lm(data=data3,formula=D_pol_int2~L_pol_int2 + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                          L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse ))['L_oilcap_int']
  #m_replace[i] <- mean(ifelse(data3$fhpolrigaug==1,replacement,data3$fhpolrigaug),na.rm=TRUE)
  #out_diff <- data3 %>% group_by(country) %>% summarize(mean_countries=mean(fhpolrigaug2))
  #m_diff[i] <- (out_diff$mean_countries[1] - out_diff$mean_countries[2])^2
}

data_frame(tw_coef,f1_coef,c1_coef,p_coef) %>% gather(model_type,estimate) %>% 
  ggplot(aes(x=estimate,fill=model_type)) + geom_density(alpha=0.5) + theme_minimal() + 
  geom_vline(xintercept=c(min(tw_coef),max(tw_coef))) + theme(panel.grid=element_blank()) +
  xlab("Two-Way FE Coefficient for Oil on Differenced Polity IV Scores") + ylab('Iterations') +
  scale_fill_brewer(palette='Set1',guide=guide_legend(title=NULL),labels=c('Case FE','Time FE','Pooled OLS','Twoway FE'))

ggsave('ar_simulate.png',width=6,units='in')

summary(lm(data=hm,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
             L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year) + factor(hmccode)))




# Iterate over A&R
over_countries <- unique(fiveyear$country)
tw_coef <- 1:length(unique(fiveyear$country))
f1_coef <- 1:length(unique(fiveyear$country))
c1_coef <- 1:length(unique(fiveyear$country))
p_coef <- 1:length(unique(fiveyear$country))
m_replace <- length(unique(fiveyear$country))
m_diff <- length(unique(fiveyear$country))

for(i in 1:length(unique(fiveyear$country))) {
  data3 <- ungroup(fiveyear) %>% filter(country != over_countries[i])
  tw_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch+ factor(year) + factor(country)))['l1_lrgdpch']
  f1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch + factor(year)))['l1_lrgdpch']
  c1_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch + factor(country)))['l1_lrgdpch']
  p_coef[i] <- coef(lm(data=data3,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch ))['l1_lrgdpch']
}


options(scipen=999)
p1 <- data_frame(countries=unique(fiveyear$country),twoway=tw_coef) %>% ggplot(aes(y=tw_coef,x=reorder(countries,tw_coef))) +
  geom_point() + theme_minimal() + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('Two-way FE\nSD=',round(sd(tw_coef),4)))+ ylab('Change in Coefficient')


p2 <- data_frame(countries=unique(fiveyear$country),time=f1_coef) %>% ggplot(aes(y=f1_coef,x=reorder(countries,f1_coef))) +
  geom_point() + theme_minimal() + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('1-Way Time\nSD=',round(sd(f1_coef),4)))+ ylab('')


p3 <- data_frame(countries=unique(fiveyear$country),time=c1_coef) %>% ggplot(aes(y=c1_coef,x=reorder(countries,c1_coef))) +
  geom_point() + theme_minimal()  + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('1-way Case\nSD=',round(sd(c1_coef),5)))+ ylab('Change in Coefficient')

p4 <- data_frame(countries=unique(fiveyear$country),time=p_coef) %>% ggplot(aes(y=p_coef,x=reorder(countries,p_coef))) +
  geom_point() + theme_minimal()  + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('Pooled OLS\nSD=',round(sd(p_coef),5)))+ ylab('')

out_p <- grid.arrange(p1,p2,p3,p4)
ggsave(plot=out_p,filename='ar_outliers.png',width=5.5,units='in')




# Iterate over H&M
over_countries <- unique(hm$hmccode)
tw_coef <- 1:length(unique(hm$hmccode))
f1_coef <- 1:length(unique(hm$hmccode))
c1_coef <- 1:length(unique(hm$hmccode))
p_coef <- 1:length(unique(hm$hmccode))


for(i in 1:length(over_countries)) {
  data3 <- hm
  data3 <- ungroup(data3) %>% filter(hmccode!=over_countries[i])
  tw_coef[i] <- coef(lm(data=data3,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                          L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year) + factor(hmccode)))['L_oilcap_int']
  f1_coef[i] <-  coef(lm(data=data3,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                           L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year)))['L_oilcap_int']
  c1_coef[i] <-  coef(lm(data=data3,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                           L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(hmccode)))['L_oilcap_int']
  p_coef[i] <-  coef(lm(data=data3,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                          L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse ))['L_oilcap_int']
}

options(scipen=999)
p1 <- data_frame(countries=unique(hm$hmccode),twoway=tw_coef) %>% ggplot(aes(y=tw_coef,x=reorder(countries,tw_coef))) +
  geom_point() + theme_minimal() + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('Two-way FE\nSD=',round(sd(tw_coef),5)))+ ylab('Change in Coefficient')


p2 <- data_frame(countries=unique(hm$hmccode),time=f1_coef) %>% ggplot(aes(y=f1_coef,x=reorder(countries,f1_coef))) +
  geom_point() + theme_minimal() + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('1-Way Time\nSD=',round(sd(f1_coef),5)))+ ylab('')


p3 <- data_frame(countries=unique(hm$hmccode),time=c1_coef) %>% ggplot(aes(y=c1_coef,x=reorder(countries,c1_coef))) +
  geom_point() + theme_minimal()  + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('1-way Case\nSD=',round(sd(c1_coef),5)))+ ylab('Change in Coefficient')

p4 <- data_frame(countries=unique(hm$hmccode),time=p_coef) %>% ggplot(aes(y=p_coef,x=reorder(countries,p_coef))) +
  geom_point() + theme_minimal()  + theme(panel.grid = element_blank(),axis.text.x = element_blank()) +
  xlab(paste0('Pooled OLS\nSD=',round(sd(p_coef),5)))+ ylab('')

out_p <- grid.arrange(p1,p2,p3,p4)
ggsave(plot=out_p,filename='hm_outliers.png',width=8.5,units='in')


