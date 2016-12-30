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
fiveyear <- left_join(fiveyear,fivey_cbalance) %>% left_join(fivey_tbalance,by='year') %>% ungroup %>% 
  select(matches('fhpol|lrgd'),country,year,panel_balance,time_balance) %>% filter(complete.cases(.))

#Check for countries with no change in DV

acemoglu_sds <- fiveyear %>% group_by(country) %>% summarize(sd_country=sd(fhpolrigaug))


hm <- read_dta("hm.dta")
hm <- select(hm,D_pol_int,L_pol_int,L_oilcap_int,D_oilcap_int,L_loggdpcap_int,L_civilwar_int,L_regdiffuse,L_worlddiffuse,
             D_loggdpcap_int,D_regdiffuse,D_worlddiffuse,year,hmccode,cnamehabmen) %>% filter(complete.cases(.)) %>% 
  mutate(year=factor(year),hmccode=factor(hmccode))
hm_sds <- hm %>% group_by(cnamehabmen) %>% summarize(sd_country=sd(D_pol_int))
summary(lm(data=hm,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
             L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year) + factor(hmccode)))

summary(lm(data=hm,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
             L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(hmccode)))
summary(lm(data=hm,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
             L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + factor(year)))
summary(lm(data=hm,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
             L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse))
        
out_vals <- lapply(1:nrow(hm),function(i) {
  this_data <- filter(hm,row_number()!=i)
  out_data <- filter(hm,row_number()==i)
  print(paste0('Now on row ',i,'\n'))
  two_way <- lm(data=this_data,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                  L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + year + hmccode)
  one_way_time <- lm(data=this_data,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                       L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + year)
  one_way_case <- lm(data=this_data,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                       L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse + hmccode)
  one_way_pooled <- lm(data=this_data,formula=D_pol_int~L_pol_int + L_oilcap_int + D_oilcap_int + L_loggdpcap_int + L_civilwar_int +
                         L_regdiffuse + L_worlddiffuse + D_loggdpcap_int + D_regdiffuse + D_worlddiffuse)
  errors <- sapply(list(two_way,one_way_time,one_way_case,one_way_pooled),function(x) {
    yhat <- predict(x,out_data,type='response')
    resids <- as.numeric(out_data$D_pol_int) - yhat
    return(resids)
  })
  return(errors)
})        
        
all_errs <- matrix(unlist(out_vals),nrow=length(out_vals),ncol=4,byrow=TRUE)

# Root mean squared leave-one-out cross-validation error (prediction error) for 2-way FE,1-way FE, and pooled OLS

avg_errs <- apply(all_errs,2,function(x) sqrt(mean(x^2)))
saveRDS(file='hm_avg_errs.rds',object=avg_errs)
saveRDS(file='hm_all_errs.rds',object=all_errs)

# Now repeat for Acemoglu-Robinson et al.

out_vals <- lapply(1:nrow(fiveyear),function(i) {
  this_data <- filter(fiveyear,row_number()!=i)
  out_data <- filter(fiveyear,row_number()==i)
  # Need to put in an error check if the model removes one country with only one observation, which will cause
  # an error for the FE models
  if(out_data$panel_balance==2) return(c(0,0,0,0))
  print(paste0('Now on row ',i,'\n'))
  two_way <- lm(data=this_data,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch+ factor(year) + factor(country))
  one_way_time <- lm(data=this_data,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch+ factor(year))
  one_way_case <- lm(data=this_data,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch+factor(country))
  one_way_pooled <- lm(data=this_data,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch)
  errors <- sapply(list(two_way,one_way_time,one_way_case,one_way_pooled),function(x) {
    yhat <- predict(x,out_data,type='response')
    resids <- as.numeric(out_data$fhpolrigaug) - yhat
    return(resids)
  })
  return(errors)
})        
        
all_errs <- matrix(unlist(out_vals),nrow=length(out_vals),ncol=4,byrow=TRUE)

# Root mean squared leave-one-out cross-validation error (prediction error) for 2-way FE,1-way FE, and pooled OLS

avg_errs <- apply(all_errs,2,function(x) sqrt(mean(x^2)))
saveRDS(file='ar_avg_errs.rds',object=avg_errs)
saveRDS(file='ar_all_errs.rds',object=all_errs)
