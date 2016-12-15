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
hm <- select(hm,D_pol_int,L_pol_int,L_oilcap_int,D_oilcap_int,L_loggdpcap_int,L_civilwar_int,L_regdiffuse,L_worlddiffuse,
             D_loggdpcap_int,D_regdiffuse,D_worlddiffuse,year,hmccode) %>% filter(complete.cases(.)) %>% 
  mutate(year=factor(year),hmccode=factor(hmccode))
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
    resids <- predict(x,out_data,type='response')
    return(resids)
  })
  return(errors)
})        
        
