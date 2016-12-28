# This is a demonstration of 2-way and one-way FX analysis using data provided by the varities of democracy
# Robert Kubinec 7/12/2016

require(twowaysim)
require(dplyr)
require(magrittr)
require(sandwich)
require(tibble)
require(ggplot2)
require(stringr)
require(xtable)
require(tidyr)
require(parallel)
source('r_scripts/Ggplot2_theme.R')


dbcon <- "vdem_data.sqlite"
varnames <- paste0('V',2:900)

# Load posterior estimates of the V-DEM indices
# These are from the MCMC estimates of the V-DEM IRT model, and we iterate over them to account for measurement
# uncertainty

# One-way case effect

model1 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP Case Effects")

# One-way time effect

model2 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor),select_vars=c('e_migdppcln','year_factor'),
         num_iters=900,
         num_cores=4,dbcon=dbcon)  %>% mutate(model_type="GDP Time Effects")

# 2-way FEs
  
model3 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + country_name,
                   select_vars=c('e_migdppcln','year_factor','country_name'),
                  num_iters=900,
                  num_cores=4,dbcon=dbcon)  %>% mutate(model_type="GDP Two-way Effects")


# All three are statistically significant, the within effect is the largest

# Combine estimates


# Try a few more exotic models

# One-way time FE that varies over time
# Note that this model implicitly accounts for autocorrelation

model4 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + e_migdppcln*year_factor,select_vars=c('e_migdppcln','year_factor'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon)  %>% mutate(model_type="GDP Interactive Time Effects")


# One-way case FE that varies between countries

model5 <- run_vdem(varnames=varnames,
                   full_formula=v2x_polyarchy ~ e_migdppcln + country_name + e_migdppcln*country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP Interactive Case Effects")

# One-way case FE that varies between time periods (test of Boix 2011)

boix_country <- run_vdem(varnames=varnames,
                         full_formula=v2x_polyarchy ~ e_migdppcln + country_name + time_period + e_migdppcln*country_name*time_period,select_vars=c('e_migdppcln','country_name','time_period'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP Interactive Case Effects")

# One-way time FE that varies between continents (test of Boix 2011)

boix_time <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + europe + e_migdppcln*year_factor*europe,select_vars=c('e_migdppcln','year_factor','europe'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon)  %>% mutate(model_type="GDP Interactive Time Effects")




#Fuel income specification

model_oilcase <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + country_name,select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','country_name'),
                          num_iters=900,
                                      num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Case Effects")


model_oiltime <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor),select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor'),
                          num_iters=900,
                          num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Time Effects")
model_oiltwoway <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor) + country_name,
                            select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor','country_name'),
                            num_iters=900,
                            num_cores=4,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Two-way Effects")

combined_all <- bind_rows(model_oilcase,model_oiltime,model_oiltwoway,model1,model2,model3,model4,model5)
saveRDS(combined_all,file = 'data/all_models.rds')

# Order factors, and output in a coefficient table format
# Rescale coefficients for oil to be on $1,000 of per capita income

just_ivs <- combined_all %>% filter(betas %in% c('e_migdppcln','e_Total_Fuel_Income_PC'))  %>% 
  gather(estimates,estimate_type,-betas,-model_type)  %>% mutate(model_type=factor(model_type,
                                                                                   levels=c('GDP Case Effects',
                                                                                            'GDP Time Effects',
                                                                                            'GDP Two-Way Effects',
                                                                                            'GDP Interactive Time Effects',
                                                                                            'GDP Interactive Case Effects',
                                                                                            'GDP-Oil Case Effects',
                                                                                            'GDP-Oil Time Effects',
                                                                                            'GDP-Oil Two-way Effects')),
                                                                 estimates=factor(estimates,levels=c('coef','sd','lower','upper')),
                                                                 estimate_type=ifelse(betas=='e_Total_Fuel_Income_PC',estimate_type*100000,estimate_type)) %>% 
  mutate_if(is.numeric,funs(format(.,digits=1,nsmall=1))) %>% 
   unite(beta_estimates,betas,estimates)  %>% 
  spread(beta_estimates,estimate_type) %>% 
  mutate(e_migdppcln_CI=paste0("(",e_migdppcln_lower,",",e_migdppcln_upper,")"),
         e_Total_Fuel_Income_PC_CI=paste0('(',e_Total_Fuel_Income_PC_lower,',',
                                       e_Total_Fuel_Income_PC_upper,')')) %>% 
  select(model_type,e_migdppcln_coef,e_migdppcln_CI,e_migdppcln_sd,
         e_Total_Fuel_Income_PC_coef,e_Total_Fuel_Income_PC_CI,e_Total_Fuel_Income_PC_sd)

just_ivs %>% as.data.frame %>% write.csv(file = 'vdem_model_summary.csv',x=.,row.names=FALSE)

#Calculate Model balance within the two-way FE model

out_data <-  panel_balance(dbcon=dbcon,select_vars=c('e_migdppcln','year_factor','country_name'),use_plotly=FALSE)
out_data$countries <- out_data$countries + ylab("Observations") +  scale_x_discrete(labels=abbreviate) + my_theme + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
out_data$countries <- out_data$countries + ylab("Observations")  + my_theme + theme(axis.ticks.x=element_blank(),
                                                                                    axis.text.x=element_blank()) +
  stat_count(geom='text',aes(y=..count..,label=country_name),vjust=-0.5,hjust='inward',check_overlap=TRUE)
out_data$years <- out_data$years + ylab("Observations") +my_theme  + 
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank()) +
  stat_count(geom="text",aes(y=..count..,label=year_factor),vjust=-0.5,hjust='inward',check_overlap=TRUE)
out_data$countries
ggsave('countries_balance.png',width=10,height=6,units='in')
out_data$years
ggsave('years_balance.png',width=10,height=6,units='in')

# Calculate interaction effects and plot
#Drop West Bank because effect is very imprecise

int_effects <- combined_all %>% filter(model_type=='GDP Interactive Case Effects',grepl('country',betas)) %>% 
  filter(grepl(':',betas)) %>% mutate(coef_type="interaction") %>% separate(betas,c('beta_type','country'),sep=24) %>% 
  filter(country!='Palestine_West_Bank')

country_effects <- combined_all %>% filter(model_type=='GDP Interactive Case Effects',grepl('country',betas)) %>% 
  filter(!grepl(':',betas)) %>% mutate(coef_type="country_fx") %>% separate(betas,c('beta_type','country'),sep=12) %>% 
  filter(country!='Palestine_West_Bank')
  
combined_fx <- left_join(int_effects,country_effects,by='country') %>% 
ggplot(aes(x=coef,y=reorder(country,coef))) + geom_point() + 
 my_theme +   theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  geom_text(aes(label=country),hjust='outward',vjust='inward',check_overlap=TRUE) + ylab('') + xlab('Log GDP Effect on Democracy') +
  geom_errorbarh(aes(xmin=lower,xmax=upper),alpha=0.5)  + geom_vline(aes(xintercept=mean(coef)),linetype=2)
ggsave('withinbetween.png',width=10,height=6,units='in')

model4 <- readRDS("../data/model4_results.rds") %>% t %>% as.data.frame %>% tbl_df
countries <- select(model4,matches(":year"))
int_matrix <- tbl_df(lapply(countries,function(x) x + model4[['e_migdppcln']]))
results <- data_frame(Coef=sapply(int_matrix,mean), SD = sapply(int_matrix,sd))
results$variables <- str_extract(colnames(countries),'[0-9]+')
results$variables_labels <- as.character(results$variables[rep(x = c(TRUE,NA,NA,NA,NA),times=nrow(results)/5)])
results$variables_labels[nrow(results)] <- results$variables[nrow(results)]
results$variables_labels <- ifelse(is.na(results$variables_labels),"",results$variables_labels)
results <- mutate(results,upper=Coef + 1.96*SD,lower=Coef - 1.96*SD)

ggplot(results,aes(y=Coef,x=variables)) + geom_point()  + 
  geom_errorbar(aes(ymin=lower,ymax=upper)) + my_theme + ylab("Log GDP Effect on Democracy") + xlab("") + 
  scale_x_discrete(labels=results$variables_labels) + theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank()) +
  geom_hline(aes(yintercept=mean(Coef)),linetype=2)

ggsave('betweenbetween.png',width=10,height=6,units='in')