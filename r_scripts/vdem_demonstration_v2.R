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


dbcon <- "data/vdem_data.sqlite"
varnames <- paste0('V',2:900)

# Load posterior estimates of the V-DEM indices
# These are from the MCMC estimates of the V-DEM IRT model, and we iterate over them to account for measurement
# uncertainty

# One-way case effect

model1 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon) 
model1 <- model1$results_condense %>% mutate(model_type="GDP Case Effects")

# One-way time effect

model2 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor),select_vars=c('e_migdppcln','year_factor'),
         num_iters=900,
         num_cores=4,dbcon=dbcon)  
model2 <- model2$results_condense %>% mutate(model_type="GDP Time Effects")
# 2-way FEs
  
model3 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + country_name,
                   select_vars=c('e_migdppcln','year_factor','country_name'),
                  num_iters=900,
                  num_cores=4,dbcon=dbcon)  
model3 <- model3$results_condense %>% mutate(model_type="GDP Two-way Effects")

# All three are statistically significant, the within effect is the largest

# Combine estimates


# Try a few more exotic models

# One-way time FE that varies over time
# Note that this model implicitly accounts for autocorrelation

model4 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + e_migdppcln*year_factor,select_vars=c('e_migdppcln','year_factor'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon)  
model4 <- model4$results_condense %>% mutate(model_type="GDP Interactive Time Effects")

# One-way case FE that varies between countries

model5 <- run_vdem(varnames=varnames,
                   full_formula=v2x_polyarchy ~ e_migdppcln + country_name + e_migdppcln*country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=4,dbcon=dbcon) 
model5 <- model5$results_condense %>% mutate(model_type="GDP Interactive Case Effects")
# One-way case FE that varies between time periods (test of Boix 2011)

boix_country <- run_vdem(varnames=varnames,
                         full_formula=v2x_polyarchy ~ e_migdppcln*country_name*factor(time_period),select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,country_interaction=TRUE,
                   num_cores=4,dbcon=dbcon) 
boix_country_condensed <- boix_country$results_condense %>% mutate(model_type="Boix Test Countries")

# One-way time FE that varies between continents (test of Boix 2011)

boix_time <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + europe + e_migdppcln*year_factor*europe,select_vars=c('e_migdppcln','year_factor','e_regionpol'),
                   num_iters=900,time_interaction=TRUE,
                   num_cores=4,dbcon=dbcon)  
boix_time_condensed <- boix_time$results_condense %>% mutate(model_type="Boix Test Years")




#Fuel income specification

model_oilcase <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + country_name,select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','country_name'),
                          num_iters=900,
                                      num_cores=4,dbcon=dbcon) 
model_oilcase <- model_oilcase$results_condense %>% mutate(model_type="GDP-Oil Case Effects")

model_oiltime <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor),select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor'),
                          num_iters=900,
                          num_cores=4,dbcon=dbcon) 
model_oiltime <- model_oiltime$results_condense %>% mutate(model_type="GDP-Oil Time Effects")

model_oiltwoway <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor) + country_name,
                            select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor','country_name'),
                            num_iters=900,
                            num_cores=4,dbcon=dbcon) 
model_oiltwoway <- model_oiltwoway$results_condense %>% mutate(model_type="GDP-Oil Two-way Effects")
combined_all <- bind_rows(model_oilcase,model_oiltime,model_oiltwoway,model1,model2,model3,model4,model5,
                          boix_time_condensed,boix_country_condensed)
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

# Calculate interaction effects and plot for Boix tests
#Drop West Bank because effect is very imprecise

boix_country_data <- boix_country$results_full

year_ints_bipolar <- boix_country_data$e_migdppcln 
year_ints_unipolar <- boix_country_data$e_migdppcln + boix_country_data$`e_migdppcln:factor(time_period)1`
year_ints_neutral <- boix_country_data$e_migdppcln + boix_country_data$`e_migdppcln:factor(time_period)0`

country_ints_bipolar <- boix_country_data[grepl('e_migdppcln:country_name[A-Za-z]+(?!.*time_period)',names(boix_country_data),perl=TRUE)]
country_ints_unipolar <- boix_country_data[grepl('e_migdppcln:country_name.+:factor\\(time_period\\)1',names(boix_country_data),perl=TRUE)]
country_ints_neutral <- boix_country_data[grepl('e_migdppcln:country_name.+:factor\\(time_period\\)0',names(boix_country_data),perl=TRUE)]

# Need to remove countries that dropped out of the triple interaction
all_countries <- str_extract(names(country_ints_bipolar),':[ _-a-zA-z]+')
all_countries <- gsub(pattern = ':|country_name','',x = all_countries)
neut_countries <- str_extract(names(country_ints_neutral),':[ _-a-zA-z]+')
neut_countries <- gsub(pattern = ':|country_name','',x = neut_countries)
keep_countries_neutral <- which(all_countries %in% neut_countries)
unipolar_countries <- str_extract(names(country_ints_unipolar),':[ _-a-zA-z]+')
unipolar_countries <- gsub(pattern = ':|country_name','',x = unipolar_countries)
keep_countries_unipolar <- which(all_countries %in% unipolar_countries)

bipolar_effect <- (year_ints_bipolar + as.matrix(country_ints_bipolar)) %>% as_data_frame %>% mutate(Period='Polarized') %>% 
  gather(parameters,estimates,-Period) %>% mutate(parameters=gsub(x=str_extract(parameters,':[ _-a-zA-z]+'),pattern=':|country_name',replacement='')) %>% 
  group_by(parameters,Period) %>% summarize(mean_est=mean(estimates),up_bd=quantile(estimates,.95),lw_bd=quantile(estimates,.05))
unipolar_effect <- (year_ints_unipolar + as.matrix(country_ints_unipolar) + as.matrix(country_ints_bipolar)[,keep_countries_unipolar]) %>% as_data_frame %>% mutate(Period='Pro-Democracy') %>% 
  gather(parameters,estimates,-Period) %>% mutate(parameters=gsub(x=str_extract(parameters,':[ _-a-zA-z]+:'),pattern=':|country_name',replacement='')) %>% 
  group_by(parameters,Period) %>% summarize(mean_est=mean(estimates),up_bd=quantile(estimates,.95),lw_bd=quantile(estimates,.05))
neutral_effect <- (year_ints_neutral + as.matrix(country_ints_neutral) + as.matrix(country_ints_bipolar)[,keep_countries_neutral]) %>% as_data_frame %>% mutate(Period='Neutral') %>% 
  gather(parameters,estimates,-Period) %>% mutate(parameters=gsub(x=str_extract(parameters,':[ _-a-zA-z]+'),pattern=':|country_name',replacement='')) %>% 
  group_by(parameters,Period) %>% summarize(mean_est=mean(estimates),up_bd=quantile(estimates,.95),lw_bd=quantile(estimates,.05))
combined_int <- bind_rows(bipolar_effect,unipolar_effect,neutral_effect) %>% 
  ungroup %>% arrange(Period,mean_est) %>% mutate(order=row_number())

combined_int %>% filter(parameters!="Palestine_West_Bank") %>% ggplot(aes(x=mean_est,y=order,colour=Period)) + geom_point(size=0.5) + facet_wrap(~Period,ncol=1,nrow=3,scales = 'free_y',strip.position = 'left') +
  geom_errorbarh(aes(xmin=lw_bd,xmax=up_bd),alpha=0.5) + geom_vline(aes(xintercept=mean(mean_est)),linetype=2) +
  my_theme +   theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  ylab('') + xlab('') +geom_text(aes(label=parameters),hjust='outward',vjust='inward',check_overlap=TRUE,colour='black',nudge_x = .02) +
  guides(colour='none')

ggsave('charts/boixcountry.png',width=10,height=6,units='in')

boix_time_data <- boix_time$results_full
year_ints_europe <- boix_time_data[grepl('e_migdppcln:europe:year_factor',names(boix_time_data))]
year_ints_noneurope <- boix_time_data[grepl('e_migdppcln:year_factor',names(boix_time_data))]
intercepts_europe <- boix_time_data$e_migdppcln + boix_time_data$`e_migdppcln:europe`
intercepts_noneurope <- boix_time_data$e_migdppcln
combined_europe <- (intercepts_europe + as.matrix(year_ints_europe) + as.matrix(year_ints_noneurope)) %>% as_data_frame %>% mutate(Origin='European') %>% 
  gather(parameters,estimates,-Origin) %>% mutate(parameters=as.character(str_extract(parameters,'[0-9]+'))) %>% 
  group_by(parameters,Origin) %>% summarize(mean_est=mean(estimates),up_bd=quantile(estimates,.95),lw_bd=quantile(estimates,.05))
combined_noneurope <- (intercepts_noneurope + as.matrix(year_ints_noneurope)) %>% as_data_frame %>% mutate(Origin='Non-European') %>% 
  gather(parameters,estimates,-Origin) %>% mutate(parameters=as.character(str_extract(parameters,'[0-9]+'))) %>% 
  group_by(parameters,Origin) %>% summarize(mean_est=mean(estimates),up_bd=quantile(estimates,.95),lw_bd=quantile(estimates,.05))
combined_int <- bind_rows(combined_europe,combined_noneurope)
combined_int$param_labels <- as.character(combined_int$parameters[rep(x = c(TRUE,NA,NA,NA,NA),times=nrow(combined_int)/5)])
combined_int$param_labels <- ifelse(is.na(combined_int$param_labels),"",combined_int$param_labels)

ggplot(combined_int,aes(y=mean_est,x=as.numeric(parameters),fill=Origin)) + geom_line()  + 
  geom_ribbon(aes(ymin=lw_bd,ymax=up_bd),alpha=0.5) + my_theme + ylab("Log GDP Effect on Democracy") + xlab("") + 
  scale_x_continuous(breaks=floor(seq(1901,2010,length.out=10))) + theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank()) +
  geom_hline(aes(yintercept=mean(mean_est)),linetype=2) + geom_vline(xintercept=1989,linetype=3) +
  annotate('text',x=1995,y=0.8,label='1989')

ggsave('charts/boixtime_europe.png',width=10,height=6,units='in')