# Re-analysis of Acemoglu et al. (2008) and Haber and Menaldo (2010)
# Jonathan Kropko and Robert Kubinec
# Why You Can't Control Both Space and Time
# December 20, 2016

require(haven)
require(readxl)
require(dplyr)
require(magrittr)
require(tidyr)
require(ggplot2)
require(gridExtra)
require(data.table)
require(stringr)
require(sandwich)
require(twowaysim)

# If twowaysim is not installed, run this code:
# devtools::install_github('saudiwin/twofe_sim')

my_theme <- theme_bw() + theme(
  panel.grid.major=element_blank(), # Remove all gridlines
  panel.grid.minor=element_blank(),# Remove all gridlines
  strip.background = element_blank(), # Remove gray background from facet_wrap()
  strip.text = element_text(face="bold",size=12), # Facet_wrap titles
  axis.title = element_text(face="bold",size=12), # All axis titles
  axis.text.x = element_text(face="bold",size=10)
)


# Meta-analysis Replication

lookat <- read.csv("Record of Papers Between Effects - Sheet1.csv")

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
FE_values$Var2 <- recode(FE_values$Var2,`One-way(Within)`='One-way(Case)')

# Plot basic time series of counts
ggplot(FE_values,aes(y=Freq,x=as.numeric(as.character(Var1)),fill=Var2)) + geom_area() + theme_bw() + xlab("") + ylab("Articles Per Year") +
  scale_fill_brewer(type="seq",guide = guide_legend(title="Type of \nFixed Effect",reverse=TRUE)) + my_theme
ggsave('all_articles.png')

# Load Paper Replication Data

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
  


hm <- read_dta("C:/Users/bobku/Documents/R projects/between_effects/hm.dta")
hm <- select(hm,D_pol_int,L_pol_int,L_oilcap_int,D_oilcap_int,L_loggdpcap_int,L_civilwar_int,L_regdiffuse,L_worlddiffuse,
             D_loggdpcap_int,D_regdiffuse,D_worlddiffuse,year,hmccode) %>% filter(complete.cases(.)) %>% 
  mutate(year=factor(year),hmccode=factor(hmccode))

# Histograms

p1 <- ggplot(oneyear,aes(x=fhpolrigaug)) + geom_bar(fill='blue') + my_theme + xlab('Freedom House') + ylab('Observations')
p2 <- ggplot(hm,aes(x=D_pol_int)) + geom_bar(fill='blue',width=0.5) + my_theme + xlab('Polity IV') + ylab('')
outplot <- grid.arrange(p1,p2,ncol=2)
ggsave(plot=outplot,filename='acemoglu_haber_count.png',height=2,units='in')
# Build heatmap of A&R coefficients

five_year_2way <- fiveyear %>% filter(time_balance>1 & panel_balance>1)

# Let's see how the 2-way FE coefficient changes as we add/drop countries
fe_grid <- expand.grid(unique(five_year_2way$country),unique(five_year_2way$year))
fe_grid <- data_frame(country=c(as.character(fe_grid$Var1),unique(five_year_2way$country),rep('star wars',length(unique(five_year_2way$year)))),
                      year=c(as.character(fe_grid$Var2),rep(9999999,length(unique(five_year_2way$country))),unique(five_year_2way$year)))


start_time <- Sys.time()
over_countries_dv <- lapply(1:nrow(fe_grid),function(i) {
  counter <- i %% 100
  this_country <- fe_grid$country[i]
  this_year <- fe_grid$year[i]
  this_reg <- filter(five_year_2way,country!=this_country,year!=as.numeric(this_year))
  this_model <- coef(lm(data=this_reg,formula=fhpolrigaug~ l1_fhpolrigaug + l1_lrgdpch+ factor(year) + factor(country)))['l1_lrgdpch']
  if(counter==0) {
    
    current_time_ratio <- difftime(Sys.time(),start_time,units='hours')/i
    remaining_units <- (nrow(fe_grid) - i)*current_time_ratio
    print(paste0('Finished row ',i,' time remaining: ',round(remaining_units,2),' hours\n'))
  }
  return(this_model)
})
over_countries_dv <- unlist(over_countries_dv)

fe_grid %<>%  mutate(coefs=over_countries_dv,year=ifelse(year=='9999999',NA,year))

fe_grid %>% 
  ggplot(aes(x=year,y=country,fill=coefs)) + geom_tile() + theme_minimal() + theme(axis.text.y = element_blank()) +
  xlab('Years') + ylab('Countries') + labs(fill='Estimates')
ggsave(filename='ar_decomp.png',width=6,units='in')

# Look at only the US and Canada from A&R

# Pull line graph

filter(fiveyear,country %in% c('United States','Canada')) %>% 
  gather(variables,estimates,fhpolrigaug,l1_lrgdpch) %>% 
  mutate(variables=if_else(variables=='fhpolrigaug','FH Score','Log GDP Per Capita')) %>% 
  ggplot(aes(y=estimates,x=year)) + geom_line() + facet_wrap(~country + variables,scales='free') + my_theme +
  xlab("") + ylab('')

ggsave('ar_twocountry_desc.png')

# Run experiment on DV

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

ggsave('ar_simulate.png',width=6,height=3,units='in')

# Perform leave-one-out cross-validation on H&M and A&R

        
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

# Now analyze V-DEM data


dbcon <- "vdem_data.sqlite"
varnames <- paste0('V',2:900)

# Load posterior estimates of the V-DEM indices
# These are from the MCMC estimates of the V-DEM IRT model, and we iterate over them to account for measurement
# uncertainty

# If this is the first time running this code, run the download_vdem() function first to download the requisite data

# One-way case effect

model1 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=1,dbcon=dbcon) %>% mutate(model_type="GDP Case Effects")

# One-way time effect

model2 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor),select_vars=c('e_migdppcln','year_factor'),
                   num_iters=900,
                   num_cores=1,dbcon=dbcon)  %>% mutate(model_type="GDP Time Effects")

# 2-way FEs

model3 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + country_name,
                   select_vars=c('e_migdppcln','year_factor','country_name'),
                   num_iters=900,
                   num_cores=1,dbcon=dbcon)  %>% mutate(model_type="GDP Two-way Effects")

# Try a few more exotic models

# One-way time FE that varies over time


model4 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + factor(year_factor) + e_migdppcln*year_factor,select_vars=c('e_migdppcln','year_factor'),
                   num_iters=900,
                   num_cores=1,dbcon=dbcon)  %>% mutate(model_type="GDP Interactive Time Effects")


# One-way case FE that varies between countries

model5 <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + country_name + e_migdppcln*country_name,select_vars=c('e_migdppcln','country_name'),
                   num_iters=900,
                   num_cores=1,dbcon=dbcon) %>% mutate(model_type="GDP Interactive Case Effects")

#Fuel income specification

model_oilcase <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + country_name,select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','country_name'),
                          num_iters=900,
                          num_cores=1,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Case Effects")


model_oiltime <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor),select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor'),
                          num_iters=900,
                          num_cores=1,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Time Effects")
model_oiltwoway <- run_vdem(varnames=varnames,full_formula=v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + factor(year_factor) + country_name,
                            select_vars=c('e_migdppcln','e_Total_Fuel_Income_PC','year_factor','country_name'),
                            num_iters=900,
                            num_cores=1,dbcon=dbcon) %>% mutate(model_type="GDP-Oil Two-way Effects")

combined_all <- bind_rows(model_oilcase,model_oiltime,model_oiltwoway,model1,model2,model3,model4,model5)
saveRDS(combined_all,file = 'all_models.rds')

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

combined_fx <- left_join(int_effects,country_effects,by='country')  %>% 
  ggplot(aes(x=coef,y=reorder(country,coef))) + geom_point() + 
  my_theme +   theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  geom_text(aes(label=country),hjust='outward',vjust='inward',check_overlap=TRUE) + ylab('') + xlab('Log GDP Effect on Democracy') +
  geom_errorbarh(aes(xmin=lower,xmax=upper),alpha=0.5)  + geom_vline(aes(xintercept=mean(coef)),linetype=2)
ggsave('withinbetween.png',width=10,height=6,units='in')

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