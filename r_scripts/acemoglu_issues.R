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
require(parallel)
fiveyear <- read_dta("Acemoglu one year panel.dta")
oneyear <- read_excel("Income and Democracy Data AER adjustment.xls", 
sheet = "Annual Panel")

# Panel Balance

fivey_cbalance <- fiveyear %>% group_by(country) %>% filter(!(is.nan(fhpolrigaug)) & !(is.nan(lrgdpch))) %>% 
  summarize(panel_balance=n())
fivey_tbalance <- fiveyear %>% group_by(year) %>% filter(!(is.nan(fhpolrigaug)) & !(is.nan(lrgdpch))) %>% 
  summarize(time_balance=n())
oney_balance <- oneyear %>% group_by(country) %>% filter(!(is.na(fhpolrigaug)) & !(is.na(lrgdpch))) %>% 
  summarize(panel_balance=n())

gplot <- fivey_cbalance %>% ggplot(aes(y=panel_balance,x=reorder(country,panel_balance))) + theme_minimal() + 
  geom_bar(stat='identity') + xlab('country') + ylab('count')
ggplotly(gplot)

gplot <- oney_balance %>% ggplot(aes(y=panel_balance,x=reorder(country,panel_balance))) + theme_minimal() + 
  geom_bar(stat='identity') + xlab('country') + ylab('count')
ggplotly(gplot)


# Add lag DVs/IVs
# For the five year panel, the t-1 lag is really a five-year lag
fiveyear <- fiveyear %>% group_by(country) %>% mutate(l1_fhpolrigaug=lag(fhpolrigaug,order_by=year),
                                                           l1_lrgdpch=lag(lrgdpch,order_by=year))
fiveyear <- left_join(fiveyear,fivey_cbalance) %>% left_join(fivey_tbalance,by='year') %>% filter(!is.na(panel_balance)) %>% 
  select(matches('fhpol|lrgd'),country,year,panel_balance,time_balance)

oneyear <- oneyear %>% group_by(country) %>% mutate(l1_fhpolrigaug=lag(fhpolrigaug,order_by=year),
                                                           l1_lrgdpch=lag(lrgdpch,order_by=year)) %>% 
  filter(!(is.na(fhpolrigaug)) & !(is.na(lrgdpch))) %>% mutate(panel_balance=n())


# Examine 2-way FE and 1-way FE with or without LDVs

# First, with LDVs as in Acemoglu specification
# We'll just use 5-year panel data from table 2

# 1-way case
coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch + l1_fhpolrigaug + factor(country)))['l1_lrgdpch']

# 1-way time

coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch + l1_fhpolrigaug + factor(year)))['l1_lrgdpch']

# 2-way -- you can see that the coefficient is .001 which is less than either .06 or .07

coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch + l1_fhpolrigaug + factor(year) + factor(country)))['l1_lrgdpch']

# Now without LDVs

# 1-way case
coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch + factor(country)))['l1_lrgdpch']

# 1-way time

coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch + factor(year)))['l1_lrgdpch']

# 2-way -- you can see that the coefficient is .061 which is between case FE (.058) and time FE (.22)
# Given that we expect there to be more auto-correlation in the cases, the collapse of the 2-way FE
# To cases would make sense
coef(lm(data=fiveyear,formula=fhpolrigaug~ l1_lrgdpch+ factor(year) + factor(country)))['l1_lrgdpch']




country_reg <- function(x) {
  output <- try(lm(formula=fhpolrigaug ~ l1_lrgdpch,data=x))
  if(class(output)=='try-error') {
    return(0)
  } else {
    return(output)
  }
}
time_reg <- function(x) {
  output <- try(lm(formula=fhpolrigaug ~ l1_lrgdpch,data=x))
  if(class(output)=='try-error') {
    return(0)
  } else {
    return(output)
  }
}

twoway_reg <- function(x) {
  output <- try(lm(formula=fhpolrigaug ~ l1_lrgdpch,data=x))
  if(class(output)=='try-error') {
    return(0)
  } else {
    return(output)
  }
}

five_year_countries <- fiveyear %>% filter(panel_balance>2)
five_year_time <- fiveyear %>% filter(time_balance>35)
five_year_2way <- fiveyear %>% filter(time_balance>1 & panel_balance>1)
regs_over_countries <- five_year_countries %>% group_by(country) %>% nest %>% mutate(linmods=map(data,country_reg))
regs_over_time <- five_year_time %>% group_by(year) %>% nest %>% mutate(linmods=map(data,country_reg))
#regs_over_2way <- five_year_2way %>% group_by(country,year) %>% nest %>% mutate(linmods=map(data,country_reg))

country_resids <- regs_over_countries %>% 
  mutate(
    resids = try(map2(data, linmods, add_residuals))
  ) %>% unnest(resids)

country_resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(alpha = 1 / 3) + 
 facet_wrap(~country,scales="free") +theme_minimal() + theme(axis.text.x=element_blank())

ggsave(filename = 'country_resids.png',width = 20,height=10,units='in')

year_resids <- regs_over_time %>% 
  mutate(
    resids = try(map2(data, linmods, add_residuals))
  ) %>% unnest(resids)

year_resids %>% 
  ggplot(aes(country, resid)) +
  geom_point() +theme_minimal() +facet_wrap(~year) + theme(axis.text.x = element_blank())

ggsave(filename = 'year_resids.png',width = 10,height=7,units='in')

## Look at the model matrix

inspect_2way <- model_matrix(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(year) + factor(country))
inspect_2way <- gather(inspect_2way,variables,dummies,matches('factor'))
examine_2way <- group_by(inspect_2way,variables) %>% summarize(obs=sum(dummies)) %>% mutate(FE_type=stringr::str_extract(variables,'\\(.+\\)'))
coefs_2way <- broom::tidy(lm(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(year) + factor(country)))
coefs_2way <- left_join(coefs_2way,examine_2way,by=c('term'='variables'))
coefs_2way %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=estimate,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth(method='lm')
coefs_2way %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=std.error,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth()

inspect_1way_case <- model_matrix(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(country))
inspect_1way_case <- gather(inspect_1way_case,variables,dummies,matches('factor'))
examine_1way_case <- group_by(inspect_1way_case,variables) %>% summarize(obs=sum(dummies)) %>% mutate(FE_type=stringr::str_extract(variables,'\\(.+\\)'))
coefs_1way_case <- broom::tidy(lm(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(country)))
coefs_1way_case <- left_join(coefs_1way_case,examine_1way_case,by=c('term'='variables'))
coefs_1way_case %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=estimate,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth(method='lm')
coefs_1way_case %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=std.error,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth()

inspect_1way_time <- model_matrix(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(year))
inspect_1way_time <- gather(inspect_1way_time,variables,dummies,matches('factor'))
examine_1way_time <- group_by(inspect_1way_time,variables) %>% summarize(obs=sum(dummies)) %>% mutate(FE_type=stringr::str_extract(variables,'\\(.+\\)'))
coefs_1way_time <- broom::tidy(lm(data=oneyear,formula=fhpolrigaug ~ l1_fhpolrigaug + l1_lrgdpch + factor(year)))
coefs_1way_time <- left_join(coefs_1way_time,examine_1way_time,by=c('term'='variables'))
coefs_1way_time %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=estimate,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth(method='lm')
coefs_1way_time %>% filter(!is.na(FE_type)) %>% ggplot(aes(y=std.error,x=obs)) + facet_wrap(~FE_type,scales='free') + geom_point() + theme_minimal() + stat_smooth()

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

data1 <-  filter(oneyear,country %in% c('United States')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data1,formula=fhpolrigaug~ lrgdpch + factor(year)))['lrgdpch']
coef(lm(data=data1,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data2 <-  filter(oneyear,country %in% c('Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data2,formula=fhpolrigaug~ lrgdpch + factor(year)))['lrgdpch']
coef(lm(data=data2,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3$fhpolrigaug[1:7] <- runif(length(1:7),2,2.01)
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']

data3 <-  filter(oneyear,country %in% c('United States','Canada')) %>% select(country,year,fhpolrigaug,lrgdpch,l1_fhpolrigaug,l1_lrgdpch)
data3$fhpolrigaug[34:41]  <- runif(length(108:125),2,2.01)


coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=data3,formula=fhpolrigaug~ lrgdpch))['lrgdpch']



out_2way_US <- rep(1:100)
out_1way_Trump <- rep(1:100)
for(i in seq(0,15,length.out=100)) {
  years <- rep(1:100,2)
  countries <- rep(c('US','Trumptocracy'),each=100)
  gdp_US <- rnorm(100,i,1)
  gdp_Trump <- rnorm(100,10,1)
  outcome <- runif(200,-1,1)
  test_data <- data_frame(years=factor(years),countries=factor(countries),gdp=c(gdp_US,gdp_Trump),outcome)
  

out_2way_US[i] <- coef(lm(formula=outcome~gdp + years + countries,data=test_data))['gdp']
out_1way_Trump[i] <- coef(lm(formula=outcome~gdp + countries,data=test_data))['gdp']

out_2way_US[i] <- coef(lm(formula=outcome~gdp + years + countries,data=test_data))['gdp']
out_1way_Trump[i] <- coef(lm(formula=outcome~gdp + countries,data=test_data))['gdp']
}
test_theory <- oneyear %>% group_by(country) %>% summarize(mean_gdp=mean(lrgdpch,na.rm=TRUE)) %>% 
  filter(!is.nan(mean_gdp) | !is.na(mean_gdp)) %>% mutate(mean_lag_gdp=lag(mean_gdp),diff=mean_gdp-mean_lag_gdp)

data_canada <- filter(oneyear,country %in% c('Canada'))

data_US <- filter(oneyear,country %in% c('United States'))

data_UK <- filter(oneyear,country %in% c('United Kingdom'))

coef(lm(data=bind_rows(data_canada,data_US,data_UK),formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
coef(lm(data=bind_rows(data_UK,data_US,data_canada),formula=fhpolrigaug~ lrgdpch+ factor(year) + factor(country)))['lrgdpch']
