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

data(vdem_data)
data(vdem_pos)

full_dataset <- vdem_data
full_dataset$year_factor <- as.factor(full_dataset$year)

#Helper Functions

to_round <- function(colvar,sigdif) {
  outputvar <- format(round(colvar,digits=sigdif),nsmall=sigdif)
  return(outputvar)
}
prepare_for_display <- function(x) {
  x <- filter(x, betas %in% control_vars) %>% mutate_if(is.numeric,to_round,sigdif=3) %>% mutate(TCI=paste0("(",lower,",",upper,")"))
  x <-  x %>%  select(betas,coef,TCI) %>% gather(value_type,to_display,-betas) %>% unite(row_value,betas,value_type)
  return(x)
}


# Descriptive Statistics
# For certain countries, calculate the two-way effect as a ratio of within and between effects (in Y and X)

x_b_bar <- filter(full_dataset,year>2000 & year < 2011) %>% group_by(.,year) %>% summarize(.,x_b_bar=mean(e_migdppcln,na.rm=TRUE))
y_i_bar <- filter(full_dataset,year>2000 & year < 2011) %>% group_by(.,country_name) %>% summarize(.,y_i_bar=mean(v2x_polyarchy,na.rm=TRUE))
x_i_bar <- filter(full_dataset,year>2000 & year < 2011) %>% group_by(.,country_name) %>% summarize(.,x_i_bar=mean(e_migdppcln,na.rm=TRUE))
full_dataset <- mutate(full_dataset,x_raw=e_migdppcln,
                       y_raw=v2x_polyarchy)
full_dataset <- full_dataset %>% left_join(.,x_b_bar,by='year') %>% left_join(.,x_i_bar,by='country_name') %>%
                left_join(.,y_i_bar,by='country_name')

# Output particular countries and save as an xtable in latex format
to_keep <- c('United States',"Ukraine",'Venezuela')

output_tables <- full_dataset %>% filter(country_name %in% to_keep,year>2000 & year < 2011) %>% 
                                      select(country_name,year,x_raw,x_i_bar,x_b_bar,
                                      y_raw,y_i_bar) %>% group_by(country_name)  %>%
                                      mutate(top1=x_raw - x_b_bar,top2=y_raw-y_i_bar,top_full=sum(top1*top2),
                                             bottom1=x_raw - x_i_bar,bottom2=x_raw-x_b_bar,bottom_full=sum(bottom1*bottom2),
                                             two_way_FX=top_full/bottom_full)

output_tables <- output_tables %>% do(tables=xtable(x=.))

descriptives <- full_dataset %>% select(e_migdppcln,v2x_polyarchy,e_WORLD_DEM_DIFFUSE,e_cap_share_unequal,e_Total_Fuel_Income_PC,
                                             e_miferrat,e_Civil_War)

xtable(descriptives)

# Load posterior estimates of the V-DEM indices

vdem_pos <- paste0(data_loc,'/vdem_pos.rds') %>% readRDS %>% as.data.frame %>% as.tbl
vdem_pos$V1 <- NULL

# combined posterior estimates with posterior uncertainty
merged_data <- left_join(full_dataset,vdem_pos,by=c('country_text_id','year'))
to_analyze <- merged_data[,c('e_migdppcln','year_factor','country_name')]

varnames <- paste0('V',2:900)



over_posterior <- function(x,y) {
  to_analyze$v2x_polyarchy <- merged_data[[x]]
  model1 <- lm(formula = y,data = to_analyze)
  # Use the sandwich estimator to adjust variances
  # Need to drop coefs that come out as NA
  # This happens with the country of the Republic of Vietnam in the within-between model (model 5)
  # The sandwich estimator will automatically drop NA coefficients from the VCOV matrix, causing an error
  # With MASS
  coefs <- coef(model1)[!is.na(coef(model1))]
  sds <- vcovHC(model1,type='HC0')
  
  samples <- MASS::mvrnorm(mu=coefs,Sigma=sds)
  return(samples)
  }

# One-way case effect

model1 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + country_name)
output_model1 <- data_frame(betas=row.names(model1),coef=apply(model1,1,mean),
                        sd=apply(model1,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)
# One-way time effect

model2 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + year_factor)
output_model2 <- data_frame(betas=row.names(model2),coef=apply(model2,1,mean),
                            sd=apply(model2,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)
# 2-way FEs
  
model3 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + year_factor + country_name)
output_model3 <- data_frame(betas=row.names(model3),coef=apply(model3,1,mean),
                            sd=apply(model3,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)


# All three are statistically significant, the within effect is the largest

# Combine estimates


# Try a few more exotic models

# One-way time FE that varies over time
# Note that this model implicitly accounts for autocorrelation

model4 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + year_factor + e_migdppcln*year_factor)
output_model4 <- data_frame(betas=row.names(model4),coef=apply(model4,1,mean),
                            sd=apply(model4,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)


# One-way case FE that varies between countries

model5 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + country_name + e_migdppcln*country_name)
output_model5 <- data_frame(betas=row.names(model5),coef=apply(model5,1,mean),
                            sd=apply(model5,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)

# Create MI datasets for models with controls because of missing data

# to_impute_year <- merged_data %>% filter(year<2011) %>% 
#                 select(e_migdppcln,year_factor,
#                                  e_Total_Fuel_Income_PC,
#                                  e_miferrat,e_miinteco,e_miinterc,e_miurbpop,e_peaveduc,v2x_polyarchy) %>% as.data.frame
# impute_year_mi <- mi::missing_data.frame(to_impute_year)
# patterns <- impute_year_mi@patterns
# # We can't impute the DV because it comes from a separate model
# to_impute_year <- filter(to_impute_year,!(grepl('v2x_polyarchy',x = patterns)))
# impute_year_mi <- mi::missing_data.frame(to_impute_year)
# # Looks good, go ahead and impute. With 10 variables this won't take too terribly long. Default is 4 chains with 30 iterations
# to_impute_year <- mi::mi(to_impute_year,n.iter=120)
# 
# to_impute_country <- merged_data %>% filter(year<2011) %>% 
#   select(e_migdppcln,country_name,
#          e_Total_Fuel_Income_PC,
#          e_miferrat,e_miurbpop,e_peaveduc,v2x_polyarchy) %>% as.data.frame
# impute_country_mi <- mi::missing_data.frame(to_impute_country)
# patterns <- impute_country_mi@patterns
# to_impute_country <- filter(to_impute_country,!(grepl('v2x_polyarchy',x = patterns)))
# impute_country_mi <- mi::missing_data.frame(to_impute_country)
# impute_country_mi <- mi::change(impute_country_mi,y='v2x_polyarchy',what='type',to='continuous')
# imputed_country <- mi::mi(impute_country_mi,n.iter=120)
#Models with controls

to_analyze <- merged_data[,c('e_migdppcln','year_factor','country_name','e_WORLD_DEM_DIFFUSE',
                             'e_cap_share_unequal','e_Total_Fuel_Income_PC',
                            'e_miferrat','e_miinteco','e_miurbpop','v2x_polyarchy')]
to_analyze <- to_analyze %>% mutate(e_Total_Fuel_Income_PC=e_Total_Fuel_Income_PC/1000,
                                    e_miurbpop=e_miurbpop/1000000,v2x_polyarchy=v2x_polyarchy*100)

model6_unmi <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_WORLD_DEM_DIFFUSE + 
                e_cap_share_unequal + e_Total_Fuel_Income_PC  + e_miurbpop + country_name)
output_model6_unmi <- data_frame(betas=row.names(model6_unmi),coef=apply(model6_unmi,1,mean),
                                 sd=apply(model6_unmi,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)
#model6_mi <- mi::pool()

model7_unmi <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln +
                    e_cap_share_unequal + e_Total_Fuel_Income_PC  + 
                   e_miinteco + e_miurbpop +year_factor)
output_model7_unmi <- data_frame(betas=row.names(model7_unmi),coef=apply(model7_unmi,1,mean),
                                 sd=apply(model7_unmi,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd)

#2-way with controls

model8 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_cap_share_unequal + country_name + year_factor)
model9 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_Total_Fuel_Income_PC + country_name + year_factor)
model10 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_miinteco + country_name + year_factor)
model11 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_WORLD_DEM_DIFFUSE + country_name + year_factor)
model12 <- sapply(varnames,over_posterior,v2x_polyarchy ~ e_migdppcln + e_miurbpop + country_name + year_factor)


twoways <- list(model8,model9,model10,model11,model12)
twoways <- lapply(twoways,function(x) data_frame(betas=row.names(x),coef=apply(x,1,mean),
                                             sd=apply(x,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd))
lapply(twoways,filter,betas=='e_migdppcln')
# Output model summaries to latex tables

# First, the basic models

basic_models <- bind_rows(filter(output_model1,betas=='e_migdppcln'),
                          filter(output_model2,betas=='e_migdppcln'),
                          filter(output_model3,betas=='e_migdppcln')) %>%  
                          mutate(models=c('Between Effect',
                             'Within Effect',
                             'Two-Way Effect')) %>% select(models,coef,sd,upper,lower)

xtable(as.data.frame(basic_models),caption="One-Way and Two-Way Fixed Effects Estimates of GDP on Democracy",label='onetwogdp',
       digits=3,align=c('l','c','c','c','c','c'),display=c('s','f','f','f','f','f'))
control_vars <- c('e_migdppcln','year_factor','country_name','e_WORLD_DEM_DIFFUSE',
                  'e_cap_share_unequal','e_Total_Fuel_Income_PC',
                  'e_miferrat','e_miinteco','e_miurbpop')


withinFX <- prepare_for_display(output_model6_unmi)
betweenFX <- prepare_for_display(output_model7_unmi)
controls_models <- full_join(withinFX,betweenFX,by='row_value') %>% arrange(row_value)
controls_models %>% as.data.frame %>% xtable(caption="GDP on Democracy with Controls",label="gdpcontrols",digits=3,align=c('l','l','c','c'),
                                             display=c('d','s','f','f'))
# Calculate interaction effects and plot

model5 <- readRDS("data/model5.rds") %>% t %>% as.data.frame %>% tbl_df
countries <- select(model5,matches(":country"))
int_matrix <- tbl_df(lapply(countries,function(x) x + model5[['e_migdppcln']]))
results <- data_frame(Coef=sapply(int_matrix,mean), SD = sapply(int_matrix,sd))
results$variables <- str_extract(colnames(countries),'[A-Z].+')
#Drop West Bank because effect is very imprecise
results <- filter(results,variables!='Palestine_West_Bank')
label_type <- as.logical(rbinom(nrow(results),1,0.5))
results <- mutate(results,upper=Coef + 1.96*SD,lower=Coef - 1.96*SD,
                  label_low=ifelse(label_type,NA,variables),
                  label_high=ifelse(!label_type,NA,variables))
ggplot(results,aes(x=Coef,y=reorder(variables,Coef))) + geom_point() + 
 my_theme +   theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  geom_text(aes(label=label_high),hjust=-1.5,vjust='inward',check_overlap=TRUE) + ylab('') + xlab('Log GDP Effect on Democracy') +
  geom_text(aes(label=label_low),hjust=2,check_overlap=TRUE) + geom_vline(xintercept=0,linetype=2) + geom_errorbarh(aes(xmin=lower,xmax=upper),alpha=0.5) 
ggsave('charts/withinbetween.png',width=10,height=6,units='in')

model4 <- readRDS("data/model4_results.rds") %>% t %>% as.data.frame %>% tbl_df
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

ggsave('charts/betweenbetween.png',width=10,height=6,units='in')