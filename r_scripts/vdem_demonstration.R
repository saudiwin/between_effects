# This is a demonstration of 2-way and one-way FX analysis using data provided by the varities of democracy
# Robert Kubinec 7/12/2016

require(haven)
require(dplyr)
require(magrittr)
require(sandwich)
require(ggplot2)

#Run once, use a faster R version
#full_dataset <- read_dta("C:/Users/bobku/Box Sync/Between Effects/V-DEM/Country_Year_V-Dem_other_STATA_v6.1/V-Dem-DS-CY+Others-v6.1.dta")
#full_dataset <- saveRDS(object = full_dataset,"data/vdem_full.rds")

full_dataset <- readRDS('data/vdem_full.rds')

full_dataset$year_factor <- as.factor(full_dataset$year)

models <- c('Within','Between','Two-way')

# Within effect

model1 <- lm(full_dataset,formula = v2x_polyarchy ~ e_migdppcln + country_name)
coef_model1 <- coef(model1)['e_migdppcln']
# Between effect

model2 <- lm(full_dataset,formula = v2x_polyarchy ~ e_migdppcln + year_factor)
coef_model2 <- coef(model2)['e_migdppcln']
# 2-way FX

model3 <- lm(full_dataset,formula = v2x_polyarchy ~ e_migdppcln + year_factor + country_name)
coef_model3 <- coef(model3)['e_migdppcln']

all_coefs <- c(coef_model1,coef_model2,coef_model3)
names(all_coefs) <- models
# All three are statistically significant, the within effect is the largest

# Use sandwich estimator on the covariance matrices

true_sds <- list(vcovHC(model1,type="HC0"),vcovHC(model2,type='HC0'),vcovHC(model3,type='HC0'))
true_sds <- lapply(true_sds,function(x) sqrt(diag(x)))
true_sds <- sapply(true_sds,function(x) x['e_migdppcln'])
names(true_sds) <- models

# Combine estimates

estimates <- tbl_df(data.frame(Beta_GDP=all_coefs,SE=true_sds,Upper_CI=all_coefs + (1.96*true_sds),Lower_CI=all_coefs - (1.96*true_sds)))

print(estimates)


# Try a few more exotic models

# Between effect that varies over time
# Note that this model implicitly accounts for autocorrelation

model4 <- lm(full_dataset,formula = v2x_polyarchy ~ e_migdppcln + year_factor + e_migdppcln*year_factor)
coef_model4 <- coef(model4)[grepl("e_migdppcln",names(coef(model4)))]
sds_model4 <- vcovHC(model4,type='HC0')
sds_model4 <- sqrt(diag(sds_model4))
sds_model4 <- sds_model4[grepl("e_migdppcln",names(sds_model4))]
# Within effect that varies between

model5 <- lm(full_dataset,formula = v2x_polyarchy ~ e_migdppcln + country_name + e_migdppcln*country_name)
coef_model5 <- coef(model5)[grepl("e_migdppcln",names(coef(model5)))]
sds_model5 <- vcovHC(model5,type='HC0')
sds_model5 <- sqrt(diag(sds_model5))
sds_model5 <- sds_model5[grepl("e_migdppcln",names(sds_model5))]