# Vdem un-unused code

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


# save models as RDS files

saveRDS(output_model1,'data/model1.rds')
saveRDS(output_model2,'data/model2.rds')
saveRDS(output_model3,'data/model3.rds')
saveRDS(output_model4,'data/model4.rds')
saveRDS(output_model5,'data/model5.rds')
saveRDS(output_model6_unmi,'data/model6.rds')
saveRDS(output_model7_unmi,'data/model7.rds')
saveRDS(model8,'data/model8.rds')
saveRDS(model9,'data/model9.rds')
saveRDS(model10,'data/model10.rds')
saveRDS(model11,'data/model11.rds')
saveRDS(model12,'data/model12.rds')


twoways <- list(model8,model9,model10,model11,model12)
twoways <- lapply(twoways,function(x) data_frame(betas=row.names(x),coef=apply(x,1,mean),
                                                 sd=apply(x,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd))
lapply(twoways,filter,betas=='e_migdppcln')

oilmodels  <- list(model_oilcase,model_oiltime)
oilmodels <- lapply(oilmodels,function(x) data_frame(betas=row.names(x),coef=apply(x,1,mean),
                                                     sd=apply(x,1,sd),upper=coef + 1.96*sd,lower=coef - 1.96*sd))
oilmodels <- lapply(oilmodels, function(x) x <- x %>% filter(betas %in% c('e_migdppcln','e_Total_Fuel_Income_PC')))