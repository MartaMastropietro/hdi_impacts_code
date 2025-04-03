### tries with gni and compare with gdp from dose 

rm(list=ls())

source("utils/libraries.R")

### output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/gni_gdp_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_original_comp_climate_1990_2020_less_na.csv")
data_pop<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

### data gdp dose, in right time 
data_dose<- read_csv("data/gdp_data/preprocesed_dose/data_gdp_dose_climate_1979_2019.csv")
data_dose_small<-data_dose[which(data_dose$year<=2020 & data_dose$year >= 1990 ), ]

#gdl_shape_file <- sf:: st_read("data/hdi_data/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
#
#gdl_shape_file<-gdl_shape_file[,c(1,4)]
#gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

fvcoviso_gni<-function(x) vcov(x, cluster~iso3)
fvcoviso_gdp<-function(x) vcov(x, cluster~GID_0)

##############################################################################

### try simple burke 2015 , area weight

### gnipc
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2   ", "|" , idx ))
mod_b_gni<-fixest::feols(f, data, panel.id=pan_id)

### gnipc in reduced number of countries
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2   ", "|" , idx ))
mod_b_gni_reduced_countries<-fixest::feols(f, data[which(data$iso3 %in% unique(data_dose$GID_0)),] , panel.id=pan_id)


### dose data

out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2   ", "|" , idx ))

mod_b_gdp<-fixest::feols(f, data_dose, panel.id=pan_id)

## reduced at 1990-2020
out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2   ", "|" , idx ))

mod_b_gdp_reduced_years<-fixest::feols(f, data_dose_small, panel.id=pan_id)



models<-list(mod_b_gni=mod_b_gni, mod_b_gdp=mod_b_gdp, mod_b_gni_reduced_countries=mod_b_gni_reduced_countries, mod_b_gdp_reduced_years=mod_b_gdp_reduced_years )
vcovs<-list(fvcoviso_gni(mod_b_gni), fvcoviso_gdp(mod_b_gdp), fvcoviso_gni(mod_b_gni_reduced_countries), fvcoviso_gdp(mod_b_gdp_reduced_years))
#summary classic
modelsummary(models,vcov=vcovs , estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "burke_mod_tp_gni_gdp_iso.html"))


################################################################################

### try diff , area weight
right_form<-"RR + RR_2+ TM+ TM_2 +  diff_TM + diff_RR+ diff_TM:TM + diff_RR:RR + lag_diff_TM + 
lag_diff_TM + lag_diff_RR + lag_diff_TM:TM + lag_diff_RR:RR  "

### full idx spec

### gnipc
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni<-fixest::feols(f, data, panel.id=pan_id)

### gnipc in reduced number of countries
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni_reduced_countries<-fixest::feols(f, data[which(data$iso3 %in% unique(data_dose$GID_0)),] , panel.id=pan_id)


### dose data

out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))

mod_b_gdp<-fixest::feols(f, data_dose, panel.id=pan_id)

## reduced at 1990-2020
out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~",right_form, "|" , idx ))

mod_b_gdp_reduced_years<-fixest::feols(f, data_dose_small, panel.id=pan_id)


models<-list(mod_b_gni=mod_b_gni, mod_b_gdp=mod_b_gdp, mod_b_gni_reduced_countries=mod_b_gni_reduced_countries, mod_b_gdp_reduced_years=mod_b_gdp_reduced_years )
vcovs<-list(fvcoviso_gni(mod_b_gni), fvcoviso_gdp(mod_b_gdp), fvcoviso_gni(mod_b_gni_reduced_countries), fvcoviso_gdp(mod_b_gdp_reduced_years))
#summary classic


modelsummary(models,estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "diff_mod_tp_gni_gdp.html"))
modelsummary(models,vcov=vcovs , estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "diff_mod_tp_gni_gdp_iso.html"))

### reduced idx 

### gnipc
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni<-fixest::feols(f, data, panel.id=pan_id)

### gnipc in reduced number of countries
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year  "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni_reduced_countries<-fixest::feols(f, data[which(data$iso3 %in% unique(data_dose$GID_0)),] , panel.id=pan_id)


### dose data

out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year  "
f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))

mod_b_gdp<-fixest::feols(f, data_dose, panel.id=pan_id)

## reduced at 1990-2020
out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year  "
f <- as.formula( paste(out_var ,"~",right_form, "|" , idx ))

mod_b_gdp_reduced_years<-fixest::feols(f, data_dose_small, panel.id=pan_id)


models<-list(mod_b_gni=mod_b_gni, mod_b_gdp=mod_b_gdp, mod_b_gni_reduced_countries=mod_b_gni_reduced_countries, mod_b_gdp_reduced_years=mod_b_gdp_reduced_years )
vcovs<-list(fvcoviso_gni(mod_b_gni), fvcoviso_gdp(mod_b_gdp), fvcoviso_gni(mod_b_gni_reduced_countries), fvcoviso_gdp(mod_b_gdp_reduced_years))
#summary classic


modelsummary(models,estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "simple_idx_diff_mod_tp_gni_gdp.html"))
modelsummary(models,vcov=vcovs , estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "simple_idx_diff_mod_tp_gni_gdp_iso.html"))

 
################################################################################

### try diff + extremes, area weight
right_form<-"RR + RR_2+  diff_TM  + diff_TM:TM +  lag_diff_TM + lag_diff_TM:TM + 
RX + RX_2 + WD + WD_2 + PEXT + TM:PEXT + TVAR+ TM:TVAR"

### full idx

### gnipc
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni<-fixest::feols(f, data, panel.id=pan_id)

### gnipc in reduced number of countries
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year + gdlcode[year]+ gdlcode[year^2] "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni_reduced_countries<-fixest::feols(f, data[which(data$iso3 %in% unique(data_dose$GID_0)),] , panel.id=pan_id)


### dose data

out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))

mod_b_gdp<-fixest::feols(f, data_dose, panel.id=pan_id)

## reduced at 1990-2020
out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year + GID_1[year]+GID_1[year^2] "
f <- as.formula( paste(out_var ,"~",right_form, "|" , idx ))

mod_b_gdp_reduced_years<-fixest::feols(f, data_dose_small, panel.id=pan_id)


models<-list(mod_b_gni=mod_b_gni, mod_b_gdp=mod_b_gdp, mod_b_gni_reduced_countries=mod_b_gni_reduced_countries, mod_b_gdp_reduced_years=mod_b_gdp_reduced_years )
vcovs<-list(fvcoviso_gni(mod_b_gni), fvcoviso_gdp(mod_b_gdp), fvcoviso_gni(mod_b_gni_reduced_countries), fvcoviso_gdp(mod_b_gdp_reduced_years))

#summary classic
modelsummary(models,estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "diff_mod_extr_gni_gdp.html"))
modelsummary(models,vcov=vcovs , estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "diff_mod_extr_gni_gdp_iso.html"))

### simple idx

### gnipc
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year  "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni<-fixest::feols(f, data, panel.id=pan_id)

### gnipc in reduced number of countries
out_var="gr_gnipc"  
pan_id<-c('gdlcode', 'year')
idx<- "gdlcode + year "

f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))
mod_b_gni_reduced_countries<-fixest::feols(f, data[which(data$iso3 %in% unique(data_dose$GID_0)),] , panel.id=pan_id)


### dose data

out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year"
f <- as.formula( paste(out_var ,"~", right_form, "|" , idx ))

mod_b_gdp<-fixest::feols(f, data_dose, panel.id=pan_id)

## reduced at 1990-2020
out_var="dlgrp_pc_usd"  
pan_id<-c('GID_1', 'year')
idx<- "GID_1 + year  "
f <- as.formula( paste(out_var ,"~",right_form, "|" , idx ))

mod_b_gdp_reduced_years<-fixest::feols(f, data_dose_small, panel.id=pan_id)


models<-list(mod_b_gni=mod_b_gni, mod_b_gdp=mod_b_gdp, mod_b_gni_reduced_countries=mod_b_gni_reduced_countries, mod_b_gdp_reduced_years=mod_b_gdp_reduced_years )
vcovs<-list(fvcoviso_gni(mod_b_gni), fvcoviso_gdp(mod_b_gdp), fvcoviso_gni(mod_b_gni_reduced_countries), fvcoviso_gdp(mod_b_gdp_reduced_years))

# summary classic
modelsummary(models,estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "simple_idx_diff_mod_extr_gni_gdp.html"))
modelsummary(models,vcov=vcovs , estimate = "{estimate}{stars}",fmt = fmt_sprintf("%.5e"),statistic = NULL, gof_omit = c("Std.Errors") ,output=file.path(out_dir, "simple_idx_diff_mod_extr_gni_gdp_iso.html"))

################################################################################

### distributions

hist(data$gr_gnipc)
data_small<-data[which(data$iso3 %in% unique(data_dose$GID_0)),]
hist(data$gr_gnipc[which(data$iso3 %in% unique(data_dose$GID_0))] )

setdiff(unique(data$iso3 ),unique(data_dose$GID_0) )

hist(data_dose$dlgrp_pc_usd)

hist(data_dose$dlgrp_pc_usd_2015)

################################################################################

# setup for lag models 
data<-data_dose
pan_id<-c('GID_1', 'year')

varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

adap=rep('lgrp_pc_usd', 10)

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~GID_1+year")), i, data)
    
  }
}

# mean hist climate

data <- data %>% group_by(GID_1) %>% mutate(
  TM_mean=mean(TM), 
  RR_mean=mean(RR), 
  TVAR_mean=mean(TVAR), 
  HW_mean=mean(HW), 
  RX_mean=mean(RX), 
  PEXT_mean=mean(PEXT), 
  WD_mean=mean(WD), 
  SPI_mean=mean(SPI), 
  SPEI_mean=mean(SPEI), 
  PET_mean=mean(PET), 
)

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")

for (i in 1:length(varns)){
  data[paste(m_modns[i],'_i_',varns[i],sep='')] <- data[m_modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns[v],'_i_',varns[v],sep=''), "~GID_1+year")), i, data)
  }
}



# n lags 
N_temp<-8
N_rain<-8
N<-8

v_temp<-which(varns=="diff_TM")
v_rain<-which(varns=="diff_RR")
v_tvar<-which(varns=="diff_TVAR")
v_hw<-which(varns=="diff_HW")
v_rx<-which(varns=="diff_RX")
v_pext<-which(varns=="diff_PEXT")
v_wd<-which(varns=="diff_WD")
v_spi<-which(varns=="diff_SPI")
v_spei<-which(varns=="diff_SPEI")


### each complete mod with/without control and autoreg 
### model specification with mean controls

spec<-"mean_mod_spec"
pattern<-"conflict|exp_edu|exp_health|trade|lag_gr" # to discard coefficients rows correspondent to controls, autoreg part 

save_model<-function(m, pattern, o, type, spec, out_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  table<-rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  write.csv(table, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coeftable.csv')) )
  write.csv(coefs, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coef.csv')) )
  write.csv(cov, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_cov.csv')) )
}

### formula part of temp, rain controls 
mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
mean_cl_adap_formula<-paste0(paste0("lag_",1:N_temp,"_",adap[v_temp],'_i_',varns[v_temp], collapse = "+"),"+",
                             paste0("lag_",1:N_rain,"_",adap[v_rain],'_i_',varns[v_rain], collapse = "+"))



o<-"dlgrp_pc_usd"

extr_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_pext], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_pext],'_i_',varns[v_pext], collapse = "+"),"+",
                     paste0(varns[v_spi]), "+",
                     paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                     paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                     paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_rx], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_rx],'_i_',varns[v_rx], collapse = "+"), "+",
                     paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"), "+",
                     paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:N,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_pext],'_i_',varns[v_pext], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_rx],'_i_',varns[v_rx], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_wd],'_i_',varns[v_wd], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_tvar],'_i_',varns[v_tvar], collapse = "+"))

### all with these fe spec 
i1 <- "GID_1 + year + GID_1[year] +GID_1[year^2]"
i2 <- "GID_1 + year"
ids<-c(i1,i2)

### complete
for (i in ids){
  if(i == i1){out_dir_i<-file.path(out_dir, "complete_id") }
  if(i == i2){out_dir_i<-file.path(out_dir, "simple_id")}
  
  if(!dir.exists(out_dir_i)){dir.create(out_dir_i)}
  models<-list()
  
  # oaat, moderating with longrun extreme itself 
  for (v_ext in c(v_pext, v_rx, v_hw, v_spi)){
    r=paste0(mean_cl_formula,"+", 
             # part of extremes
             paste0(varns[v_ext]), "+",
             paste0(m_modns[v_ext], "_i_", varns[v_ext]),"+", 
             paste0("lag_",1:N,"_",varns[v_ext], collapse = "+"),"+", 
             paste0("lag_",1:N,"_",m_modns[v_ext],'_i_',varns[v_ext], collapse = "+")
    )
    
    f= as.formula(paste( o, "~", r, "|" ,i ))
    m = fixest::feols(f, data , panel.id = pan_id)
    models[[length(models)+1]]<-m
  }
  
  
  ### all, moderating with extr
  type<-"all_extr"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, out_dir_i)
  
  
  
  ### all, moderating with extr + ADAP
  type<-"all_extr_adap"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # adap
           mean_cl_adap_formula,"+",
           extr_adap_formula
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, out_dir_i)
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_i, paste0(o,"_", spec , "_lags_models.html")))
  #modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcoviso_gdp), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_i, paste0(o,"_", spec ,"_lags_models_iso.html")))
  
}



