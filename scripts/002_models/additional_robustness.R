
rm(list=ls())

source("scripts/003_models/feols_lags_plot_funcs.R")

source("utils/cross_validation_fixest.R")

# xlsx write
require(openxlsx)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)
library(fixest)
library(modelsummary)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)

# output dir
out_dir<-"output/models/final_lag_mods/additional_rob"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_save<-"output/models/final_lag_mods/conservative_N_lags_mix"
if(!dir.exists(out_dir_save)){dir.create(out_dir_save)}

out_dir_final_plots<-file.path(out_dir_save, "plots")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

### data climate
data_cl <- read_csv("data/climate_data/era5/data_climate_gdl_pop_weight_1950_2023.csv")


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')


# mean hist climate
# ten years avg of previous years 

data<-left_join(data_cl, data)

library(slider)
data <- data %>%
  arrange(gdlcode, year) %>%  
  group_by(gdlcode) %>%
  mutate(
    TM_mean = mean(TM),
    RR_mean = mean(RR),
    TVAR_mean = mean(TVAR),
    HW_mean = mean(HW),
    RX_mean = mean(RX),
    PEXT_mean = mean(PEXT),
    WD_mean = mean(WD),
    SPI_mean = mean(SPI),
    SPEI_mean = mean(SPEI),
    PET_mean = mean(PET),
    # Compute 10-year moving averages
    TM_10y_mean = slide_dbl(TM, mean, .before = 9, .complete = TRUE),
    RR_10y_mean = slide_dbl(RR, mean, .before = 9, .complete = TRUE),
    TVAR_10y_mean = slide_dbl(TVAR, mean, .before = 9, .complete = TRUE),
    HW_10y_mean = slide_dbl(HW, mean, .before = 9, .complete = TRUE),
    RX_10y_mean = slide_dbl(RX, mean, .before = 9, .complete = TRUE),
    PEXT_10y_mean = slide_dbl(PEXT, mean, .before = 9, .complete = TRUE),
    WD_10y_mean = slide_dbl(WD, mean, .before = 9, .complete = TRUE),
    SPI_10y_mean = slide_dbl(SPI, mean, .before = 9, .complete = TRUE),
    SPEI_10y_mean = slide_dbl(SPEI, mean, .before = 9, .complete = TRUE),
    PET_10y_mean = slide_dbl(PET, mean, .before = 9, .complete = TRUE)
  )


# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns_mean=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")
m_modns_10y=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean")
m_modns_cont=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")


adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(m_modns_mean[i],'_i_',varns[i],sep='')] <- data[m_modns_mean[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_10y[i],'_i_',varns[i],sep='')] <- data[m_modns_10y[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_cont[i],'_i_',varns[i],sep='')] <- data[m_modns_cont[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 


for (i in 1:length(varns)){ # 
  data[paste("TM_10y_mean",'_i_',varns[i],sep='')] <- data["TM_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_10y_mean",'_i_',varns[i],sep='')] <- data["RR_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 0:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste(varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_mean[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_mean[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_10y[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_10y[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_cont[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_cont[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# go back to right time period -> exclude covid years 

data<-data%>%filter(year>=1990 & year<=2019)


v_temp<-which(varns=="diff_TM")
v_rain<-which(varns=="diff_RR")
v_tvar<-which(varns=="diff_TVAR")
v_hw<-which(varns=="diff_HW")
v_rx<-which(varns=="diff_RX")
v_pext<-which(varns=="diff_PEXT")
v_wd<-which(varns=="diff_WD")
v_spi<-which(varns=="diff_SPI")
v_spei<-which(varns=="diff_SPEI")



################################################################################

### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

pan_id<-c('gdlcode', 'year')


specs<-list("mean_mod")#, "mean_mod_10y", "cont_mod")

out_variables<-c("gr_gnipc", "gr_leb", "gr_eys")

cl_variables<-c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")


pattern<-"conflict|exp_edu|exp_health|trade|lag_gr|gr" # to discard coefficients rows correspondent to contr, autoreg part 


vars_corr<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)", "PET"),
  modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "", "" )
)

vars_corr_10y<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

vars_corr_cont<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

save_model<-function(m, pattern, o, type, spec, NL, out_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  cov_iso=vcov(m, cluster=~iso3)[!grepl(pattern, row.names(vcov(m, cluster=~iso3))), !grepl(pattern, colnames(vcov(m, cluster=~iso3))) ]
  
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
  write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
  write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
  write.csv(cov_iso, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov_iso.csv')))
  
  return(list(tab, coefs, cov))
}


################################################################################



# n lags tp selected before
# N_temp_eys<-8
# N_rain_eys<-8
# N_temp_leb<-8
# N_rain_leb<-8
# N_temp_gnipc<-8
# N_rain_gnipc<-8

vars_correspondaces=vars_corr
m_modns=m_modns_mean

######################

o_eys<-"gr_eys"
N_temp<-7
N_rain<-7

### formula part of temp, rain contr 

mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
# mean_cl_formula<-paste0(varns[v_temp], "+",  
#                         paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
#                         paste0("lag_",N_temp,"_",varns[v_temp], collapse = "+"),"+", 
#                         paste0("lag_",N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"))
# 

wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
pext_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))


extr_formula<- paste0(c(wd_formula,rx_formula,
                        tvar_formula),
                      collapse = "+")


### all extr tp 
r_eys=paste0(mean_cl_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)
summary(m_eys, cluster="iso3")

r_eys=paste0( 
             extr_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)
summary(m_eys, cluster="iso3")


r_eys=paste0(mean_cl_formula,"+", 
         extr_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
summary(m_eys, cluster="iso3")
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)

save_model(m_eys, pattern, o_eys, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_eys, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_eys,"all_vars_pdlm", "mean_mod", "mix")


#### correlation of errors 

# Extract the vector of removed row indices
removed <- abs(m_eys$obs_selection$obsRemoved)

# Create logical vector marking rows used in estimation
used_idx <- seq_len(nrow(data))
used_idx <- used_idx[!used_idx %in% removed]

# Now safely align residuals
resids <- data.frame(
  gdlcode = data$gdlcode[used_idx],
  iso3    = data$iso3[used_idx],
  year    = data$year[used_idx],
  resid   = residuals(m_eys)
)

data$resid<-residuals(m_eys, na.rm=FALSE)

hetero_test <- fixest::feols(resid ~ TM_mean , data , panel.id = pan_id)

summary(hetero_test) # no sign, no different process in cold hot regions


library(dplyr)
library(tidyr)
library(purrr)

# Ensure each region-year residual exists
resid_wide <- resids %>%
  dplyr::select(iso3, gdlcode, year, resid) %>%
  pivot_wider(names_from = gdlcode, values_from = resid)

# Compute correlations of residuals between regions within each country

cor_stats <- resids %>%
  group_by(iso3) %>%
  group_modify(~{
    dat <- .x %>%
      dplyr::select(gdlcode, year, resid) %>%
      pivot_wider(names_from = gdlcode, values_from = resid)
    
    n_regions <- ncol(dat) - 1  # minus 'year' column
    
    if (n_regions >= 2 ) {
      cors <- cor(dat[,-1], use = "pairwise.complete.obs")
      tibble(
        cor = cors[lower.tri(cors)],
        n_regions = n_regions
      )
    } else {
      tibble(
        cor = NA_real_,
        n_regions = n_regions
      )
    }
  }) %>%
  ungroup()



summary(cor_stats$cor)
mean(cor_stats$cor, na.rm = TRUE)
quantile(cor_stats$cor, c(0.25, 0.5, 0.75), na.rm = TRUE)

mean_by_iso <- cor_stats %>%
  group_by(iso3) %>%
  summarise(
    mean_cor = mean(cor, na.rm = TRUE),
    n_regions = first(n_regions)
  )

hist(mean_by_iso$mean_cor)


### 


data_dem_eys <- fixest::demean(as.formula(paste0(o_eys , "+", r_eys , "~", i )), data = data, na.rm=FALSE)
f_eys <- as.formula(paste( o_eys, "~", r_eys ,"-1" ))
data_dem_eys$year<-data$year
m_dem_eys = lm(f_eys, data_dem_eys )
summary(m_dem_eys)


library(car)
# linearHypothesis(m_dem_eys, c("lag_1_diff_TM + lag_2_diff_TM + lag_3_diff_TM + lag_4_diff_TM + lag_5_diff_TM + lag_6_diff_TM + lag_7_diff_TM +
#                               lag_1_TM_mean_i_diff_TM + lag_2_TM_mean_i_diff_TM + lag_3_TM_mean_i_diff_TM + lag_4_TM_mean_i_diff_TM + lag_5_TM_mean_i_diff_TM + lag_6_TM_mean_i_diff_TM + lag_7_TM_mean_i_diff_TM 
#                               = 0"))


car::vif(m_dem_eys)
# Visualizing the model
plot(m_dem_eys, which = 1, main = "Model Fit")
std_residuals <- cooks.distance(m_dem_eys)
outliers <- which(std_residuals > 2 * sd(std_residuals))

data_dem_eys_no_out <- data_dem_eys[-outliers, ]
m_dem_eys_rob = lm(f_eys, data_dem_eys_no_out )
summary(m_dem_eys_rob)
plot(m_dem_eys_rob, which = 1, main = "Model Fit")



######################

o_leb<-"gr_leb"

N_temp<-2
N_rain<-4

### formula part of temp, rain contr 
mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))

#mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
#                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
#                        paste0(m_modns[v_rain], "_i_", varns[v_rain],  collapse = "+"))


hw_formula<-paste0(paste0(varns[v_hw]), "+",
                   m_modns[v_temp], "_i_", varns[v_hw],"+", 
                   paste0("lag_",1,"_",varns[v_hw], collapse = "+"),"+", 
                   paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:1,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:1,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))

extr_formula<- paste0(c(hw_formula, 
                        rx_formula,
                        wd_formula),
                      collapse = "+")



### all extr tp 
r_leb=paste0(mean_cl_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)
summary(m_leb, cluster="iso3")


r_leb=paste0(
  extr_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)
summary(m_leb, cluster="iso3")


r_leb=paste0(mean_cl_formula,"+", 
             extr_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)

summary(m_leb, cluster="iso3")


save_model(m_leb, pattern, o_leb, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb,"all_vars_pdlm", "mean_mod", "mix")


B_hat <- coef(m_leb)

# Draw from both
draws_robust <- MASS::mvrnorm(1000, B_hat, vcov(m_leb, type="robust"))
draws_cluster <- MASS::mvrnorm(1000, B_hat, vcov(m_leb, cluster="iso3"))

# Compare marginal distributions of some key coefficients
apply(draws_robust, 2, sd) / apply(draws_cluster, 2, sd)

#### correlation of errors 

# Extract the vector of removed row indices
removed <- abs(m_leb$obs_selection$obsRemoved)

# Create logical vector marking rows used in estimation
used_idx <- seq_len(nrow(data))
used_idx <- used_idx[!used_idx %in% removed]

# Now safely align residuals
resids <- data.frame(
  gdlcode = data$gdlcode[used_idx],
  iso3    = data$iso3[used_idx],
  year    = data$year[used_idx],
  resid   = residuals(m_leb)
)

library(dplyr)
library(tidyr)
library(purrr)

# Ensure each region-year residual exists
resid_wide <- resids %>%
  dplyr::select(iso3, gdlcode, year, resid) %>%
  pivot_wider(names_from = gdlcode, values_from = resid)

# Compute correlations of residuals between regions within each country

cor_stats <- resids %>%
  group_by(iso3) %>%
  group_modify(~{
    dat <- .x %>%
      dplyr::select(gdlcode, year, resid) %>%
      pivot_wider(names_from = gdlcode, values_from = resid)
    
    n_regions <- ncol(dat) - 1  # minus 'year' column
    
    if (n_regions >= 2 ) {
      cors <- cor(dat[,-1], use = "pairwise.complete.obs")
      tibble(
        cor = cors[lower.tri(cors)],
        n_regions = n_regions
      )
    } else {
      tibble(
        cor = NA_real_,
        n_regions = n_regions
      )
    }
  }) %>%
  ungroup()



summary(cor_stats$cor)
mean(cor_stats$cor, na.rm = TRUE)
quantile(cor_stats$cor, c(0.25, 0.5, 0.75), na.rm = TRUE)

mean_by_iso <- cor_stats %>%
  group_by(iso3) %>%
  summarise(
    mean_cor = mean(cor, na.rm = TRUE),
    n_regions = first(n_regions)
  )

hist(mean_by_iso$mean_cor)


### 

data_dem_leb <- fixest::demean(as.formula(paste0(o_leb , "+", r_leb , "~", i )), data = data, na.rm=FALSE)
f_leb <- as.formula(paste( o_leb, "~", r_leb ,"-1" ))
data_dem_leb$year<-data$year
m_dem_leb = lm(f_leb, data_dem_leb )
summary(m_dem_leb)

data_dem_leb$iso3<-data$iso3

cv<-cross_validation_iso(data_dem_leb, f_leb,o_leb )

cv_no_pred<-cross_validation_iso(data_dem_leb, as.formula(paste( o_leb, "~", "1")),o_leb )


cv_time<-cross_validation_fixest(data_dem_leb, f_leb ,o_leb)

cv_time_no_pred<-cross_validation_fixest(data_dem_leb, as.formula(paste( o_leb, "~", "1")),o_leb )


######################

o_gnipc<-"gr_gnipc"

N_temp<-9
N_rain<-1

### formula part of temp, rain contr 
mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))



hw_formula<-paste0(paste0(varns[v_hw]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                   paste0("lag_",1:7,"_",varns[v_hw], collapse = "+"),"+", 
                   paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
#hw_formula<-paste0(paste0(varns[v_hw]), "+",paste0(m_modns[v_temp], "_i_", varns[v_hw]))
wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1:5,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1:5,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
pext_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:8,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                   paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                   paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))

extr_formula<- paste0(c(hw_formula, 
                        wd_formula, 
                       rx_formula,tvar_formula
                        ),
                      collapse = "+")



### all extr tp 

r_gnipc=paste0(mean_cl_formula
)
f_gnipc= as.formula(paste(o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")



r_gnipc=paste0( 
  extr_formula
)
f_gnipc= as.formula(paste(o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")

r_gnipc=paste0(mean_cl_formula,"+", 
               extr_formula
)
f_gnipc= as.formula(paste( o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")


save_model(m_gnipc, pattern, o_gnipc, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_gnipc, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_gnipc,"all_vars_pdlm", "mean_mod", "mix")

#### correlation of errors 

# Extract the vector of removed row indices
removed <- abs(m_gnipc$obs_selection$obsRemoved)

# Create logical vector marking rows used in estimation
used_idx <- seq_len(nrow(data))
used_idx <- used_idx[!used_idx %in% removed]

# Now safely align residuals
resids <- data.frame(
  gdlcode = data$gdlcode[used_idx],
  iso3    = data$iso3[used_idx],
  year    = data$year[used_idx],
  resid   = residuals(m_gnipc)
)

library(dplyr)
library(tidyr)
library(purrr)

# Ensure each region-year residual exists
resid_wide <- resids %>%
  select(iso3, gdlcode, year, resid) %>%
  pivot_wider(names_from = gdlcode, values_from = resid)

# Compute correlations of residuals between regions within each country

cor_stats <- resids %>%
  group_by(iso3) %>%
  group_modify(~{
    dat <- .x %>%
      select(gdlcode, year, resid) %>%
      pivot_wider(names_from = gdlcode, values_from = resid)
    
    n_regions <- ncol(dat) - 1  # minus 'year' column
    
    if (n_regions >= 2) {
      cors <- cor(dat[,-1], use = "pairwise.complete.obs")
      tibble(
        cor = cors[lower.tri(cors)],
        n_regions = n_regions
      )
    } else {
      tibble(
        cor = NA_real_,
        n_regions = n_regions
      )
    }
  }) %>%
  ungroup()



summary(cor_stats$cor)
mean(cor_stats$cor, na.rm = TRUE)
quantile(cor_stats$cor, c(0.25, 0.5, 0.75), na.rm = TRUE)

mean_by_iso <- cor_stats %>%
  group_by(iso3) %>%
  summarise(
    mean_cor = mean(cor, na.rm = TRUE),
    n_regions = first(n_regions)
  )

hist(mean_by_iso$mean_cor)


### 


data_dem_gnipc <- fixest::demean(as.formula(paste0(o_gnipc , "+", r_gnipc , "~", i )), data = data, na.rm=FALSE)
f_gnipc <- as.formula(paste( o_gnipc, "~", r_gnipc ,"-1" ))
data_dem_gnipc$year<-data$year
m_dem_gnipc = lm(f_gnipc, data_dem_gnipc )
summary(m_dem_gnipc)





################################################################################
############################   CORR plots    ###################################
################################################################################


candidate_vars <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar], 
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), 
  paste0(m_modns[v_rain], "_i_", varns[v_rain]),
  paste0(m_modns[v_temp], "_i_", varns[v_hw]),
  paste0(m_modns[v_hw], "_i_", varns[v_hw]),
  paste0(m_modns[v_rain], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),
  paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),
  paste0(m_modns[v_temp], "_i_", varns[v_tvar])
)

candidate_vars_small <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar]
)


candidate_vars_sum<-paste(
  varns[v_temp],"+", varns[v_rain], "+",
  varns[v_wd],"+", varns[v_hw], "+",varns[v_pext], "+",varns[v_rx], "+",varns[v_tvar], "+",
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
  paste0(m_modns[v_rain], "_i_", varns[v_rain]),"+",
  paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+",
  paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+",
  paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+",
  paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+",
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+",
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+",
  paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+",
  paste0(m_modns[v_temp], "_i_", varns[v_tvar])
)
data_dem <- fixest::demean(as.formula(paste0("gr_eys" , "+", "gr_leb" , "+", "gr_gnipc" , "+", 
                                             candidate_vars_sum , "~", i )), data = data, na.rm=FALSE)

library(corrplot)

M_big = cor(data_dem[,candidate_vars],use="complete.obs")

png(filename=paste0(out_dir,"/corr_all_diff_vars",".png"), width = 1000, height = 1000)
corrplot(M_big, method = 'number', type = 'lower', diag = FALSE)
dev.off()


M_small = cor(data_dem[,candidate_vars_small],use="complete.obs")

png(filename=paste0(out_dir,"/corr_diff_vars",".png"), width = 500, height = 500)
corrplot(M_small, method = 'number', type = 'lower', diag = FALSE)
dev.off()




################################################################################
############################   LASSO    ########################################
################################################################################


library(glmnet)
library(fixest)
library(dplyr)
library(tidyr)

# ----------------------------
# Helper function to generate lagged variable names
# ----------------------------
get_lagged_vars <- function(var_names, max_lags = 9) {
  lag_vars <- c()
  for(v in var_names) {
    lag_vars <- c(lag_vars, paste0("lag_", 0:max_lags, "_", v))
  }
  return(lag_vars)
}

# ----------------------------
# LASSO selection function
# ----------------------------
run_lasso_selection <- function(X, y, a) {
  
  X_scaled <- scale(X)
  cv_fit <- cv.glmnet(X_scaled, y, alpha = a)
  coef_lasso <- coef(cv_fit, s = "lambda.min")
  selected_vars <- rownames(coef_lasso)[coef_lasso[,1] != 0]
  selected_vars <- setdiff(selected_vars, "(Intercept)")
  return(selected_vars)
}

# ----------------------------
# Outcomes and candidate variables
# ----------------------------
outcomes <- list(
  eys = "gr_eys",
  leb = "gr_leb",
  gnipc = "gr_gnipc"
)

candidate_vars <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar], 
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), 
  paste0(m_modns[v_rain], "_i_", varns[v_rain]),
  paste0(m_modns[v_temp], "_i_", varns[v_hw]),
  paste0(m_modns[v_hw], "_i_", varns[v_hw]),
  paste0(m_modns[v_rain], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),
  paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),
  paste0(m_modns[v_temp], "_i_", varns[v_tvar])
)

candidate_vars_eys <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar], 
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), 
  paste0(m_modns[v_rain], "_i_", varns[v_rain]),
  paste0(m_modns[v_temp], "_i_", varns[v_hw]),
  paste0(m_modns[v_wd], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),
  paste0(m_modns[v_temp], "_i_", varns[v_tvar])
)

candidate_vars_gnipc <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar], 
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), 
  paste0(m_modns[v_rain], "_i_", varns[v_rain]),
  paste0(m_modns[v_temp], "_i_", varns[v_hw]),
  paste0(m_modns[v_rain], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),
  paste0(m_modns[v_temp], "_i_", varns[v_tvar])
)


candidate_vars_leb <- c(
  varns[v_temp], varns[v_rain], 
  varns[v_wd], varns[v_hw], varns[v_pext], varns[v_rx], varns[v_tvar], 
  paste0(m_modns[v_temp], "_i_", varns[v_temp]), 
  paste0(m_modns[v_rain], "_i_", varns[v_rain]), 
  paste0(m_modns[v_hw], "_i_", varns[v_hw]),
  paste0(m_modns[v_temp], "_i_", varns[v_wd]),
  paste0(m_modns[v_temp], "_i_", varns[v_pext]),
  paste0(m_modns[v_temp], "_i_", varns[v_rx]),
  paste0(m_modns[v_tvar], "_i_", varns[v_tvar])
)


# ----------------------------
# Main pipeline
# ----------------------------

summary_table<-list()
summary_table[["0"]] <- data.frame()
summary_table[["1"]] <- data.frame()
summary_table[["0.5"]] <- data.frame()

results_list <- list()

for (penalty in c(0,1,0.5)){
  for(out in outcomes) {
    
    if(out=="gr_gnipc"){candidate_vars=candidate_vars_gnipc}
    if(out=="gr_eys"){candidate_vars=candidate_vars_eys}
    if(out=="gr_leb"){candidate_vars=candidate_vars_leb}
    
    # Generate all lags (1–8) for each variable
    lag_vars <- get_lagged_vars(candidate_vars, max_lags = 9)
    
    # Demean
    formula_dem <- as.formula(paste0(out, " + ", paste(lag_vars, collapse = " + "), " ~ ", i))
    data_dem <- fixest::demean(formula_dem, data = data, na.rm = FALSE)
    
    # X and y
    X <- as.matrix(data_dem[, lag_vars, drop = FALSE])
    y <- data_dem[[out]]
    
    # Keep only complete cases
    complete_idx <- complete.cases(X, y)
    X_clean <- X[complete_idx, ]
    y_clean <- y[complete_idx]
    
    # Then run LASSO
    selected_vars <- run_lasso_selection(X_clean, y_clean, penalty)
    
    # Group selected lags by original variable
    selected_lags_by_var <- list()
    for(var in candidate_vars) {
      sel_vars <- selected_vars[grep(paste0("^lag_[0-9]+_", var, "$"), selected_vars)]
      nums <- as.numeric(gsub("\\D+", "", sel_vars))
      nums <- sort(unique(nums))
      if (length(nums) == 1) {
        nums<-as.character(nums)
      } else if (all(diff(nums) == 1)) {
        nums<-paste0(min(nums), "-", max(nums))
      } else {
        nums<-paste(nums, collapse = ",")
      }
      
      selected_lags_by_var[[var]] <- nums
    }
    
    # Store FE model
    if(length(selected_vars) > 0) {
      f_fe <- as.formula(paste(out, "~", paste(selected_vars, collapse = "+"), "|", i))
      m_fe <- feols(f_fe, data = data, panel.id = pan_id)
    } else {
      m_fe <- NULL
    }
    
    results_list[[out]][[as.character(penalty)]] <- list(
      lasso_selected = selected_lags_by_var,
      fe_model = m_fe
    )
    
    # ----------------------------
    # Add to summary table
    # ----------------------------
    for(var in names(selected_lags_by_var)) {
      lags <- selected_lags_by_var[[var]]
      if(length(lags) == 0) lags <- NA
      summary_table[[as.character(penalty)]] <- rbind(summary_table[[as.character(penalty)]],
                             data.frame(outcome = out,
                                        variable = var,
                                        selected_lags = paste(lags, collapse = ", ")))
    }
  }
  
  # Optional: clean up summary table for display
  summary_table[[as.character(penalty)]] <- summary_table[[as.character(penalty)]] %>% arrange(outcome, variable)
  
  # ----------------------------
  # Display summary
  # ----------------------------
  print(summary_table[[as.character(penalty)]])
}

summary_0<-summary_table[[as.character(0)]]
summary_1<-summary_table[[as.character(1)]]
summary_05<-summary_table[[as.character(0.5)]]



get_lags <- function(summary_df, outcome_name, var_name) {
  out <- summary_df$selected_lags[
    summary_df$outcome == outcome_name &
      summary_df$variable == var_name
  ]
  if (length(out) == 0) NA_character_ else out
}

build_lasso_table <- function(
    outcome_name,
    candidate_vars,
    summary_0,
    summary_1,
    summary_05
) {
  data.frame(
    outcome = outcome_name,
    variable = candidate_vars,
    `pen = 0`   = sapply(candidate_vars, get_lags,
                         summary_df = summary_0,
                         outcome_name = outcome_name),
    `pen = 1`   = sapply(candidate_vars, get_lags,
                         summary_df = summary_1,
                         outcome_name = outcome_name),
    `pen = 0.5` = sapply(candidate_vars, get_lags,
                         summary_df = summary_05,
                         outcome_name = outcome_name),
    stringsAsFactors = FALSE
  )
}


tab_eys <- build_lasso_table(
  outcome_name   = "gr_eys",
  candidate_vars = candidate_vars_eys,
  summary_0      = summary_0,
  summary_1      = summary_1,
  summary_05     = summary_05
)

tab_gnipc <- build_lasso_table(
  outcome_name   = "gr_gnipc",
  candidate_vars = candidate_vars_gnipc,
  summary_0      = summary_0,
  summary_1      = summary_1,
  summary_05     = summary_05
)

tab_leb <- build_lasso_table(
  outcome_name   = "gr_leb",
  candidate_vars = candidate_vars_leb,
  summary_0      = summary_0,
  summary_1      = summary_1,
  summary_05     = summary_05
)

pretty_names <- function(x) {
  x |>
    gsub("_mean_i_", " interact. ", x = _) |>
    gsub("^diff_", "", x = _) |>
    gsub("_", "\\\\_", x = _)
}

tab_eys$variable <- pretty_names(tab_eys$variable)
tab_gnipc$variable <- pretty_names(tab_gnipc$variable)
tab_leb$variable <- pretty_names(tab_leb$variable)

library(xtable)

print(
  xtable(tab_eys,
         caption = "Selected lags for eys",
         label   = "tab:lasso_eys"),
  include.rownames = FALSE
)

print(
  xtable(tab_gnipc,
         caption = "Selected lags for eys",
         label   = "tab:lasso_eys"),
  include.rownames = FALSE
)

print(
  xtable(tab_leb,
         caption = "Selected lags for eys",
         label   = "tab:lasso_eys"),
  include.rownames = FALSE
)


### coefficients


results_list[["gr_gnipc"]][["1"]]$fe_model
results_list[["gr_gnipc"]][["0"]]$fe_model
results_list[["gr_gnipc"]][["05"]]$fe_model

library(broom)
library(dplyr)

extract_coefs <- function(model, penalty) {
  if (is.null(model)) return(NULL)
  
  tidy(model) |>
    filter(term != "(Intercept)") |>
    mutate(penalty = penalty)
}

coefs_gnipc <- bind_rows(
  extract_coefs(results_list[["gr_gnipc"]][["0"]]$fe_model,  "ridge (0)"),
  extract_coefs(results_list[["gr_gnipc"]][["0.5"]]$fe_model, "enet (0.5)"),
  extract_coefs(results_list[["gr_gnipc"]][["1"]]$fe_model,  "lasso (1)")
)

library(tidyr)

coef_wide <- coefs_gnipc |>
  dplyr::select(term, penalty, estimate) |>
  pivot_wider(names_from = penalty, values_from = estimate)
coef_wide$`ridge (0)`<-ifelse( is.na(coef_wide$`ridge (0)`), 0,coef_wide$`ridge (0)`)
coef_wide$`enet (0.5)`<-ifelse( is.na(coef_wide$`enet (0.5)`), 0,coef_wide$`enet (0.5)`)
coef_wide$`lasso (1)`<-ifelse( is.na(coef_wide$`lasso (1)`), 0,coef_wide$`lasso (1)`)

round(cor(coef_wide[,-1], use = "pairwise.complete.obs"), 5)

library(ggplot2)

ggplot(coef_wide, aes(`ridge (0)`, `lasso (1)`)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Ridge coefficients",
    y = "Lasso coefficients",
    title = "Coefficient comparison: Ridge vs Lasso"
  ) +
  theme_minimal()

ggplot(coef_wide, aes(`enet (0.5)`, `lasso (1)`)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Elastic net coefficients",
    y = "Lasso coefficients",
    title = "Coefficient comparison: Elastic net vs Lasso"
  ) +
  theme_minimal()


################################################################################
### group lasso

library(grpreg)
library(fixest)

i<-"gdlcode + year + iso3[year] +iso3[year^2]"


max_lag <- 9

### 

output<-"gr_gnipc"
candidate_vars<-candidate_vars_gnipc

# Generate all lagged variable names
lag_vars <- unlist(lapply(candidate_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))

# Group assignment: all lags of a variable belong to same group
group <- rep(1:length(candidate_vars), each = max_lag)
names(group) <- lag_vars

formula_dem <- as.formula(
  paste0(output, " + ", paste(lag_vars, collapse = " + "), " ~ ", i)
)
data_dem <- fixest::demean(formula_dem, data = data, na.rm = FALSE)

# Prepare matrix X and response y
X <- as.matrix(data_dem[, lag_vars, drop = FALSE])
y <- data_dem[[output]]  # demeaned response

complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]


# Fit group LASSO
fit <- cv.grpreg(X_clean, y_clean, group = group, penalty = "grLasso", seed = 123)

plot(fit)

# Best lambda
lambda_best <- fit$lambda.min
lambda_best

# Coefficients at best lambda
coef_group <- coef(fit, lambda = lambda_best)

# Identify selected groups
selected_groups <- unique(group[names(coef_group)[coef_group[1] != 0 & names(coef_group) != "(Intercept)"]])
selected_vars <- candidate_vars[selected_groups]
cat("Selected variables (groups) by group LASSO:", selected_vars, "\n")

# Collect selected lagged variables
selected_lag_vars <- unlist(lapply(selected_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))
# all



###

output<-"gr_eys"
candidate_vars<-candidate_vars_gnipc

# Generate all lagged variable names
lag_vars <- unlist(lapply(candidate_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))

# Group assignment: all lags of a variable belong to same group
group <- rep(1:length(candidate_vars), each = max_lag)
names(group) <- lag_vars

formula_dem <- as.formula(
  paste0(output, " + ", paste(lag_vars, collapse = " + "), " ~ ", i)
)
data_dem <- fixest::demean(formula_dem, data = data, na.rm = FALSE)

# Prepare matrix X and response y
X <- as.matrix(data_dem[, lag_vars, drop = FALSE])
y <- data_dem[[output]]  # demeaned response

complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]


# Fit group LASSO
fit <- cv.grpreg(X_clean, y_clean, group = group, penalty = "grLasso", seed = 123)

plot(fit)

# Best lambda
lambda_best <- fit$lambda.min
lambda_best

# Coefficients at best lambda
coef_group <- coef(fit, lambda = lambda_best)

# Identify selected groups
selected_groups <- unique(group[names(coef_group)[coef_group[1] != 0 & names(coef_group) != "(Intercept)"]])
selected_vars <- candidate_vars[selected_groups]
cat("Selected variables (groups) by group LASSO:", selected_vars, "\n")

# Collect selected lagged variables
selected_lag_vars <- unlist(lapply(selected_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))
# all

###

output<-"gr_leb"
candidate_vars<-candidate_vars_gnipc

# Generate all lagged variable names
lag_vars <- unlist(lapply(candidate_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))

# Group assignment: all lags of a variable belong to same group
group <- rep(1:length(candidate_vars), each = max_lag)
names(group) <- lag_vars

formula_dem <- as.formula(
  paste0(output, " + ", paste(lag_vars, collapse = " + "), " ~ ", i)
)
data_dem <- fixest::demean(formula_dem, data = data, na.rm = FALSE)

# Prepare matrix X and response y
X <- as.matrix(data_dem[, lag_vars, drop = FALSE])
y <- data_dem[[output]]  # demeaned response

complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]


# Fit group LASSO
fit <- cv.grpreg(X_clean, y_clean, group = group, penalty = "grLasso", seed = 123)

plot(fit)

# Best lambda
lambda_best <- fit$lambda.min
lambda_best

# Coefficients at best lambda
coef_group <- coef(fit, lambda = lambda_best)

# Identify selected groups
selected_groups <- unique(group[names(coef_group)[coef_group[1] != 0 & names(coef_group) != "(Intercept)"]])
selected_vars <- candidate_vars[selected_groups]
cat("Selected variables (groups) by group LASSO:", selected_vars, "\n")

# Collect selected lagged variables
selected_lag_vars <- unlist(lapply(selected_vars, function(v) paste0("lag_", 1:max_lag, "_", v)))



  