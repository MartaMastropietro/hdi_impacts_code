# plot cumulative effects of final dam functions for each output, each cl var 


rm(list=ls())

library(dplyr)
library(readr)
library(ggplot2)
source("scripts/003_models/feols_lags_plot_funcs.R")

spec<-"mean_mod"
type="all_extr_tp"
NL<-"_mix_"

################################################################################

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

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

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
for ( i in 0:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# check
# data$TVAR_i_diff_TVAR
# data$lag_0_TVAR_i_diff_TVAR

# mean hist climate

# avg and ten years avg of previous years 
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

# go back to right time period

data<-data%>%filter(year>=1990 & year<=2019)



################################################################################


compute_cum_effect <- function(coefs, climate_var, mod_var, cov_rob, cov_iso, mean_ref) {
  
  
  
  # Load precomputed coefficients and variance-covariance matrix
  full <- coefs$variable
  no_int<-full[!grepl("_i_", full)]
  int<-setdiff(full, no_int)
  betas <- coefs[which(coefs$variable %in% no_int[grepl(climate_var,no_int)] ),]
  gammas <- coefs[which(coefs$variable %in% int[grepl(mod_var,int)] ),]
  
  beta_cum <- sum(betas$x, na.rm = TRUE)
  gamma_cum <- sum(gammas$x, na.rm = TRUE)
  
  # Extract relevant covariances, rob
  var_beta_cum <- sum(cov_rob[which(cov_rob$X %in% betas$variable), betas$variable], na.rm = TRUE)
  var_gamma_cum <- sum(cov_rob[which(cov_rob$X %in% gammas$variable), gammas$variable], na.rm = TRUE)
  cov_beta_gamma_cum <- sum(cov_rob[which(cov_rob$X %in% betas$variable), gammas$variable], na.rm = TRUE)
  
  # Compute cumulative effect at reference temperature
  cum_effect <- beta_cum + gamma_cum * mean_ref
  var_cum_effect <- var_beta_cum + mean_ref^2 * var_gamma_cum + 2 * mean_ref * cov_beta_gamma_cum
  se_cum_effect <- sqrt(var_cum_effect)
  
  # Compute confidence intervals
  ci_lower <- cum_effect - 1.96 * se_cum_effect
  ci_upper <- cum_effect + 1.96 * se_cum_effect
  
  #
  
  # Extract relevant covariances, rob
  iso_var_beta_cum <- sum(cov_iso[which(cov_iso$X %in% betas$variable), betas$variable], na.rm = TRUE)
  iso_var_gamma_cum <- sum(cov_iso[which(cov_iso$X %in% gammas$variable), gammas$variable], na.rm = TRUE)
  iso_cov_beta_gamma_cum <- sum(cov_iso[which(cov_iso$X %in% betas$variable), gammas$variable], na.rm = TRUE)
  
  # Compute cumulative effect at reference temperature
  iso_cum_effect <- beta_cum + gamma_cum * mean_ref
  iso_var_cum_effect <- iso_var_beta_cum + mean_ref^2 * iso_var_gamma_cum + 2 * mean_ref * iso_cov_beta_gamma_cum
  iso_se_cum_effect <- sqrt(iso_var_cum_effect)
  
  # Compute confidence intervals
  ci_lower_iso <- cum_effect - 1.96 * iso_se_cum_effect
  ci_upper_iso <- cum_effect + 1.96 * iso_se_cum_effect
  
  # Format label (modify as needed)
  #custom_label <- round(cum_effect, 4)
  
  return(c(
    CumEffect = cum_effect,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    CI_Lower_iso = ci_lower_iso,
    CI_Upper_iso = ci_upper_iso
  ))
}

################################################################################
# output dir
out_dir<-"output/models/final_lag_mods/conservative_N_lags_mix"
if(!dir.exists(out_dir)){dir.create(out_dir)}


out_variables<-c("gr_gnipc", "gr_leb", "gr_eys")


directory<-out_dir

plot_data<-data.frame()

for (o in out_variables){
  
  coefs <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_coef.csv"), sep = ",", header = TRUE)
  colnames(coefs)<-c("variable","x" )
  
  cov_rob <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_cov.csv"), sep = ",", header = TRUE)
  cov_iso <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_cov_iso.csv"), sep = ",", header = TRUE)
  
  colnames(cov_rob)<-c("X", paste0(cov_rob$X))
  colnames(cov_iso)<-c("X", paste0(cov_iso$X))
  
  # Extract variables, moderators, and lags
  extract_result <- extract_vars_mods(coefs, NL)
  varns <- extract_result[[1]]
  mods <- extract_result[[2]]
  modns <- extract_result[[3]]
  NVLs <- extract_result[[4]]
  means <- calc_means(data, modns, "gdlcode")
  rownames(means)<-mods
  
  stds <- calc_stds(data, varns, "gdlcode")
  
  
  for (i in 1:length(varns)){
     
    res<-compute_cum_effect(coefs, varns[i], mods[i], cov_rob, cov_iso, means[i,2])
    
    plot_data<-rbind(plot_data,
                     data.frame(
                       Response = o,
                       ClimateVar = mods[i],
                       CumEffect = 100*stds[i]*res[names(res)=="CumEffect"],
                       CI_Lower = 100*stds[i]*res[names(res)=="CI_Lower"],
                       CI_Upper = 100*stds[i]*res[names(res)=="CI_Upper"],
                       CI_Lower_iso = 100*stds[i]*res[names(res)=="CI_Lower_iso"],
                       CI_Upper_iso = 100*stds[i]*res[names(res)=="CI_Upper_iso"],
                       Nl=NVLs[i],
                       ref="med")
                     )
    
    res<-compute_cum_effect(coefs, varns[i], mods[i], cov_rob, cov_iso, means[i,3])
    
    plot_data<-rbind(plot_data,
                     data.frame(
                       Response = o,
                       ClimateVar = mods[i],
                       CumEffect = 100*stds[i]*res[names(res)=="CumEffect"],
                       CI_Lower = 100*stds[i]*res[names(res)=="CI_Lower"],
                       CI_Upper = 100*stds[i]*res[names(res)=="CI_Upper"],
                       CI_Lower_iso = 100*stds[i]*res[names(res)=="CI_Lower_iso"],
                       CI_Upper_iso = 100*stds[i]*res[names(res)=="CI_Upper_iso"],
                       Nl=NVLs[i],
                       ref="q75")
                     )
    
    res<-compute_cum_effect(coefs, varns[i], mods[i], cov_rob, cov_iso, means[i,1])
    
    plot_data<-rbind(plot_data,
                     data.frame(
                       Response = o,
                       ClimateVar = mods[i],
                       CumEffect = 100*stds[i]*res[names(res)=="CumEffect"],
                       CI_Lower = 100*stds[i]*res[names(res)=="CI_Lower"],
                       CI_Upper = 100*stds[i]*res[names(res)=="CI_Upper"],
                       CI_Lower_iso = 100*stds[i]*res[names(res)=="CI_Lower_iso"],
                       CI_Upper_iso = 100*stds[i]*res[names(res)=="CI_Upper_iso"],
                       Nl=NVLs[i],
                       ref="q25")
                    )
  }
  
}




################################################################################
# plot in squared box, cumulative values only where significant 


plot_data$var_name<-NA
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_TM" )]<-"diff_TM"
plot_data$var_name[which(plot_data$ClimateVar=="RR_mean_i_diff_RR" )]<-"diff_RR"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_HW" )]<-"diff_HW"
plot_data$var_name[which(plot_data$ClimateVar=="RR_mean_i_diff_WD" )]<-"diff_WD"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_PEXT" )]<-"diff_PEXT"
plot_data$var_name[which(plot_data$ClimateVar=="HW_mean_i_diff_HW" )]<-"diff_HW"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_RX" )]<-"diff_RX"
plot_data$var_name[which(plot_data$ClimateVar=="TVAR_mean_i_diff_TVAR" )]<-"diff_TVAR"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_WD" )]<-"diff_WD"
plot_data$var_name[which(plot_data$ClimateVar=="WD_mean_i_diff_WD" )]<-"diff_WD"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_TVAR" )]<-"diff_TVAR"

variable_order<-c("diff_TM", "diff_RR","diff_HW", "diff_TVAR",
                  "diff_WD","diff_PEXT" ,"diff_RX"
)

plot_data$CI_Lower<-NULL
plot_data$CI_Upper<-NULL
plot_data$CI_Lower_iso<-NULL
plot_data$CI_Upper_iso<-NULL
plot_data$Nl<-NULL



library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpattern)  # For striped patterns
library(RColorBrewer)

# Reshape data for plotting
data_long <- plot_data %>%
  pivot_wider(names_from = ref, values_from = CumEffect)

ggplot(data_long, aes(x = var_name, y = Response)) +
  geom_tile(color = "black", fill = "white") +  # Empty cells white
  geom_tile(aes(fill = med), data = data_long, alpha = 0.7, width = 0.3, color="black") +
  geom_tile(aes(fill = q25), data = data_long, alpha = 0.7, width = 0.3,  color="black", position = position_nudge(x = -0.3)) +
  geom_tile(aes(fill = q75), data = data_long, alpha = 0.7, width = 0.3,  color="black", position = position_nudge(x = 0.3)) +
  #scale_fill_manual(values = ref_colors) +
  theme_bw() +labs(fill = "CumEffect")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_steps2() 
  
ggsave(file.path(out_dir, paste0("cumulative_lag_effects_simple_", type,'_',spec, ".png")), width = 7, height=4)




################################################################################

plot_data$var_name<-NA
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_TM" )]<-"diff_TM, TM"
plot_data$var_name[which(plot_data$ClimateVar=="RR_mean_i_diff_RR" )]<-"diff_RR, RR"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_HW" )]<-"diff_HW, TM"
plot_data$var_name[which(plot_data$ClimateVar=="RR_mean_i_diff_WD" )]<-"diff_WD, RR"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_PEXT" )]<-"diff_PEXT, TM"
plot_data$var_name[which(plot_data$ClimateVar=="HW_mean_i_diff_HW" )]<-"diff_HW, HW"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_RX" )]<-"diff_RX, TM"
plot_data$var_name[which(plot_data$ClimateVar=="TVAR_mean_i_diff_TVAR" )]<-"diff_TVAR, TVAR"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_WD" )]<-"diff_WD, TM"
plot_data$var_name[which(plot_data$ClimateVar=="WD_mean_i_diff_WD" )]<-"diff_WD, WD"
plot_data$var_name[which(plot_data$ClimateVar=="TM_mean_i_diff_TVAR" )]<-"diff_TVAR, TM"

variable_order<-c("diff_TM, TM", "diff_RR, RR","diff_HW, TM","diff_HW, HW", "diff_TVAR, TM", "diff_TVAR, TVAR", 
                  "diff_WD, TM","diff_WD, RR", "diff_WD, WD", "diff_PEXT, TM" ,"diff_RX, TM"
                  )

# try bar plot
colors <- c("gr_gnipc" = "#1b9e77", "gr_eys" = "#d95f02", "gr_leb" = "#7570b3")

g1<-ggplot(plot_data%>%filter(ref=="q75"), aes(x = var_name, y = CumEffect, fill = Response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +  # Bars
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.7), width = 0.2, color = "black") +  # CI bars
  scale_x_discrete(limits = variable_order) + 
  # Custom labels
  geom_label(aes(label = Nl), 
             position = position_dodge(width = 0.7), vjust = -1.5, size = 2,show.legend = FALSE) +
  scale_fill_manual(values = colors, name = "Response Variable") +
  labs(#title = "Cumulative Effects of Climate Variables on Growth Indicators",
    x = "Climate Variable",
    y = "Cumulative Effect of 1sd shock, \n 0.75 quantile moderating var") +
  ylim(c(-6,2))+
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust = 1))

g2<-ggplot(plot_data%>%filter(ref=="med"), aes(x = var_name, y = CumEffect, fill = Response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +  # Bars
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.7), width = 0.2, color = "black") +  # CI bars
  scale_x_discrete(limits = variable_order) + 
  # Custom labels
  geom_label(aes(label = Nl), 
             position = position_dodge(width = 0.7), vjust = -1.5, size = 2,show.legend = FALSE) +
  scale_fill_manual(values = colors, name = "Response Variable") +
  labs(#title = "Cumulative Effects of Climate Variables on Growth Indicators",
    x = "Climate Variable",
    y = "Cumulative Effect of 1sd shock, \n 0.5 quantile moderating var") +
  ylim(c(-6,2))+
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust = 1))

g3<-ggplot(plot_data%>%filter(ref=="q25"), aes(x = var_name, y = CumEffect, fill = Response)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +  # Bars
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.7), width = 0.2, color = "black") +  # CI bars
  scale_x_discrete(limits = variable_order) + 
  # Custom labels
  geom_label(aes(label = Nl), 
             position = position_dodge(width = 0.7), vjust = -1.5, size = 2,show.legend = FALSE) +
  scale_fill_manual(values = colors, name = "Response Variable") +
  labs(#title = "Cumulative Effects of Climate Variables on Growth Indicators",
    x = "Climate Variable",
    y = "Cumulative Effect of 1sd shock, \n 0.25 quantile moderating var") +
  ylim(c(-6,2))+
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust = 1))
  
arr<-ggpubr::ggarrange(g1,g2,g3,nrow=3)
ggsave(file.path(out_dir, paste0("cumulative_lag_effects_", type,'_',spec, ".png")), width = 7, height=10)
