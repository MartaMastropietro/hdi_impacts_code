
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
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try/tests"
if(!dir.exists(out_dir)){dir.create(out_dir)}



### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_nat<- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")


#controls
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )
data_nat<-left_join(data_nat,all_controls )

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
for ( i in 1:10){
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

# mean hist climate

data <- data %>% group_by(gdlcode) %>% mutate(
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

pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

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
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
  }
}


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

## irf one var at the time, plot, for each out var

out_dir_irf<-file.path(out_dir, "irf")
if(!dir.exists(out_dir_irf)){dir.create(out_dir_irf)}


# Load necessary libraries
library(fixest)    # For handling panel data
library(ggplot2) # For visualization
library(lmtest)  # For robust standard errors

pan_id<-c("gdlcode", "year")

idx<-"gdlcode + year + iso3[year] +iso3[year^2]"


# Function to perform local projection with robust standard errors
local_projection_se <- function(data, y_var, x_var, max_horizon, ar_control=TRUE, idx) {
  irf_list <- list()
  se_list <-se_list_iso <- list()
  
  for (h in 0:max_horizon) {
    
    # Create lagged dependent variable for time t-1, lead for time h
    
    data$y_var <- data[[y_var]]
                      
    data<-data%>%
      arrange(gdlcode, year)%>%
      group_by(gdlcode)%>%
      mutate(
        y_lag = dplyr::lag(y_var, 1),
        y_lead = dplyr::lead(y_var, h)
      )%>%
      mutate(growth=y_lead-y_lag)

        
    # Prepare the formula for the regression with fixed effects
    if(ar_control){
      r<-paste0(x_var, " + l(conflict, 0:5) + l(trade, 0:5) + l(exp_health, 0:5) + l(exp_edu, 0:5)", 
                paste0("+ l(y_var, 2:5)")) 
    }else{ 
      r<-paste0(x_var, "+ l(conflict, 0:5) + l(trade, 0:5) + l(exp_health, 0:5) + l(exp_edu, 0:5)") 
      }
    formula <- as.formula(paste( "growth", "~", r, "|" , idx))
   
    # Run the regression for horizon h using feols
    reg_result <- fixest::feols(formula, data = data,  panel.id = pan_id)
    
    # Calculate robust standard errors using vcovHC
    robust_se <- sqrt(diag(vcov(reg_result, type = "NW")))[[x_var]]  # SE of the x_var (shock)
    robust_se_iso <- sqrt(diag(vcov(reg_result, cluster= ~iso3)))[[x_var]]  # SE of the x_var (shock)
    
    # Extract the coefficient and standard error for impulse response at horizon h
    irf_list[[h + 1]] <- coef(reg_result)[[x_var]]  # coefficient on x_var (shock)
    se_list[[h + 1]] <- robust_se           # standard error of the coefficient
    se_list_iso[[h + 1]] <- robust_se_iso           # standard error of the coefficient
  }
  
  # Combine coefficients and standard errors into a data frame
  result_df <- data.frame(horizon = 0:max_horizon, 
                          irf = unlist(irf_list), 
                          se = unlist(se_list),
                          se_iso = unlist(se_list_iso))
  
  return(result_df)
}


out_variables<-c("lgnipc", "lleb", "leys")
cl_variables<-c(varns, modns)

for(out_var in out_variables){
  for (var in cl_variables){
    
    
    # Run the local projection with robust standard errors for y on x (shock) up to horizon 10
    irfs_se_df <- local_projection_se(data, y_var = out_var, x_var = var, max_horizon = 8, ar_control=TRUE, idx)
    
    # Calculate the upper and lower bounds of the confidence intervals (95% CI)
    irfs_se_df$lower <- irfs_se_df$irf - 1.96 * irfs_se_df$se
    irfs_se_df$upper <- irfs_se_df$irf + 1.96 * irfs_se_df$se
    
    irfs_se_df$lower_iso <- irfs_se_df$irf - 1.96 * irfs_se_df$se_iso
    irfs_se_df$upper_iso <- irfs_se_df$irf + 1.96 * irfs_se_df$se_iso
    
    
    # Plot the impulse responses with confidence intervals
    g<-ggplot(irfs_se_df, aes(x = horizon)) +
      geom_line(aes(y = irf),color = "blue") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
      geom_ribbon(aes(ymin = lower_iso, ymax = upper_iso), alpha = 0.2, fill = "violet") +
      geom_abline(slope=0, intercept=0)+
      labs(title = paste0("IRF (Local Projections) for ", out_var, " with a shock of ", var),
           x = "Horizon (Time Periods)",
           y = "Impulse Response") +
      theme_minimal()
    ggsave(filename=file.path(out_dir_irf, paste0(out_var,"_",var, "_simple_irf.png")), g, width=10, height=7)
    
    
  }
}


################################################################################