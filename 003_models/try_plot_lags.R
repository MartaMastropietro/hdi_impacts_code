

rm(list=ls())

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
out_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_plots<-"output/models/original_components_tp_extr_complex_models_try/lag_models/shocks_figures"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

#controls
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

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



### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

n<-9

# n lags 
N_temp<-n
N_rain<-n
N<-n




################################################################################
################################################################################
################################################################################

### each complete mod with/without control and autoreg 
### model specification with mean controls t and r 

spec<-"mean_mod_tp"
pattern<-"conflict|exp_edu|exp_health|trade|lag_gr" # to discard coefficients rows correspondent to controls, autoreg part 

save_model<-function(m, pattern, o, type, spec, NL, out_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
  write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
  write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
  
  return(list(tab, coefs, cov))
}

vars_correspondaces<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)"),
  modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean"),
  # modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" )
)

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



################################################################################

o<-"gr_mys"

extr_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"),"+",
                     paste0(varns[v_spi]), "+",
                     paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                     paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                     paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_rx], collapse = "+")
)
extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_pext],'_i_',varns[v_pext], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_rx],'_i_',varns[v_rx], collapse = "+")
)

# extr_formula<-paste0(paste0(varns[v_spi]), "+",
#                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                      paste0(varns[v_hw]), "+",
#                      paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
# extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                           paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"))
models<-list()

### all, moderating with extr
type<-"all_extr"
r=paste0(mean_cl_formula,"+", 
         extr_formula
)
f= as.formula(paste( o, "~", r, "|" ,i ))
m = fixest::feols(f, data , panel.id = pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save 
save_model(m, pattern, o, type, spec, N, out_dir)


# directory, vars_correspondaces,out_directory, o, type, spec, NL
plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)


plot_lags<-function(directory, vars_correspondaces, out_directory, o, type, spec, NL){
  
  coefs <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_coef.csv"), sep = ",", header = TRUE)
  colnames(coefs)<-c("variable","x" )
  cov <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_cov.csv"), sep = ",", header = TRUE)
  colnames(cov)<-c("X", paste0(cov$X))
  coeftable <- read.table(paste0(directory, "/", o, '_',type, '_',spec, "_lagN", NL, "_coeftab.csv"), sep = ",", header = TRUE)
  colnames(coeftable)<-c("variable","Estimate", "Std_error", "t_value", "p_value" )
  
  wr2 <- coeftable[coeftable$variable == "wr2", "Estimate"]
  BIC <- coeftable[coeftable$variable == "BIC", "Estimate"]
  AIC <- coeftable[coeftable$variable == "AIC", "Estimate"]
  
  # Extract variables, moderators, and lags
  extract_result <- extract_vars_mods(coefs, NL)
  varns <- extract_result[[1]]
  mods <- extract_result[[2]]
  modns <- extract_result[[3]]
  NVLs <- extract_result[[4]]
  
  # Calculate means of moderating variables and standard deviations of climate variables
  means <- calc_means(data, modns, "gdlcode")
  stds <- calc_stds(data, varns, "gdlcode")
  
  # Units, scaling factors, and labels (DEPENDENT ON VARS WE USE)
  # unit <- c("C","mm", "C", "days", "C", "mm", " ")
  # margunit <- c("1C", "1std",  "1std", "1std", "1C", "1std", "1std")
  # scales <- c(100 * 1, 100 * stds[2], 100 * stds[3], 100 * stds[4], 100*1 , 100 * stds[6] , 100 * stds[7])
  # efunit <- "(%-point growth rate)"
  
  # using sd only 
  unit<-vars_correspondaces$unit[match(modns, vars_correspondaces$modns)]
  scales<-stds*100
  margunit<-rep("1std", length(scales))
  efunit <- "(%-point growth rate)"
  
  # Calculate marginal effects and their errors
  MEs <- calc_specNL_MEs(coefs, varns, mods, means, NVLs)
  errors <- calc_specNL_errors(cov, varns, mods, means, NVLs)
  
  folder <- out_directory
  
  
  varnlabels<-vars_correspondaces[ match( varns, vars_correspondaces$varns) , "ext_names" ]
  
  # Plot results
  plot_response(o, type,varns, varnlabels, mods, modns, MEs, errors, means, spec, NL, NVLs, unit, margunit, scales, efunit,
                folder,  summ = FALSE, wr2 = wr2, BIC = BIC, AIC = AIC)
  
}



coefs <- read.table(paste0(out_dir_oaat,"/", o, '_',type, '_',spec, "_lagN", 4, "_coef.csv"), sep = ",", header = TRUE)
colnames(coefs)<-c("variable","x" )

full <- coefs$variable
# indvs <- full[!grepl("l\\(", full)]
indvs <- full[!grepl("lag", full)]
varns <- indvs[!grepl("_i_|_2", indvs)]
potint <- setdiff(indvs, varns)
mods <- sapply(varns, function(varn) {
  mod <- potint[grepl(varn, potint)]
  if (length(mod) > 0) mod[1] else "None"
})
modns <- sapply(mods, function(mod) strsplit(mod, "_i_|_2")[[1]][1])

NLs<-rep(NA, length(varns))
for (id in 1:length(modns)){
  NLs[id]<-length(full[grepl(modns[id], full)])-1
}

list(varns = varns, mods = mods, modns = modns, NLs = NLs)

indvs[1]
full[grepl(modns[1], full)]
