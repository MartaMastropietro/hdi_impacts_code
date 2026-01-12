### rationale: tm, rr controls, but discard non significant lags for each variable. add extremes

rm(list=ls())
source("utils/libraries.R")

# xlsx write
require(openxlsx)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)
library(plm)

library(gt)

# output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_country <- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")
all_controls <- read_csv("data/controls/all_controls.csv")

hdi_classes <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")
hdiclass <- hdi_classes %>%select(iso3,year,hdicode)%>%
  group_by(iso3) %>%
  summarise(
    hdiclass = if (all(is.na(hdicode))) {
      NA_character_
    } else {
      names(which.max(table(hdicode)))
    }
  )


data<-left_join(data, all_controls)
data<-left_join(data, hdiclass)
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


data_country<-left_join(data_country, all_controls)

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

# historical mean vars 
mean_modns=c('mTM','mRR', "mTVAR", "mHW", "mRX", "mPEXT", "mWD", "mSPI", "mSPEI", "mPET")

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
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[modns[i]]
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

# country data

for (i in 1:length(varns)){
  data_country[paste(modns[i],'_i_',varns[i],sep='')] <- data_country[modns[i]] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste(adap[i],'_i_',varns[i],sep='')] <- data_country[adap[i]] * data_country[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data_country[paste(adap[i],'_i_',modns[i],sep='')] <- data_country[adap[i]] * data_country[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',varns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',varns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',modns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',modns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data_country[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data_country)
    
  }
}


### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

################################################################################
### one at the time extreme variables addition
################################################################################

run_lasso_selection <- function(X, y, a) {
  
  X_scaled <- scale(X)
  cv_fit <- cv.glmnet(X_scaled, y, alpha = a)
  coef_lasso <- coef(cv_fit, s = "lambda.min")
  selected_vars <- rownames(coef_lasso)[coef_lasso[,1] != 0]
  selected_vars <- setdiff(selected_vars, "(Intercept)")
  return(selected_vars)
}



### models edu -> mean years of schooling age 25+

o<-"gr_eys"

models<-list()

#burke
r <- "TM + TM_2 + RR + RR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
#summary(m, vcov = "DK")

# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
# summary(m, vcov = "DK")
models[[length(models)+1]]<-m




# diff model + wd
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
WD +  WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# diff model + pext
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
PEXT + TM_i_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2 +
RX + TM_i_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)


# diff model + rx
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
RX + TM_i_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + tvar
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
TVAR + TM_i_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# wd
r <- "WD +  WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# pext
r <- "PEXT +  TM_i_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# rx
r <- "RX +  TM_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# tvar
r <- "TVAR +  TM_i_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# all
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
TVAR + TM_i_TVAR+  PEXT + PEXT_2 + TM_i_PEXT + WD +  WD_2 + RX +  TM_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# all
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
TVAR + TM_i_TVAR+  PEXT + PEXT_2+ TM_i_PEXT +  WD +  WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# all by classes
models_all_classes<-list()
models_all_classes[[length(models_all_classes)+1]]<-m # all

m <- fixest::feols(f, data%>%filter(hdiclass=="Very High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m

m <- fixest::feols(f, data%>%filter(hdiclass=="High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Medium"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Low"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m
names(models_all_classes)<-c("All", "Very High", "High", "Medium", "Low" )

modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.html")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.html")))
modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.tex")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.tex")))

# save different m
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )


# LASSO

library(glmnet)
candidate_vars_lasso<-c( trimws(unlist(strsplit(r, "\\+"))))

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
m_dem = lm(f, data_dem )
summary(m_dem)

# X and y
X <- as.matrix(data_dem[, candidate_vars_lasso, drop = FALSE])
y <- data_dem[[o]]

# Keep only complete cases
complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]

# Then run LASSO, RIDGE, ELASTIC
candidate_vars_lasso
run_lasso_selection(X_clean, y_clean, 1)
run_lasso_selection(X_clean, y_clean, 0)
run_lasso_selection(X_clean, y_clean, 0.5)

################################################################################

### models health -> life exp birth 

o<-"gr_leb"


models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
 

# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + HW
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW + HW_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff model + WD
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + RX
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
RX + TM_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + RX
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + TVAR
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# HW
r <- "diff_HW + HW_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


#  WD
r <- "diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

#  RX
r <- "diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


#  TVAR
r <- "diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# diff model + all
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW + HW_i_diff_HW  + diff_TVAR + TVAR_i_diff_TVAR+ 
diff_WD + TM_i_diff_WD + diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW + HW_i_diff_HW  + diff_TVAR + TVAR_i_diff_TVAR+ 
diff_WD + TM_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# all by classes
models_all_classes<-list()
models_all_classes[[length(models_all_classes)+1]]<-m # all

m <- fixest::feols(f, data%>%filter(hdiclass=="Very High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m

m <- fixest::feols(f, data%>%filter(hdiclass=="High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Medium"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Low"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m
names(models_all_classes)<-c("All", "Very High", "High", "Medium", "Low" )

modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.html")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.html")))
modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.tex")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.tex")))

# save different m
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
#modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
#modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

# LASSO

library(glmnet)
candidate_vars_lasso<-c( trimws(unlist(strsplit(r, "\\+"))))

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
m_dem = lm(f, data_dem )
summary(m_dem)

# X and y
X <- as.matrix(data_dem[, candidate_vars_lasso, drop = FALSE])
y <- data_dem[[o]]

# Keep only complete cases
complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]

# Then run LASSO, RIDGE, ELASTIC
candidate_vars_lasso
run_lasso_selection(X_clean, y_clean, 1)
run_lasso_selection(X_clean, y_clean, 0)
run_lasso_selection(X_clean, y_clean, 0.5)

################################################################################

### models gnipc

o<-"gr_gnipc"

models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff model -> keep this, as we use this for all others, for easier comparison
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +l(diff_TM,1) + l(TM_i_diff_TM,1)   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff + wd

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
      diff_WD + RR_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff + hw

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
      diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_WD + RR_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# all

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
  + diff_WD + RR_i_diff_WD + diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# all by classes
models_all_classes<-list()
models_all_classes[[length(models_all_classes)+1]]<-m # all

m <- fixest::feols(f, data%>%filter(hdiclass=="Very High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m

m <- fixest::feols(f, data%>%filter(hdiclass=="High"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Medium"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m


m <- fixest::feols(f, data%>%filter(hdiclass=="Low"), panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
models_all_classes[[length(models_all_classes)+1]]<-m
names(models_all_classes)<-c("All", "Very High", "High", "Medium", "Low" )

modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.html")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.html")))
modelsummary(models_all_classes, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models_by_class.tex")))
modelsummary(models_all_classes, vcov=lapply(models_all_classes, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_by_class_iso.tex")))

# save different m
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
#modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
#modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

# LASSO

library(glmnet)
candidate_vars_lasso<-c( trimws(unlist(strsplit(r, "\\+"))))

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
m_dem = lm(f, data_dem )
summary(m_dem)

# X and y
X <- as.matrix(data_dem[, candidate_vars_lasso, drop = FALSE])
y <- data_dem[[o]]

# Keep only complete cases
complete_idx <- complete.cases(X, y)
X_clean <- X[complete_idx, ]
y_clean <- y[complete_idx]

# Then run LASSO, RIDGE, ELASTIC
candidate_vars_lasso
run_lasso_selection(X_clean, y_clean, 1)
run_lasso_selection(X_clean, y_clean, 0)
run_lasso_selection(X_clean, y_clean, 0.5)


################################################################################
### save adap models

adap_models<-list()

o<-"gr_mys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      diff_RX +  RR_i_diff_RX +
diff_PEXT +  TM_i_diff_PEXT + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR +
lag_log_gni_pc_i_diff_PEXT +lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

o<-"gr_mys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      diff_RX + 
diff_PEXT +  
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR +
lag_log_gni_pc_i_diff_PEXT +lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m



o<-"gr_eys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
WD + WD_2 + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_WD +lag_log_gni_pc_i_WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m




o<-"gr_leb"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_WD + TM_i_diff_WD + 
diff_TVAR + TVAR_i_diff_TVAR + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m



o<-"gr_leb"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_WD +
diff_TVAR +
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m


o<-"gr_gnipc"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + 
diff_WD + RR_i_diff_WD + 
diff_PEXT + TM_i_diff_PEXT + 
diff_SPI+
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

o<-"gr_gnipc"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + 
diff_WD + 
diff_PEXT +
diff_SPI+
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

names(adap_models)<-c("gr_mys","gr_mys",  "gr_eys", "gr_leb", "gr_leb" ,"gr_gnipc", "gr_gnipc")

modelsummary(adap_models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0("_complex_adap_models.html")))
modelsummary(adap_models, vcov=lapply(adap_models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0("_complex_adap_models_dk.html")))
modelsummary(adap_models, vcov=lapply(adap_models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0("_complex_adap_models_iso.html")))



################################################################################


library(fixest)
library(dplyr)
library(purrr)
library(stringr)

# outcomes
outcomes <- c("gr_eys", "gr_leb", "gr_gnipc")

# extreme climate variables to test
extremes <- c("WD", "TVAR", "PEXT", "HW", "RX")

# panel fixed effects part
fe_part <- i   # your fixed effect

# how extreme variables enter the model
ext_specs <- list(
  base        = "%s",                     # WD
  square      = "%s + %s_2",              # WD + WD_2
  int_TM      = "%s + %s:TM",             # WD + WD:TM
  int_RR      = "%s + %s:RR",             # WD + WD:RR
  diff        = "diff_%s",                # diff_WD
  diff_int    = "diff_%s + diff_%s:%s",   # diff_WD + diff_WD:WD
  diff_int_TM = "diff_%s + diff_%s:TM",   # diff_WD + diff_WD:TM
  diff_int_RR = "diff_%s + diff_%s:RR"    # diff_WD + diff_WD:RR
)


build_ext_rhs <- function(spec_template, ext) {
  n_placeholders <- stringr::str_count(spec_template, "%s")
  args <- rep(ext, n_placeholders)       # repeat `ext` as many times as needed
  do.call(sprintf, c(list(spec_template), as.list(args)))
}


run_model <- function(outcome, rhs, data, ext_vars, cluster = "iso3") {
  f <- as.formula(paste(outcome, "~", rhs, "|", fe_part))
  m <- feols(f, data = data, panel.id = pan_id)
  sm <- summary(m, vcov = reformulate(cluster))  
  
  coef_names <- rownames(sm$coeftable)
  
  find_pvals_for_ext <- function(ext) {
    pattern <- paste0(
      "(^", ext, "$)",
      "|(^", ext, "_)",
      "|(:", ext, "$)",
      "|(^", ext, ":)",
      "|(:", ext, ":)",
      "|\\b", ext, "\\b"
    )
    matches <- grepl(pattern, coef_names, perl = TRUE)
    if (!any(matches)) return(NA_real_)
    pvals <- sm$coeftable[matches, "Pr(>|t|)"]
    setNames(as.numeric(pvals), coef_names[matches])
  }
  
  pvals_list <- lapply(ext_vars, find_pvals_for_ext)
  names(pvals_list) <- ext_vars
  
  return(list(model = m, summary = sm, pvals = pvals_list))
}



outcomes <- c("gr_eys", "gr_leb", "gr_gnipc")
extremes <- c("WD", "TVAR", "PEXT", "HW", "RX")

burke_base <- "TM + TM_2 + RR + RR_2"
diff_base  <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "

results <- list()

for (o in outcomes) {
  for (ext in extremes) {
    for (spec_name in names(ext_specs)) {
      
      # Build RHS for extreme variable
      ext_rhs <- build_ext_rhs(ext_specs[[spec_name]], ext)
      
      ####### BURKE MODEL #######
      rhs_burke <- paste(burke_base, "+", ext_rhs)
      res_burke <- run_model(outcome=o, rhs=rhs_burke, data, ext_vars = c(ext, paste0("diff_", ext)))
      
      # collect all extreme-related p-values
      pvals <- res_burke$pvals[[ext]]
      diff_pvals <- res_burke$pvals[[paste0("diff_", ext)]]
      all_pvals <- c(pvals, diff_pvals)
      sig_terms <- names(all_pvals)[which(all_pvals < 0.1)]
      any_sig <- length(sig_terms) > 0
      
      results[[length(results)+1]] <- data.frame(
        outcome = o,
        extreme = ext,
        model_type = "burke",
        spec = spec_name,
        rhs = rhs_burke,
        all_pvals = paste(round(all_pvals,4), collapse = "; "),
        significant = any_sig,
        sig_terms = paste(sig_terms, collapse = "; ")
      )
      
      ####### DIFF MODEL #######
      rhs_diff <- paste(diff_base, "+", ext_rhs)
      res_diff <- run_model(o, rhs_diff, data, ext_vars = c(ext, paste0("diff_", ext)))
      
      pvals <- res_diff$pvals[[ext]]
      diff_pvals <- res_diff$pvals[[paste0("diff_", ext)]]
      all_pvals <- c(pvals, diff_pvals)
      sig_terms <- names(all_pvals)[which(all_pvals < 0.1)]
      any_sig <- length(sig_terms) > 0
      
      results[[length(results)+1]] <- data.frame(
        outcome = o,
        extreme = ext,
        model_type = "diff",
        spec = spec_name,
        rhs = rhs_diff,
        all_pvals = paste(round(all_pvals,4), collapse = "; "),
        significant = any_sig,
        sig_terms = paste(sig_terms, collapse = "; ")
      )
      
    }
  }
}

results_df <- do.call(rbind, results)

summary_table <- results_df %>%
  mutate(sig = ifelse(significant, "YES", "no")) %>%
  select(outcome, extreme, model_type, spec, sig, sig_terms, all_pvals)

summary_table




library(dplyr)

collapsed_table <- results_df %>%
  group_by(outcome, extreme) %>%
  summarise(
    any_sig = any(significant),
    
    # collapse all significant terms into a single string
    sig_terms = {
      terms <- sig_terms[significant]        # select only significant specs
      if(length(terms) == 0) {
        ""
      } else {
        paste(unique(unlist(strsplit(paste(terms, collapse="; "), "; "))), collapse="; ")
      }
    },
    
    # collapse p-values of significant terms into a single string
    all_pvals = {
      pvals <- all_pvals[significant]       # select only significant specs
      if(length(pvals) == 0) {
        ""
      } else {
        paste(unique(unlist(strsplit(paste(pvals, collapse="; "), "; "))), collapse="; ")
      }
    },
    
    .groups = "drop"
  ) %>%
  mutate(sig = ifelse(any_sig, "YES", "no")) %>%
  select(outcome, extreme, sig, sig_terms, all_pvals)

collapsed_table

