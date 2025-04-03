rm(list=ls())

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
out_dir<-"output/models/hdi_and_comp_tp_literature_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
colnames(data)

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)


################################################################################

### 
# we try: 
# - burke 2015, on diff for indeces bet 0,100 and on growth for gni 
# - kalkhul wenz 2020 
# - adding to all adaptation in the sense of gni 
# - adding to all adaptation in the sense of hdi 
# - them, plus resp variable at year before (autoregressive model)

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)

colnames(data)

# out variables 
out_variables<-c("diff_hdi", "diff_health_index", "diff_edu_index", "diff_income_index", "gr_log_gni_pc")

pan_id<-c('gdlcode', 'year')

# ids 
idx_small<-"gdlcode + year"
idx_iso<-"gdlcode + year + iso3[year] +iso3[year^2]"
idx_full<-"gdlcode + year + gdlcode[year] + gdlcode[year^2]"

indeces<-list(idx_small, idx_iso,  idx_full)
names(indeces)<-c("idx_small", "idx_iso",  "idx_full")


# test unit root 
library(plm)
library(tseries)
library(tidyr)
pdata<-pdata.frame(data[ ,c("year", "gdlcode","diff_hdi", "diff_health_index", "diff_edu_index", "diff_income_index", "gr_log_gni_pc" )], index = pan_id)
pdata<-drop_na(pdata)
adf.test(pdata$diff_income_index)
#purtest(pdata$diff_hdi,exo = "trend", test = "hadri")

# formulas
right_list_classic<- list(
  
  "TM + TM_2 + RR + RR_2", 
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + lag_diff_TM:TM + lag_diff_RR:RR + TM + TM_2 + RR + RR_2"
  
)

right_list_adap_gni<- list(
  
  "TM + TM_2 + RR + RR_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR + lag_log_gni_pc:RR_2", 
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR ++lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + lag_diff_TM:TM + lag_diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR + lag_log_gni_pc:diff_RR_2"
  
)

right_list_adap_hdi<- list(
  
  "TM + TM_2 + RR + RR_2 + lag_hdi:TM + lag_hdi:TM_2 + lag_hdi:RR + lag_hdi:RR_2", 
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 +lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR +lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR ++lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2",
  "diff_TM + diff_RR + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2",
  "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 + lag_diff_TM + lag_diff_RR + diff_TM:TM + diff_RR:RR + lag_diff_TM:TM + lag_diff_RR:RR + TM + TM_2 + RR + RR_2 +lag_hdi:diff_TM + lag_hdi:diff_TM_2 + lag_hdi:diff_RR + lag_hdi:diff_RR_2"
  
)

mod_names<-c("burke",
             "diff_sq",
             "diff_sq_lag",
             "diff_lag_interact",
             "diff_lag_interact_burke",
             "diff_sq_lag_interact_burke",
             "diff_sq_lag_interact_lag_burke"
             )

names(right_list_classic)<-mod_names
names(right_list_adap_gni)<-mod_names
names(right_list_adap_hdi)<-mod_names

right_lists<-list(right_list_classic, right_list_adap_gni , right_list_adap_hdi)
names(right_lists)<-c("classic", "adap_gni", "adap_hdi")

# models , no autoregress
models<-list()
for ( o in out_variables){
  for (i in indeces){
    for (l in names(right_lists)){
      for (r in right_lists[[l]]){
        f <- as.formula(paste( o, "~", r, "|" ,i ))
        models[[o]][[i]][[names(right_lists[l])]][[r]] <- fixest::feols(f, data, panel.id=pan_id)
      }
    }
  }
}


# save models , no autoregress
for ( o in 1:length(out_variables)){
  for (i in 1:length(indeces)){
    for (l in names(right_lists)){
      
    mods<-  models[[o]][[i]][[names(right_lists[l])]]
    modelsummary(mods, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                 output=file.path(out_dir, paste0("models", "_", out_variables[o], "_" , l , "_", names(indeces)[i] , "_" , "reg", ".html")))
    modelsummary(mods, lapply(mods, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                 output=file.path(out_dir, paste0("models", "_", out_variables[o], "_", names(indeces)[i], "_" , "dk", ".html")))
    modelsummary(mods, lapply(mods, FUN=fvcov_iso),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                 output=file.path(out_dir, paste0("models", "_", out_variables[o], "_", names(indeces)[i], "_" , "iso", ".html")))
      
    }
  }
}

###

# models , autoregressive
models_ar<-list()
for ( o in out_variables){
  for (i in indeces){
    for (l in names(right_lists)){
      for (r in right_lists[[l]]){
        f <- as.formula(paste( o, "~", r, "+", paste0("lag_",o), "|" ,i )) # addedd lagged response variable here 
        models_ar[[o]][[i]][[names(right_lists[l])]][[r]] <- fixest::feols(f, data, panel.id=pan_id)
      }
    }
  }
}


# save models , autoregressive
for ( o in 1:length(out_variables)){
  for (i in 1:length(indeces)){
    for (l in names(right_lists)){
      
      mods<-  models_ar[[o]][[i]][[names(right_lists[l])]]
      modelsummary(mods, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                   output=file.path(out_dir, paste0("models", "_", out_variables[o], "_" , l , "_", names(indeces)[i] , "_" , "reg", "_ar", ".html")))
      modelsummary(mods, lapply(mods, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                   output=file.path(out_dir, paste0("models", "_", out_variables[o], "_", names(indeces)[i], "_" ,  "dk", "_ar",  ".html")))
      modelsummary(mods, lapply(mods, FUN=fvcov_iso),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
                   output=file.path(out_dir, paste0("models", "_", out_variables[o], "_", names(indeces)[i], "_" ,  "iso", "_ar",  ".html")))
      
    }
  }
}

################################################################################

# plots of comparison
# bic, aic, rsquared of the models, assessing:
# - autoregressive / not 
# - adaptation 1 / adaptation 2 / not 

# table with : type of model, type of index, autoreg or not, rsq, bic, aic 

results<-expand.grid(out_var=as.character(out_variables),
                     id=as.character(names(indeces)), 
                     adap=as.character(names(right_lists)), 
                     model=as.character(mod_names),
                     ar=as.numeric(c(0,1))
                     )
results$rsq<-NA
results$AIC<-NA
results$BIC<-NA

for ( o in out_variables){
  for (i in names(indeces)){
    for (l in names(right_lists)){
      n=1
      for (r in right_lists[[l]] ){
        mod<- models[[o]][[indeces[[i]]]][[names(right_lists[l])]][[r]]
        rsq<-r2( mod, type='wr2' )
        aic<-AIC(mod)
        bic<-BIC(mod)
        results[which(results$out_var==o & results$id==i & results$ar==0 & results$adap==l & results$model==mod_names[n]), c("rsq")]<-rsq
        results[which(results$out_var==o & results$id==i & results$ar==0 & results$adap==l & results$model==mod_names[n]), c("AIC")]<-aic
        results[which(results$out_var==o & results$id==i & results$ar==0 & results$adap==l & results$model==mod_names[n]), c("BIC")]<-bic
        
        mod_ar<- models_ar[[o]][[indeces[[i]]]][[names(right_lists[l])]][[r]]
        rsq<-r2( mod_ar, type='wr2' )
        aic<-AIC(mod_ar)
        bic<-BIC(mod_ar)
        results[which(results$out_var==o & results$id==i & results$ar==1 & results$adap==l & results$model==mod_names[n]), c("rsq")]<-rsq
        results[which(results$out_var==o & results$id==i & results$ar==1 & results$adap==l & results$model==mod_names[n]), c("AIC")]<-aic
        results[which(results$out_var==o & results$id==i & results$ar==1 & results$adap==l & results$model==mod_names[n]), c("BIC")]<-bic
       
        n<-n+1
        
      }
    }
  }
}

results$adap_ar<-paste0(results$adap, "_", results$ar)

results %>%
  filter(out_var=="diff_hdi")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  geom_text(size = 2, colour = "black", vjust = -1) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")
results %>%
  filter(out_var=="diff_edu_index")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  #geom_text(size = 1, colour = "black", vjust = -1) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")
results %>%
  filter(out_var=="diff_health_index")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  #geom_text(size = 1, colour = "black", vjust = -1) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")
results %>%
  filter(out_var=="diff_income_index")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  #geom_text(size = 1, colour = "black", vjust = -1) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")


# keep only idx iso

results %>%
  filter( id=="idx_iso")%>%
  filter(out_var=="diff_hdi")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")

results %>%
  filter( id=="idx_iso")%>%
  filter(out_var=="diff_hdi", ar==1)%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")
results %>%
  filter( id=="idx_iso")%>%
  filter(out_var=="diff_hdi", ar==0)%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=model, label = id)) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")


results %>%
  filter( id=="idx_iso")%>%
  filter(out_var=="diff_hdi")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=as.factor(ar), label = id)) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")

results %>%
  filter( id=="idx_iso")%>%
  filter(out_var=="diff_hdi")%>%
  arrange(desc(rsq)) %>%
  ggplot(aes(x=AIC, y=BIC, size=rsq, color=as.factor(adap), label = id)) +
  geom_point(aes(shape = adap_ar),alpha=1) + scale_color_brewer(palette = "Set1")


