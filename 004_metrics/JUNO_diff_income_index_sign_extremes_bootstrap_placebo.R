### diff_hdi as response 

### with selected idx thanks to cv, we analyse tables results and keep, from simpler models, meaningful extremes for each response var
### in addition, we do bootstrap placebo test for meaningful variables in all specifications, to check variables importance

#install.libraries(c("parallel", "foreach", "doParallel"), ncpus=72)
### 
rm(list=ls())

library(dplyr)
library(fixest)
library(readr)
library(modelsummary)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)

# output dir
out_dir<-"output"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics/sign_extremes_bootstrap_placebo"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("data_hdi_climate_gdl_pop_weight_1990_2021.csv")

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET", 
        'diff_TM_2','diff_RR_2', "diff_TVAR_2", "diff_HW_2", "diff_RX_2", "diff_PEXT_2", "diff_WD_2", "diff_SPI_2", "diff_SPEI_2", "diff_PET_2")
modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET", 
        'TM_2','RR_2', "TVAR_2", "HW_2", "RX_2", "PEXT_2", "WD_2", "SPI_2", "SPEI_2", "PET_2")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 
       'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# n lags 
N_temp<-8
N_rain<-4
N<-8

################################################################################

# parallelize 
cores=detectCores()
# cl <- makeCluster(cores[1]-1) # locale 
cl <- makeCluster(cores[1]) # zeus
registerDoParallel(cl)

# setup
out_var<-"diff_income_index"
idx<-"gdlcode + iso3[year] + iso3[year^2] + year"

extr_variables<-c("TVAR", "HW", "RX" , "PEXT", "WD", "SPI", "SPEI", "PET")

nboot<-1000

set.seed(666)

data %>% filter(TM!=0) %>%
  group_by(gdlcode) %>% 
  filter(n()>1) -> panel_rand


formulations<-c("burke_extr_sq", "burke_extr_sq_adap")


for (form in formulations){
  for (ext in extr_variables){
    
    
    
    if (form == "burke_extr_sq"){ 
       
      r <- paste0("TM + TM_2 + RR + RR_2 + ext_rand + ext_rand_2")
      f <- as.formula(paste( out_var, "~", r, "|" ,idx ))
      
      #rand_coefs_nonrand <- rand_coefs_overall <- 
      #  rand_coefs_withinregion <-rand_coefs_withinyear <-data.frame("boot"=c(1:nboot),
      #                                                               "coef_1"=rep(0,nboot),
      #                                                               "coef_2"=rep(0,nboot))
      for (k in c(1:4)){
        
      coefs<-foreach(i = (1:nboot), .combine = 'rbind', .export = c(), .packages = c("dplyr") )%dopar%{ 
      #for ( n in c(1:nboot)){
        
          # randomize three different ways
          if (k==1){ # original model -- bootstrap by region
            panel_rand <- panel_rand %>% 
              mutate(ext_rand = (!!as.name(ext)))
            ids <- unique(panel_rand$gdlcode)
            regions_boot <- sample(ids,size=length(ids),replace=T)
            df_boot <- lapply(regions_boot, function(x) which(panel_rand[,'gdlcode']==x))
            u<-unlist(df_boot)
            panel_rand_final <- panel_rand[u,]
          } else if (k==2){
            panel_rand_final <- panel_rand %>% 
              mutate(ext_rand = sample(!!as.name(ext)))
          } else if (k==3){
            panel_rand_final <- panel_rand %>% 
              group_by(year) %>%
              mutate(ext_rand = sample(!!as.name(ext)))
          } else {
            panel_rand_final <- panel_rand %>% 
              group_by(gdlcode) %>%
              mutate(ext_rand = sample(!!as.name(ext)))
          }
          
          panel_rand_final<- panel_rand_final%>%
            mutate(ext_rand_2 = (ext_rand)^2)
          # estimate model
          mdl <- fixest::feols(f, panel_rand_final)
          
          cbind(as.numeric(coef(summary(mdl))["ext_rand"]), 
                as.numeric(coef(summary(mdl))["ext_rand_2"]))
          
          #if (k==1){
          #  rand_coefs_nonrand[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_nonrand[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else if (k==2){
          #  rand_coefs_overall[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_overall[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else if (k==3){
          #  rand_coefs_withinyear[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_withinyear[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else {
          #  rand_coefs_withinregion[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_withinregion[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #}
      }
      
      coefs<-data.frame(coefs)
      colnames(coefs)<-c("coef_1", "coef_2")
      coefs$boot<-c(1:nboot)
      if (k==1){
        rand_coefs_nonrand<-coefs
      } else if (k==2){
        rand_coefs_overall<-coefs
      } else if (k==3){
        rand_coefs_withinyear<-coefs
      } else {
        rand_coefs_withinregion<-coefs
      }
      
      }
      
      write.csv(rand_coefs_nonrand,file.path(out_dir,paste0(out_var, "_",ext, "_",form, "_coefs_randomization_nonrandom.csv")))
      write.csv(rand_coefs_overall,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_fullsample.csv")))
      write.csv(rand_coefs_withinyear,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinyear.csv")))
      write.csv(rand_coefs_withinregion,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinregion.csv")))
      
      print(paste0("done","_",  out_var, "_",ext, "_",form))
      
    }
    
    
    if (form == "burke_extr_sq_adap"){ 
      
      r <- paste0("TM + TM_2 + RR + RR_2 + ext_rand + ext_rand_2 + ext_rand_i_lag_log_gni_pc +ext_rand_2_i_lag_log_gni_pc")
      f <- as.formula(paste( out_var, "~", r, "|" ,idx ))
      
      #rand_coefs_nonrand <- rand_coefs_overall <- 
      #  rand_coefs_withinregion <-rand_coefs_withinyear <-data.frame("boot"=c(1:nboot),
      #                                                               "coef_1"=rep(0,nboot),
      #                                                               "coef_2"=rep(0,nboot))
      for (k in c(1:4)){
        
        coefs<-foreach(i = (1:nboot), .combine = 'rbind', .export = c(), .packages = c("dplyr") )%dopar%{ 
          #for ( n in c(1:nboot)){
          
          # randomize three different ways
          if (k==1){ # original model -- bootstrap by region
            panel_rand <- panel_rand %>% 
              mutate(ext_rand = (!!as.name(ext)))
            ids <- unique(panel_rand$gdlcode)
            regions_boot <- sample(ids,size=length(ids),replace=T)
            df_boot <- lapply(regions_boot, function(x) which(panel_rand[,'gdlcode']==x))
            u<-unlist(df_boot)
            panel_rand_final <- panel_rand[u,]
          } else if (k==2){
            panel_rand_final <- panel_rand %>% 
              mutate(ext_rand = sample(!!as.name(ext)))
          } else if (k==3){
            panel_rand_final <- panel_rand %>% 
              group_by(year) %>%
              mutate(ext_rand = sample(!!as.name(ext)))
          } else {
            panel_rand_final <- panel_rand %>% 
              group_by(gdlcode) %>%
              mutate(ext_rand = sample(!!as.name(ext)))
          }
          
          panel_rand_final<- panel_rand_final%>%
            mutate(ext_rand_2 = (ext_rand)^2)%>%
            mutate(ext_rand_i_lag_log_gni_pc= ext_rand*lag_log_gni_pc, 
                   ext_rand_2_i_lag_log_gni_pc= ext_rand_2*lag_log_gni_pc)
                   
          
          # estimate model
          mdl <- fixest::feols(f, panel_rand_final)
          
          cbind(as.numeric(coef(summary(mdl))["ext_rand"]), 
                as.numeric(coef(summary(mdl))["ext_rand_2"]), 
                as.numeric(coef(summary(mdl))["ext_rand_i_lag_log_gni_pc"]),
                as.numeric(coef(summary(mdl))["ext_rand_2_i_lag_log_gni_pc"])
                )
          
          #if (k==1){
          #  rand_coefs_nonrand[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_nonrand[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else if (k==2){
          #  rand_coefs_overall[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_overall[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else if (k==3){
          #  rand_coefs_withinyear[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_withinyear[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #} else {
          #  rand_coefs_withinregion[n,"coef_1"] <- coef(summary(mdl))["ext_rand"]
          #  rand_coefs_withinregion[n,"coef_2"] <- coef(summary(mdl))["ext_rand_2"]
          #}
        }
        
        coefs<-data.frame(coefs)
        colnames(coefs)<-c("coef_1", "coef_2", "coef_1_adap", "coef_2_adap")
        coefs$boot<-c(1:nboot)
        if (k==1){
          rand_coefs_nonrand<-coefs
        } else if (k==2){
          rand_coefs_overall<-coefs
        } else if (k==3){
          rand_coefs_withinyear<-coefs
        } else {
          rand_coefs_withinregion<-coefs
        }
        
      }
      
      write.csv(rand_coefs_nonrand,file.path(out_dir,paste0(out_var, "_",ext, "_",form, "_coefs_randomization_nonrandom.csv")))
      write.csv(rand_coefs_overall,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_fullsample.csv")))
      write.csv(rand_coefs_withinyear,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinyear.csv")))
      write.csv(rand_coefs_withinregion,file.path(out_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinregion.csv")))
      
      print(paste0("done","_",  out_var, "_",ext, "_",form))
      
    }

    
  }
}










# callahan code
# 
# 
# # randomize within region and within year
# # drop singleton rows from panel_rand or else
# # R gets mad when we try to "sample"
# panel %>% filter(t!=0) %>%
#   group_by(region) %>% 
#   filter(n()>1) -> panel_rand
# nboot <- 1000
# set.seed(120)
# extr <- "tx5d"
# i <- "t"
# fe <- "region + time"
# cl <- "0"
# form <- as.formula(paste0("growth ~ t + t2 + tx_rand + tx_rand:",i," + var + var:seas + p | ",fe," | 0 | ",cl))
# 
# 
# rand_coefs_nonrand <- data.frame("boot"=c(1:nboot),
#                                  "coef_main"=c(1:nboot),
#                                  "coef_interact"=c(1:nboot)) # original model
# rand_coefs_overall <- data.frame("boot"=c(1:nboot),
#                                  "coef_main"=c(1:nboot),
#                                  "coef_interact"=c(1:nboot))
# rand_coefs_withinregion <- data.frame("boot"=c(1:nboot),
#                                       "coef_main"=c(1:nboot),
#                                       "coef_interact"=c(1:nboot))
# rand_coefs_withinyear <- data.frame("boot"=c(1:nboot),
#                                     "coef_main"=c(1:nboot),
#                                     "coef_interact"=c(1:nboot))
# 
# for (n in c(1:nboot)){
#   print(n)
#   for (k in c(1:4)){
#     
#     # randomize three different ways
#     if (k==1){ # original model -- bootstrap by region
#       panel_rand <- panel_rand %>% 
#         mutate(tx_rand = (!!as.name(extr)))
#       ids <- unique(panel_rand$region)
#       regions_boot <- sample(ids,size=length(ids),replace=T)
#       df_boot <- sapply(regions_boot, function(x) which(panel_rand[,'region']==x))
#       panel_rand_final <- panel_rand[unlist(df_boot),]
#     } else if (k==2){
#       panel_rand_final <- panel_rand %>% 
#         mutate(tx_rand = sample(!!as.name(extr)))
#     } else if (k==3){
#       panel_rand_final <- panel_rand %>% 
#         group_by(time) %>%
#         mutate(tx_rand = sample(!!as.name(extr)))
#     } else {
#       panel_rand_final <- panel_rand %>% 
#         group_by(region) %>%
#         mutate(tx_rand = sample(!!as.name(extr)))
#     }
#     
#     # estimate model
#     mdl <- felm(form,data=panel_rand_final)
#     
#     if (k==1){
#       rand_coefs_nonrand[n,"coef_main"] <- coef(summary(mdl))["tx_rand","Estimate"]
#       rand_coefs_nonrand[n,"coef_interact"] <- coef(summary(mdl))[paste0(i,":tx_rand"),"Estimate"]
#     } else if (k==2){
#       rand_coefs_overall[n,"coef_main"] <- coef(summary(mdl))["tx_rand","Estimate"]
#       rand_coefs_overall[n,"coef_interact"] <- coef(summary(mdl))[paste0(i,":tx_rand"),"Estimate"]
#     } else if (k==3){
#       rand_coefs_withinyear[n,"coef_main"] <- coef(summary(mdl))["tx_rand","Estimate"]
#       rand_coefs_withinyear[n,"coef_interact"] <- coef(summary(mdl))[paste0(i,":tx_rand"),"Estimate"]
#     } else {
#       rand_coefs_withinregion[n,"coef_main"] <- coef(summary(mdl))["tx_rand","Estimate"]
#       rand_coefs_withinregion[n,"coef_interact"] <- coef(summary(mdl))[paste0(i,":tx_rand"),"Estimate"]
#     }
#   }
# }
# write.csv(rand_coefs_nonrand,paste0(loc_save_reg,extr,"_coefs_randomization_nonrandom.csv"))
# write.csv(rand_coefs_overall,paste0(loc_save_reg,extr,"_coefs_randomization_fullsample.csv"))
# write.csv(rand_coefs_withinyear,paste0(loc_save_reg,extr,"_coefs_randomization_withinyear.csv"))
# write.csv(rand_coefs_withinregion,paste0(loc_save_reg,extr,"_coefs_randomization_withinregion.csv"))
# 
# print("done!")
# 
# 
# 