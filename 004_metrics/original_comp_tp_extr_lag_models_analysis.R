
rm(list=ls())

source("utils/libraries.R")

### output dir
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}

# example
load(paste0(out_dir, "/JUNO/components_tp_extr_lag_models_performances/", "fe_linear_iso", "_", "gr_gnipc","_all_extr_all_lags_models_metrics.RData"))

fixed_effects<-c("fe_linear_iso", "fe_quad_gdl", "fe_quad_iso", "fe_simple")
out_variables<-c("gr_eys", "gr_gnipc", "gr_mys", "gr_leb")
model_types_simple_higher<-c("level","diff")
model_types_simple<-c( names(metrics[[1]][[1]]), names(metrics[[1]][[2]]) )

model_types_lags<-c("lag_level_interact", "lag_level", "lag_diff_interact_extr", "lag_diff_interact_TM", "lag_diff_interact_RR")

variables<-c("TVAR",  "HW", "SPI", "SPEI", "RX", "PEXT", "WD", "PET")

results_all<-list() 

for (var in variables){
  
  results<-data.frame(expand.grid(fe=fixed_effects,out_var=out_variables, model_type=model_types_lags , model_type_higher="lags", 
                                  n_lags_TM=1:8, n_lags_RR=1:8, n_lags_EXTR=1:8))
  results<-rbind(results, data.frame(expand.grid(fe=fixed_effects,out_var=out_variables, model_type=model_types_simple ,  model_type_higher=model_types_simple_higher, 
                                                 n_lags_TM=0, n_lags_RR=0, n_lags_EXTR=0)))
  results$loocv<-NA
  
  for (fe in fixed_effects){
    for (out_var in out_variables){
      
      for (model_type in model_types_lags){
        for (lag_TM in c(1:8)){
          for (lag_RR in c(1:8)){
            for (lag_EXTR in c(1:8)){
              load(paste0(out_dir, "/JUNO/components_tp_extr_lag_models_performances/", fe, "_", out_var,"_all_extr_all_lags_models_metrics.RData"))
              
              results[which(results$fe==fe & results$out_var==out_var & results$model_type==model_type &
                              results$n_lags_TM  == lag_TM &results$n_lags_RR  == lag_RR &results$n_lags_EXTR  == lag_EXTR), "loocv"] <- 
                metrics[[paste0(var)]][[paste0(model_type)]][[paste0("TM_",lag_TM,  "_RR_",lag_RR,  "_",var,"_", lag_EXTR)]]$loo_cv
            }
          }
        }
        
      }
      for (model_type in model_types_simple){
        for (h in model_types_simple_higher){
          
          if(!is.null(metrics[[paste0(var)]][[paste0(h)]][[paste0(model_type)]]$loo_cv)){
            results[which(results$fe==fe & results$out_var==out_var & results$model_type==model_type & results$model_type_higher==h & 
                            results$n_lags_TM  == 0 & results$n_lags_RR  == 0 & results$n_lags_EXTR  == 0), "loocv"] <- 
              metrics[[paste0(var)]][[paste0(h)]][[paste0(model_type)]]$loo_cv
          }
          
          
        }
      }
      
      
    }
  }
  results_all[[paste0(out_var)]]<-results 
}


