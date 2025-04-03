###

rm(list=ls())

library(dplyr)


specifications<-c("mean_mod") #   ,"mean_mod_spec")
#types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_extr_tp", "extr_only")
# spec_type<-"dlm"
# effect<-"growth_eff"

NL<-"_mix_"

spec<-specifications[1]
#type<-types[1]
vars<-vars_in_proj[1]


out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

n_boot<-1000


library(sensobol)
N <- 250
params <- c("boot", "ssp", "model")
order <-  "third"

res<-list()

resp_types<-"diff"

for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
  
  for (type in resp_types){
    load(file.path(out_dir, paste0(o, "_", type, '_',spec,'_',vars, "_pop_weight_nlags",NL,"_sobol_dec_glob.RData")))
    res[[paste0(o)]][[paste0(type)]]<-results_all_glob
  
    }
  
}

