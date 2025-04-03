### functions for proj damages 


rm(list=ls())
source("utils/libraries.R")

source("scripts/005_projections/proj_functions.R")

### data for historical estimation of dam func
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
modns_2=c('TM_2','RR_2', "TVAR_2", "HW_2", "RX_2", "PEXT_2", "WD_2", "SPI_2", "SPEI_2", "PET_2")

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


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns_2[i],sep='')] <- data[adap[i]] * data[modns_2[i]]
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



### out dir 

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp" 
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
if(!dir.exists(out_dir_pop)){dir.create(out_dir_pop)}


### codes for connecting to gdlcode  
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")


ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


#######################

out_variables<-c( "leb", "gnipc" , "eys", "mys" )





################################################################################
### distributed lag model 

### iteratively, for each lag model, no specification division since they were VERY similar in coefs (simple, with controls, with autoreg)


out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

#lags number 
N<-8

coefs_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_vars")#, "extr_only")

#define regions 
regions<-gdlcodes_iso$gdlcode#[ which(gdlcodes_iso$gdlcode %in% unique(data_proj_gni$gdlcode)) ]

for (spec in specifications){
  out_dir_lag_spec<-file.path(out_dir_lag, spec)
  if(!dir.exists(out_dir_lag_spec)){dir.create(out_dir_lag_spec)}
  
  for (out_var in out_variables){
    out_dir_lag_spec_var<-file.path(out_dir_lag_spec, out_var)
    if(!dir.exists(out_dir_lag_spec_var)){dir.create(out_dir_lag_spec_var)}
    
    for (type in types){
      out_dir_lag_spec_var_type<-file.path(out_dir_lag_spec_var, type)
      if(!dir.exists(out_dir_lag_spec_var_type)){dir.create(out_dir_lag_spec_var_type)}
      
      gc()
      
      # data
      data_proj<-read_csv(file.path(out_dir, paste0("preproc_proj_", out_var, ".csv")))
      data_proj<-left_join(data_proj, gdlcodes_iso)
      
      
      for (vars in vars_in_proj){
        
        out_dir_lag_spec_var_type_vars<-file.path(out_dir_lag_spec_var_type, vars)
        if(!dir.exists(out_dir_lag_spec_var_type_vars)){dir.create(out_dir_lag_spec_var_type_vars)}
        
        # coefs
        coefs_table<-read_csv(file.path(coefs_dir,paste0("gr_",out_var, '_',type, '_',spec,'_lagN',as.character(N),'_coef.csv')))
        coefs<-coefs_table$x
        names(coefs)<-coefs_table$...1
        
        # if we only consider extremes coefficients 
        if (vars == "extr_only"){
          coefs<-coefs[!grepl("TM", names(coefs))]
          coefs<-coefs[!grepl("RR", names(coefs))]
        }
        
        climate_projections_dir<-"output/projections/climate_projections_gdl"
        agg<-"pop_weight"
        
        deltas_path<-file.path(out_dir_lag_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,".feather"))
        
        if(!file.exists(deltas_path)){
          cl_deltas<-clim_deltas_calc(climate_projections_dir,
                                      agg,
                                      gdlcodes_iso$gdlcode, 
                                      coefs, 
                                      paste0("gr_",out_var))
          arrow::write_feather(cl_deltas,deltas_path )
        }else{
          cl_deltas<-arrow::read_feather(deltas_path)
        }
        
        
        
        # distr lag model proj, growth effect: we assume persistency of loss
        project_impacts(paste0("gr_",out_var),  
                        regions, 
                        data_proj,
                        out_dir_lag_spec_var_type_vars,
                        cl_deltas, 
                        "dlm",
                        "growth_eff",
                        maps=FALSE)
        
        # distr lag model proj, level effect: we assume no persistency of loss
        # project_impacts(paste0("gr_",out_var),  
        #                 regions, 
        #                 data_proj,
        #                 out_dir_lag_spec_var_type_vars,
        #                 cl_deltas, 
        #                 "dlm",
        #                 "level_eff",
        #                 maps=FALSE)
        
  
      }
    
    }
    
  }
  
}






################################################################################

### bhm style
# mean hist climate for level damage functions 

data_mean <- data %>% group_by(gdlcode) %>% transmute(
  TM=mean(TM), 
  RR=mean(RR), 
  TVAR=mean(TVAR), 
  HW=mean(HW), 
  RX=mean(RX), 
  PEXT=mean(PEXT), 
  WD=mean(WD), 
  SPI=mean(SPI), 
  SPEI=mean(SPEI), 
  PET=mean(PET), 
  
  TM_2=mean(TM_2), 
  RR_2=mean(RR_2), 
  TVAR_2=mean(TVAR_2), 
  HW_2=mean(HW_2), 
  RX_2=mean(RX_2), 
  PEXT_2=mean(PEXT_2), 
  WD_2=mean(WD_2), 
  SPI_2=mean(SPI_2), 
  SPEI_2=mean(SPEI_2), 
  PET_2=mean(PET_2), 
  
  lag_log_gni_pc=mean(lag_log_gni_pc, na.rm=TRUE)
  
  # diff_TM= mean(diff_TM, na.rm=TRUE), 
  # diff_RR= mean(diff_RR, na.rm=TRUE)
  # 
  # lag_log_gni_pc_i_TM=mean(lag_log_gni_pc_i_TM), 
  # lag_log_gni_pc_i_RR=mean(lag_log_gni_pc_i_RR), 
  # lag_log_gni_pc_i_TVAR=mean(lag_log_gni_pc_i_TVAR), 
  # lag_log_gni_pc_i_HW=mean(lag_log_gni_pc_i_HW), 
  # lag_log_gni_pc_i_RX=mean(lag_log_gni_pc_i_RX), 
  # lag_log_gni_pc_i_PEXT=mean(lag_log_gni_pc_i_PEXT), 
  # lag_log_gni_pc_i_WD=mean(lag_log_gni_pc_i_WD), 
  # lag_log_gni_pc_i_SPI=mean(lag_log_gni_pc_i_SPI), 
  # lag_log_gni_pc_i_SPEI=mean(lag_log_gni_pc_i_SPEI), 
  # lag_log_gni_pc_i_PET=mean(lag_log_gni_pc_i_PET), 
  # 
  # lag_log_gni_pc_i_TM_2=mean(lag_log_gni_pc_i_TM_2), 
  # lag_log_gni_pc_i_RR_2=mean(lag_log_gni_pc_i_RR_2), 
  # lag_log_gni_pc_i_TVAR_2=mean(lag_log_gni_pc_i_TVAR_2), 
  # lag_log_gni_pc_i_HW_2=mean(lag_log_gni_pc_i_HW_2), 
  # lag_log_gni_pc_i_RX_2=mean(lag_log_gni_pc_i_RX_2), 
  # lag_log_gni_pc_i_PEXT_2=mean(lag_log_gni_pc_i_PEXT_2), 
  # lag_log_gni_pc_i_WD_2=mean(lag_log_gni_pc_i_WD_2), 
  # lag_log_gni_pc_i_SPI_2=mean(lag_log_gni_pc_i_SPI_2), 
  # lag_log_gni_pc_i_SPEI_2=mean(lag_log_gni_pc_i_SPEI_2), 
  # lag_log_gni_pc_i_PET_2=mean(lag_log_gni_pc_i_PET_2), 
  
)
data_mean<-unique(data_mean)



out_dir_bhm<-file.path(out_dir_pop, "bhm_models")
if(!dir.exists(out_dir_bhm)){dir.create(out_dir_bhm)}


coefs_dir<-"output/models/original_components_bhm_tp_extr_adap_complex_models"

specifications<-c("no_adap" ,"adap")
types<-c("all_extr") #  , "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c("all_vars", "extr_only")

data_proj_gni<- read_csv(file.path(out_dir, paste0("preproc_proj_gnipc.csv")))
data_proj_gni<-left_join(data_proj_gni, gdlcodes_iso)

data_proj_gni<- data_proj_gni %>% 
  arrange(Scenario, gdlcode, year) %>%
  group_by(Scenario, gdlcode) %>%
  mutate(lag_log_gni_pc = dplyr::lag( log(value_interp) ))
colnames(data_proj_gni)[colnames(data_proj_gni)=="Scenario"]<-"ssp"


# define regions 
regions<-gdlcodes_iso$gdlcode[ which(gdlcodes_iso$gdlcode %in% unique(data_proj_gni$gdlcode)) ]
out_variables<-"gnipc"


for (spec in specifications){
  out_dir_bhm_spec<-file.path(out_dir_bhm, spec)
  if(!dir.exists(out_dir_bhm_spec)){dir.create(out_dir_bhm_spec)}
  
  for (out_var in out_variables){
    out_dir_bhm_spec_var<-file.path(out_dir_bhm_spec, out_var)
    if(!dir.exists(out_dir_bhm_spec_var)){dir.create(out_dir_bhm_spec_var)}
    
    for (type in types){
      out_dir_bhm_spec_var_type<-file.path(out_dir_bhm_spec_var, type)
      if(!dir.exists(out_dir_bhm_spec_var_type)){dir.create(out_dir_bhm_spec_var_type)}
      
      gc()
      
      # data
      data_proj<-read_csv(file.path(out_dir, paste0("preproc_proj_", out_var, ".csv")))
      data_proj<-inner_join(data_proj, gdlcodes_iso)
      
     
      
      for (vars in vars_in_proj){
        
        # coefs handling
        coefs_table<-read_csv(file.path(coefs_dir,paste0("gr_", out_var, '_',type, '_bhm_',spec,'_coef.csv')))
        coefs<-coefs_table$x
        names(coefs)<-coefs_table$...1
        
        out_dir_bhm_spec_var_type_vars<-file.path(out_dir_bhm_spec_var_type, vars)
        if(!dir.exists(out_dir_bhm_spec_var_type_vars)){dir.create(out_dir_bhm_spec_var_type_vars)}
        
        # if we only consider extremes coefficients 
        if (vars == "extr_only"){
          coefs<-coefs[!grepl("TM", names(coefs))]
          coefs<-coefs[!grepl("RR", names(coefs))]
        }
        
        
        # adap coefs
        coefs_adap<-coefs[grepl("lag_log", names(coefs))]
        # no adap coefs
        coefs<-coefs[!grepl("lag_log",  names(coefs))]
        # rename adap coefs to match variables names 
        if(all(grepl(":lag_log_gni_pc" ,names(coefs_adap))) ){
          names(coefs_adap)<-gsub(":lag_log_gni_pc","", names(coefs_adap))
        }else if(all(grepl("lag_log_gni_pc_i_" ,names(coefs_adap))) ){
          names(coefs_adap)<-gsub("lag_log_gni_pc_i_","", names(coefs_adap))
        }
        
        
        climate_projections_dir<-"output/projections/climate_projections_gdl"
        agg<-"pop_weight"
        
        
        # coefs deltas 
        deltas_file<-file.path(out_dir_bhm_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,".feather"))
        if (file.exists(deltas_file)){
          cl_deltas<-read_feather(deltas_file)
        }else{
          cl_deltas<-clim_deltas_calc(
            climate_projections_dir,
            agg,
            regions, 
            coefs, paste0("gr_",out_var)
          )
          arrow::write_feather(cl_deltas, deltas_file)
        }
        
        
        if(spec=="adap"){
          
          deltas_adap_file<-file.path(out_dir_bhm_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_adap_", agg ,".feather"))
          # coefs adap deltas 
          if (file.exists(deltas_adap_file)){
            cl_deltas_adap<-read_feather(deltas_adap_file)
          }else{
            cl_deltas_adap_cl_only<-clim_deltas_calc(
              climate_projections_dir,
              agg,
              regions, 
              coefs_adap, paste0("gr_",out_var)
            )
            cl_deltas_adap<-dplyr::left_join(cl_deltas_adap_cl_only, data_proj_gni)
            cl_deltas_adap$delta_clim_proj = cl_deltas_adap$delta_clim_proj*cl_deltas_adap$lag_log_gni_pc
            
            arrow::write_feather(cl_deltas_adap, deltas_adap_file )
            
          }
        }
        
        ### 
        
        # level (in climate) based function proj, we update deltas by subtracting historical evaluation of the function 
        # subtract hist eval, using era5 for now -> evaluate dam fun in hist mean value for each region, then join data and calc new delta 
        
        data_mean$hist<-as.numeric(as.matrix(data_mean[, c(names(coefs))]) %*% c(coefs))
        cl_deltas<-left_join(cl_deltas,data_mean[, c("gdlcode", "hist")] )
        
        if(spec=="adap"){
          data_mean$hist_adap<-as.numeric(as.matrix(data_mean[, c(names(coefs_adap))]) %*% c(coefs_adap))*data_mean$lag_log_gni_pc
          cl_deltas_adap<-left_join(cl_deltas_adap,data_mean[, c("gdlcode", "hist_adap")] )
        }
        
        
        ### subtract historical comp
        cl_deltas$delta_clim_proj_old <- cl_deltas$delta_clim_proj
        cl_deltas$delta_clim_proj <- cl_deltas$delta_clim_proj_old-cl_deltas$hist
        
        
        ### if adap, subtract historical to adap and then put together bhm adap parts
        if(spec=="adap"){
          
          cl_deltas_adap$delta_clim_proj_old<-cl_deltas_adap$delta_clim_proj
          cl_deltas_adap$delta_clim_proj_adap<-cl_deltas_adap$delta_clim_proj_old-cl_deltas_adap$hist_adap
          ### sum together component bhm and adap
          cl_deltas<-inner_join(cl_deltas[, c("gdlcode", "year", "model", "ssp", "delta_clim_proj")],cl_deltas_adap[, c("gdlcode", "year", "model", "ssp", "delta_clim_proj_adap")] )
          cl_deltas$delta_clim_proj<-cl_deltas$delta_clim_proj+cl_deltas$delta_clim_proj_adap
          cl_deltas$delta_clim_proj_adap<-NULL
          
        }
        
        ### project 
        
        # bhm model proj, growth effect: we assume persistency of loss
        project_impacts(var_name=paste0("gr_",out_var),  
                        regions=regions, 
                        data=data_proj,
                        out_dir= out_dir_bhm_spec_var_type_vars,
                        all_deltas=cl_deltas, 
                        spec_type=paste0("bhm_", spec),
                        effect="growth_eff",
                        maps=FALSE)
        
        # bhm model proj, level effect: we assume no persistency of loss
        project_impacts(paste0("gr_",out_var),  
                        regions, 
                        data_proj,
                        out_dir_bhm_spec_var_type_vars,
                        cl_deltas, 
                        paste0("bhm_", spec),
                        "level_eff",
                        maps=FALSE)
      }
      
    }
    
  }
  
}

