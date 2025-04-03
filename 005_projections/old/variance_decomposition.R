
rm(list=ls())

library(dplyr)

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_vars", "extr_only")
spec_type<-"dlm"
effect<-"growth_eff"


spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]


out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}


n_boot<-1000


for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
  
  data_int<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_boot_impacts_intervals.feather")))
  
  data_all<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_boot_impacts_all.feather")))
  
  
  pop_projection_gdlcode <- readr::read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
  colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
  gdlcodes_iso <- readr::read_csv("data/gdlcodes_iso.csv")
  pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
  pop_projection_gdlcode<-pop_projection_gdlcode%>%
    group_by(iso3, year, ssp)%>%
    mutate(pop_country=sum(pop_gdl))
  pop_projection_gdlcode<-unique(pop_projection_gdlcode)
  
  data_int<-inner_join(data_int, pop_projection_gdlcode )
  
  resp_types<-c("diff", "perc_diff", "cc_values")
  
  #########
  # TO REMOVE 
  
  # 
  # data_all<-data_int
  # data_all<-data_all[-which(is.na(data_all$cc_proj_value_mean)), ]
  # data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]
  # 
  # data_all$result.1<-sample(data_all$cc_proj_value_mean)
  # data_all$result.2<-sample(data_all$cc_proj_value_mean)
  # data_all$result.3<-sample(data_all$cc_proj_value_mean)
  # data_all$result.4<-sample(data_all$cc_proj_value_mean)
  # data_all$result.5<-sample(data_all$cc_proj_value_mean)
  #   
  # data_all<-data_all%>%select(result.1, result.2, result.3,result.4,result.5)
  # n_boot<-5
  # 
  ##########
  
  data_all<-data_all[-which(is.na(data_all$result.1)), ]
  data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]
  
  data_all$gdlcode<-data_int$gdlcode
  data_all$iso3<-data_int$iso3
  data_all$year<-data_int$year
  data_all$value_interp<-data_int$value_interp
  data_all$ssp<-data_int$ssp
  data_all$model<-data_int$model
  data_all$pop_gdl<-data_int$pop_gdl
  data_all$pop_country<-data_int$pop_country
  
  
  ssp_models<-data_all%>%select(ssp , model)
  ssp_models<-unique(ssp_models)
  ssp_models<-ssp_models%>%group_by(model)%>%mutate(n=n())
  
  models_keep<-unique(ssp_models$model[which(ssp_models$n==4)])
  
  data_all<-data_all%>%filter(model %in% models_keep)
  ssp_match=data.frame(ssp=unique(data_all$ssp), numeric_ssp=c(1:4))
  model_match=data.frame(model=unique(data_all$model), numeric_model=c(1:5))
  data_all<-left_join(data_all,ssp_match )
  data_all<-left_join(data_all,model_match )
  
  
  ################################################################################
  # setup
  
  index_fun<-function(mat,data) {
    ret<-rep(0,nrow(mat))
    
    for (i in 1:nrow(mat)){
      b<-mat[i,1]
      sc<-mat[i,2]
      m<-mat[i,3]
      b_col=which(colnames(data)==colnames(data)[which(grepl(paste0("result.",b),colnames(data)))] )
      r<-data[which(data$numeric_ssp==sc & data$numeric_model==m), b_col] #ssp from 1 to 4, model from 1 to 5
      ret[i]<-as.numeric(r)
    }
    ret
  }
  
  
  qunifdisc <- function(p, weights = NULL, Nlabel = NULL) { # column by column p of sobol_matrix , nlabel is the number of discrete indeces i want
    if (is.null(weights)) {
      return(cut(p, seq(0, 1, length.out = Nlabel+1), labels = FALSE))
    } else {
      return(cut(p, c(0, weights), labels = FALSE))
    }
  }
  
  
  set.seed(999)
  
  library(sensobol)
  N <- 250
  params <- c("boot", "ssp", "model")
  order <-  "third"
  
  ################################################################################
  
  for (type in resp_types){
    
    if(type=="diff"){
      data<-data_all
      data[1:n_boot] <- data[1:n_boot] - data$value_interp
    }
    
    if(type=="perc_diff"){
      data<-data_all
      data[1:n_boot] <- 100*(data[1:n_boot] - data$value_interp)/ data$value_interp
    }
    
    if(type=="cc_values"){
      data<-data_all
    }
    
    # select year , gdlcode 
    
    results_all<-list()
    
    for(y in unique(data$year)){
      for (g in unique(data$gdlcode)){
        
        data_temp<-data%>%filter(year==y, gdlcode==g)
        
        # Create sample matrix using Sobol' Quasi Random Numbers.
        mat <- sobol_matrices(N = N, params = params, order = order)
        
        n_sc<-length(unique(data_temp$ssp))
        n_mod<-length(unique(data_temp$model))
        
        mat[,1]<-qunifdisc(mat[,1], Nlabel=n_boot)
        mat[,2]<-qunifdisc(mat[,2], Nlabel=n_sc)
        mat[,3]<-qunifdisc(mat[,3], Nlabel=n_mod)
        
        Y<-index_fun(mat, data_temp)
        
        ind <- sobol_indices(Y = Y, N = N, params = params)
        
        
        results_all[[paste0(g)]][[paste0(y)]]<-ind
        
      }
    }
    save(results_all, file=file.path(out_dir_lag, paste0(o,"_", type,  "_sobol_decomp_all_regions.RData")))
    
    
    ################################################################################
    # select year , iso3 
    
    data_iso<-data %>%
      group_by(iso3, ssp, model, year) %>%
      summarise(across(result.1:result.5, ~weighted.mean(. ,w=pop_gdl)))
    
    data_iso<-left_join(data_iso,ssp_match )
    data_iso<-left_join(data_iso,model_match )
    
    results_all_iso<-list()
    
    for(y in unique(data_iso$year)){
      for (g in unique(data_iso$iso3)){
        
        data_temp<-data_iso%>%filter(year==y, iso3==g)
        data_temp
        
        # Create sample matrix using Sobol' Quasi Random Numbers.
        mat <- sobol_matrices(N = N, params = params, order = order)
        
        n_sc<-length(unique(data_temp$ssp))
        n_mod<-length(unique(data_temp$model))
        
        mat[,1]<-qunifdisc(mat[,1], Nlabel=n_boot)
        mat[,2]<-qunifdisc(mat[,2], Nlabel=n_sc)
        mat[,3]<-qunifdisc(mat[,3], Nlabel=n_mod)
        
        Y<-index_fun(mat, data_temp)
        
        ind <- sobol_indices(Y = Y, N = N, params = params)
        
        
        results_all_iso[[paste0(g)]][[paste0(y)]]<-ind
        
      }
    }
    save(results_all_iso, file=file.path(out_dir_lag, paste0(o, "_", type,  "_sobol_decomp_by_iso3.RData")))
    
    
    
    ################################################################################
    # select year , gdlcode 
    
    
    data_glob<-data %>%
      group_by(ssp, model, year) %>%
      summarise(across(result.1:result.5, ~weighted.mean(. ,w=pop_gdl)))
    
    
    data_glob<-left_join(data_glob,ssp_match )
    data_glob<-left_join(data_glob,model_match )
    
    results_all_glob<-list()
    
    for(y in unique(data$year)){
      
      data_temp<-data_glob%>%filter(year==y)
      data_temp
      
      # Create sample matrix using Sobol' Quasi Random Numbers.
      mat <- sobol_matrices(N = N, params = params, order = order)
      
      n_sc<-length(unique(data_temp$ssp))
      n_mod<-length(unique(data_temp$model))
      
      mat[,1]<-qunifdisc(mat[,1], Nlabel=n_boot)
      mat[,2]<-qunifdisc(mat[,2], Nlabel=n_sc)
      mat[,3]<-qunifdisc(mat[,3], Nlabel=n_mod)
      
      Y<-index_fun(mat, data_temp)
      
      ind <- sobol_indices(Y = Y, N = N, params = params)
      
      
      results_all_glob[[paste0(y)]]<-ind
      
      
    }
    save(results_all_glob, file=file.path(out_dir_lag, paste0(o, "_", type,  "_sobol_decomp_global.RData")))
    
  }
  
}
