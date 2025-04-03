### variance decomposition for aggregated data


rm(list=ls())

library(dplyr)
library(ggplot2)


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

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}


n_boot<-1000


################################################################################
# setup

index_fun<-function(mat,data) {
  ret<-rep(0,nrow(mat))
  
  for (i in 1:nrow(mat)){
    b<-mat[i,1]
    sc<-mat[i,2]
    m<-mat[i,3]
    b_col=which(colnames(data)==colnames(data)[which(grepl(paste0("\\bresult.",b, "\\b"),colnames(data)))] ) # exact match
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
N <- 6500
params <- c("boot", "ssp", "model")
order <-  "third"

################################################################################

resp_types<-"diff"

for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
 
  
  for (type in resp_types){
    
    ## global agg
    
    data_glob<-read.csv(file.path(out_dir_lag,paste0('glob_agg_', o,'_',type,'_',spec,'_',vars, "_pop_weight_nlags",NL,"_boot_impacts_all.csv")))
    
    results_all_glob<-data.frame()
    
    for(y in unique(data_glob$year)){
      
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
      
      ind <- sobol_indices(Y = Y, N = N, params = params, order=order)
      res<-ind$results
      res$year<-y
      
      results_all_glob<-rbind(results_all_glob, res)
      
      
    }
    save(results_all_glob, file=file.path(out_dir, paste0(o, "_", type, '_',spec,'_',vars, "_pop_weight_nlags",NL,"_sobol_dec_glob.RData")))

  }
  
  
}

# TRY PLOT

for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
  
  load(file.path(out_dir, paste0(o, "_", type, '_',spec,'_',vars, "_pop_weight_nlags",NL,"_sobol_dec_glob.RData")))
  
  # Sum of Sij and Sijl per year
  sij_sijl_summary <- results_all_glob %>%
    filter(sensitivity %in% c("Sij", "Sijl")) %>%
    group_by(year) %>%
    summarise(original = sum(original), .groups = "drop") %>%
    mutate(sensitivity = "Sij+Sijl", parameters = "Sij+Sijl")
  
  # Filter Si components
  si_ti_data <- results_all_glob %>%
    filter(sensitivity %in% c("Si"))
  
  # Combine Si, and Sij+Sijl in the correct stacking order
  plot_data <- bind_rows(si_ti_data, sij_sijl_summary)
  
  # Convert 'parameters' to factor and ensure "Sij+Sijl" is first (stacked on top)
  plot_data <- plot_data %>%
    mutate(parameters = factor(parameters, levels = c( "Sij+Sijl", "boot", "ssp", "model")))
  
  plot_data<-plot_data%>%group_by(year)%>%mutate(
    min=min(original, na.rm=TRUE),
    original=ifelse(min<0, original-min, original),
    mult=1/sum(original), 
    original=original*mult , 
    check=sum(original)  )
  
  # Define colorblind-friendly colors
  colors <- c("boot" = "#E69F00",    # Orange
              "ssp" = "#56B4E9",     # Sky Blue
              "model" = "#009E73",   # Green
              "Sij+Sijl" = "#CC79A7") # Magenta-Purple
  
  # Define new labels
  new_labels <- c("boot" = "Bootstrap",
                  "ssp" = "SSP Scenario",
                  "model" = "Climate Model",
                  "Sij+Sijl" = "Interactions")
  
  
  # Plot with renamed parameters
  g<-ggplot(plot_data, aes(x = factor(year), y = original, fill = parameters)) +
    geom_bar(stat = "identity", position = "stack", width=1) +
    scale_fill_manual(values = colors, labels = new_labels) +  # Rename parameters
    labs(title = paste0("Sobol Indices Contributions Over Time (",o,")"),
         x = "Year", y = "Sensitivity",
         fill = "Parameters") +theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(file.path(out_dir, paste0(o, "_", type, '_',spec,'_',vars, "_pop_weight_nlags",NL,"_sobol_dec_glob.png")), plot = g, width = 12, height = 6, dpi = 150)
  
}


# for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
#   
#   
#   for (type in resp_types){
#     ## country agg
#     
#     data_iso<-read.csv(file.path(out_dir_lag,paste0('country_agg_', o,'_',type,'_',spec,'_',vars, "_pop_weight_nlags",NL,"_boot_impacts_all.csv")))
#     
#     results_all_iso<-list()
#     
#     for(y in c(2030,2050,2080,2100)){
#       for (g in unique(data_iso$iso3)){
#         
#         data_temp<-data_iso%>%filter(year==y, iso3==g)
#         data_temp
#         
#         # Create sample matrix using Sobol' Quasi Random Numbers.
#         mat <- sobol_matrices(N = N, params = params, order = order)
#         
#         n_sc<-length(unique(data_temp$ssp))
#         n_mod<-length(unique(data_temp$model))
#         
#         mat[,1]<-qunifdisc(mat[,1], Nlabel=n_boot)
#         mat[,2]<-qunifdisc(mat[,2], Nlabel=n_sc)
#         mat[,3]<-qunifdisc(mat[,3], Nlabel=n_mod)
#         
#         Y<-index_fun(mat, data_temp)
#         
#         ind <- sobol_indices(Y = Y, N = N, params = params)
#         
#         
#         results_all_iso[[paste0(g)]][[paste0(y)]]<-ind
#         
#       }
#     }
#     save(results_all_iso, file=file.path(out_dir, paste0(o, "_", type,'_',spec,'_',vars,"_pop_weight_nlags",NL,  "_sobol_dec_by_iso3.RData")))
#     
#     
#   }
# }
