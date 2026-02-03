### variance decomposition for aggregated data


rm(list=ls())

library(dplyr)
library(ggplot2)


specifications<-c("mean_mod") #   ,"mean_mod_spec")
#types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_extr_tp", "extr_only")
# spec_type<-"dlm"
# effect<-"growth_eff"

NL<-"mix"

spec<-"mean_mod"
type<-"all_vars_pdlm"
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
    b_col=which(colnames(data)==colnames(data)[which(grepl(paste0("\\bperc_delta.",b, "\\b"),colnames(data)))] ) # exact match
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

for (cluster in c("iso")){
  
  for(o in c(  "gr_gnipc")){
   
    ## global agg
    
    data_glob<-read.csv(file.path(out_dir_lag,paste0('g_agg_',o,"_",type,'_',spec, "_pop_weight_nlags_",NL,"_", cluster,"_cov_boot_all.csv")))
    data_glob <- data_glob %>%
      group_by(model) %>%
      filter(n_distinct(ssp) == n_distinct(data_glob$ssp)) %>%
      ungroup()
    
    data_glob <- data_glob%>%
      mutate(
        numeric_ssp = as.numeric(factor(ssp)),     # convert SSP to numeric codes
        numeric_model = as.numeric(factor(model))  # convert model to numeric codes
      )
    
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
    save(results_all_glob, file=file.path(out_dir, paste0(o, "_", type, '_',spec,"_pop_w_nlags",NL,"_",cluster, "_sobol_dec_glob.RData")))
    
    
    
  }

}

# TRY PLOT

for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
  
  for(cluster in c("rob", "iso")){
    
  load(file.path(out_dir,  paste0(o, "_", type, '_',spec,"_pop_w_nlags",NL,"_",cluster, "_sobol_dec_glob.RData")))
  
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
  new_labels <- c("boot" = "Climate Impacts",
                  "ssp" = "SSP Scenario",
                  "model" = "Climate Model",
                  "Sij+Sijl" = "Interactions")
  
  if(o=="gr_gnipc"){out_var<-"GNIPC"}
  if(o=="gr_leb"){out_var<-"LEB"}
  if(o=="gr_eys"){out_var<-"EYS"}
  
  # Plot with renamed parameters
  # g<-ggplot(plot_data, aes(x = factor(year), y = original, fill = parameters)) +
  #   geom_bar(stat = "identity", position = "stack", width=1) +
  #   scale_fill_manual(values = colors, labels = new_labels) +  # Rename parameters
  #   labs(title = paste0(out_var),
  #        x = "Year", y = "Sensitivity",
  #        fill = "Parameters") +theme_bw()+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1),
  #         text = element_text(family = "CM Roman",size = 16))
  
  
  years_to_show <- levels(factor(plot_data$year))[as.integer(levels(factor(plot_data$year))) %% 5 == 0]
  
  g <- ggplot(plot_data, aes(x = factor(year), y = original, fill = parameters)) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    scale_fill_manual(values = colors, labels = new_labels) +
    scale_x_discrete(breaks = years_to_show) +  # Show only selected years
    labs(title = paste0(out_var),
         x = "Year", y = "Sensitivity",
         fill = "Parameters") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(family = "CM Roman", size = 16))
  
  ggsave(file.path(out_dir, paste0(o, "_", type, '_',spec,'_',cluster, "_pop_w_nlags",NL,"_sobol_dec_glob.png")), plot = g, width = 15, height = 5, dpi = 150)
  
  
  
  
  library(dplyr)
  library(ggplot2)
  if(o=="gr_gnipc"){out_var<-"GNIPC"}
  if(o=="gr_leb"){out_var<-"LEB"}
  if(o=="gr_eys"){out_var<-"EYS"}
  
  # --- 1. Extract all components ---
  plot_data <- results_all_glob %>%
    filter(sensitivity %in% c("Si","Sij","Sijl")) %>%
    mutate(
      # --- 2. Clean parameter names for interactions ---
      parameters_clean = case_when(
        sensitivity == "Si" ~ parameters,
        sensitivity == "Sij" & parameters == "boot.ssp" ~ "boot-ssp",
        sensitivity == "Sij" & parameters == "boot.model" ~ "boot-model",
        sensitivity == "Sij" & parameters == "ssp.model" ~ "ssp-model",
        sensitivity == "Sijl" ~ "boot-ssp-model",
        TRUE ~ parameters
      ),
      # --- 3. Define readable labels for the plot ---
      parameters_label = case_when(
        parameters_clean == "boot" ~ "Climate Impacts",
        parameters_clean == "ssp" ~ "SSP Scenario",
        parameters_clean == "model" ~ "Climate Model",
        parameters_clean == "boot-ssp" ~ "Climate Impacts – SSP",
        parameters_clean == "boot-model" ~ "Climate Impacts – Climate Model",
        parameters_clean == "ssp-model" ~ "Climate Model – SSP",
        parameters_clean == "boot-ssp-model" ~ "Three-way",
        TRUE ~ parameters_clean
      )
    )
  
  # --- 4. Define plotting order ---
  plot_data$parameters_label <- factor(
    plot_data$parameters_label,
    levels = c(
      "Climate Impacts",
      "SSP Scenario",
      "Climate Model",
      "Climate Impacts – SSP",
      "Climate Impacts – Climate Model",
      "Climate Model – SSP",
      "Three-way"
    )
  )
  
  # --- 5. Colors (colorblind-friendly, 7 categories) ---
  colors <- c(
    "Climate Impacts" = "#E69F00",
    "SSP Scenario" = "#56B4E9",
    "Climate Model" = "#009E73",
    "Climate Impacts – SSP" = "#CC79A7",
    "Climate Impacts – Climate Model" = "#0072B2",
    "Climate Model – SSP" = "#D55E00",
    "Three-way" = "#999999"
  )
  
  # ---- 6. Normalize per year (same as before) ---
  plot_data <- plot_data %>%
    group_by(year) %>%
    mutate(
      min_val = min(original, na.rm = TRUE),
      original = ifelse(min_val < 0, original - min_val, original),
      mult = 1/sum(original),
      original = original * mult
    )
  
  # ---- 7. Select x-axis labels every 5 years ---
  years_to_show <- levels(factor(plot_data$year))[
    as.integer(levels(factor(plot_data$year))) %% 5 == 0
  ]
  
  # ---- 8. Plot ---
  g <- ggplot(plot_data,
              aes(x = factor(year),
                  y = original,
                  fill = parameters_label)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_manual(values = colors) +
    scale_x_discrete(breaks = years_to_show) +
    labs(
      title = out_var,
      x = "Year",
      y = "Sensitivity",
      fill = "Parameters"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(family = "CM Roman", size = 16)
    )
  
  ggsave(
    file.path(out_dir,
              paste0(o, "_", type, "_", spec, "_", cluster,
                     "_pop_w_nlags", NL, "_sobol_dec_glob_all_int.png")),
    plot = g, width = 15, height = 5, dpi = 150
  )
  
  
  }
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
