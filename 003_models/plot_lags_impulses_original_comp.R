# Load necessary libraries


rm(list=ls())
source("scripts/003_models/feols_lags_plot_funcs.R")


library(dplyr)
library(ggplot2)

# Read the data
data <-  read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

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

# Parameters

directory <- "output/models/original_components_tp_extr_complex_models_try/lag_models/"
out_directory <- "output/models/original_components_tp_extr_complex_models_try/lag_models/shocks_figures"
if(!dir.exists(out_directory)){dir.create(out_directory)}

specs <- c("mean_mod_tp_spec")#, "mean_mod_spec", "mean_mod_adap_spec")
types<-c("all_extr", "all_extr_controls", "all_extr_autoreg", "all_extr_autoreg_controls")
covn <- ""

out_variables<- c( "gr_gnipc",
                  "gr_eys"  ,
                   "gr_leb"  #,
                 # "gr_mys"  
                  )
  
################################################################################

# Label names for climate variables (dependent on variables we use)
# all_varnlabels <- list(
#   gr_gnipc = c("Mean temp.", "Total precip.", "Heat Waves", "Wet days", "Droughts", "Temp. var.","Extreme precip." ),
#   gr_eys =c("Mean temp.", "Total precip.",  "Wet days",  "Droughts"),
#   gr_leb =c("Mean temp.", "Total precip.", "Heat Waves", "Wet days", "Droughts", "Temp. var.","Cumulative 5days precip."),
#   gr_mys =c("Mean temp.", "Total precip.",  "Droughts", "Extreme precip.", "Cumulative 5days precip.")
#   ) 
# 
# units<-list(
#   gr_gnipc = c("C","mm", "C", "days", " ", "C", "mm" ),
#   gr_eys   = c("C","mm", "days", " "),
#   gr_leb   = c("C","mm", "C", "days", " ", "C", "mm" ),
#   gr_mys   = c("C","mm", "C",  " ", "mm", "mm" )
# )


all_varnlabels <- list(
  gr_gnipc = c("Mean temp.", "Total precip.","Wet days", "Heat Waves",  "Droughts"),
  gr_eys =c("Mean temp.", "Total precip.",  "Wet days",  "Extreme precip."),
  gr_leb =c("Mean temp.", "Total precip.", "Heat Waves",  "Wet Days", "Temp. var.")
  # gr_mys =c("Mean temp.", "Total precip.",  "Droughts", "Extreme precip.", "Cumulative 5days precip.")
) 

units<-list(
  mean_mod_spec=   list( gr_gnipc = c("°C","mm", "days", "°C", " " ),
                         gr_eys   = c("°C","mm", "days"),
                         gr_leb   = c("°C","mm", "°C","days ", "°C")), 
  
  mean_mod_tp_spec=list( gr_gnipc = c("°C","mm", "mm", "°C", " °C"  ),
                         gr_eys   = c("°C","mm", "days"),
                         gr_leb   = c("°C","mm", "°C", "°C", "°C")), 
  
  mean_mod_adap_spec=list( gr_gnipc = c("$ (log)","$ (log)", "$ (log)", "$ (log)", "$ (log)" ),
                         gr_eys   = c("$ (log)","$ (log)", "$ (log)"),
                         gr_leb   = c("$ (log)","$ (log)", "$ (log)", "$ (log)", "$ (log) " ))
  
  
)


# Loop over regression specifications
for (NL in c(7:9)){
  for (o in out_variables){
    
    for (r in seq_along(specs)) {
      for (type in types){
        
        varnlabels<-all_varnlabels[[o]]
        
        spec <- specs[r]
        
        
        coefs <- read.table(paste0(directory, o, '_',type, '_',spec, "_lagN", NL, "_coef.csv"), sep = ",", header = TRUE)
        colnames(coefs)<-c("variable","x" )
        cov <- read.table(paste0(directory,o, '_',type, '_',spec, "_lagN", NL, "_cov.csv"), sep = ",", header = TRUE)
        colnames(cov)<-c("X", paste0(cov$X))
        coeftable <- read.table(paste0(directory, o, '_',type, '_',spec, "_lagN", NL, "_coeftab.csv"), sep = ",", header = TRUE)
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
        unit<-units[[spec]][[o]]
        scales<-stds*100
        margunit<-rep("1std", length(scales))
        efunit <- "(%-point growth rate)"
        
        # Calculate marginal effects and their errors
        MEs <- calc_specNL_MEs(coefs, varns, mods, means, NVLs)
        errors <- calc_specNL_errors(cov, varns, mods, means, NVLs)
        
        folder <- out_directory
        
        # Plot results
        plot_response(o, type,varns, varnlabels, mods, modns, MEs, errors, means, spec, NL, NVLs, unit, margunit, scales, efunit,
                      folder,  summ = FALSE, wr2 = wr2, BIC = BIC, AIC = AIC)
      }
    }
  }
  
}
