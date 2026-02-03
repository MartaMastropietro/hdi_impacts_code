# hdi_climate_impacts_code
Code for reproducing the work "Past and Projected Climate Impacts on Human Development"

R version 4.4.3

The following packages are used:
Package       Version   
arrow         "21.0.0.1"
broom         "1.0.10"  
car           "3.1-3"   
clubSandwich  "0.6.1"   
data.table    "1.17.8"  
dplyr         "1.1.4"   
exactextractr "0.10.0"  
fixest        "0.12.1"  
ggplot2       "4.0.0"   
gsubfn        "0.7"     
margins       "0.3.28"  
modelsummary  "2.5.0"   
ncdf4         "1.24"    
plm           "2.6-6"   
progress      "1.2.3"   
raster        "3.6-32"  
readr         "2.1.5"   
readxl        "1.4.5"   
REdaS         "0.9.4"   
reshape       "0.8.10"  
reshape2      "1.4.4"   
sf            "1.0-21"  
sp            "2.2-0"   
stargazer     "5.2.3"   
terra         "1.8-70"  
tidyr         "1.3.1"   
tidyverse     "2.0.0"   
lattice       "0.22-6"  

To obtain the results and images in the paper, one should run all scripts in the "scripts" folder in order:
- 001_data_prep
- 002_models
- 003_projections


To create projections:
- run scripts/002_models/original_comp_oaat_and_final_lags_models to generate coefficients and covariance matrix 
- run in order scripts in 003_projections/001_variables_prep
- run script in 003_projections/002_climate_prep

-JUNO_MC_impacts_proj....R
 take projected preprocessed climate variables (generated from code in folder 002_climate_prep), 
 take projected preprocessed ssp variables (generated from code in folder 001_variables_prep) 
 and apply damage functions we select (need coefs, covariance computed with robust or iso level errors), 
 save all bootstraps, intervals for each region, year, ssp, model

-JUNO_boot_glob_computation_and_agg....R
 take all bootstrapped values ad aggregates impacts at the global and iso3 level by population weighting 
-JUNO_boot_hdi_subnat_computation....R
 take all bootstrapped values
 computes indeces, hdi for robust or iso level errors

-JUNO_boot_hdi_subnat_delta_computation_and_agg....R
 take all bootstrapped indeces from JUNO_boot_hdi_subnat_computation_final.R
 compute deltas, perc deltas 
 aggregate at global, country level (selected countries) for robust or iso level errors

Scripts boot_agg_plots_rob_cov_final.R, boot_agg_plots_iso_cov_final.R, final_preproc_plot_boot_agg_c_sel_data.R and var_decomp_agg.R produce the plots and maps present in the paper

Scripts marked with JUNO were run on the omonym supercomputer in CMCC's high performance computing center.
Typical runs require up to ~400 GB RAM at peak usage.

Data in data/data_population/ need to be downloaded from the relative sources indicated in the folders.