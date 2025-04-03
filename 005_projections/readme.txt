
- JUNO_MC_impacts_proj_final.R
 takes projected preprocessed climate variables (from folder 002_climate_prep), 
 takes projected preprocessed ssp variables (from folder 001_variables_prep) 
 and applies damage functions we select (need coefs, covariance) 
 it saves all bootstraps, intervals for each region, year, ssp, model

-JUNO_boot_hdi_subnat_computation_final.R
 takes all bootstrapped from JUNO_MC_impacts_proj_final.R (still variables)
 computes indeces, hdi

-JUNO_boot_hdi_subnat_delta_computation_and_agg.R
 takes all bootstrapped indeces from JUNO_boot_hdi_subnat_computation_final.R
 computes deltas, perc deltas 
 aggregates at global, country level (selected countries) 

-JUNO_boot_hdi_subnat_sign_delta_comput_and_agg.R
 takes deltas, perc deltas but only aggregates using significant values (90% ci of bootstrap + 2/3 models agreement on sign)

