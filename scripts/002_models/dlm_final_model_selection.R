
rm(list=ls())

source("scripts/003_models/feols_lags_plot_funcs.R")

source("utils/cross_validation_fixest.R")

# xlsx write
require(openxlsx)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)
library(fixest)
library(modelsummary)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)

# output dir

out_dir_save<-"output/models/final_lag_mods/conservative_N_lags_mix"
if(!dir.exists(out_dir_save)){dir.create(out_dir_save)}

out_dir_final_plots<-file.path(out_dir_save, "plots")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

iso_gdlcode<-unique(data[, c("gdlcode", "iso3")])

all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')


library(slider)
data <- data %>%
  arrange(gdlcode, year) %>%  
  group_by(gdlcode) %>%
  mutate(
    TM_mean = mean(TM),
    RR_mean = mean(RR),
    TVAR_mean = mean(TVAR),
    HW_mean = mean(HW),
    RX_mean = mean(RX),
    PEXT_mean = mean(PEXT),
    WD_mean = mean(WD),
    SPI_mean = mean(SPI),
    SPEI_mean = mean(SPEI),
    PET_mean = mean(PET),
    # Compute 10-year moving averages
    TM_10y_mean = slide_dbl(TM, mean, .before = 9, .complete = TRUE),
    RR_10y_mean = slide_dbl(RR, mean, .before = 9, .complete = TRUE),
    TVAR_10y_mean = slide_dbl(TVAR, mean, .before = 9, .complete = TRUE),
    HW_10y_mean = slide_dbl(HW, mean, .before = 9, .complete = TRUE),
    RX_10y_mean = slide_dbl(RX, mean, .before = 9, .complete = TRUE),
    PEXT_10y_mean = slide_dbl(PEXT, mean, .before = 9, .complete = TRUE),
    WD_10y_mean = slide_dbl(WD, mean, .before = 9, .complete = TRUE),
    SPI_10y_mean = slide_dbl(SPI, mean, .before = 9, .complete = TRUE),
    SPEI_10y_mean = slide_dbl(SPEI, mean, .before = 9, .complete = TRUE),
    PET_10y_mean = slide_dbl(PET, mean, .before = 9, .complete = TRUE)
  )


################################################################################

library(fixest)
library(data.table)
library(ggplot2)

data<-data %>%
  arrange(gdlcode, year)

# Function to estimate model with k lags
estimate_lag_model <- function(data, climate_var, mean_climate_var, max_lag, outcome, controls = TRUE, only_diff=FALSE) {
  
  data <- as.data.table(data)

  
  # Create lags for the main climate variable
  lag_formula_parts <- c()
  
  for (lag in 0:max_lag) {
    
    # Main effect
    data[order(year), paste0("L", lag, "_diff_", climate_var) :=
           shift(get(paste0("diff_", climate_var)), n = lag), by = gdlcode]
    
    # Interaction
    data[order(year), paste0("L", lag, "_int_", climate_var) :=
           shift(get(paste0("diff_", climate_var)) * get(mean_climate_var), n = lag),
         by = gdlcode]
    
    if(only_diff==FALSE){
      lag_formula_parts <- c(lag_formula_parts,
                             paste0("L", lag, "_diff_", climate_var),
                             paste0("L", lag, "_int_", climate_var))
    }else{
      lag_formula_parts <- c(lag_formula_parts,
                             paste0("L", lag, "_diff_", climate_var))
    }
    
  }
  
  # ==========================================================
  # ADD CONTROLS
  # ==========================================================
  if (controls) {
    
    # Determine which other climate variables to control for
    control_vars <- c()
    
    if (climate_var == "TM") {
      control_vars <- c("RR")   # only rainfall
    } else if (climate_var == "RR") {
      control_vars <- c("TM")   # only temperature
    } else {
      control_vars <- c("TM", "RR")  # both
    }
    
    for (ctrl in control_vars) {
      mean_ctrl <- paste0(ctrl, "_mean")
      
      for (lag in 0:8) {
        # Control: diff term
        data[order(year), paste0("L", lag, "_diff_", ctrl) :=
               shift(get(paste0("diff_", ctrl)), n = lag), by = gdlcode]
        
        # Control: interaction term
        data[order(year), paste0("L", lag, "_int_", ctrl) :=
               shift(get(paste0("diff_", ctrl)) * get(mean_ctrl), n = lag),
             by = gdlcode]
        
        lag_formula_parts <- c(lag_formula_parts,
                               paste0("L", lag, "_diff_", ctrl),
                               paste0("L", lag, "_int_", ctrl))
      }
    }
  }
  # ==========================================================
  
  # Build regression formula
  formula_str <- paste0(
    outcome, " ~ ",
    paste(lag_formula_parts, collapse = " + "),
    " | gdlcode + year + iso3[year] + iso3[year^2]"
  )
  
  # Filter estimation sample
  data_mod <- data %>% dplyr::filter(year >= 1990 & year <= 2019)
  
  # Estimate model with fixest
  model <- feols(
    as.formula(formula_str),
    data = data_mod,
    cluster = ~iso3,
    panel.id = pan_id
  )
  
  return(list(model=model, data=data_mod, formula_cl=paste0(paste(lag_formula_parts, collapse = " + "))) )
}


# Compare models with different lag lengths
compare_lags <- function(data, climate_var, mean_climate_var, max_lag_test = 9, outcome , controls=TRUE, only_diff=FALSE) {
  
  results <- data.table(
    lag_length = 0:max_lag_test,
    AIC = NA_real_,
    BIC = NA_real_,
    HQIC = NA_real_,
    loyocv = NA_real_,
    adj_r2 = NA_real_,
    N_obs = NA_integer_,
    N_countries = NA_integer_
  )
  
  for(k in 0:max_lag_test) {
    res <- estimate_lag_model(data, climate_var, mean_climate_var, max_lag = k, outcome = outcome, controls = controls, only_diff=only_diff)
    
    data_to_dem<-res$data
    formula_cl<-res$formula_cl
    data_dem <- fixest::demean(as.formula(paste0(outcome , "+", formula_cl, "~", "gdlcode + year + iso3[year] + iso3[year^2]" )), data = as.data.frame(data_to_dem), na.rm=FALSE)
    f <- as.formula(paste( outcome, "~", formula_cl ,"-1" ))
    data_dem$year<-data_to_dem$year
    cv_y<-cross_validation_fixest(data_dem=data_dem, f=f,outcome)
    
    model<-res$model
    # Extract key statistics
    ll <- logLik(model)[1]  # log-likelihood
    n_params <- length(coef(model))  # parameters (without FE)
    n_obs <- nobs(model)
    n_fe <- length(unique(data$gdlcode)) + length(unique(data$year))  # Fixed effects
    
    # Total parameters including FE
    k_total <- n_params + n_fe
    
    # Calculate criteria
    results[lag_length == k, AIC := -2*ll + 2*k_total]
    results[lag_length == k, BIC := -2*ll + log(n_obs)*k_total]
    results[lag_length == k, HQIC := -2*ll + 2*log(log(n_obs))*k_total]
    results[lag_length == k, loyocv := cv_y]
    results[lag_length == k, adj_r2 := r2(model, "ar2")]
    results[lag_length == k, N_obs := n_obs]
    results[lag_length == k, N_countries := length(unique(data$gdlcode))]
  }
  
  # Find minimums
  results[, AIC_min := AIC == min(AIC)]
  results[, BIC_min := BIC == min(BIC)]
  results[, HQIC_min := HQIC == min(HQIC)]
  
  # BIC +2 rule: models within 2 units of minimum
  results[, BIC_acceptable := (BIC - min(BIC)) <= 2]
  
  return(results)
}

# Test if lags k1 to k2 are jointly zero
test_joint_significance <- function(model, climate_var, lag_start, lag_end) {
  
  # Identify coefficients to test
  coef_names <- names(coef(model))
  
  # Get all lags in range for both main effect and interaction
  test_terms <- c()
  for(lag in lag_start:lag_end) {
    test_terms <- c(test_terms,
                    paste0("L", lag, "_diff_", climate_var),
                    paste0("L", lag, "_int_", climate_var))
  }
  
  # Keep only terms that exist in model
  test_terms <- test_terms[test_terms %in% coef_names]
  
  # Wald test
  wald_test <- wald(model, test_terms)
  
  return(wald_test)
}



# test 


# model<-estimate_lag_model(data=data, climate_var="TM", mean_climate_var="TM", max_lag=9, outcome="gr_eys" )
# summary(model, cluster="iso3")

o<-"gr_gnipc"
cl<-"TVAR"
mean_cl<-"TM_mean"

# Get HQIC-selected lag
selected_lag <- 6

# Estimate selected model
final_model <- estimate_lag_model(data, cl, mean_cl,selected_lag , o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            6, 7)
print(test_wald_result$p)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0, 4)
print(test_wald_result$p)

wald(final_model, keep = "diff_TVAR",  cluster = ~iso3)
wald(final_model, keep = "diff_TVAR", drop = "mean", cluster = ~iso3)
wald(final_model, keep = "mean_i_diff_TVAR",  cluster = ~iso3)


################################################################################
# Define climate variable configurations for each outcome
# Structure: outcome -> climate_var -> mean_climate_var to use

climate_configs <- list(
  
  gr_eys = list(
    TM = "TM_mean",      
    RR = "RR_mean",
    TVAR = "TM_mean",
    RX = "TM_mean",          
    PEXT = "TM_mean",
    WD = "WD_mean"
  ),
  
  gr_leb = list(
    TM = "TM_mean",          
    RR = "RR_mean",
    TVAR = "TVAR_mean",  
    HW = "HW_mean",
    RX = "TM_mean",
    WD = "TM_mean"
    
  ),
  
  gr_gnipc = list(
    TM = "TM_mean",          
    RR = "RR_mean",
    TVAR = "TM_mean",  
    HW = "TM_mean",
    RX = "TM_mean",
    WD = "RR_mean",
    PEXT = "TM_mean"
  )
)

climate_configs <- list(
  
  gr_eys = list(
    HW = "TM_mean"    
    
  ),
  
  gr_leb = list(
    PEXT = "TM_mean"
    
  )
)

# Update the lag selection function to use this structure
lag_selection_decision <- function(data, climate_var, mean_climate_var, 
                                   max_lag_test, outcome, controls,
                                   theory_max = 9, out_dir) {
  
  cat("\n=== LAG SELECTION:", climate_var, "→", outcome, "===\n")
  cat("Using mean variable:", mean_climate_var, "\n\n")
  
  # Step 1: Compare criteria
  comparison <- compare_lags(data, climate_var, mean_climate_var, 
                             max_lag_test, outcome, controls = controls)
  
  test_plot <- melt(comparison[, .(lag_length, AIC, BIC, HQIC, loyocv)],
                    id.vars = "lag_length")
  
  g<-ggplot(test_plot, aes(x = lag_length, y = value)) +
    geom_line() + facet_wrap(~variable, scales="free", nrow=1)+
    scale_x_continuous(breaks=seq(0, 9, 1))+
    geom_point() +
    labs(title = paste0("Information Criteria by Lag Length, ",outcome, " - ",climate_var, " - controls ", controls),
         x = "Number of Lags",
         y = "Criterion Value"
    ) +
    theme_minimal()
  ggsave(filename=file.path(out_dir, paste0(outcome, "_",climate_var, "_controls_", controls,".png")), width=10, height=2)
  
  hqic_lag <- comparison[HQIC_min == TRUE, lag_length]
  bic_lag <- comparison[BIC_acceptable == TRUE, lag_length]
  aic_lag <- comparison[AIC_min == TRUE, lag_length]
  
  cat("HQIC selects:", hqic_lag, "lags\n")
  cat("BIC selects:", bic_lag, "lags\n\n")
  cat("AIC selects:", aic_lag, "lags\n\n")
  
  # Step 2: Start with HQIC selection, but cap at theory maximum
  candidate_lag <- min(hqic_lag, theory_max)
  
  if(candidate_lag > hqic_lag) {
    cat("Capping at", candidate_lag, "lags (theory maximum)\n\n")
  }
  
  # Step 3: Test joint significance
  # if(candidate_lag == 0) {
  #   cat("DECISION: 0 lags (no distributed lag effects)\n")
  #   cat("REASON: HQIC/BIC selected 0\n\n")
  #   
  #   return(list(
  #     selected_lag = 0,
  #     model = estimate_lag_model(data, climate_var, mean_climate_var, 0, outcome),
  #     reason = "HQIC selected 0",
  #     comparison = comparison,
  #     mean_var_used = mean_climate_var
  #   ))
  # }
  
  # Test theoretically relevant lags
  test_lag <- min(candidate_lag, theory_max)
  model_test <- estimate_lag_model(data, climate_var, mean_climate_var, 
                                   test_lag, outcome, controls)$model
  
  # Joint test of all lags
  joint_all <- test_joint_significance(model_test, climate_var, 0, test_lag)
  cat("Joint test lags 0 -", test_lag, ": p =", round(joint_all$p, 4), "\n")
  
  # Also test early lags only (most theoretically relevant)
  early_lag <- min(3, test_lag)
  if(early_lag < test_lag) {
    joint_early <- test_joint_significance(model_test, climate_var, 0, early_lag)
    cat("\n Joint test lags 0-", early_lag, " (early effects): p =", 
        round(joint_early$p, 4), "\n")
  }
  
  cat("\n")
  
  # Decision logic
  if(joint_all$p < 0.10) {
    cat("DECISION: Use", test_lag, "lags\n")
    cat("REASON: Jointly significant at p =", round(joint_all$p, 4), "\n\n")
    
    return(list(
      selected_lag = test_lag,
      model = model_test,
      reason = paste0("Joint sig (p=", round(joint_all$p, 3), ")"),
      joint_pval = joint_all$p,
      comparison = comparison,
      mean_var_used = mean_climate_var
    ))
    
  }else{
    cat("DECISION: NO lags (no distributed lag effects)\n")
    cat("REASON: Lags 0-", test_lag, " jointly insignificant (p =", 
        round(joint_all$p, 4), ")\n\n")
    
    model_0 <- estimate_lag_model(data, climate_var, mean_climate_var, 0, outcome, controls)$model
    
    
    return(list(
      selected_lag = NA,
      model = model_0,
      reason = paste0("Joint insig (p=", round(joint_all$p, 3), ")"),
      joint_pval = joint_all$p,
      comparison = comparison,
      mean_var_used = mean_climate_var
    ))
  }
  
  
}


out_dir_plots_lag_sel<-file.path(out_dir_save, "lag_sel_plots")
if(!dir.exists(out_dir_plots_lag_sel)){dir.create(out_dir_plots_lag_sel)}

# Run analysis for all combinations, no controls
for (controls in c(TRUE, FALSE)){
  all_results <- list()
  
  for(outcome in names(climate_configs)) {
    
    # Get the climate variable configuration for this outcome
    outcome_config <- climate_configs[[outcome]]
    
    for(clim_var in names(outcome_config)) {
      
      # Get the corresponding mean climate variable
      mean_clim_var <- outcome_config[[clim_var]]
      
      # Create unique key
      key <- paste0(outcome, "_", clim_var)
      
      cat("\n", rep("=", 80), "\n", sep="")
      
      # Run lag selection with outcome-specific mean variable
      all_results[[key]] <- tryCatch({
        lag_selection_decision(
          data = data,
          climate_var = clim_var,
          mean_climate_var = mean_clim_var,
          max_lag_test = 9,
          outcome = outcome,
          theory_max = 9,  # Can also make this outcome-specific
          controls=controls,
          out_dir=out_dir_plots_lag_sel
        )
      }, error = function(e) {
        cat("ERROR for", key, ":", conditionMessage(e), "\n")
        return(NULL)
      })
      
      cat(rep("=", 80), "\n\n", sep="")
    }
  }
  
  
  # Create summary table
  library(purrr)
  
  summary_table <- map_df(names(all_results), function(key) {
    res <- all_results[[key]]
    
    # Handle NULL results (errors)
    if(is.null(res)) {
      parts <- strsplit(key, "_")[[1]]
      return(data.frame(
        outcome = parts[1],
        climate_var = paste(parts[-1], collapse="_"),
        selected_lags = NA,
        reason = "ERROR",
        joint_pval = NA,
        mean_var_used = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    parts <- strsplit(key, "_")[[1]]
    
    data.frame(
      outcome = parts[1],
      climate_var = paste(parts[-1], collapse="_"),
      selected_lags = res$selected_lag,
      reason = res$reason,
      joint_pval = ifelse(is.null(res$joint_pval), NA, res$joint_pval),
      mean_var_used = res$mean_var_used,
      stringsAsFactors = FALSE
    )
  })
  
  print(summary_table)
  
  # # Save results
  # write.csv(summary_table, 
  #           file.path(out_dir_save, paste0("lag_selection_summary_controls_",controls,".csv")),
  #           row.names = FALSE)
  # 
  # # Also save the full results object for later use
  # saveRDS(all_results, 
  #         file.path(out_dir_save, paste0("all_lag_selection_results_controls_",controls,".rds")))
  
  
  
}




# outcome climate_var selected_lags                reason   joint_pval mean_var_used
# 1       gr      eys_TM            NA Joint insig (p=0.212) 2.119098e-01       TM_mean
# 2       gr      eys_RR            NA Joint insig (p=0.105) 1.052033e-01       RR_mean
# 3       gr    eys_TVAR             6   Joint sig (p=0.038) 3.801645e-02       TM_mean
# 4       gr      eys_RX            NA Joint insig (p=0.445) 4.448752e-01       TM_mean
# 5       gr    eys_PEXT            NA Joint insig (p=0.768) 7.682288e-01       TM_mean
# 6       gr      eys_WD             1   Joint sig (p=0.018) 1.848974e-02       WD_mean
# 7       gr      leb_TM            NA Joint insig (p=0.133) 1.331973e-01       TM_mean
# 8       gr      leb_RR            NA Joint insig (p=0.155) 1.547008e-01       RR_mean
# 9       gr    leb_TVAR            NA  Joint insig (p=0.18) 1.800106e-01     TVAR_mean
# 10      gr      leb_HW             0   Joint sig (p=0.011) 1.061426e-02       HW_mean
# 11      gr      leb_RX            NA Joint insig (p=0.205) 2.048552e-01       TM_mean
# 12      gr      leb_WD             3   Joint sig (p=0.005) 4.936701e-03       TM_mean
# 13      gr    gnipc_TM             8    Joint sig (p=0.01) 1.013509e-02       TM_mean
# 14      gr    gnipc_RR            NA Joint insig (p=0.912) 9.121805e-01       RR_mean
# 15      gr  gnipc_TVAR            NA Joint insig (p=0.192) 1.917950e-01       TM_mean
# 16      gr    gnipc_HW             7       Joint sig (p=0) 2.570008e-04       TM_mean
# 17      gr    gnipc_RX            NA Joint insig (p=0.196) 1.956349e-01       TM_mean
# 18      gr    gnipc_WD             9       Joint sig (p=0) 7.456535e-06       RR_mean
# 19      gr  gnipc_PEXT            NA Joint insig (p=0.287) 2.868044e-01       TM_mean

### test what was significant in the other model, using aic selected lag n

# test 
o<-"gr_eys"
cl<-"RX"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-5

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) # 5 - 0.46 , 0 - 0.44

# test 
o<-"gr_eys"
cl<-"RR"
mean_cl<-"RR_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-0

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) # 2 sign, 0 pval 0.1

# test 
o<-"gr_eys"
cl<-"TM"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-3

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) # 3 and 5 insign

o<-"gr_eys"
cl<-"TVAR"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-6

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) 




# test 
o<-"gr_leb"
cl<-"HW"
mean_cl<-"HW_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()


test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-0

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            1,selected_lag )


# test 
o<-"gr_leb"
cl<-"RX"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-3


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag)
print(test_wald_result$p)



# test 
o<-"gr_leb"
cl<-"TVAR"
mean_cl<-"TVAR_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-3

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)
summary(final_model)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p)


# test 
o<-"gr_leb"
cl<-"TM"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-5

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )


test_wald_result <- test_joint_significance(final_model, cl, 
                                            3,selected_lag )

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,2 )
print(test_wald_result$p) # no, keep control



# test 
o<-"gr_leb"
cl<-"RR"
mean_cl<-"RR_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-5

final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) # no, keep control





# test 
o<-"gr_gnipc"
cl<-"PEXT"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-0


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p) # 0, 3 non sign

# test 
o<-"gr_gnipc"
cl<-"RR"
mean_cl<-"RR_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-2


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p)


# test 
o<-"gr_gnipc"
cl<-"RX"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-3


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag)
print(test_wald_result$p)


# test 
o<-"gr_gnipc"
cl<-"TVAR"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-6


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p)

# test 
o<-"gr_gnipc"
cl<-"HW"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-2


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o, controls = FALSE)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,1 )
print(test_wald_result$p)

### 

o<-"gr_gnipc"
cl<-"TM"
mean_cl<-"TM_mean"

test=compare_lags(data=data, climate_var=cl, mean_climate_var=mean_cl, max_lag_test =9, outcome=o )$model

test_plot <- melt(test[, .(lag_length, AIC, BIC, HQIC)],
                  id.vars = "lag_length")

ggplot(test_plot, aes(x = as.integer(lag_length), y = value)) +
  geom_line() + facet_wrap(~variable, scales="free")+
  geom_point() +
  labs(title = paste0("Information Criteria by Lag Length for ", o, " ", cl),
       x = "Number of Lags",
       y = "Criterion Value"
  ) +
  theme_minimal()

test[BIC_acceptable == TRUE]$lag_length
test[AIC_min == TRUE]$lag_length
test[HQIC_min == TRUE]$lag_length

selected_lag<-7


final_model <- estimate_lag_model(data, cl, mean_cl, selected_lag, o)$model

test_wald_result <- test_joint_significance(final_model, cl, 
                                            0,selected_lag )
print(test_wald_result$p)




################################################################################



# setup for old lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns_mean=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")
m_modns_10y=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean")
m_modns_cont=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")


adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(m_modns_mean[i],'_i_',varns[i],sep='')] <- data[m_modns_mean[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_10y[i],'_i_',varns[i],sep='')] <- data[m_modns_10y[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_cont[i],'_i_',varns[i],sep='')] <- data[m_modns_cont[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 


for (i in 1:length(varns)){ # 
  data[paste("TM_10y_mean",'_i_',varns[i],sep='')] <- data["TM_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_10y_mean",'_i_',varns[i],sep='')] <- data["RR_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 0:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste(varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_mean[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_mean[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_10y[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_10y[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_cont[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_cont[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# go back to right time period -> exclude covid years 

data<-data%>%filter(year>=1990 & year<=2019)


v_temp<-which(varns=="diff_TM")
v_rain<-which(varns=="diff_RR")
v_tvar<-which(varns=="diff_TVAR")
v_hw<-which(varns=="diff_HW")
v_rx<-which(varns=="diff_RX")
v_pext<-which(varns=="diff_PEXT")
v_wd<-which(varns=="diff_WD")
v_spi<-which(varns=="diff_SPI")
v_spei<-which(varns=="diff_SPEI")



################################################################################

### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

pan_id<-c('gdlcode', 'year')


specs<-list("mean_mod")#, "mean_mod_10y", "cont_mod")

out_variables<-c("gr_gnipc", "gr_leb", "gr_eys")

cl_variables<-c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")


pattern<-"conflict|exp_edu|exp_health|trade|lag_gr|gr" # to discard coefficients rows correspondent to contr, autoreg part 


vars_corr<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)", "PET"),
  modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "", "" )
)

vars_corr_10y<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

vars_corr_cont<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

save_model<-function(m, pattern, o, type, spec, NL, out_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  cov_iso=vcov(m, cluster=~iso3)[!grepl(pattern, row.names(vcov(m, cluster=~iso3))), !grepl(pattern, colnames(vcov(m, cluster=~iso3))) ]
  
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
  write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
  write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
  write.csv(cov_iso, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov_iso.csv')))
  
  return(list(tab, coefs, cov))
}


################################################################################



# n lags tp selected before
# N_temp_eys<-8
# N_rain_eys<-8
# N_temp_leb<-8
# N_rain_leb<-8
# N_temp_gnipc<-8
# N_rain_gnipc<-8

vars_correspondaces=vars_corr
m_modns=m_modns_mean



######################

o_eys<-"gr_eys"
N_temp<-7
N_rain<-2

### formula part of temp, rain contr 

mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
# mean_cl_formula<-paste0(varns[v_temp], "+",  
#                         paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
#                         paste0("lag_",N_temp,"_",varns[v_temp], collapse = "+"),"+", 
#                         paste0("lag_",N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"))
# 

wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
pext_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))


extr_formula<- paste0(c(wd_formula,
                        tvar_formula),
                      collapse = "+")


### all extr tp 
r_eys=paste0(mean_cl_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)
summary(m_eys, cluster="iso3")


r_eys=paste0( 
  extr_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)
summary(m_eys, cluster="iso3")


r_eys=paste0(mean_cl_formula,"+", 
             extr_formula
)
f_eys= as.formula(paste( o_eys, "~", r_eys, "|" ,i ))
m_eys = fixest::feols(f_eys, data , panel.id = pan_id)
#summary(m_eys)
summary(m_eys, cluster="iso3")
fixest::r2(m_eys, type="all")
AIC(m_eys)
BIC(m_eys)

wald(m_eys, keep = "diff_TM",  cluster = ~iso3)
wald(m_eys, keep = "diff_TM", drop = "mean", cluster = ~iso3)
wald(m_eys, keep = "mean_i_diff_TM",  cluster = ~iso3)


wald(m_eys, keep = "diff_RR",  cluster = ~iso3)
wald(m_eys, keep = "diff_RR", drop = "mean", cluster = ~iso3)
wald(m_eys, keep = "mean_i_diff_RR",  cluster = ~iso3)

wald(m_eys, keep = "diff_TVAR",  cluster = ~iso3)
wald(m_eys, keep = "diff_TVAR", drop = "mean", cluster = ~iso3)
wald(m_eys, keep = "mean_i_diff_TVAR",  cluster = ~iso3)

wald(m_eys, keep = "diff_WD",  cluster = ~iso3)
wald(m_eys, keep = "diff_WD", drop = "mean", cluster = ~iso3)
wald(m_eys, keep = "mean_i_diff_WD",  cluster = ~iso3)


save_model(m_eys, pattern, o_eys, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_eys, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_eys,"all_vars_pdlm", "mean_mod", "mix")



### 


data_dem_eys <- fixest::demean(as.formula(paste0(o_eys , "+", r_eys , "~", i )), data = data, na.rm=FALSE)
f_eys <- as.formula(paste( o_eys, "~", r_eys ,"-1" ))
data_dem_eys$year<-data$year
m_dem_eys = lm(f_eys, data_dem_eys )
summary(m_dem_eys)


library(car)
# linearHypothesis(m_dem_eys, c("lag_1_diff_TM + lag_2_diff_TM + lag_3_diff_TM + lag_4_diff_TM + lag_5_diff_TM + lag_6_diff_TM + lag_7_diff_TM +
#                               lag_1_TM_mean_i_diff_TM + lag_2_TM_mean_i_diff_TM + lag_3_TM_mean_i_diff_TM + lag_4_TM_mean_i_diff_TM + lag_5_TM_mean_i_diff_TM + lag_6_TM_mean_i_diff_TM + lag_7_TM_mean_i_diff_TM 
#                               = 0"))


car::vif(m_dem_eys)
# Visualizing the model
plot(m_dem_eys, which = 1, main = "Model Fit")
std_residuals <- cooks.distance(m_dem_eys)
outliers <- which(std_residuals > 2 * sd(std_residuals))

data_dem_eys_no_out <- data_dem_eys[-outliers, ]
m_dem_eys_rob = lm(f_eys, data_dem_eys_no_out )
summary(m_dem_eys_rob)
plot(m_dem_eys_rob, which = 1, main = "Model Fit")




# If baseline temperature predicts error variance → suggests different
# process in hot vs. cold regions (misspecification)

######################

o_leb<-"gr_leb"

N_temp<-1

N_rain<-5

### formula part of temp, rain contr 
mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))

#mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
#                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
#                        paste0(m_modns[v_rain], "_i_", varns[v_rain],  collapse = "+"))


hw_formula<-paste0(paste0(varns[v_hw]), "+",
                   m_modns[v_hw], "_i_", varns[v_hw],"+", 
                   paste0("lag_",1,"_",varns[v_hw], collapse = "+"),"+", 
                   paste0("lag_",1,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
hw_formula<-paste0(paste0(varns[v_hw]), "+",
                   m_modns[v_hw], "_i_", varns[v_hw])

rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:3,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]))


tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:3,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:3,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))

tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_tvar], "_i_", varns[v_tvar]))

wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1:3,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))

extr_formula<- paste0(c(hw_formula, 
                        wd_formula,
                        tvar_formula),
                      collapse = "+")



### all extr tp 
r_leb=paste0(mean_cl_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)
summary(m_leb, cluster="iso3")

r_leb=paste0(
  extr_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)
summary(m_leb, cluster="iso3")



r_leb=paste0(mean_cl_formula,"+", 
             extr_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)

summary(m_leb, cluster="iso3")


wald(m_leb, keep = "diff_HW",  cluster = ~iso3)
wald(m_leb, keep = "diff_HW", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_HW",  cluster = ~iso3)

wald(m_leb, keep = "diff_TM", cluster = ~iso3)
wald(m_leb, keep = "diff_TM", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_TM",  cluster = ~iso3)

wald(m_leb, keep = "diff_RR",  cluster = ~iso3)
wald(m_leb, keep = "diff_RR", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_RR",  cluster = ~iso3)

wald(m_leb, keep = "diff_TVAR",  cluster = ~iso3)
wald(m_leb, keep = "diff_TVAR", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_TVAR",  cluster = ~iso3)

wald(m_leb, keep = "diff_WD",  cluster = ~iso3)
wald(m_leb, keep = "diff_WD", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_WD",  cluster = ~iso3)

wald(m_leb, keep = "diff_RX",  cluster = ~iso3)
wald(m_leb, keep = "diff_RX", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_RX",  cluster = ~iso3)


save_model(m_leb, pattern, o_leb, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb,"all_vars_pdlm", "mean_mod", "mix")


### alt
N_temp<-5

### formula part of temp, rain contr 
mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))



r_leb=paste0(mean_cl_formula,"+", 
             extr_formula
)
f_leb= as.formula(paste( o_leb, "~", r_leb, "|" ,i ))
m_leb = fixest::feols(f_leb, data , panel.id = pan_id)
fixest::r2(m_leb, type="all")
AIC(m_leb)
BIC(m_leb)

summary(m_leb, cluster="iso3")


wald(m_leb, keep = "diff_HW",  cluster = ~iso3)
wald(m_leb, keep = "diff_HW", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_HW",  cluster = ~iso3)

wald(m_leb, keep = "diff_TM",drop = "3|4|5", cluster = ~iso3)
wald(m_leb, keep = "diff_TM", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_TM",  cluster = ~iso3)

wald(m_leb, keep = "diff_RR",  cluster = ~iso3)
wald(m_leb, keep = "diff_RR", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_RR",  cluster = ~iso3)

wald(m_leb, keep = "diff_TVAR",  cluster = ~iso3)
wald(m_leb, keep = "diff_TVAR", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_TVAR",  cluster = ~iso3)

wald(m_leb, keep = "diff_WD",  cluster = ~iso3)
wald(m_leb, keep = "diff_WD", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_WD",  cluster = ~iso3)

wald(m_leb, keep = "diff_RX",  cluster = ~iso3)
wald(m_leb, keep = "diff_RX", drop = "mean", cluster = ~iso3)
wald(m_leb, keep = "mean_i_diff_RX",  cluster = ~iso3)


save_model(m_leb, pattern, o_leb, "all_vars_pdlm", "mean_mod", "mix_alt", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb, "all_vars_pdlm", "mean_mod", "mix_alt", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_leb,"all_vars_pdlm", "mean_mod", "mix_alt")


### 

data_dem_leb <- fixest::demean(as.formula(paste0(o_leb , "+", r_leb , "~", i )), data = data, na.rm=FALSE)
f_leb <- as.formula(paste( o_leb, "~", r_leb ,"-1" ))
data_dem_leb$year<-data$year
m_dem_leb = lm(f_leb, data_dem_leb )
summary(m_dem_leb)

data_dem_leb$iso3<-data$iso3

cv<-cross_validation_iso(data_dem_leb, f_leb,o_leb )

cv_no_pred<-cross_validation_iso(data_dem_leb, as.formula(paste( o_leb, "~", "1")),o_leb )


cv_time<-cross_validation_fixest(data_dem_leb, f_leb ,o_leb)

cv_time_no_pred<-cross_validation_fixest(data_dem_leb, as.formula(paste( o_leb, "~", "1")),o_leb )


######################

o_gnipc<-"gr_gnipc"

N_temp<-8
N_rain<-0

### formula part of temp, rain contr 

#mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
#                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
#                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
#                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
#                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
#                        paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
#                        paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
mean_cl_formula<-paste0(varns[v_temp], "+","+", varns[v_rain], "+", 
                        paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                        paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                        paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                        paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"))



hw_formula<-paste0(paste0(varns[v_hw]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                   paste0("lag_",1:7,"_",varns[v_hw], collapse = "+"),"+", 
                   paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
#hw_formula<-paste0(paste0(varns[v_hw]), "+",paste0(m_modns[v_temp], "_i_", varns[v_hw]))
wd_formula<-paste0(paste0(varns[v_wd]), "+",
                   paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                   paste0("lag_",1:9,"_",varns[v_wd], collapse = "+"),"+", 
                   paste0("lag_",1:9,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
pext_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
rx_formula<-paste0(paste0(varns[v_rx]), "+",
                   paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                   paste0("lag_",1:2,"_",varns[v_rx], collapse = "+"),"+", 
                   paste0("lag_",1:2,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))

extr_formula<- paste0(c(wd_formula, 
                        tvar_formula, hw_formula
),
collapse = "+")



### all extr tp 

r_gnipc=paste0(mean_cl_formula
)
f_gnipc= as.formula(paste(o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")



r_gnipc=paste0( 
  extr_formula
)
f_gnipc= as.formula(paste(o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")


r_gnipc=paste0(mean_cl_formula,"+", 
               extr_formula
)
f_gnipc= as.formula(paste( o_gnipc, "~", r_gnipc, "|" ,i ))
m_gnipc = fixest::feols(f_gnipc, data , panel.id = pan_id)
#summary(m_gnipc)
fixest::r2(m_gnipc, type="all")
AIC(m_gnipc)
BIC(m_gnipc)
summary(m_gnipc, cluster="iso3")

wald(m_gnipc, keep = "diff_TVAR",  cluster = ~iso3)
wald(m_gnipc, keep = "diff_TVAR", drop = "mean", cluster = ~iso3)
wald(m_gnipc, keep = "mean_i_diff_TVAR",  cluster = ~iso3)

wald(m_gnipc, keep = "diff_WD",  cluster = ~iso3)
wald(m_gnipc, keep = "diff_WD", drop = "mean", cluster = ~iso3)
wald(m_gnipc, keep = "mean_i_diff_WD",  cluster = ~iso3)

wald(m_gnipc, keep = "diff_TM",  cluster = ~iso3)
wald(m_gnipc, keep = "diff_TM", drop = "mean", cluster = ~iso3)
wald(m_gnipc, keep = "mean_i_diff_TM",  cluster = ~iso3)

wald(m_gnipc, keep = "diff_HW",  cluster = ~iso3)
wald(m_gnipc, keep = "diff_HW", drop = "mean", cluster = ~iso3)
wald(m_gnipc, keep = "mean_i_diff_TM",  cluster = ~iso3)

wald(m_gnipc, keep = "diff_HW", drop = "1|2|3", cluster = ~iso3)


save_model(m_gnipc, pattern, o_gnipc, "all_vars_pdlm", "mean_mod", "mix", out_dir_save)
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_gnipc, "all_vars_pdlm", "mean_mod", "mix", se="iso")
plot_lags(out_dir_save , vars_correspondaces, out_dir_final_plots, o_gnipc,"all_vars_pdlm", "mean_mod", "mix")

### 


data_dem_gnipc <- fixest::demean(as.formula(paste0(o_gnipc , "+", r_gnipc , "~", i )), data = data, na.rm=FALSE)
f_gnipc <- as.formula(paste( o_gnipc, "~", r_gnipc ,"-1" ))
data_dem_gnipc$year<-data$year
m_dem_gnipc = lm(f_gnipc, data_dem_gnipc )
summary(m_dem_gnipc)



