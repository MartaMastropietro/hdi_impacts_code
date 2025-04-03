library(dplyr)
library(ggplot2)

# Helper function to round numbers to significant digits
round_sig <- function(x, sig = 2) {
  if (x != 0) {
    round(x, sig - floor(log10(abs(x))) - 1)
  } else {
    0
  }
}

# Extract variables and their moderating interaction terms/variables from a set of lagged regression results
extract_vars_mods <- function(coefs, NLs) {
  full <- coefs$variable
  # indvs <- full[!grepl("l\\(", full)]
  indvs <- full[!grepl("lag", full)]
  varns <- indvs[!grepl("_i_|_2", indvs)]
  potint <- setdiff(indvs, varns)
  mods <- sapply(varns, function(varn) {
    mod <- potint[grepl(varn, potint)]
    if (length(mod) > 0) mod[1] else "None"
  })
  modns <- sapply(mods, function(mod) strsplit(mod, "_i_|_2")[[1]][1])
  
  NLs<-rep(NA, length(varns))
  for (id in 1:length(varns)){
    NLs[id]<-length(full[grepl(varns[id], full)])/2-1
  }
  
  list(varns = varns, mods = mods, modns = modns, NLs = NLs)
}

calc_means <- function(table, modns, unit) {
  # Initialize a list to store the means
  means <- list()
  
  # Quartiles to calculate
  qs <- c(0.25, 0.5, 0.75)
  
  for (v in seq_along(modns)) {
    mod <- modns[v]
    
    # Create a placeholder for this variable's quartiles
    means[[v]] <- c()
    
    if (mod != "None") {
      # Calculate the mean of the variable within each unit
      group_means <- tapply(table[[mod]], table[[unit]], mean, na.rm = TRUE)
      
      # Calculate the quantiles of the group means
      means[[v]] <- quantile(group_means, probs = qs, na.rm = TRUE)
    } else {
      # Append zeros for "None"
      means[[v]] <- rep(0, length(qs))
    }
  }
  
  # Convert the list of results into a matrix for output
  means_matrix <- do.call(rbind, means)
  return(means_matrix)
}

calc_stds <- function(table, varns, unit) {
  # Initialize a vector to store the standard deviations
  stds <- numeric(length(varns))
  
  for (v in seq_along(varns)) {
    varn <- varns[v]
    
    # Calculate the standard deviation within each unit
    group_stds <- tapply(table[[varn]], table[[unit]], sd, na.rm = TRUE)
    
    # Store the mean of these standard deviations
    stds[v] <- mean(group_stds, na.rm = TRUE)
  }
  
  return(stds)
}



calc_specNL_MEs <- function(coefs, varns, mods, means, NVLs) {
  # Initialize a list to store results for all variables
  MEs <- list()
  
  for (v in seq_along(varns)) {
    var <- varns[v]
    mod <- mods[v]
    mean_mod <- means[v, ]
    NL <- NVLs[v]
    
    # Initialize a matrix to hold the results for this variable
    # Rows = lags (0 to NL), Columns = quantiles (3: lower, median, upper)
    ME_matrix <- matrix(0, nrow = NL+1 , ncol = ncol(means))
    
    for (L in 0:NL) {
      if (L == 0) {
        varn <- var
        modn <- mod
      } else {
        # varn <- paste0("l", var, ", ", L, ")")
        # modn <- paste0("l(", mod, ", ", L, ")")
        
        varn <- paste0("lag_", L, "_", var)
        modn <- paste0("lag_", L, "_", mod)
      }
      
      # Extract the coefficient for the variable and the moderator
      coef_row <- coefs[coefs$variable == varn, ]
      coef <- if (nrow(coef_row) > 0) coef_row$x[1] else 0
      
      if (mod != "None") {
        mod_coef_row <- coefs[coefs$variable == modn, ]
        mod_coef <- if (nrow(mod_coef_row) > 0) mod_coef_row$x[1] else 0
      } else {
        mod_coef <- 0
      }
      
      # Calculate the marginal effect for each quantile
      for (q in seq_len(ncol(means))) {
        ME <- coef + mean_mod[q] * mod_coef
        ME_matrix[(L+1), q] <- ME  # Store in the matrix
      }
    }
    
    # Store the matrix for this variable in the results list
    MEs[[var]] <- ME_matrix
  }
  
  return(MEs)
}


calc_specNL_errors <- function(cov, varns, mods, means, NVLs) {
  # Initialize a list to store results for all variables
  errors <- list()
  
  for (v in seq_along(varns)) {
    var <- varns[v]
    mod <- mods[v]
    mean_mod <- means[v, ]
    NL <- NVLs[v]
    
    # Initialize a matrix to hold the results for this variable
    # Rows = lags (0 to NL), Columns = quantiles (3: lower, median, upper)
    error_matrix <- matrix(0, nrow = NL+1 , ncol = ncol(means))
    
    for (L in 0:NL) {
      if (L == 0) {
        varn <- var
        modn <- mod
      } else {
        # varn <- paste0("l(", var, ", ", L, ")")
        # modn <- paste0("l(", mod, ", ", L, ")")
        
        varn <- paste0("lag_", L, "_", var)
        modn <- paste0("lag_", L, "_", mod)
      }
      
      # Extract covariance matrix entries
      err1 <- cov[ cov$X==varn , paste0(varn)]
      err1 <- if (length(err1) > 0) err1 else 0
      
      if (mod != "None") {
        err2 <-cov[ cov$X==varn , paste0(modn)]
        err2 <- if (length(err2) > 0) err2 else 0
        
        err3 <- cov[ cov$X==modn , paste0(modn)]
        err3 <- if (length(err3) > 0) err3 else 0
      } else {
        err2 <- 0
        err3 <- 0
      }
      
      # Calculate the error for each quantile
      for (q in seq_len(ncol(means))) {
        err <- err1 + 2 * err2 * mean_mod[q] + err3 * (mean_mod[q]^2)
        err <- sqrt(err)
        error_matrix[(L+1), q] <- err * 1.96  # 95% confidence interval
      }
    }
    
    # Store the matrix for this variable in the results list
    errors[[var]] <- error_matrix
  }
  
  return(errors)
}



plot_response <- function(o, se, type , varns, varnlabels, mods, modns, MEs, errors, means, spec, NL, NVLs, unit, margunit, scales, efunit, folder, covn, summ, wr2, BIC, AIC) {
  
  
  library(ggplot2)
  library(gridExtra)
  
  # Color palette for lines
  cols <- c("#66c2a5", "#fc8d62", "#8da0cb")  # Using Dark2 palette colors
  
  # Initialize a list to store ggplot objects
  plots <- list()
  
  for (v in seq_along(varns)) {
    var <- varns[v]
    mod <- mods[v]
    NLs <- seq(0, NVLs[v])  # Lags from 0 to NVL
    labels <- sprintf("%.3g", means[v, ])
    
    MEv <- MEs[[var]]  # Matrix of marginal effects
    errorv <- errors[[var]]  # Matrix of errors
    
    # Create a data frame for ggplot
    data <- data.frame(
      Lag = rep(NLs, times = ncol(means)),
      Quantile = as.factor(rep(seq_len(ncol(means)),each = length(NLs) )),
      ME = as.vector(scales[v] * MEv),
      Lower = as.vector(scales[v] * (MEv - errorv)),
      Upper = as.vector(scales[v] * (MEv + errorv))
    )
    
    # # Base plot
    # p <- ggplot(data, aes(x = Lag, y = ME, color = Quantile, fill = Quantile)) +
    #   geom_line(aes(group = Quantile), size = 0.8) +
    #   geom_point(aes(group = Quantile), size = 2) +
    #   geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, linetype = 0) +
    #   scale_color_manual(values = cols[1:ncol(means)], labels = paste(labels, unit[v])) +
    #   scale_fill_manual(values = cols[1:ncol(means)], labels = paste(labels, unit[v])) +
    #   labs(
    #     x = "Lag (year)",
    #     y = paste("Effect per", margunit[v], "increase\n", efunit),
    #     title = varnlabels[v]
    #   ) +
    #   theme_minimal(base_size = 10) +
    #   theme(
    #     legend.position = "top",
    #     legend.title = element_blank(),
    #     plot.title = element_text(size = 11, face = "bold"),
    #     axis.title.x = element_text(size = 10),
    #     axis.title.y = element_text(size = 10),
    #     axis.text = element_text(size = 8),
    #     legend.text = element_text(size = 8)
    #   ) +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
    #  
    
    # Base plot with cum effect 
    
    data<-data%>%group_by(Quantile)%>%mutate(cum_ME=sum(ME))
    
    # Extract unique quantile values and cumulative effects
    quantile_labels <- unique(data$Quantile)
    cum_ME_values <- sapply(quantile_labels, function(q) unique(data$cum_ME[data$Quantile == q]))
    
    # Create the new legend labels by appending the cumulative effect to the original labels
    legend_labels <- paste0(labels," ", unit[v], " (Cum. Eff.: ", round(cum_ME_values, 3), ")")
    
    # Modify the plot
    p <- ggplot(data, aes(x = Lag, y = ME, color = as.factor(Quantile), fill = as.factor(Quantile))) +
      geom_line(aes(group = Quantile), size = 0.8) +
      geom_point(aes(group = Quantile), size = 2) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, linetype = 0) +
      scale_color_manual(values = cols[1:length(quantile_labels)], labels = legend_labels) +
      scale_fill_manual(values = cols[1:length(quantile_labels)], labels = legend_labels) +
      labs(
        x = "Lag (year)",
        y = paste("Effect per", margunit[v], "increase\n", efunit),
        title = varnlabels[v]
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 13)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    
    
    
    # Add plot to list
    plots[[v]] <- p
  }
  
  # Arrange the plots in a grid
  #plot_grid <- do.call(grid.arrange, c(plots, nrow = 2, 
  #                     top = grid::textGrob(
  #                       sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
  #                       gp = grid::gpar(fontsize = 10, fontface = "bold") ) ) )
  library(grid)
  if(length(plots)==1){
    nrows=1
  }else{nrows=2}
  plot_grid <- do.call(grid.arrange, c(plots, list(nrow = nrows, 
                                                   top = textGrob(
                                                     sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
                                                     gp = gpar(fontsize = 11, fontface = "bold")
                                                   )
  )))
  # # Add a global title
  # grid::grid.text(
  #   sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
  #   x = 0.5, y = 0.98, just = "right", gp = grid::gpar(fontsize = 10, fontface = "bold")
  #)
  
  # Save the plot
  filename <- if (summ) {
    paste0(folder, "/SUM_", o, "_", type,"_", spec, "_lagN", NL, "_", se,".png")
  } else {
    paste0(folder, "/", o, "_", type,"_",  spec, "_lagN", NL,"_", se, ".png")
  }
  
  
  ggsave(filename, plot = plot_grid, width = 25, height = 12, dpi = 150)
  message("Done plotting ", spec)
  
  plot_grid
}



plot_lags<-function(directory, vars_correspondaces, out_directory, o, type, spec, NL, se="rob"){
  
  coefs <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_coef.csv"), sep = ",", header = TRUE)
  colnames(coefs)<-c("variable","x" )
  
  if(se=="rob"){
    cov <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_cov.csv"), sep = ",", header = TRUE)
  }else if(se=="iso"){
    cov <- read.table(paste0(directory,"/", o, '_',type, '_',spec, "_lagN", NL, "_cov_iso.csv"), sep = ",", header = TRUE)
  }
  colnames(cov)<-c("X", paste0(cov$X))
  
  coeftable <- read.table(paste0(directory, "/", o, '_',type, '_',spec, "_lagN", NL, "_coeftab.csv"), sep = ",", header = TRUE)
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
  unit<-vars_correspondaces$unit[match(modns, vars_correspondaces$modns)]
  scales<-stds*100
  margunit<-rep("1std", length(scales))
  efunit <- "(%-point growth rate)"
  
  # Calculate marginal effects and their errors
  MEs <- calc_specNL_MEs(coefs, varns, mods, means, NVLs)
  errors <- calc_specNL_errors(cov, varns, mods, means, NVLs)
  
  folder <- out_directory
  
  
  varnlabels<-vars_correspondaces[ match( varns, vars_correspondaces$varns) , "ext_names" ]
  
  # Plot results
  plot_response(o, se, type, varns, varnlabels, mods, modns, MEs, errors, means, spec, NL, NVLs, unit, margunit, scales, efunit,
                folder,  summ = FALSE, wr2 = wr2, BIC = BIC, AIC = AIC)
  
}


