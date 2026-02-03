
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

library(RColorBrewer)

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj"
if(!dir.exists(out_dir)){dir.create(out_dir)}

#lags number 
N<-"mix"
#N<-8

out_dir<-file.path(out_dir, paste0("N",N,"lags_pdlm"))
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

out_variables<-c( "leb", "eys", "gnipc"  )

out_dir_comp<-"output/projections/original_comp" 

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_extr_tp", "extr_only")


spec_type<-"dlm"

effect<-"growth_eff"

spec<-specifications[1]

vars<-"all_vars_pdlm"

type<-"all_vars_pdlm"

quant05 <- function(x){
  quantile(x, probs=0.05, na.rm=TRUE)
}
quant95 <- function(x){
  quantile(x, probs=0.95, na.rm=TRUE)
}
quant10 <- function(x){
  quantile(x, probs=0.10, na.rm=TRUE)
}
quant90 <- function(x){
  quantile(x, probs=0.90, na.rm=TRUE)
}


################################################################################
################################################################################
################################################################################

# for each model, global weighted hdi delta, perc delta course , all


out_dir<-file.path(out_dir, paste0("rob_cov"))
if(!dir.exists(out_dir)){dir.create(out_dir)}


# hdi

out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)

glob_data$cc_med<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, median, na.rm=TRUE)



glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3006:3013]
# A tibble: 4 × 8
#     delta_mean delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 
#       -0.00266  -0.00266  -0.00328  -0.00203        -0.00340       -0.00341       -0.00421       -0.00260
#       -0.00597  -0.00599  -0.00739  -0.00454        -0.00849       -0.00853       -0.0105        -0.00646
#       -0.0126   -0.0127   -0.0156   -0.00961        -0.0227        -0.0227        -0.0280        -0.0174 
#       -0.0149   -0.0149   -0.0182   -0.0117         -0.0186        -0.0186        -0.0227        -0.0146 

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]

# delta_mean delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# <dbl>     <dbl>     <dbl>     <dbl>           <dbl>          <dbl>          <dbl>          <dbl>
# 1   -0.00200  -0.00201  -0.00255  -0.00145        -0.00325       -0.00326       -0.00413       -0.00238
# 2   -0.00211  -0.00211  -0.00273  -0.00146        -0.00374       -0.00374       -0.00483       -0.00257
# 3   -0.00288  -0.00289  -0.00364  -0.00209        -0.00584       -0.00587       -0.00736       -0.00428
# 4   -0.00354  -0.00354  -0.00453  -0.00257        -0.00560       -0.00562       -0.00714       -0.00409

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    cc_med_med=median(cc_med, na.rm = TRUE),
     .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "hdi")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = hdi[year == 2024],
    hdi_2050 = hdi[year == 2050],
    hdi_2100 = hdi[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])
# ssp    absolute_change_2050 absolute_change_2100 percentage_change_2050 percentage_change_2100
# ssp126               0.109                0.240                   19.8                    43.4
# ssp245               0.0723               0.188                   13.3                    34.6
# ssp370               0.0305               0.0591                   5.68                   11.0
# ssp585               0.121                0.270                   21.9                    48.7

cc<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "cc_med_med")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
cc_summ <- cc %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = cc_med_med[year == 2024],
    hdi_2050 = cc_med_med[year == 2050],
    hdi_2100 = cc_med_med[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()
print(cc_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])
# ssp     absolute_change_2050 absolute_change_2100 percentage_change_2050 percentage_change_2100
#   ssp126               0.107                0.237                   19.5                   42.9 
#   ssp245               0.0702               0.182                   12.9                   33.5 
#   ssp370               0.0277               0.0464                   5.14                   8.63
#   ssp585               0.118                0.255                   21.3                   46.0 


perc_change_cc_vs_baseline <- baseline_summ %>%
  select(ssp, starts_with("absolute_change")) %>%
  left_join(
    cc_summ %>% 
      select(ssp, starts_with("absolute_change")),
    by = "ssp",
    suffix = c("_baseline", "_cc")
  ) %>%
  mutate(
    perc_change_2050 = 
      (absolute_change_2050_cc - absolute_change_2050_baseline) /
      absolute_change_2050_baseline * 100,
    
    perc_change_2100 = 
      (absolute_change_2100_cc - absolute_change_2100_baseline) /
      absolute_change_2100_baseline * 100
  ) %>%
  select(ssp, perc_change_2050, perc_change_2100)

print(perc_change_cc_vs_baseline)

abated_long <- perc_change_cc_vs_baseline %>%
  pivot_longer(
    cols = starts_with("perc_change"),
    names_to = "year",
    values_to = "perc_change"
  ) %>%
  mutate(
    year = ifelse(year == "perc_change_2050", "2050", "2100"),
    year = factor(year, levels = c("2050", "2100"))
  )


## 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) + ggtitle("HDI")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) + ggtitle("HDI")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)


g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.03) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.6) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 1.8, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("HDI")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



p_inset <- ggplot(
  abated_long,
  aes(x = year, y = perc_change, fill = ssp)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    color = "black",
    linewidth = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Foregone development",
    y = "% change",
    x = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    panel.grid.minor = element_blank(),panel.grid.major = element_blank()
  )+theme(
    panel.border = element_rect(linewidth = 0.4),
    axis.ticks = element_blank()
  )#+theme(text = element_text(family = "CM Roman",size = 15))

g_final <- g +
  inset_element(
    p_inset,
    left   = 0.04,
    bottom = 0.05,
    right  = 0.55,
    top    = 0.45
  )

ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_with_inset.png")), g_final, width=12, height=8)




g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("HDI")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)





# income_index

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)

glob_data$cc_med<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)

glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3006:3013]

# delta_mean delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
#   -0.00584  -0.00586  -0.00742  -0.00421        -0.00794       -0.00798        -0.0101       -0.00573
#   -0.0133   -0.0133   -0.0169   -0.00948        -0.0195        -0.0196         -0.0248       -0.0139 
#   -0.0283   -0.0283   -0.0360   -0.0204         -0.0547        -0.0548         -0.0695       -0.0396 
#   -0.0306   -0.0307   -0.0395   -0.0214         -0.0380        -0.0380         -0.0488       -0.0266

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    cc_med_med=median(cc_med, na.rm = TRUE),
    .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "income_index")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = income_index[year == 2024],
    hdi_2050 = income_index[year == 2050],
    hdi_2100 = income_index[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

cc<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "cc_med_med")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
cc_summ <- cc %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = cc_med_med[year == 2024],
    hdi_2050 = cc_med_med[year == 2050],
    hdi_2100 = cc_med_med[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()
print(cc_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Income Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Income Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Income Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))

ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Income Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)





# edu_index

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)

glob_data$cc_med<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)


glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3006:3013]

# delta_mean delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
#   -0.00124  -0.00124  -0.00156 -0.000838        -0.00169       -0.00168       -0.00212       -0.00111
#   -0.00141  -0.00142  -0.00175 -0.00108         -0.00194       -0.00195       -0.00241       -0.00147
#   -0.00355  -0.00355  -0.00468 -0.00232         -0.00635       -0.00635       -0.00834       -0.00421
#   -0.00551  -0.00547  -0.00737 -0.00371         -0.00771       -0.00764       -0.0103        -0.00519


glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    cc_med_med=median(cc_med, na.rm = TRUE),
    .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "edu_index")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = edu_index[year == 2024],
    hdi_2050 = edu_index[year == 2050],
    hdi_2100 = edu_index[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

cc<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "cc_med_med")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
cc_summ <- cc %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = cc_med_med[year == 2024],
    hdi_2050 = cc_med_med[year == 2050],
    hdi_2100 = cc_med_med[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()
print(cc_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

### 90%

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Edu Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Edu Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Edu Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))

ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Edu Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)





# lifex_index

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)

glob_data$cc_med<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)


glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3006:3013]

#  delta_mean delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
#   -0.000574 -0.000575 -0.000711 -0.000432       -0.000664      -0.000665      -0.000818      -0.000495
#   -0.000853 -0.000849 -0.00106  -0.000649       -0.00110       -0.00109       -0.00138       -0.000829
#   -0.00263  -0.00263  -0.00322  -0.00204        -0.00444       -0.00444       -0.00540       -0.00347 
#   -0.00644  -0.00645  -0.00783  -0.00503        -0.00730       -0.00730       -0.00890       -0.00569 

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    cc_med_med=median(cc_med, na.rm = TRUE),
    .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "lifex_index")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = lifex_index[year == 2024],
    hdi_2050 = lifex_index[year == 2050],
    hdi_2100 = lifex_index[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

cc<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "cc_med_med")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
cc_summ <- cc %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    hdi_2024 = cc_med_med[year == 2024],
    hdi_2050 = cc_med_med[year == 2050],
    hdi_2100 = cc_med_med[year == 2100],
    absolute_change_2050 = hdi_2050 - hdi_2024,
    percentage_change_2050 = ((hdi_2050 - hdi_2024) / hdi_2024) * 100,
    absolute_change_2100 = hdi_2100 - hdi_2024,
    percentage_change_2100 = ((hdi_2100 - hdi_2024) / hdi_2024) * 100
  )%>%distinct()
print(cc_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])


### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Lifex Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Lifex Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Lifex Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))

ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +ggtitle("Lifex Index")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

# very similar to sign, put this in SI 

############ original vars 


# gr_gnipc

out_var<-"gr_gnipc"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)


glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3017:3023]

# 2100 income
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1    -2611.    -3283.    -1903.         -0.0384        -0.0386        -0.0485        -0.0279
# 2    -4259.    -5340.    -3059.         -0.0841        -0.0850        -0.106         -0.0613
# 3    -3327.    -4221.    -2378.         -0.168         -0.169         -0.209         -0.125 
# 4   -19424.   -25525.   -13254.         -0.174         -0.175         -0.220         -0.126 

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3017:3023]

# 2050 income 
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1     -825.    -1109.     -517.         -0.0333        -0.0334        -0.0434        -0.0240
# 2     -684.     -913.     -452.         -0.0356        -0.0357        -0.0462        -0.0245
# 3     -542.     -742.     -339.         -0.0428        -0.0428        -0.0553        -0.0297
# 4    -1580.    -2185.    -1002.         -0.0545        -0.0545        -0.0721        -0.0370


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    cc_med_med=median(cc_med, na.rm = TRUE),
    .groups = 'drop'
  )



baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "value_interp")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    val_2024 = value_interp[year == 2024],
    val_2050 = value_interp[year == 2050],
    val_2100 = value_interp[year == 2100],
    absolute_change_2050 = val_2050 - val_2024,
    percentage_change_2050 = ((val_2050 - val_2024) / val_2024) * 100,
    absolute_change_2100 = val_2100 - val_2024,
    percentage_change_2100 = ((val_2100 - val_2024) / val_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])
# ssp    absolute_change_2050 absolute_change_2100 percentage_change_2050 percentage_change_2100
# <fct>                 <dbl>                <dbl>                  <dbl>                  <dbl>
# 1 ssp126               16849.               58022.                   92.5                  319. 
# 2 ssp245                9833.               41565.                   55.4                  234. 
# 3 ssp370                3002.                6943.                   17.5                   40.4
# 4 ssp585               25192.              122721.                  135.                   657. 

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("gnipc_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("gnipc_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) + ggtitle("Income per Capita") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 18), 
                   legend.position="none", axis.title.y = element_blank())

ggsave(filename=file.path(out_dir, paste0("gnipc_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=4, height=5)
g1<-g


g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("gnipc_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)




# gr_eys

out_var<-"gr_eys"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)


glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3017:3023]

# edu 2100
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1   -0.0253   -0.0320   -0.0173        -0.00146       -0.00145       -0.00183      -0.000967
# 2   -0.0298   -0.0368   -0.0226        -0.00172       -0.00172       -0.00214      -0.00131 
# 3   -0.0744   -0.0981   -0.0487        -0.00536       -0.00536       -0.00705      -0.00355 
# 4   -0.115    -0.156    -0.0779        -0.00681       -0.00675       -0.00907      -0.00456 


glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3017:3023]

# edu 2050
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1  -0.00891   -0.0153  -0.00342       -0.000618      -0.000613      -0.00101       -0.000207
# 2  -0.00808   -0.0122  -0.00393       -0.000583      -0.000579      -0.000867      -0.000280
# 3  -0.0108    -0.0196  -0.00179       -0.000796      -0.000801      -0.00134       -0.000177
# 4  -0.0235    -0.0315  -0.0159        -0.00149       -0.00148       -0.00199       -0.000987


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "value_interp")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    val_2024 = value_interp[year == 2024],
    val_2050 = value_interp[year == 2050],
    val_2100 = value_interp[year == 2100],
    absolute_change_2050 = val_2050 - val_2024,
    percentage_change_2050 = ((val_2050 - val_2024) / val_2024) * 100,
    absolute_change_2100 = val_2100 - val_2024,
    percentage_change_2100 = ((val_2100 - val_2024) / val_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])
# ssp    absolute_change_2050 absolute_change_2100 percentage_change_2050 percentage_change_2100
# <fct>                 <dbl>                <dbl>                  <dbl>                  <dbl>
# 1 ssp126                2.03                  3.97                  14.9                   29.2 
# 2 ssp245                1.52                  3.46                  11.3                   25.6 
# 3 ssp370                0.975                 1.09                  7.25                   8.10
# 4 ssp585                2.05                  4.00                  15.0                   29.3 

### 90%


### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("eys_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("eys_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+ggtitle("Exp. Years of Schooling") +
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 18), 
                   legend.position="none", axis.title.y = element_blank())

ggsave(filename=file.path(out_dir, paste0("eys_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=4, height=5)
g2<-g


g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("eys_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)



# gr_leb

out_var<-"gr_leb"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q05<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
glob_data$delta_q95<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant95)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q05<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
glob_data$perc_delta_q95<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)


glob_data_2100<-glob_data%>%filter(year==2100)
glob_data_2100 <- glob_data_2100 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2100[, 3017:3023]

# life exp 2100
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1   -0.0438   -0.0542   -0.0329       -0.000471      -0.000472      -0.000581      -0.000355
# 2   -0.0651   -0.0815   -0.0498       -0.000758      -0.000753      -0.000949      -0.000573
# 3   -0.201    -0.246    -0.156        -0.00278       -0.00278       -0.00339       -0.00216 
# 4   -0.494    -0.600    -0.385        -0.00524       -0.00524       -0.00638       -0.00408 

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3017:3023]

# life exp 2050
# delta_med delta_q05 delta_q95 perc_delta_mean perc_delta_med perc_delta_q05 perc_delta_q95
# 1   -0.0177 -0.0232    -0.00897       -0.000231      -0.000232     -0.000312       -0.000115
# 2    0.0140  0.000322   0.0228         0.000174       0.000176      0.0000150       0.000289
# 3   -0.0201 -0.0309    -0.00950       -0.000305      -0.000305     -0.000451       -0.000160
# 4   -0.0623 -0.0790    -0.0465        -0.000756      -0.000759     -0.000951       -0.000566

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

glob_data <- glob_data %>%
  group_by(year, ssp) %>%
  mutate(
    delta_mean_median = median(delta_mean, na.rm = TRUE),
    perc_delta_mean_median = median(perc_delta_mean, na.rm = TRUE),
    .groups = 'drop'
  )

baseline<-glob_data[which(glob_data$year==2024 | glob_data$year==2050 | glob_data$year==2100), c("ssp", "year", "value_interp")]%>%distinct()
# Calculate percentage change from 2024 to 2050 and 2100
baseline_summ <- baseline %>%
  filter(year %in% c(2024, 2050, 2100)) %>%
  group_by(ssp) %>%
  summarise(
    val_2024 = value_interp[year == 2024],
    val_2050 = value_interp[year == 2050],
    val_2100 = value_interp[year == 2100],
    absolute_change_2050 = val_2050 - val_2024,
    percentage_change_2050 = ((val_2050 - val_2024) / val_2024) * 100,
    absolute_change_2100 = val_2100 - val_2024,
    percentage_change_2100 = ((val_2100 - val_2024) / val_2024) * 100
  )%>%distinct()

print(baseline_summ[, c("ssp", "absolute_change_2050", "absolute_change_2100","percentage_change_2050", "percentage_change_2100")])

# ssp    absolute_change_2050 absolute_change_2100 percentage_change_2050 percentage_change_2100
# <fct>                 <dbl>                <dbl>                  <dbl>                  <dbl>
# 1 ssp126                8.03                 20.6                   10.8                   27.6 
# 2 ssp245                4.64                 13.1                    6.31                  17.9 
# 3 ssp370                0.978                 3.03                   1.35                   4.17
# 4 ssp585                8.19                 21.1                   11.0                   28.2

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  geom_line( aes(y = delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  labs(
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("leb_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q05*100, ymax = delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Change (cc - baseline) in % points",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("leb_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=0.8) + 
  geom_line( aes(y = perc_delta_mean_median*100, color = ssp), size = 2, linetype = 1) +
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+ ggtitle("Life Expectancy") +
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 18), 
                   legend.position="none", axis.title.y = element_blank())

ggsave(filename=file.path(out_dir, paste0("leb_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=4, height=5)
g3<-g


g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q05*100, ymax = perc_delta_q95*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Change (cc - baseline)/baseline",
    fill = "SSP",
    color= "SSP",
    linetype= "Climate Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("leb_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)



library(patchwork)
combined<-g1+g2+g3
ggsave(filename=file.path(out_dir, paste0("all_orig_vars_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), combined, width=12, height=5)

################################################################################
################################################################################
################################################################################

### delta decomposition

library(ggplot2)
library(dplyr)
library(tidyr)


out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$delta_hdi_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
all_data<-glob_data[, c("ssp", "model", "year", "delta_hdi_mean", "hdi")]

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$delta_income_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "delta_income_index_mean", "income_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec,"_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$delta_edu_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "delta_edu_index_mean", "edu_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec,"_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$delta_lifex_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "delta_lifex_index_mean", "lifex_index")]
all_data<-inner_join(all_data, glob_data)

# med across models
all_data_med<-all_data%>%group_by(ssp, year)%>%
  transmute(hdi=median(hdi, na.rm=TRUE),
            income_index=median(income_index, na.rm=TRUE),
            lifex_index=median(lifex_index, na.rm=TRUE),
            edu_index=median(edu_index, na.rm=TRUE),
            
            delta_hdi_mean=median(delta_hdi_mean, na.rm=TRUE),
            delta_income_index_mean=median(delta_income_index_mean, na.rm=TRUE),
            delta_lifex_index_mean=median(delta_lifex_index_mean, na.rm=TRUE),
            delta_edu_index_mean=median(delta_edu_index_mean, na.rm=TRUE))%>%distinct()%>%
  
  mutate(hdi_cc=hdi+delta_hdi_mean,
         income_index_cc=income_index+delta_income_index_mean,
         edu_index_cc=edu_index+delta_edu_index_mean,
         lifex_index_cc=lifex_index+delta_lifex_index_mean)%>%
  
  mutate(glob_comp_lifex=(1/3)*hdi_cc*(log(lifex_index_cc)-log(lifex_index)),
         glob_comp_income=(1/3)*hdi_cc*(log(income_index_cc)-log(income_index)),
         glob_comp_edu=(1/3)*hdi_cc*(log(edu_index_cc)-log(edu_index)))%>%
  #normalize
  mutate(const=delta_hdi_mean/(glob_comp_lifex+glob_comp_income+glob_comp_edu),
         glob_comp_lifex=glob_comp_lifex*const,
         glob_comp_income=glob_comp_income*const,
         glob_comp_edu=glob_comp_edu*const)

plots<-list()
for (y in c(2030, 2050, 2080, 2100)){
  
  # all
  hdi_val<- all_data_med %>% filter(year==y)%>%
    select(ssp, delta_hdi_mean)
  hdi_val<-unique(hdi_val)
  print(hdi_val)
  
  data_long <- all_data_med %>% filter(year==y)%>%ungroup()%>%
    select(ssp, glob_comp_income, 
           glob_comp_lifex, 
           glob_comp_edu) %>%
    pivot_longer(cols = -ssp, 
                 names_to = "component", 
                 values_to = "delta_value")
  data_long<-unique(data_long)
  
  data_long$component <- factor(data_long$component, 
                                levels = c("glob_comp_income", 
                                           "glob_comp_lifex", 
                                           "glob_comp_edu"),
                                labels = c("Income", "Life Expectancy", "Education"))
  
  g<-ggplot(data_long, aes(x = ssp, y = delta_value*100, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Change (% points)",
         fill = "Component") +
    ylim(-3.5, 0)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("all_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  plots[[length(plots)+1]]<-g
  
  # sign, simple
  data_long <- all_data_med %>% filter(year==y)%>%ungroup()%>%
    select(ssp, delta_income_index_mean, 
           delta_lifex_index_mean, 
           delta_edu_index_mean) %>%
    pivot_longer(cols = -ssp, 
                 names_to = "component", 
                 values_to = "delta_value")
  data_long<-unique(data_long)
  # 
  data_long$component <- factor(data_long$component, 
                                levels = c("delta_income_index_mean", 
                                           "delta_lifex_index_mean", 
                                           "delta_edu_index_mean"),
                                labels = c("Income", "Life Expectancy", "Education"))
  # 
  g<-ggplot(data_long, aes(x = ssp, y = 100*delta_value, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    #scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Change (% points)",
         fill = "Component") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("all_simple_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
}


# over time 

data_long <- all_data_med %>%
  select(year, ssp, delta_hdi_mean,
         glob_comp_income, 
         glob_comp_lifex, 
         glob_comp_edu) %>%
  pivot_longer(cols = starts_with("glob_comp"), 
               names_to = "component", 
               values_to = "delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("glob_comp_income", 
                                         "glob_comp_lifex", 
                                         "glob_comp_edu"),
                              labels = c("Income", "Life Expectancy", "Education"))

g<-ggplot(data_long, aes(x = year, y = delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Change (% points)",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("all_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


# over time simple

data_long <- all_data_med %>%
  select(year, ssp, 
         delta_income_index_mean, 
         delta_lifex_index_mean, 
         delta_edu_index_mean) %>%
  pivot_longer(cols = starts_with("delta"), 
               names_to = "component", 
               values_to = "delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("delta_income_index_mean", 
                                         "delta_lifex_index_mean", 
                                         "delta_edu_index_mean"),
                              labels = c("Income", "Life Expectancy", "Education"))

g<-ggplot(data_long, aes(x = year, y = delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Change (% points)",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("all_simple_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


###  PERC delta decomposition

library(ggplot2)
library(dplyr)
library(tidyr)


out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$perc_delta_hdi_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
all_data<-glob_data[, c("ssp", "model", "year", "perc_delta_hdi_mean", "hdi")]

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$perc_delta_income_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "perc_delta_income_index_mean", "income_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$perc_delta_edu_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "perc_delta_edu_index_mean", "edu_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")))
glob_data$perc_delta_lifex_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "perc_delta_lifex_index_mean", "lifex_index")]
all_data<-inner_join(all_data, glob_data)

# med across models
all_data_med<-all_data%>%group_by(ssp, year)%>%
  transmute(hdi=median(hdi, na.rm=TRUE),
            income_index=median(income_index, na.rm=TRUE),
            lifex_index=median(lifex_index, na.rm=TRUE),
            edu_index=median(edu_index, na.rm=TRUE),
            
            perc_delta_hdi_mean=median(perc_delta_hdi_mean, na.rm=TRUE),
            perc_delta_income_index_mean=median(perc_delta_income_index_mean, na.rm=TRUE),
            perc_delta_lifex_index_mean=median(perc_delta_lifex_index_mean, na.rm=TRUE),
            perc_delta_edu_index_mean=median(perc_delta_edu_index_mean, na.rm=TRUE))%>%distinct()%>%
  
  mutate(hdi_cc=hdi+hdi*perc_delta_hdi_mean,
         income_index_cc=income_index+income_index*perc_delta_income_index_mean,
         edu_index_cc=edu_index+edu_index*perc_delta_edu_index_mean,
         lifex_index_cc=lifex_index+lifex_index*perc_delta_lifex_index_mean)%>%
  
  mutate(glob_comp_lifex=(1/3)*(log(lifex_index_cc)-log(lifex_index)),
         glob_comp_income=(1/3)*(log(income_index_cc)-log(income_index)),
         glob_comp_edu=(1/3)*(log(edu_index_cc)-log(edu_index)))%>%
  #normalize
  mutate(const=perc_delta_hdi_mean/(glob_comp_lifex+glob_comp_income+glob_comp_edu),
         glob_comp_lifex=glob_comp_lifex*const,
         glob_comp_income=glob_comp_income*const,
         glob_comp_edu=glob_comp_edu*const)

plots<-list()
for (y in c(2030, 2050, 2080, 2100)){
  
  # all
  hdi_val<- all_data_med %>% filter(year==y)%>%
    select(ssp, perc_delta_hdi_mean)
  hdi_val<-unique(hdi_val)
  print(hdi_val)
  
  data_long <- all_data_med %>% filter(year==y)%>%ungroup()%>%
    select(ssp, glob_comp_income, 
           glob_comp_lifex, 
           glob_comp_edu) %>%
    pivot_longer(cols = -ssp, 
                 names_to = "component", 
                 values_to = "perc_delta_value")
  data_long<-unique(data_long)
  
  data_long$component <- factor(data_long$component, 
                                levels = c("glob_comp_income", 
                                           "glob_comp_lifex", 
                                           "glob_comp_edu"),
                                labels = c("Income", "Life Expectancy", "Education"))
  
  g<-ggplot(data_long, aes(x = ssp, y = perc_delta_value*100, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Percentage Change",
         fill = "Component") +
    ylim(-4, 0)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
  plots[[length(plots)+1]]<-g
  ggsave(filename=file.path(out_dir, paste0("all_perc_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
  
  # sign, simple
  data_long <- all_data_med %>% filter(year==y)%>%ungroup()%>%
    select(ssp, perc_delta_income_index_mean, 
           perc_delta_lifex_index_mean, 
           perc_delta_edu_index_mean) %>%
    pivot_longer(cols = -ssp, 
                 names_to = "component", 
                 values_to = "perc_delta_value")
  data_long<-unique(data_long)
  # 
  data_long$component <- factor(data_long$component, 
                                levels = c("perc_delta_income_index_mean", 
                                           "perc_delta_lifex_index_mean", 
                                           "perc_delta_edu_index_mean"),
                                labels = c("Income", "Life Expectancy", "Education"))
  # 
  g<-ggplot(data_long, aes(x = ssp, y = 100*perc_delta_value, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    #scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Percentage Change",
         fill = "Component") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("all_perc_simple_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
}
require(grid)
require(ggpubr)

final<-ggpubr::ggarrange(plots[[2]]+ rremove("ylab") + rremove("xlab"), plots[[4]]+ rremove("ylab") + rremove("xlab"), common.legend = TRUE, legend = "right")
final<-annotate_figure(final, left = text_grob("Total Percentage Change", rot = 90, vjust = 1, size = 20),
                       bottom = text_grob("SSP Scenario", size = 20))
ggsave(filename=file.path(out_dir, paste0("all_perc_dam_share_ssp_comparison_2050_2100_",spec,'_',vars, ".png")), final, width=11, height=4)


# over time 

data_long <- all_data_med %>%
  select(year, ssp, perc_delta_hdi_mean,
         glob_comp_income, 
         glob_comp_lifex, 
         glob_comp_edu) %>%
  pivot_longer(cols = starts_with("glob_comp"), 
               names_to = "component", 
               values_to = "perc_delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("glob_comp_income", 
                                         "glob_comp_lifex", 
                                         "glob_comp_edu"),
                              labels = c("Income", "Life Expectancy", "Education"))

g<-ggplot(data_long, aes(x = year, y = perc_delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*perc_delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Percentage Change",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("all_evol_perc_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


# over time simple

data_long <- all_data_med %>%
  select(year, ssp, 
         perc_delta_income_index_mean, 
         perc_delta_lifex_index_mean, 
         perc_delta_edu_index_mean) %>%
  pivot_longer(cols = starts_with("perc_delta"), 
               names_to = "component", 
               values_to = "perc_delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("perc_delta_income_index_mean", 
                                         "perc_delta_lifex_index_mean", 
                                         "perc_delta_edu_index_mean"),
                              labels = c("Income", "Life Expectancy", "Education"))

g<-ggplot(data_long, aes(x = year, y = perc_delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*perc_delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Percentage Change",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(family = "CM Roman",size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("all_simple_evol_perc_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


################################################################################
################################################################################
################################################################################



options(arrow.skip_nul = TRUE)
# maps: median diff or perc diff (med_values_deltas) selecting 90% and 2/3 models from the subnat (deltas boot intervals)

out_dir_maps<-file.path(out_dir, "maps")
if(!dir.exists(out_dir_maps)){dir.create(out_dir_maps)}

gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)


library(ggplot2)
library(ggpubr)
library(ggpattern)

#### hdi

gc()
out_var<-"hdi"
out_var_plot<-"HDI"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_rob_cov_boot_int.feather")))



glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q95) == sign(delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop")

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q95) == sign(perc_delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop")

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,hdi)%>%distinct()

gc()
years<-c(2050,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,"_dam.png")), g, width=8, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y, "_perc_dam.png")), g, width=8, height=4)
    
  }
}

#### income_index

gc()
out_var<-"income_index"
out_var_plot<-"Income Index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_rob_cov_boot_int.feather")))


glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q95) == sign(delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop")

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q95) == sign(perc_delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop")

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,income_index)%>%distinct()

rm(glob_data)
gc()
years<-c(2050, 2100)#c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,"_dam.png")), g, width=8, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y, "_perc_dam.png")), g, width=8, height=4)
    
  }
}

#### lifex_index

gc()
out_var<-"lifex_index"
out_var_plot<-"Lifex Index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_rob_cov_boot_int.feather")))


glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q95) == sign(delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop")

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q95) == sign(perc_delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop")

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,lifex_index)%>%distinct()

rm(glob_data)
gc()
years<-c(2050, 2100)#c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,"_dam.png")), g, width=8, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y, "_perc_dam.png")), g, width=8, height=4)
    
  }
}



#### edu_index

gc()
out_var<-"edu_index"
out_var_plot<-"Edu Index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_rob_cov_boot_int.feather")))



glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q95) == sign(delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop")

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q95) == sign(perc_delta_q05)) %>%
  group_by(gdlcode, year, ssp) %>%
  mutate(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop")

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,edu_index)%>%distinct()

rm(glob_data)

gc()
years<-c(2050, 2100)#c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,"_dam.png")), g, width=8, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,"_perc_dam.png")), g, width=8, height=4)
    
  }
}




################################################################################
