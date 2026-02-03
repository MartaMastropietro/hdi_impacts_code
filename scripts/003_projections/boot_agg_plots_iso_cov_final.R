
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)

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


out_dir<-file.path(out_dir, paste0("iso_cov"))
if(!dir.exists(out_dir)){dir.create(out_dir)}


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



# hdi

out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
  ) +ggtitle("HDI")+
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill") )+
  # (colour = ssp, show_guide = FALSE) +
  guides(fill = "none")+
  theme_bw()+theme(text = element_text(family = "CM Roman",size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
    .groups = 'drop'
  )

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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
    .groups = 'drop'
  )

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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
    .groups = 'drop'
  )

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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
glob_data_2100[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998 delta.999 perc_delta.999
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
#   1    -2421.        -0.0359    -2476.        -0.0348    -2608.        -0.0376    -2990.        -0.0445
# 2    -4095.        -0.0805    -3669.        -0.0709    -3998.        -0.0800    -4980.        -0.0985
# 3    -3157.        -0.157     -3119.        -0.151     -3049.        -0.160     -3859.        -0.191 
# 4   -19168.        -0.167    -19646.        -0.166    -17200.        -0.161    -23532.        -0.204 

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998 delta.999 perc_delta.999
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
#   1     -807.        -0.0310     -795.        -0.0294     -736.        -0.0323    -1023.        -0.0395
# 2     -669.        -0.0344     -672.        -0.0309     -549.        -0.0301     -798.        -0.0417
# 3     -518.        -0.0391     -533.        -0.0379     -451.        -0.0387     -674.        -0.0494
# 4    -1531.        -0.0506    -1570.        -0.0500    -1336.        -0.0485    -1979.        -0.0639

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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
glob_data_2100[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
# 1   -0.0294       -0.00167   -0.0202       -0.00114   -0.0246       -0.00143
# 2   -0.0279       -0.00170   -0.0336       -0.00196   -0.0344       -0.00199
# 3   -0.0661       -0.00476   -0.0453       -0.00331   -0.0702       -0.00506
# 4   -0.104        -0.00607   -0.0826       -0.00485   -0.110        -0.00648ç

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
# 1  -0.00772      -0.000522  -0.00675      -0.000402  -0.00781      -0.000524
# 2  -0.00816      -0.000564  -0.00582      -0.000399  -0.00907      -0.000645
# 3  -0.00626      -0.000521  -0.00943      -0.000689  -0.0102       -0.000758
# 4  -0.0195       -0.00122   -0.0153       -0.000970  -0.0211       -0.00136 


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
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_iso_cov_boot_all.csv")))

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
glob_data_2100[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
# 1   -0.0294       -0.00167   -0.0202       -0.00114   -0.0246       -0.00143
# 2   -0.0279       -0.00170   -0.0336       -0.00196   -0.0344       -0.00199
# 3   -0.0661       -0.00476   -0.0453       -0.00331   -0.0702       -0.00506
# 4   -0.104        -0.00607   -0.0826       -0.00485   -0.110        -0.00648ç

glob_data_2050<-glob_data%>%filter(year==2050)
glob_data_2050 <- glob_data_2050 %>%
  group_by(ssp) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
glob_data_2050[, 3006:3013]

# delta.996 perc_delta.996 delta.997 perc_delta.997 delta.998 perc_delta.998
# <dbl>          <dbl>     <dbl>          <dbl>     <dbl>          <dbl>
# 1  -0.00772      -0.000522  -0.00675      -0.000402  -0.00781      -0.000524
# 2  -0.00816      -0.000564  -0.00582      -0.000399  -0.00907      -0.000645
# 3  -0.00626      -0.000521  -0.00943      -0.000689  -0.0102       -0.000758
# 4  -0.0195       -0.00122   -0.0153       -0.000970  -0.0211       -0.00136 


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


