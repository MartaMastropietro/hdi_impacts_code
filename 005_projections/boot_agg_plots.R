
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)

library(RColorBrewer)

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj"
if(!dir.exists(out_dir)){dir.create(out_dir)}

#lags number 
N<-"_mix_"
#N<-8

out_dir<-file.path(out_dir, paste0("N",N,"lags"))
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
type<-types[1]
vars<-vars_in_proj[1]



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
# for each model, global weighted hdi delta course and components, only sign 


# hdi

out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

# mean values of median model at some years, with ci 
glob_data_med<-glob_data%>%group_by(year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()
glob_data_med$delta_mean<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$delta_q10<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data_med$delta_q90<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant90)
glob_data_med$perc_delta_mean<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$perc_delta_q10<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data_med$perc_delta_q90<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
print(out_var)
glob_data_med[which(glob_data_med$year==2050), c(1,2,3007:3012)]
glob_data_med[which(glob_data_med$year==2100), c(1,2,3007:3012)]


glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)




# income_index

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))


# mean values of median model at some years, with ci 
glob_data_med<-glob_data%>%group_by(year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()
glob_data_med$delta_mean<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$delta_q10<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data_med$delta_q90<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant90)
glob_data_med$perc_delta_mean<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$perc_delta_q10<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data_med$perc_delta_q90<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
print(out_var)
glob_data_med[which(glob_data_med$year==2050), c(1,2,3007:3012)]
glob_data_med[which(glob_data_med$year==2100), c(1,2,3007:3012)]



glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)



# lifex_index

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))



# mean values of median model at some years, with ci 
glob_data_med<-glob_data%>%group_by(year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()
glob_data_med$delta_mean<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$delta_q10<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data_med$delta_q90<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant90)
glob_data_med$perc_delta_mean<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$perc_delta_q10<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data_med$perc_delta_q90<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
print(out_var)
glob_data_med[which(glob_data_med$year==2050), c(1,2,3007:3012)]
glob_data_med[which(glob_data_med$year==2100), c(1,2,3007:3012)]




glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)



# edu_index

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))



# mean values of median model at some years, with ci 
glob_data_med<-glob_data%>%group_by(year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()
glob_data_med$delta_mean<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$delta_q10<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data_med$delta_q90<-apply(glob_data_med%>%select(matches("^delta\\.\\d+$")), 1, quant90)
glob_data_med$perc_delta_mean<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data_med$perc_delta_q10<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data_med$perc_delta_q90<-apply(glob_data_med%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
print(out_var)
glob_data_med[which(glob_data_med$year==2050), c(1,2,3007:3012)]
glob_data_med[which(glob_data_med$year==2100), c(1,2,3007:3012)]




glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_s_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)


################################################################################
# for each model, global weighted PERC hdi delta course and components, only sign 


# hdi

out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)




# income_index

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)




# lifex_index

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)




# edu_index

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_s_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)



################################################################################
################################################################################
################################################################################

# for each model, global weighted hdi delta, perc delta course 



# hdi

out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$delta_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)


glob_data$perc_delta_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)


glob_data$model<-as.factor(glob_data$model)
glob_data$ssp<-as.factor(glob_data$ssp)
glob_data$model_ssp<-as.factor(paste0(glob_data$model, "_", glob_data$ssp ))

gc()

### 90%

### 90%, CI all together 
g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = delta_q10*100, ymax = delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(glob_data$model)) +
  labs(
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci.png")), g, width=12, height=9)



g<-ggplot( data=glob_data, aes(x = year ) ) +
  geom_ribbon(aes(ymin = perc_delta_q10*100, ymax = perc_delta_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = perc_delta_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Percentage Loss (cc - baseline)/baseline",
    color = "SSP",
    shape= "Model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()+theme(text = element_text(size = 20))
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_perc_cc_dam_int_90_ci_by_mod.png")), g, width=12, height=9)

# very similar to sign, put this in SI 


################################################################################
################################################################################
################################################################################

### delta decomposition

library(ggplot2)
library(dplyr)
library(tidyr)


out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_hdi_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
all_data<-glob_data[, c("ssp", "model", "year", "delta_hdi_mean", "hdi")]

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_income_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "delta_income_index_mean", "income_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_edu_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "delta_edu_index_mean", "edu_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
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
                                labels = c("Income Index", "Life Expectancy Index", "Education Index"))
  
  g<-ggplot(data_long, aes(x = ssp, y = delta_value*100, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Loss (% points)",
         fill = "Component") +
    ylim(-3.5, 0)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("sign_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
  
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
                                labels = c("Income Index", "Life Expectancy Index", "Education Index"))
  # 
  g<-ggplot(data_long, aes(x = ssp, y = 100*delta_value, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    #scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Loss (% points)",
         fill = "Component") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("sign_simple_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
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
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = year, y = delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Loss (% points)",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


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
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = year, y = delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Loss (% points)",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_simple_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


###  PERC delta decomposition

library(ggplot2)
library(dplyr)
library(tidyr)


out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$perc_delta_hdi_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
all_data<-glob_data[, c("ssp", "model", "year", "perc_delta_hdi_mean", "hdi")]

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$perc_delta_income_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "perc_delta_income_index_mean", "income_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$perc_delta_edu_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data<-glob_data[, c("ssp", "model", "year", "perc_delta_edu_index_mean", "edu_index")]
all_data<-inner_join(all_data, glob_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
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
                                labels = c("Income Index", "Life Expectancy Index", "Education Index"))
  
  g<-ggplot(data_long, aes(x = ssp, y = perc_delta_value*100, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Percentage Loss",
         fill = "Component") +
    ylim(-4, 0)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("sign_perc_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
  
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
                                labels = c("Income Index", "Life Expectancy Index", "Education Index"))
  # 
  g<-ggplot(data_long, aes(x = ssp, y = 100*perc_delta_value, fill = component)) +
    geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
    scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
    #scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
    labs(title = paste0("Year ",y),
         x = "SSP Scenario", 
         y = "Total Percentage Loss",
         fill = "Component") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
  ggsave(filename=file.path(out_dir, paste0("sign_perc_simple_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)
  
}


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
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = year, y = perc_delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*perc_delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Percentage Loss",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_evol_perc_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


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
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = year, y = perc_delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#048ba8", "#f18f01", "#2e4057") ) +
  #geom_line(aes(x=year, y=100*perc_delta_hdi_mean))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
    x = "SSP Scenario", 
    y = "Total Percentage Loss",
    fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size = 20))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_simple_evol_perc_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


################################################################################
################################################################################
################################################################################



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
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_int.feather")))

glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,hdi)%>%distinct()

gc()
years<-c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("HDI change (% points)") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #   geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
    #                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("HDI perc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    #  gc()
    #  # hdi
    #  g<-ggplot(plot_map)+geom_sf( aes(fill=hdi*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #    labs(fill=paste0("HDI baseline") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #    #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    #    #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #    #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    #  ggsave(filename=file.path(out_dir_maps, paste0("hdi_", spec,'_',vars,"_", sc ,"_", y, "_val_baseline.png")), g, width=9, height=4)
    #  
    #  # hdi cc
    #  g<-ggplot(plot_map)+geom_sf( aes(fill=median_cc_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #    labs(fill=paste0("HDI cc") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #    #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    #    #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #    #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    #    ggsave(filename=file.path(out_dir_maps, paste0("hdi_", spec,'_',vars,"_", sc ,"_", y, "_val_cc.png")), g, width=9, height=4)
    
  }
}


#### income_index

gc()
out_var<-"income_index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_int.feather")))

glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,income_index)%>%distinct()

rm(glob_data)
gc()
years<-c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("income_index change (% points)") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #   geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
    #                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_income_index_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("income_index perc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_income_index_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    # gc()
    #  # income_index
    #  g<-ggplot(plot_map)+geom_sf( aes(fill=income_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #    labs(fill=paste0("income_index baseline") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #  #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    #  #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #  #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    #  ggsave(filename=file.path(out_dir_maps, paste0("income_index_", spec,'_',vars,"_", sc ,"_", y, "_val_baseline.png")), g, width=9, height=4)
    #  
    #  # income_index cc
    #  g<-ggplot(plot_map)+geom_sf( aes(fill=median_cc_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #    labs(fill=paste0("income_index cc") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #  #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    #  #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #  #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    #  ggsave(filename=file.path(out_dir_maps, paste0("income_index_", spec,'_',vars,"_", sc ,"_", y, "_val_cc.png")), g, width=9, height=4)
    
  }
}



#### lifex_index

gc()
out_var<-"lifex_index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_int.feather")))

glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,lifex_index)%>%distinct()

rm(glob_data)
gc()
years<-c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("lifex_index change (% points)") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #   geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
    #                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    # 
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("lifex_index perc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    # gc()
    # # lifex_index
    # g<-ggplot(plot_map)+geom_sf( aes(fill=lifex_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("lifex_index baseline") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    # #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    # #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    # #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_val_baseline.png")), g, width=9, height=4)
    # 
    # # lifex_index cc
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_cc_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("lifex_index cc") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    # #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    # #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    # #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_val_cc.png")), g, width=9, height=4)
    
  }
}



#### edu_index

gc()
out_var<-"edu_index"
glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,'_values_deltas_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_int.feather")))

glob_data <- glob_data %>%
  mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

glob_data <- glob_data %>%
  mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
  right_join(glob_data, by = c("gdlcode", "year", "ssp"))  

data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
  mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
         median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
         median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
  select(gdlcode, year, ssp, iso3, 
         sign_90,sign_perc_90,
         median_delta_mean,median_perc_delta_mean,median_cc_mean,edu_index)%>%distinct()

rm(glob_data)

gc()
years<-c(2030,2050,2080,2100)

for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # # 90 , absolute changes
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("edu_index change (% points)") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    #   geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
    #                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    #                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_edu_index_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    # gc()
    
    # 90 , perc changes
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("edu_index perc change (% points)") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_edu_index_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    # gc()
    # # edu_index
    # g<-ggplot(plot_map)+geom_sf( aes(fill=edu_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("edu_index baseline") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    # #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    # #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    # #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("edu_index_", spec,'_',vars,"_", sc ,"_", y, "_val_baseline.png")), g, width=9, height=4)
    # gc()
    # 
    # # edu_index cc
    # g<-ggplot(plot_map)+geom_sf( aes(fill=median_cc_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
    #   labs(fill=paste0("edu_index cc") ) #+ ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
    # #geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
    # #                pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
    # #                fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    # ggsave(filename=file.path(out_dir_maps, paste0("edu_index_", spec,'_',vars,"_", sc ,"_", y, "_val_cc.png")), g, width=9, height=4)
    # gc()
  }
}



################################################################################
################################################################################
################################################################################


rm(list=ls())
library(readr)
#library(data.table)
library(reshape2)
library(dplyr)
library(purrr)

library(stringr)

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_extr_tp", "extr_only")
spec_type<-"dlm"
effect<-"growth_eff"
#lags number 
#N<-8
N<-"_mix_"

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


nboot<-1000


pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
pop_projection_gdlcode<-pop_projection_gdlcode%>%
  group_by(iso3, year, ssp)%>%
  mutate(pop_country=sum(pop_gdl))
pop_projection_gdlcode<-unique(pop_projection_gdlcode)

# fix countries for aggregations 
sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")


# create hdi projections with past, future proj values 

out_dir_comp<-"output/projections/original_comp" 

data_proj_gni<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "gnipc", ".csv")))
length(unique(data_proj_gni$iso3))
data_proj_leb<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "leb", ".csv")))
length(unique(data_proj_leb$iso3))
data_proj_eys<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "eys", ".csv")))
length(unique(data_proj_eys$iso3))


common_iso<-intersect(intersect(unique(data_proj_eys$iso3), unique(data_proj_gni$iso3)), unique(data_proj_leb$iso3))
data_proj_gni<-data_proj_gni%>%filter(iso3 %in% common_iso)
data_proj_leb<-data_proj_leb%>%filter(iso3 %in% common_iso)
data_proj_eys<-data_proj_eys%>%filter(iso3 %in% common_iso)


data_proj_gni$pop<-NULL
data_proj_leb$pop<-NULL
data_proj_eys$pop<-NULL

data_proj_gni<-unique(data_proj_gni)
data_proj_eys<-unique(data_proj_eys)
data_proj_leb<-unique(data_proj_leb)

# max min values calculated over all horizon 1990-2100
# max min log gnipc
data_proj_gni$lgnipc<-log(data_proj_gni$value_interp)
max_lgnipc<-max(data_proj_gni$lgnipc, na.rm=TRUE)
min_lgnipc<-min(data_proj_gni$lgnipc, na.rm=TRUE)
# max min eys
max_eys<-max(data_proj_eys$value_interp, na.rm=TRUE)
min_eys<-min(data_proj_eys$value_interp, na.rm=TRUE)
# max min log gnipc
max_leb<-max(data_proj_leb$value_interp, na.rm=TRUE)
min_leb<-min(data_proj_leb$value_interp, na.rm=TRUE)

colnames(data_proj_gni)[which(colnames(data_proj_gni)=="Scenario")]<-"ssp"
colnames(data_proj_eys)[which(colnames(data_proj_eys)=="Scenario")]<-"ssp"
colnames(data_proj_leb)[which(colnames(data_proj_leb)=="Scenario")]<-"ssp"

data_proj_gni<-inner_join(data_proj_gni, unique(pop_projection_gdlcode[, c("iso3", "year", "ssp", "pop_country")]) )
data_proj_eys<-inner_join(data_proj_eys, unique(pop_projection_gdlcode[, c("iso3", "year", "ssp", "pop_country")]) )
data_proj_leb<-inner_join(data_proj_leb, unique(pop_projection_gdlcode[, c("iso3", "year", "ssp", "pop_country")]) )

data_proj_gni$log_value_interp<-log(data_proj_gni$value_interp)
data_proj_gni$income_index<-(data_proj_gni$log_value_interp-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_proj_leb$lifex_index<-(data_proj_leb$value_interp-min_leb)/(max_leb-min_leb)
data_proj_eys$edu_index<-(data_proj_eys$value_interp-min_eys)/(max_eys-min_eys)


data_proj_gni$value_interp<-NULL
data_proj_gni$log_value_interp<-NULL
data_proj_gni$lgnipc<-NULL

data_proj_leb$value_interp<-NULL
data_proj_eys$value_interp<-NULL

data_proj_gni$pop<-NULL
data_proj_leb$pop<-NULL
data_proj_eys$pop<-NULL

data_proj_leb$pop_country<-NULL
data_proj_eys$pop_country<-NULL

data_all<-inner_join(data_proj_gni, data_proj_eys)
data_all<-inner_join(data_all, data_proj_leb)


data_all$hdi<-(data_all$income_index*data_all$edu_index*data_all$lifex_index)^(1/3)

data_all_glob<-data_all%>%group_by(year, ssp)%>%transmute(
  glob_income_index=weighted.mean(income_index, pop_country, na.rm=TRUE),
  glob_edu_index=weighted.mean(edu_index, pop_country, na.rm=TRUE),
  glob_lifex_index=weighted.mean(lifex_index, pop_country, na.rm=TRUE),
  glob_hdi=weighted.mean(hdi, pop_country, na.rm=TRUE)
)%>%distinct()


ggplot(data_all_glob, aes(x=year, y=glob_hdi, color=ssp))+geom_line()+theme_bw()

ggplot(data_all_glob, aes(x=year, y=glob_income_index, color=ssp))+geom_line()+theme_bw()

ggplot(data_all_glob, aes(x=year, y=glob_edu_index, color=ssp))+geom_line()+theme_bw()

ggplot(data_all_glob, aes(x=year, y=glob_lifex_index, color=ssp))+geom_line()+theme_bw()

data_all_glob$glob_hdi[which(data_all_glob$year==2019 & data_all_glob$ssp=="ssp370")]-data_all_glob$glob_hdi[which(data_all_glob$year==2020 & data_all_glob$ssp=="ssp370")]
data_all_glob$glob_income_index[which(data_all_glob$year==2019 & data_all_glob$ssp=="ssp370")]-data_all_glob$glob_income_index[which(data_all_glob$year==2020 & data_all_glob$ssp=="ssp370")]
data_all_glob$glob_edu_index[which(data_all_glob$year==2019 & data_all_glob$ssp=="ssp370")]-data_all_glob$glob_edu_index[which(data_all_glob$year==2020 & data_all_glob$ssp=="ssp370")]
data_all_glob$glob_lifex_index[which(data_all_glob$year==2019 & data_all_glob$ssp=="ssp370")]-data_all_glob$glob_lifex_index[which(data_all_glob$year==2020 & data_all_glob$ssp=="ssp370")]


