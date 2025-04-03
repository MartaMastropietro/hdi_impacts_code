

rm(list=ls())
source("utils/libraries.R")


### out dir 

out_dir<-"output/hazards_exploration"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/hazards_exploration/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

climate_projections<- arrow::read_feather("output/projections/climate_projections_country_area_weight.feather")
climate_projections<-tibble::as_tibble(climate_projections)

preproc_proj_population <- read_csv("output/projections/original_comp/preproc_proj_population.csv")
colnames(preproc_proj_population)<-c("year", "pop", "iso3", "ssp")

climate_projections<-dplyr::inner_join(climate_projections, preproc_proj_population)

mean_glob_cl_proj<-climate_projections %>% 
  group_by(model, ssp,  year) %>%
  mutate(glob_pop_TM = weighted.mean(TM, pop, na.rm=TRUE),
         glob_pop_TVAR = weighted.mean(TVAR, pop, na.rm=TRUE),
         glob_pop_RR = weighted.mean(RR, pop, na.rm=TRUE) , 
         glob_pop_HW = weighted.mean(HW, pop, na.rm=TRUE), 
         glob_pop_SPEI = weighted.mean(SPEI, pop, na.rm=TRUE) , 
         glob_pop_SPI = weighted.mean(SPI, pop, na.rm=TRUE) , 
         glob_pop_RX = weighted.mean(RX, pop, na.rm=TRUE), 
         glob_pop_PEXT = weighted.mean(PEXT, pop, na.rm=TRUE), 
         glob_pop_WD = weighted.mean(WD, pop, na.rm=TRUE), 
         
         glob_TM = mean(TM, na.rm=TRUE),
         glob_TVAR = mean(TVAR, na.rm=TRUE),
         glob_RR = mean(RR, na.rm=TRUE) , 
         glob_HW = mean(HW, na.rm=TRUE), 
         glob_SPEI = mean(SPEI, na.rm=TRUE) , 
         glob_SPI = mean(SPI, na.rm=TRUE) , 
         glob_RX = mean(RX, na.rm=TRUE), 
         glob_PEXT = mean(PEXT, na.rm=TRUE), 
         glob_WD = mean(WD, na.rm=TRUE),
         
         glob_pop_diff_TM = weighted.mean(diff_TM, pop, na.rm=TRUE),
         glob_pop_diff_TVAR = weighted.mean(diff_TVAR, pop, na.rm=TRUE),
         glob_pop_diff_RR = weighted.mean(diff_RR, pop, na.rm=TRUE) , 
         glob_pop_diff_HW = weighted.mean(diff_HW, pop, na.rm=TRUE), 
         glob_pop_diff_SPEI = weighted.mean(diff_SPEI, pop, na.rm=TRUE) , 
         glob_pop_diff_SPI = weighted.mean(diff_SPI, pop, na.rm=TRUE) , 
         glob_pop_diff_RX = weighted.mean(diff_RX, pop, na.rm=TRUE), 
         glob_pop_diff_PEXT = weighted.mean(diff_PEXT, pop, na.rm=TRUE), 
         glob_pop_diff_WD = weighted.mean(diff_WD, pop, na.rm=TRUE), 
         
         glob_diff_TM = mean(diff_TM, na.rm=TRUE),
         glob_diff_TVAR = mean(diff_TVAR, na.rm=TRUE),
         glob_diff_RR = mean(diff_RR, na.rm=TRUE) , 
         glob_diff_HW = mean(diff_HW, na.rm=TRUE), 
         glob_diff_SPEI = mean(diff_SPEI, na.rm=TRUE) , 
         glob_diff_SPI = mean(diff_SPI, na.rm=TRUE) , 
         glob_diff_RX = mean(diff_RX, na.rm=TRUE), 
         glob_diff_PEXT = mean(diff_PEXT, na.rm=TRUE), 
         glob_diff_WD = mean(diff_WD, na.rm=TRUE),
         
         glob_pop_TM_i_diff_TM = weighted.mean(TM_i_diff_TM, pop, na.rm=TRUE),
         glob_pop_TVAR_i_diff_TVAR = weighted.mean(TVAR_i_diff_TVAR, pop, na.rm=TRUE),
         glob_pop_RR_i_diff_RR = weighted.mean(RR_i_diff_RR, pop, na.rm=TRUE) , 
         glob_pop_HW_i_diff_HW = weighted.mean(HW_i_diff_HW, pop, na.rm=TRUE), 
         glob_pop_SPEI_i_diff_SPEI = weighted.mean(SPEI_i_diff_SPEI, pop, na.rm=TRUE) , 
         glob_pop_SPI_i_diff_SPI = weighted.mean(SPI_i_diff_SPI, pop, na.rm=TRUE) , 
         glob_pop_RX_i_diff_RX = weighted.mean(RX_i_diff_RX, pop, na.rm=TRUE), 
         glob_pop_PEXT_i_diff_PEXT = weighted.mean(PEXT_i_diff_PEXT, pop, na.rm=TRUE), 
         glob_pop_WD_i_diff_WD = weighted.mean(WD_i_diff_WD, pop, na.rm=TRUE), 
         
         glob_TM_i_diff_TM = mean(TM_i_diff_TM, na.rm=TRUE),
         glob_TVAR_i_diff_TVAR = mean(TVAR_i_diff_TVAR, na.rm=TRUE),
         glob_RR_i_diff_RR = mean(RR_i_diff_RR, na.rm=TRUE) , 
         glob_HW_i_diff_HW = mean(HW_i_diff_HW, na.rm=TRUE), 
         glob_SPEI_i_diff_SPEI = mean(SPEI_i_diff_SPEI, na.rm=TRUE) , 
         glob_SPI_i_diff_SPI = mean(SPI_i_diff_SPI, na.rm=TRUE) , 
         glob_RX_i_diff_RX = mean(RX_i_diff_RX, na.rm=TRUE), 
         glob_PEXT_i_diff_PEXT = mean(PEXT_i_diff_PEXT, na.rm=TRUE), 
         glob_WD_i_diff_WD = mean(WD_i_diff_WD, na.rm=TRUE),
         
         .keep="none"
         )


mean_glob_cl_proj$model<-as.factor(mean_glob_cl_proj$model)
mean_glob_cl_proj$ssp<-as.factor(mean_glob_cl_proj$ssp)
mean_glob_cl_proj$model_ssp<-as.factor(paste0(mean_glob_cl_proj$model, "_", mean_glob_cl_proj$ssp ))


### global damages plot 


for (var in setdiff(colnames(mean_glob_cl_proj), c("year", "model", "ssp", "model_ssp"))){
  g<-ggplot( data=mean_glob_cl_proj, aes(x = year, y = get(var), group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(mean_glob_cl_proj$model)) +
    geom_line() + 
    geom_point(size=2.7)+
    labs(
      x = "Year",
      y = paste0(var),
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #g
  ggsave(filename=file.path(out_dir, paste0(var,".png")), g, width=15, height=10)
  
  
}

######


data_long <- mean_glob_cl_proj %>%
  pivot_longer(
    cols = starts_with("glob_"), # Select all climate variables
    names_to = "variable",
    values_to = "value"
  )
data_long$model_ssp<-NULL

# Rank models for each variable, year, and SSP
ranked_data <- data_long %>%
  group_by(ssp, variable, year) %>%
  mutate(max_value = max(value)) %>%
  filter(value == max_value) %>%
  group_by(ssp, variable, model) %>%
  summarise(max_count = n(), .groups = "drop") %>%
  group_by(ssp, variable) %>%
  arrange(desc(max_count)) %>%
  mutate(rank = row_number())

# Prepare data for plotting
plot_data <- ranked_data %>%
  ungroup() %>%
  mutate(model = factor(model, levels = unique(model[order(rank)])))

# Plot the data
g<-ggplot(plot_data, aes(x = variable, y = max_count, fill = model)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ssp, scales = "free_y") +
  labs(
    title = "Ranking of Models by Climate Variable and SSP",
    x = "Climate Variable",
    y = "Number of Years with Highest Value",
    fill = "Model"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12)
  )
ggsave(filename=file.path(out_dir, paste0("models_ranking_max_variables",".png")), g, width=20, height=10)
