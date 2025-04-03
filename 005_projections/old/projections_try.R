### models no adap, for now just mean values, CI 


rm(list=ls())
source("utils/libraries.R")


out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/try"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,4)]
countries$geometry<-NULL



### climate projections 
ssp_names<-c("ssp126", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6") #for now just these two

data_cl_dir<-"data/climate_data/projections"

climate_projections<-read.csv(file.path(data_cl_dir, "projection_data_climate_country_2015_2100.csv"))


# climate manipulation

climate_projections <- climate_projections %>% 
  arrange(iso3, model, ssp, year) %>% 
  group_by(iso3) %>%
  dplyr::mutate(diff_TM = TM - dplyr::lag(TM, n = 1, default = NA),
                diff_TVAR = TVAR - dplyr::lag(TVAR, n = 1, default = NA),
                diff_RR = RR - dplyr::lag( RR, n = 1, default = NA), 
                diff_HW = HW - dplyr::lag( HW, n = 1, default = NA), 
                diff_SPEI = SPEI - dplyr::lag(SPEI , n = 1, default = NA), 
                diff_SPI = SPI - dplyr::lag(SPI , n = 1, default = NA), 
                diff_RX = RX - dplyr::lag( RX, n = 1, default = NA), 
                diff_PEXT = PEXT - dplyr::lag( PEXT, n = 1, default = NA), 
                diff_WD = WD - dplyr::lag( WD, n = 1, default = NA)
  )%>% 
  ungroup()

varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI")
modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI")

for (i in 1:length(varns)){
  climate_projections[paste(modns[i],'_i_',varns[i],sep='')] <- climate_projections[modns[i]] * climate_projections[varns[i]]
} 

for (i in 1:length(varns)){ # 
  climate_projections[paste("TM",'_i_',varns[i],sep='')] <- climate_projections["TM"] * climate_projections[varns[i]]
} 

for (i in 1:length(varns)){ # 
  climate_projections[paste("RR",'_i_',varns[i],sep='')] <- climate_projections["RR"] * climate_projections[varns[i]]
} 

for (i in 1:length(varns)){ # 
  climate_projections[paste("TM",'_i_',modns[i],sep='')] <- climate_projections["TM"] * climate_projections[varns[i]]
} 

for (i in 1:length(varns)){ # 
  climate_projections[paste("RR",'_i_',modns[i],sep='')] <- climate_projections["RR"] * climate_projections[varns[i]]
} 


### socio econ projections 

data_dir<-"data/hdi_data/projections"

life_exp_birth <- read_delim(file.path(data_dir, "life_exp_birth_2020_2100_all_ssp_male_female.csv"), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
life_exp_birth<-life_exp_birth[-which(life_exp_birth$Scenario=="SSP1"),]
life_exp_birth_ssp1 <- read_delim(file.path(data_dir, "life_exp_birth_1950_2100_ssp1_male_female.csv"), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

mean_years_sch_25 <- read_delim(file.path(data_dir, "mean_years_schooling_over25_2020_2100_all_ssp.csv"), 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

################################################################################
### preproc life exp 

# mean leb male female 
life_exp_birth_male<-life_exp_birth[which(life_exp_birth$Sex=="Male"),]
colnames(life_exp_birth_male)[which(colnames(life_exp_birth_male)=="Years")]="Years_male"
life_exp_birth_female<-life_exp_birth[which(life_exp_birth$Sex=="Female"),]
colnames(life_exp_birth_female)[which(colnames(life_exp_birth_female)=="Years")]="Years_female"
life_exp_birth_male$Sex<-life_exp_birth_female$Sex<-NULL
life_exp_birth<-inner_join(life_exp_birth_male,life_exp_birth_female)
life_exp_birth$Years<-(life_exp_birth$Years_male+life_exp_birth$Years_female)/2


# mean leb male female ssp1
life_exp_birth_ssp1_male<-life_exp_birth_ssp1[which(life_exp_birth_ssp1$Sex=="Male"),]
colnames(life_exp_birth_ssp1_male)[which(colnames(life_exp_birth_ssp1_male)=="Years")]="Years_male"
life_exp_birth_ssp1_female<-life_exp_birth_ssp1[which(life_exp_birth_ssp1$Sex=="Female"),]
colnames(life_exp_birth_ssp1_female)[which(colnames(life_exp_birth_ssp1_female)=="Years")]="Years_female"
life_exp_birth_ssp1_male$Sex<-life_exp_birth_ssp1_female$Sex<-NULL
life_exp_birth_ssp1<-inner_join(life_exp_birth_ssp1_male,life_exp_birth_ssp1_female)
life_exp_birth_ssp1$Years<-(life_exp_birth_ssp1$Years_male+life_exp_birth_ssp1$Years_female)/2
life_exp_birth_ssp1$Scenario<-"SSP1"

# unify ssp1 
life_exp_birth<-rbind(life_exp_birth , life_exp_birth_ssp1)

# add iso
colnames(life_exp_birth)[which(colnames(life_exp_birth)=="Area")]="NAME_ENGL"
names_leb<-unique(life_exp_birth$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names_leb, names_countries)

library(stringr)
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Bolivia (Plurinational State of)", "Bolivia")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Brunei Darussalam", "Brunei")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Cote d'Ivoire", "Côte D’Ivoire")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Czech Republic", "Czechia")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Democratic People's Republic of Korea", "North Korea")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Democratic Republic of the Congo", "Democratic Republic of The Congo")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Hong Kong Special Administrative Region of China", "Hong Kong")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Iran (Islamic Republic of)", "Iran")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Lao People's Democratic Republic", "Laos")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Libyan Arab Jamahiriya", "Libya")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Macao Special Administrative Region of China", "Macau")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Micronesia (Federated States of)", "Micronesia")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Myanmar", "Myanmar/Burma")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Occupied Palestinian Territory", "Palestine")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Republic of Korea", "South Korea")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Republic of Moldova", "Moldova")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Saint Vincent and the Grenadines", "Saint Vincent and The Grenadines")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Sao Tome and Principe", "São Tomé and Príncipe")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Swaziland", "Eswatini")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Syrian Arab Republic", "Syria")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Taiwan Province of China", "Taiwan")  # if you consider Taiwan separate
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "The former Yugoslav Republic of Macedonia", "North Macedonia")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "United States Virgin Islands", "Us Virgin Islands")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "United States of America", "United States")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Venezuela (Bolivarian Republic of)", "Venezuela")
life_exp_birth$NAME_ENGL <- str_replace(life_exp_birth$NAME_ENGL, "Viet Nam", "Vietnam")
names_leb<-unique(life_exp_birth$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names_leb, names_countries)

life_exp_birth<-inner_join(life_exp_birth , countries)
colnames(life_exp_birth)[which(colnames(life_exp_birth)=="ISO3_CODE")]="iso3"


# assumption: time frames refer to last year
life_exp_birth$Period<-substr(life_exp_birth$Period, 6, 9)
life_exp_birth$year<-as.numeric(life_exp_birth$Period)

# turn to 0-1 index (and then 0-100), with old min max values 
min<-20
max<-85
life_exp_birth$health_index<-((life_exp_birth$Years-min)/(max-min))*100


ggplot(life_exp_birth[which(life_exp_birth$iso3=="BRA"),], aes(x = year, y = health_index, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years
life_exp_birth<-life_exp_birth[, c("Scenario", "iso3", "year", "health_index")]
life_exp_birth_interp<-data.frame()

# interpolate in years 
for (s in unique(life_exp_birth$Scenario)){
  for (c in unique(life_exp_birth$iso3)){
    
    temp<-life_exp_birth[which(life_exp_birth$Scenario == s & life_exp_birth$iso3 == c  ), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$health_index, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$health_index_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    life_exp_birth_interp<-rbind(life_exp_birth_interp,filler )
    }
  
}

life_exp_birth_interp<-life_exp_birth_interp %>% 
  arrange(Scenario, iso3, year) %>% 
  group_by(Scenario, iso3) %>% 
  mutate(diff_health_index_interp = health_index_interp - dplyr::lag(health_index_interp, n = 1, default = NA))


life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP1")]<-"ssp126" # or another, should duplicate 
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP2")]<-"ssp245"
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP3")]<-"ssp370"
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP5")]<-"ssp585" # or another, should duplicate 

life_exp_birth_interp<-life_exp_birth_interp %>% filter(Scenario %in% ssp_names)

ggplot(life_exp_birth_interp, aes(x = year, y = health_index_interp, color = Scenario)) +
  geom_line(size = 1)

#colnames(life_exp_birth_interp)[which(colnames(life_exp_birth_interp)=="Scenario")]="ssp"

### 

# coefficients damage function
coefs<-c(diff_HW = -3.71062*10e-3 , TM_i_diff_HW= 1.17602*10e-04, 
         diff_WD = 3.88061*10e-3	, TM_i_diff_WD = -2.14887*10e-04, 
         diff_SPI = -9.82927*10e-3, SPI_i_diff_SPI = 7.08045*10e-04,
         diff_TVAR = 1.98799*10e-1, TM_i_diff_TVAR = -2.97817*10e-2, 
         
         diff_TM = 8.10394*10e-2	 , TM_i_diff_TM = -4.32231*10e-03	, 
         diff_RR =  -2.08075*10e-04, RR_i_diff_RR = 2.76769*10e-08	
         )

vars<-names(coefs)

expand_data<-expand.grid( year= unique(life_exp_birth_interp$year), 
                          iso3= unique(life_exp_birth_interp$iso3), 
                          Scenario = unique(life_exp_birth_interp$Scenario), 
                          Model = unique(models_names))
life_exp_birth_interp<-inner_join(life_exp_birth_interp, expand_data)

life_exp_birth_interp$diff_health_index_proj<-NA

for (m in models_names){
  for (ssp in ssp_names){
    
    for ( iso in  unique(life_exp_birth_interp$iso3)){
      for (y in  unique(life_exp_birth_interp$year)){
        
        life_exp_birth_interp$diff_health_index_proj[ which(life_exp_birth_interp$year == y & life_exp_birth_interp$iso3 == iso & life_exp_birth_interp$Scenario == ssp & life_exp_birth_interp$Model==m )] <-
          sum(coefs * climate_projections[which(climate_projections$year==y & climate_projections$iso3==iso & climate_projections$model==m & climate_projections$ssp==ssp), vars] )
        
      }
    }
    
  }
}

write.csv(life_exp_birth_interp, file = file.path(out_dir, "projections_health_index_try.csv"), row.names = FALSE)


################################################################################

projections_health_index_try <- read_csv("output/projections/try/projections_health_index_try.csv")

# set values in 2020 to 0
y<-min(unique(projections_health_index_try$year))
projections_health_index_try[which(projections_health_index_try$year==y), c("diff_health_index_interp", "diff_health_index_proj")]<-0

projections_health_index_try<-projections_health_index_try %>% 
  filter(Scenario %in% ssp_names)%>% 
  arrange(Model, Scenario, iso3, year)%>% 
  group_by(Model, Scenario, iso3) %>%
  mutate(
    cumulative_effect = cumsum(lag(diff_health_index_interp + diff_health_index_proj)),
    cc_proj_health_index = if_else(year == y, health_index_interp ,
                                        # For years after 2020, add the cumulative sum of (baseline_diff + damage) to the initial index
                                        first(health_index_interp) + cumulative_effect ) ) %>%
  ungroup()

library(dplyr)

# # Sample data frame to try
# data <- data.frame(
#   year = rep(2020:2025, times = 2),
#   country = c(rep("A", 12)),
#   scenario = c(rep("S1", 6), rep("S2", 6)),
#   model = c(rep("M1", 6), rep("M2", 6)),
#   index = c(100, 110, 120, 130, 140, 150, 100, 120, 140, 160, 180, 200),  # Assume initial index in 2020
#   damage = c(-1, -2, -3, -1, -4, -2, -2, -3, -1,-2, -3, -1),
#   baseline_diff = c(NA, 10,10,10,10,10,NA,20,20,20,20,20)
# )
# data[which(data$year==2020), c("damage", "baseline_diff")]<-0
# 
# # Calculation
# data <- data %>%
#   arrange(country, scenario, model, year) %>%
#   group_by(country, scenario, model) %>%
#   mutate(
#     cumulative_effect = cumsum(lag(baseline_diff + damage)),
#     new_index = if_else(year == 2020, index, first(index) + cumulative_effect)
#   ) %>%
#   ungroup()

library(ggplot2)

library(RColorBrewer)
# 1. Filter data for 5 random countries
set.seed(52)  # Set seed for reproducibility
selected_countries <- c("NOR", "RUS","CAN", 
                        "BRA", "ECU", "DZA", 
                        "AFG", "ZAF", "NGA")

# Filter data for these countries
plot_data <- projections_health_index_try %>%
  filter(iso3 %in% selected_countries)

plot_data_baseline=unique(plot_data[, c("year", "iso3", "health_index_interp", "Scenario")])
plot_data_cc=unique(plot_data[, c("year", "iso3",  "Scenario", "Model", "cc_proj_health_index")])


ggplot() +
  geom_line( data=plot_data_baseline, aes(x = year, y = health_index_interp, color = Scenario)) + 
  geom_point( data=plot_data_cc,  aes(x = year, y = cc_proj_health_index, color = Scenario)) + 
  facet_wrap(~ iso3, scales = "free_y") +
  labs(
    title = "Index Projections Over Time with Climate Change Impact",
    x = "Year",
    y = "Index Value",
    color = "Model-Scenario",
    linetype = "Projection Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 






# 2. Prepare the data for ggplot by reshaping to long format if needed
# (Assumes you have both baseline and climate change projections in separate columns)
plot_data <- plot_data %>%
  pivot_longer(cols = c(cc_proj_health_index, health_index_interp), 
               names_to = "projection_type", 
               values_to = "index_value") %>%
  mutate(projection_type = if_else(projection_type == "health_index_interp", 
                                   "Baseline", 
                                   "Climate Change Projection"),
         model_scenario = paste(Model, Scenario, sep = " - ") )

# Automatically generate a color palette for each scenario
# Get unique scenarios
unique_scenarios <- unique(plot_data$Scenario)

# Generate colors for each scenario using RColorBrewer (or choose another palette as needed)
# This uses the "Set1" palette; adjust the palette and number as needed
palette <- brewer.pal(min(length(unique_scenarios), 9), "Set1")

# Map colors to scenarios with similar shades
scenario_colors <- setNames(palette, unique_scenarios)

# Generate color mapping for each model-scenario combination
model_scenario_colors <- plot_data %>%
  distinct(model_scenario, Scenario) %>%
  mutate(color = scenario_colors[Scenario]) 

names<-model_scenario_colors$model_scenario
model_scenario_colors<-model_scenario_colors$color
names(model_scenario_colors)<-names



# Plot with ggplot2
ggplot(plot_data, aes(x = year, y = index_value, color = model_scenario, linetype = projection_type)) +
  geom_line(size = 1) +
  facet_wrap(~ iso3, scales = "free_y") +
  labs(
    title = "Index Projections Over Time with Climate Change Impact",
    x = "Year",
    y = "Index Value",
    color = "Model-Scenario",
    linetype = "Projection Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = model_scenario_colors)
