
rm(list=ls())
source("utils/libraries.R")


out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/try_original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,4)]
countries$geometry<-NULL

### socio econ projections preproc

data_dir<-"data/hdi_data/projections"

var_name<-"leb"

life_exp_birth <- read_delim(file.path(data_dir, "life_exp_birth_2020_2100_all_ssp_male_female.csv"), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
life_exp_birth<-life_exp_birth[-which(life_exp_birth$Scenario=="SSP1"),]
life_exp_birth_ssp1 <- read_delim(file.path(data_dir, "life_exp_birth_1950_2100_ssp1_male_female.csv"), 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

population_data<-read_delim("data/hdi_data/projections/population_size_000s_all_ssp_2020_2100.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

################################################################################
### preproc pop data

# add iso
colnames(population_data)[which(colnames(population_data)=="Area")]="NAME_ENGL"
names<-unique(population_data$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names, names_countries)

population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Bolivia (Plurinational State of)", "Bolivia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Brunei Darussalam", "Brunei")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Cote d'Ivoire", "Côte D’Ivoire")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Czech Republic", "Czechia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Democratic People's Republic of Korea", "North Korea")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Democratic Republic of the Congo", "Democratic Republic of The Congo")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Hong Kong Special Administrative Region of China", "Hong Kong")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Iran (Islamic Republic of)", "Iran")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Lao People's Democratic Republic", "Laos")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Libyan Arab Jamahiriya", "Libya")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Macao Special Administrative Region of China", "Macau")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Micronesia (Federated States of)", "Micronesia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Myanmar", "Myanmar/Burma")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Occupied Palestinian Territory", "Palestine")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Republic of Korea", "South Korea")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Republic of Moldova", "Moldova")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Saint Vincent and the Grenadines", "Saint Vincent and The Grenadines")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Sao Tome and Principe", "São Tomé and Príncipe")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Swaziland", "Eswatini")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Syrian Arab Republic", "Syria")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Taiwan Province of China", "Taiwan")  # if you consider Taiwan separate
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "The former Yugoslav Republic of Macedonia", "North Macedonia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United States Virgin Islands", "Us Virgin Islands")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United States of America", "United States")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Venezuela (Bolivarian Republic of)", "Venezuela")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Viet Nam", "Vietnam")
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Venezuela (Bolivarian Republic of)")]<-"Venezuela"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Bolivia (Plurinational State of)")]<-"Bolivia"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Micronesia (Federated States of)")]<-"Micronesia"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Iran (Islamic Republic of)")]<-"Iran"
names<-unique(population_data$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names, names_countries)
setdiff(names_countries, names)

population_data<-inner_join(population_data , countries)
colnames(population_data)[which(colnames(population_data)=="ISO3_CODE")]="iso3"


# assumption: time frames refer to last year

population_data$year<-as.numeric(population_data$Year)
population_data$Year<-NULL

colnames(population_data)[which(colnames(population_data)=="Population")]="value"


ggplot(population_data[which(population_data$iso3=="BRA"),], aes(x = year, y = value, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years
population_data<-population_data[, c("Scenario", "iso3", "year", "value")]
population_data_interp<-data.frame()

# interpolate in years 
for (s in unique(population_data$Scenario)){
  for (c in unique(population_data$iso3)){
    
    temp<-population_data[which(population_data$Scenario == s & population_data$iso3 == c  ), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    population_data_interp<-rbind(population_data_interp,filler )
  }
  
}

population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP1")]<-"ssp126" # or another, should duplicate 
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP2")]<-"ssp245"
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP3")]<-"ssp370"
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP5")]<-"ssp585" # or another, should duplicate 

ggplot(population_data_interp[which(population_data_interp$iso3=="BRA"),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_point()

colnames(population_data_interp)[which(colnames(population_data_interp)=="value_interp")]<-"pop"


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

life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Bolivia (Plurinational State of)", "Bolivia")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Brunei Darussalam", "Brunei")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Cote d'Ivoire", "Côte D’Ivoire")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Czech Republic", "Czechia")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Democratic People's Republic of Korea", "North Korea")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Democratic Republic of the Congo", "Democratic Republic of The Congo")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Hong Kong Special Administrative Region of China", "Hong Kong")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Iran (Islamic Republic of)", "Iran")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Lao People's Democratic Republic", "Laos")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Libyan Arab Jamahiriya", "Libya")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Macao Special Administrative Region of China", "Macau")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Micronesia (Federated States of)", "Micronesia")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Myanmar", "Myanmar/Burma")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Occupied Palestinian Territory", "Palestine")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Republic of Korea", "South Korea")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Republic of Moldova", "Moldova")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Saint Vincent and the Grenadines", "Saint Vincent and The Grenadines")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Sao Tome and Principe", "São Tomé and Príncipe")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Swaziland", "Eswatini")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Syrian Arab Republic", "Syria")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Taiwan Province of China", "Taiwan")  # if you consider Taiwan separate
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "The former Yugoslav Republic of Macedonia", "North Macedonia")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "United States Virgin Islands", "Us Virgin Islands")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "United States of America", "United States")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Venezuela (Bolivarian Republic of)", "Venezuela")
life_exp_birth$NAME_ENGL <- stringr::str_replace(life_exp_birth$NAME_ENGL, "Viet Nam", "Vietnam")
life_exp_birth$NAME_ENGL[which(life_exp_birth$NAME_ENGL=="Venezuela (Bolivarian Republic of)")]<-"Venezuela"
life_exp_birth$NAME_ENGL[which(life_exp_birth$NAME_ENGL=="Bolivia (Plurinational State of)")]<-"Bolivia"
life_exp_birth$NAME_ENGL[which(life_exp_birth$NAME_ENGL=="Micronesia (Federated States of)")]<-"Micronesia"
names_leb<-unique(life_exp_birth$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names_leb, names_countries)
setdiff(names_countries, names_leb)

life_exp_birth<-inner_join(life_exp_birth , countries)
colnames(life_exp_birth)[which(colnames(life_exp_birth)=="ISO3_CODE")]="iso3"


# assumption: time frames refer to last year
life_exp_birth$Period<-substr(life_exp_birth$Period, 6, 9)
life_exp_birth$year<-as.numeric(life_exp_birth$Period)

colnames(life_exp_birth)[which(colnames(life_exp_birth)=="Years")]="value"


ggplot(life_exp_birth[which(life_exp_birth$iso3=="BRA"),], aes(x = year, y = value, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years
life_exp_birth<-life_exp_birth[, c("Scenario", "iso3", "year", "value")]
life_exp_birth_interp<-data.frame()

# interpolate in years 
for (s in unique(life_exp_birth$Scenario)){
  for (c in unique(life_exp_birth$iso3)){
    
    temp<-life_exp_birth[which(life_exp_birth$Scenario == s & life_exp_birth$iso3 == c  ), ]
    filler<-expand.grid(year=seq( 2025, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    life_exp_birth_interp<-rbind(life_exp_birth_interp,filler )
  }
  
}

life_exp_birth_interp<-life_exp_birth_interp %>% 
  arrange(Scenario, iso3, year) %>% 
  group_by(Scenario, iso3) %>% 
  mutate(gr_value_interp = log(value_interp) - log(dplyr::lag(value_interp, n = 1, default = NA)))


life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP1")]<-"ssp126" # or another, should duplicate 
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP2")]<-"ssp245"
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP3")]<-"ssp370"
life_exp_birth_interp$Scenario[which(life_exp_birth_interp$Scenario=="SSP5")]<-"ssp585" # or another, should duplicate 

ggplot(life_exp_birth_interp, aes(x = year, y = value_interp, color = Scenario)) +
  geom_point()


### add pop proj to data

life_exp_birth_interp<-inner_join(life_exp_birth_interp, population_data_interp)



###############################################################################
### project damages 


# coefficients damage function
coefs<-c(diff_HW = -8.265936*10e-6 , TM_i_diff_HW= 0, 
         diff_WD = 3.847182*10e-5	, TM_i_diff_WD = -2.809809*10e-06, 
         diff_SPI = -2.552806*10e-5, TM_i_diff_SPI = -3.517618*10e-06,
         diff_TVAR = 2.919521*10e-3, TM_i_diff_TVAR = -3.779435*10e-4, 
         
         diff_TM = 7.105190*10e-4	 , TM_i_diff_TM = -2.331020*10e-05	, 
         diff_RR =  -2.437781*10e-06, RR_i_diff_RR = 2.770000*10e-10	
)

vars<-names(coefs)

colnames(life_exp_birth_interp)[colnames(life_exp_birth_interp)=="Scenario"]<-"ssp"

expand_data<-expand.grid( year= unique(life_exp_birth_interp$year), 
                          iso3= unique(life_exp_birth_interp$iso3), 
                          ssp = unique(life_exp_birth_interp$ssp), 
                          model = unique(models_names))

life_exp_birth_interp<-inner_join(life_exp_birth_interp, expand_data)
life_exp_birth_interp<-inner_join(life_exp_birth_interp, climate_projections)

life_exp_birth_interp$gr_value_proj<-(as.matrix(life_exp_birth_interp[, vars]) %*% c(coefs) ) #rowSums(t(apply(as.matrix(climate_projections[, vars]), 1, function(x) coefs*x)),na.rm=T)
 

### save 
write.csv(life_exp_birth_interp, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)
