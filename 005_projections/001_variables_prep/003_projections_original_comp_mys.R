
rm(list=ls())
source("utils/libraries.R")


out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,4)]
countries$geometry<-NULL


### socio econ projections 

data_dir<-"data/hdi_data/projections"

var_name<-"mys"

mean_years_sch_25 <- read_delim(file.path(data_dir, "mean_years_schooling_over25_2020_2100_all_ssp.csv"), 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)


population_data_interp<-read_csv(file.path(out_dir, "preproc_proj_population.csv"))

################################################################################
### preproc mys +25

# add iso
colnames(mean_years_sch_25)[which(colnames(mean_years_sch_25)=="Area")]="NAME_ENGL"
names_leb<-unique(mean_years_sch_25$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names_leb, names_countries)

mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Bolivia (Plurinational State of)", "Bolivia")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Brunei Darussalam", "Brunei")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Cote d'Ivoire", "Côte D’Ivoire")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Czech Republic", "Czechia")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Democratic People's Republic of Korea", "North Korea")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Democratic Republic of the Congo", "Democratic Republic of The Congo")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Hong Kong Special Administrative Region of China", "Hong Kong")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Iran (Islamic Republic of)", "Iran")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Lao People's Democratic Republic", "Laos")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Libyan Arab Jamahiriya", "Libya")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Macao Special Administrative Region of China", "Macau")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Micronesia (Federated States of)", "Micronesia")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Myanmar", "Myanmar/Burma")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Occupied Palestinian Territory", "Palestine")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Republic of Korea", "South Korea")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Republic of Moldova", "Moldova")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Saint Vincent and the Grenadines", "Saint Vincent and The Grenadines")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Sao Tome and Principe", "São Tomé and Príncipe")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Swaziland", "Eswatini")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Syrian Arab Republic", "Syria")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Taiwan Province of China", "Taiwan")  # if you consider Taiwan separate
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "The former Yugoslav Republic of Macedonia", "North Macedonia")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "United States Virgin Islands", "Us Virgin Islands")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "United States of America", "United States")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Venezuela (Bolivarian Republic of)", "Venezuela")
mean_years_sch_25$NAME_ENGL <- stringr::str_replace(mean_years_sch_25$NAME_ENGL, "Viet Nam", "Vietnam")
mean_years_sch_25$NAME_ENGL[which(mean_years_sch_25$NAME_ENGL=="Venezuela (Bolivarian Republic of)")]<-"Venezuela"
mean_years_sch_25$NAME_ENGL[which(mean_years_sch_25$NAME_ENGL=="Bolivia (Plurinational State of)")]<-"Bolivia"
mean_years_sch_25$NAME_ENGL[which(mean_years_sch_25$NAME_ENGL=="Micronesia (Federated States of)")]<-"Micronesia"
mean_years_sch_25$NAME_ENGL[which(mean_years_sch_25$NAME_ENGL=="Iran (Islamic Republic of)")]<-"Iran"
names_leb<-unique(mean_years_sch_25$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names_leb, names_countries)
setdiff(names_countries, names_leb)

mean_years_sch_25<-inner_join(mean_years_sch_25 , countries)
colnames(mean_years_sch_25)[which(colnames(mean_years_sch_25)=="ISO3_CODE")]="iso3"



colnames(mean_years_sch_25)[which(colnames(mean_years_sch_25)=="Years")]="value"

colnames(mean_years_sch_25)[which(colnames(mean_years_sch_25)=="Year")]="year"


ggplot(mean_years_sch_25[which(mean_years_sch_25$iso3=="BRA"),], aes(x = year, y = value, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years
mean_years_sch_25<-mean_years_sch_25[, c("Scenario", "iso3", "year", "value")]
mean_years_sch_25_interp<-data.frame()

# interpolate in years 
for (s in unique(mean_years_sch_25$Scenario)){
  for (c in unique(mean_years_sch_25$iso3)){
    
    temp<-mean_years_sch_25[which(mean_years_sch_25$Scenario == s & mean_years_sch_25$iso3 == c  ), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    mean_years_sch_25_interp<-rbind(mean_years_sch_25_interp,filler )
  }
  
}

# scenario names
mean_years_sch_25_interp$Scenario[which(mean_years_sch_25_interp$Scenario=="SSP1")]<-"ssp126" # or another, should duplicate 
mean_years_sch_25_interp$Scenario[which(mean_years_sch_25_interp$Scenario=="SSP2")]<-"ssp245"
mean_years_sch_25_interp$Scenario[which(mean_years_sch_25_interp$Scenario=="SSP3")]<-"ssp370"
mean_years_sch_25_interp$Scenario[which(mean_years_sch_25_interp$Scenario=="SSP5")]<-"ssp585" # or another, should duplicate 

# add pop proj to data
mean_years_sch_25_interp<-inner_join(mean_years_sch_25_interp, population_data_interp)


### save 
write.csv(mean_years_sch_25_interp, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)
