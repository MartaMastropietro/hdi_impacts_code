
rm(list=ls())
gc()
source("utils/libraries.R")

data_dir<-"data/climate_data/era5"
load(file.path(data_dir, "climate_data_1950_2023.RData"))

years<-1950:2023

# aggregation type
var_agg <-data.frame(var=  names(all_rast), 
                     agg_type= "mean" )

# Import data population
pop_data_dir<- "data/data_population/pop_density_hist"
pop_2015<-terra::rast(file.path(pop_data_dir,"gpw_v4_population_density_rev11_2015_30_sec.tif"))
pop_2015 <- subst(pop_2015, NA, 0) 
pop_2015 <- raster::raster(pop_2015)
# crs adjust
crs(pop_2015)  = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
#plot(pop_2015)

################################################################################

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
crs(countries)
#plot(countries)

### Sub-national shape file (hdi+components data)
gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 
crs(gdl_sub_nations)
#plot(gdl_sub_nations)


### Weight by pop
areas=area(all_rast[[1]][[1]]) #they are all the same, lets take the first
plot(areas)
weight = resample(pop_2015, areas, method='bilinear')


################################################################################

data_country <- data.frame(expand.grid(year= years, iso3= countries$ISO3_CODE))
data_gdl <- data.frame(expand.grid(year= years, gdlcode= gdl_shape_file$gdlcode))

### extract for each var 
for (var in var_agg$var){
    
  gc()
  
    data<-brick(all_rast[[paste0(var)]]) #temporary data with raster of corresp variable , all years
    
    areas=area(data)
    
    if (var_agg$agg_type[var_agg$var==var]=="mean"){
      #countries
      agg_country = exact_extract(data, countries, fun = "weighted_mean", weights = areas*weight)
      agg_country$ISO3_CODE<-countries$ISO3_CODE
      colnames(agg_country)<-as.character(c(years, "iso3"))
      agg_country<-reshape2::melt(agg_country, id.vars=c("iso3"))
      colnames(agg_country)<-c("iso3", "year", var)
      agg_country$year<-as.numeric(as.character(agg_country$year))
      length(unique(agg_country$iso3)) # 257
      
      #gdl
      agg_gdl = exact_extract(data, gdl_sub_nations, fun = "weighted_mean", weights = areas*weight)
      agg_gdl$gdlcode<-gdl_sub_nations$gdlcode
      colnames(agg_gdl)<-as.character(c(years, "gdlcode"))
      agg_gdl<-reshape2::melt(agg_gdl, id.vars=c("gdlcode"))
      colnames(agg_gdl)<-c("gdlcode", "year", var)
      agg_gdl$year<-as.numeric(as.character(agg_gdl$year))
      length(unique(agg_gdl$gdlcode)) # 1798
    }
  
  ### complete dataframes of regional agg with new columns
  data_country <- dplyr::inner_join(data_country, agg_country)
  data_gdl <- dplyr::inner_join(data_gdl, agg_gdl)
  
  
}
################################################################################

### save all dataframes 
write.csv(data_country, file = file.path(data_dir, "data_climate_country_pop_weight_1950_2023.csv"), row.names = FALSE)
write.csv(data_gdl, file = file.path(data_dir, "data_climate_gdl_pop_weight_1950_2023.csv"), row.names = FALSE)


################################################################################
################################################################################
################################################################################




data_dir<-"data/climate_data/era5"

## check missing places
data_climate_country_1950_2023 <- read_csv("data/climate_data/era5/data_climate_country_pop_weight_1950_2023.csv")
no_data<-data_climate_country_1950_2023%>%group_by(iso3)%>%filter(TM==0 & RR==0)
countries$no_data<-0
countries$no_data[which(countries$ISO3_CODE %in% no_data$iso3)]<-1
#plot(countries[,c(2,3)]) #invisible :)

#plot(countries[which(countries$no_data==1),c(2,3)])

# save data with new countries 
countries_data<-setdiff(unique(countries$ISO3_CODE),unique(countries$ISO3_CODE[countries$no_data==1]))

data_climate_country_1950_2023<-data_climate_country_1950_2023%>%filter(iso3 %in% countries_data )
19018-15392
74*(257-208) #ok
write.csv(data_climate_country_1950_2023, file = file.path(data_dir, "data_climate_country_pop_weight_1950_2023_updated.csv"), row.names = FALSE)




## check missing places
data_climate_gdl_1950_2023 <- read_csv("data/climate_data/era5/data_climate_gdl_pop_weight_1950_2023.csv")
no_data<-data_climate_gdl_1950_2023%>%group_by(gdlcode)%>%filter(TM==0 & RR==0)
gdl_sub_nations$no_data<-0
gdl_sub_nations$no_data[which(gdl_sub_nations$gdlcode %in% no_data$gdlcode)]<-1
#plot(gdl_sub_nations[,c(2,3)]) #invisible :)
gdl_no_data<-unique(gdl_sub_nations$gdlcode[gdl_sub_nations$no_data==1])
gdl_data<-setdiff(unique(gdl_sub_nations$gdlcode),gdl_no_data)

plot(gdl_sub_nations[which(gdl_sub_nations$no_data==1),c(2,3)])

# save data with new regions 
data_climate_gdl_1950_2023<-data_climate_gdl_1950_2023%>%filter(gdlcode %in% gdl_data)
write.csv(data_climate_gdl_1950_2023, file = file.path(data_dir, "data_climate_gdl_pop_weight_1950_2023_updated.csv"), row.names = FALSE)
