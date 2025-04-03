### unification of historical, proj data for emulator 

library(readr)
library(dplyr)
library(tidyr)

projection_data_climate_country_pop_2020_weight_2015_2100 <- read_csv("data/climate_data/projections/projection_data_climate_country_pop_2020_weight_2015_2100.csv")
projection_data_climate_country_2015_2100 <- read_csv("data/climate_data/projections/projection_data_climate_country_2015_2100.csv")

data_climate_country_1950_2023_updated <- read_csv("data/climate_data/era5/data_climate_country_1950_2023_updated.csv")
data_climate_country_pop_weight_1950_2023_updated <- read_csv("data/climate_data/era5/data_climate_country_pop_weight_1950_2023_updated.csv")

length(unique(data_climate_country_1950_2023_updated$iso3))
length(unique(data_climate_country_pop_weight_1950_2023_updated$iso3))

setdiff(unique(data_climate_country_pop_weight_1950_2023_updated$iso3), unique(data_climate_country_1950_2023_updated$iso3))
setdiff( unique(data_climate_country_1950_2023_updated$iso3), unique(data_climate_country_pop_weight_1950_2023_updated$iso3))

data_climate_country_1950_2023_updated[data_climate_country_1950_2023_updated$iso3=="ATA", ]<-NA
data_climate_country_pop_weight_1950_2023_updated<-data_climate_country_pop_weight_1950_2023_updated%>%drop_na()
data_climate_country_1950_2023_updated<-data_climate_country_1950_2023_updated%>%drop_na()

data_climate_country_pop_weight_1950_2023_updated$model<-"hist"
data_climate_country_pop_weight_1950_2023_updated$ssp<-"hist"

data_climate_country_1950_2023_updated$model<-"hist"
data_climate_country_1950_2023_updated$ssp<-"hist"

data_climate_country_pop_weight_1950_2023_updated<-data_climate_country_pop_weight_1950_2023_updated[,!names(data_climate_country_pop_weight_1950_2023_updated) %in% c("CW", "PET", "SNX", "TN", "TX")]
data_climate_country_1950_2023_updated<-data_climate_country_1950_2023_updated[,!names(data_climate_country_1950_2023_updated) %in% c("CW", "PET", "SNX", "TN", "TX")]

projection_and_hist_data_climate_country_pop_2020_weight_2015_2100<-bind_rows(projection_data_climate_country_pop_2020_weight_2015_2100, data_climate_country_pop_weight_1950_2023_updated)
projection_and_hist_data_climate_country_2015_2100<-bind_rows(projection_data_climate_country_2015_2100 , data_climate_country_1950_2023_updated  )


data_dir<-"data/climate_data/projections"

### save all dataframes 
write.csv(projection_and_hist_data_climate_country_pop_2020_weight_2015_2100, file = file.path(data_dir, "projection_and_hist_data_climate_country_pop_2020_weight_2015_2100.csv"), row.names = FALSE)
write.csv(projection_and_hist_data_climate_country_2015_2100, file = file.path(data_dir, "projection_and_hist_data_climate_country_2015_2100.csv"), row.names = FALSE)
