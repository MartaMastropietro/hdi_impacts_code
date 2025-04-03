
rm(list=ls())

source("utils/libraries.R")

### data hdi, shape file
### explore data 

data_pop_weight_less_na <- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_pop_weight <- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020.csv")

out_dir<-"output/eda"
if(!dir.exists(out_dir)){
  dir.create(out_dir)
}

###
data_pop_weight_less_na$iso3<-as.factor(data_pop_weight_less_na$iso3)
data_pop_weight_less_na$Level<-as.factor(data_pop_weight_less_na$Level)
data_pop_weight_less_na$gdlcode<-as.factor(data_pop_weight_less_na$gdlcode)

imputed = data_pop_weight_less_na[, c(1,18,19,21,23,24)]

# imp<-mice::mice(imputed, m = 3, method = 'pmm')
# save(data_pop_weight_less_na, imp, file=file.path(out_dir, "imputed_responses.RData"))
load("output/eda/imputed_responses.RData")

complete<-complete(imp, "long", include = TRUE)

table(complete$.imp)

complete$eys.NA <- mice::cci(complete$eys)
head(complete[, c("eys", "eys.NA")])

library(ggplot2)
ggplot(complete,
       aes(x = .imp, y = eys, color = eys.NA)) + 
  geom_jitter(show.legend = FALSE,
              width = .1)

try_imp<-complete[which(complete$.imp==2), 3:8]
sum(is.na(try_imp$eys)) # ok

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
crs(countries)

colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-c("iso3")

#################################################################################

library(plm)
plm_data<-pdata.frame(data_pop_weight_less_na, index=c("gdlcode", "year"))
colnames(plm_data)

# gnipc stationary, especially in growth, in level some cases not (like when no trends, intercepts allowed)
purtest(lgnipc~trend, data=plm_data, pmax = 5,  test = "madwu")
purtest(lgnipc~1, data=plm_data, pmax = 5,  test = "madwu")
purtest(lgnipc~0, data=plm_data, pmax = 7,  test = "madwu")

purtest(gr_gnipc~trend, data=plm_data, pmax = 6,  test = "madwu")
purtest(gr_gnipc~1, data=plm_data, pmax = 6,  test = "madwu")
purtest(gr_gnipc~0, data=plm_data, pmax = 7,  test = "madwu")

purtest(gr_gnipc~trend, data=plm_data, pmax = 6,  test = "Pm")
purtest(gr_gnipc~1, data=plm_data, pmax = 6,  test = "Pm")
purtest(gr_gnipc~0, data=plm_data, pmax = 7,  test = "Pm")

##

purtest(TM~trend, data=plm_data, pmax = 8,  test = "madwu")
purtest(TM~1, data=plm_data, pmax = 8,  test = "madwu")
purtest(TM~0, data=plm_data, pmax = 8,  test = "madwu")

purtest(TM~trend, data=plm_data, pmax = 8,  test = "Pm")
purtest(TM~1, data=plm_data, pmax = 8,  test = "Pm")
purtest(TM~0, data=plm_data, pmax = 8,  test = "Pm")


purtest(SPI~trend, data=plm_data, lags='AIC',   test = "madwu")
purtest(SPI~1, data=plm_data, lags='AIC',   test = "madwu")
purtest(SPI~0, data=plm_data, lags='AIC',   test = "madwu")

purtest(WD~trend, data=plm_data, lags='AIC',   test = "madwu")
purtest(WD~1, data=plm_data, lags='AIC',   test = "madwu")
purtest(WD~0, data=plm_data, lags='AIC',   test = "madwu")

purtest(TVAR~trend, data=plm_data, lags='AIC',   test = "madwu")
purtest(TVAR~1, data=plm_data, lags='AIC',   test = "madwu")
purtest(TVAR~0, data=plm_data, lags='AIC',   test = "madwu")

purtest(RX~trend, data=plm_data, lags='AIC',   test = "madwu")
purtest(RX~1, data=plm_data, lags='AIC',   test = "madwu")
purtest(RX~0, data=plm_data, lags='AIC',   test = "madwu")

purtest(HW~trend, data=plm_data, lags='AIC',   test = "madwu")
purtest(HW~1, data=plm_data, lags='AIC',   test = "madwu")
purtest(HW~0, data=plm_data, lags='AIC',   test = "madwu")


### on imputed data

try_imp<-try_imp %>% group_by(gdlcode)%>% arrange(gdlcode, year) %>% mutate(
  gr_gnipc=lgnipc-dplyr::lag(lgnipc, n=1, default=NA),
  gr_leb=log(leb)-dplyr::lag(log(leb), n=1, default=NA),
  gr_mys=log(mys)-dplyr::lag(log(mys), n=1, default=NA),
  gr_eys=log(eys)-dplyr::lag(log(eys), n=1, default=NA)
)


# plm_data_imp<-plm_data_imp[-which(plm_data_imp$year==1990),]
# plm_data_imp$leb<-as.numeric(plm_data_imp$leb)

# erase gdlcodes with stationary values
try_imp<-try_imp %>% 
  group_by(gdlcode) %>% 
  mutate(sum_gr_leb=sum(gr_leb, na.rm=TRUE), 
         sum_gr_mys=sum(gr_mys, na.rm=TRUE), 
         sum_gr_eys=sum(gr_eys, na.rm=TRUE), 
         sum_gr_gnipc=sum(gr_gnipc, na.rm=TRUE), )

sum_leb<-unique(try_imp[, c("gdlcode", "sum_gr_leb")])
codes_leb_erase<-as.character(unique(sum_leb$gdlcode[which(sum_leb$sum_gr_leb==0)]))

sum_eys<-unique(try_imp[, c("gdlcode", "sum_gr_eys")])
codes_eys_erase<-as.character(unique(sum_eys$gdlcode[which(sum_eys$sum_gr_eys==0)]))

sum_mys<-unique(try_imp[, c("gdlcode", "sum_gr_mys")])
codes_mys_erase<-as.character(unique(sum_mys$gdlcode[which(sum_mys$sum_gr_mys==0)]))

sum_gnipc<-unique(try_imp[, c("gdlcode", "sum_gr_gnipc")])
codes_gnipc_erase<-as.character(unique(sum_gnipc$gdlcode[which(sum_gnipc$sum_gr_gnipc==0)]))

codes_erase=c(codes_leb_erase,codes_eys_erase,codes_mys_erase, codes_gnipc_erase  )

try_imp<-try_imp[-which(try_imp$gdlcode %in% codes_erase), ]

plm_data_imp<-pdata.frame(try_imp, index=c("gdlcode", "year"))

# gnipc stationary, especially in growth, in level some cases not (like when no trends, intercepts only allowed)
# same for others. use growth for being safe

purtest(lgnipc~trend, data=plm_data_imp, lags='AIC',  test = "madwu")
purtest(lgnipc~1, data=plm_data_imp, lags='AIC',  test = "madwu")
purtest(lgnipc~0, data=plm_data_imp, lags='AIC',  test = "madwu")


purtest(gr_gnipc~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_gnipc~1, data=plm_data_imp,lags='AIC', test = "Pm")
purtest(gr_gnipc~0, data=plm_data_imp, lags='AIC', test = "Pm")


purtest(leb~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(leb~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(leb~0, data=plm_data_imp, lags='AIC', test = "Pm")

purtest(gr_leb~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_leb~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_leb~0, data=plm_data_imp, lags='AIC', test = "Pm")


purtest(mys~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(mys~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(mys~0, data=plm_data_imp, lags='AIC', test = "Pm")

purtest(gr_mys~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_mys~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_mys~0, data=plm_data_imp, lags='AIC', test = "Pm")


purtest(mys~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(mys~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(mys~0, data=plm_data_imp, lags='AIC', test = "Pm")

purtest(gr_mys~trend, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_mys~1, data=plm_data_imp, lags='AIC',  test = "Pm")
purtest(gr_mys~0, data=plm_data_imp, lags='AIC', test = "Pm")



# 

library(plm)
library(lmtest)  # For autocorrelation tests

# Fixed effects model (in levels)
fe_model <- plm(lgnipc ~ TM+ RR + as.factor(iso3)*year, data = plm_data, model = "within", effect="twoways", index = c("gdlcode", "year"))
summary(fe_model)

# Test for serial correlation
pbgtest(fe_model)  # Wooldridge or Breusch-Godfrey test for panel data

