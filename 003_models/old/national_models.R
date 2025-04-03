
rm(list=ls())

source("utils/libraries.R")

# output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/national_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### data

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
colnames(countries)[colnames(countries)=="ISO3_CODE"]<-"iso3"

### years of analysis
years<-1990:2020


data<- read_csv("output/data_hdi_climate_country_1990_2021.csv")
data_pop<- read_csv("output/data_hdi_climate_country_pop_weight_1990_2021.csv")
colnames(data)


### data climate and extremes
data_dir <- "data/climate_data/era5/"
data_cl <-  read_csv(file.path(data_dir, "data_climate_country_1950_2023_updated.csv"), show_col_types = FALSE)

### data national gdp gni from wb
data_wb<-  read_excel("data/gdp_data/P_Data_Extract_From_World_Development_Indicators/gdp_gni_national_1990_2023.xlsx")
# regions and world as well here, join with climate will solve

# prepoc, add climate
data_wb<-data_wb[, -2]
colnames(data_wb)<-c("var", "country","iso3", 
                     "1960", "1961", "1962", "1963", "1964", "1965", "1966","1967", "1968", "1969",
                     "1970", "1971", "1972", "1973", "1974", "1975", "1976","1977", "1978", "1979",
                     "1980", "1981", "1982", "1983", "1984", "1985", "1986","1987", "1988", "1989",
                     "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
            "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
            "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
            "2020", "2021", "2022", "2023")
data_wb<-data_wb %>% mutate(across(everything(), ~replace(., . == "..", NA)))


data_wb<-reshape2::melt(data_wb, id.vars = c("var", "country", "iso3"), variable.name = "year")
data_wb$year<-as.numeric(as.character(data_wb$year))
data_wb$value<-as.numeric(as.character(data_wb$value))
data_wb<-reshape(data = data_wb,
        idvar = c("iso3", "country", "year"),
        timevar =  "var",
        direction = "wide")
colnames(data_wb)<-c("country","iso3", "year", "gni_pc_ppp_curr", "gni_pc_2015", "gni_pc_ppp_2021", "gdp_pc_2015", "gdp_pc_ppp_2021")

###  cut years
data_wb<-data_wb[which(data_wb$year<=range(years)[2] & data_wb$year>=range(years)[1]), ]

### log and growth calc
data_wb <- data_wb %>% 
  dplyr::mutate(log_gni_pc_ppp_curr=log(gni_pc_ppp_curr))%>% 
  dplyr::mutate(log_gni_pc_2015 =log(gni_pc_2015))%>%
  dplyr::mutate(log_gni_pc_ppp_2021 =log(gni_pc_ppp_2021))%>% 
  dplyr::mutate(log_gdp_pc_2015 =log(gdp_pc_2015))%>%
  dplyr::mutate(log_gdp_pc_ppp_2021 =log(gdp_pc_ppp_2021))%>%
  ungroup()

data_wb <- data_wb %>% 
  arrange(iso3, year) %>% 
  group_by(iso3) %>%
  dplyr::mutate(gr_log_gni_pc_ppp_curr = log_gni_pc_ppp_curr - dplyr::lag(log_gni_pc_ppp_curr), n = 1, default = NA)%>% 
  dplyr::mutate(gr_log_gni_pc_2015  = log_gni_pc_2015 - dplyr::lag(log_gni_pc_2015), n = 1, default = NA)%>% 
  dplyr::mutate(gr_log_gni_pc_ppp_2021  = log_gni_pc_ppp_2021 - dplyr::lag(log_gni_pc_ppp_2021), n = 1, default = NA)%>% 
  dplyr::mutate(gr_log_gdp_pc_2015 = log_gdp_pc_2015 - dplyr::lag(log_gdp_pc_2015), n = 1, default = NA)%>% 
  dplyr::mutate(gr_log_gdp_pc_ppp_2021= log_gdp_pc_ppp_2021 - dplyr::lag(log_gdp_pc_ppp_2021), n = 1, default = NA)%>% 
  ungroup()

### unify 
data_wb_cl <- dplyr::inner_join(data_cl, data_wb) # if region not present in data, we dont add useless NA

data<-dplyr::inner_join(data_wb_cl, data)
# here log_gni_pc is the one from gdl 

cor(x=data$log_gni_pc, y=data$log_gni_pc_ppp_curr ,use="complete.obs")
cor(x=data$log_gni_pc, y=data$log_gni_pc_2015 ,use="complete.obs")
cor(x=data$log_gni_pc, y=data$log_gni_pc_ppp_2021 ,use="complete.obs")
cor(x=data$log_gni_pc, y=data$log_gdp_pc_2015 ,use="complete.obs")
cor(x=data$log_gni_pc, y=data$log_gdp_pc_ppp_2021 ,use="complete.obs")

plot(x=data$log_gni_pc, y=data$log_gni_pc_ppp_curr , col=factor(data$iso3))
plot(x=data$log_gni_pc, y=data$log_gni_pc_2015 ,col=factor(data$iso3))
plot(x=data$log_gni_pc, y=data$log_gni_pc_ppp_2021 ,col=factor(data$iso3))
plot(x=data$log_gni_pc, y=data$log_gdp_pc_2015 ,col=factor(data$iso3))
plot(x=data$log_gni_pc, y=data$log_gdp_pc_ppp_2021 ,col=factor(data$iso3))



### data inequality 
data_ineq<-arrow::read_parquet("data/gdp_data/decile_shares.parquet")
data_ineq$precip2<-(data_ineq$precip)^2


data_ineq<-data_ineq%>%
  arrange(iso3, year) %>% 
  group_by(iso3) %>%
  dplyr::mutate(lag_gdppc_growth = dplyr::lag(gdppc_growth, n = 1, default = NA))%>% 
  ungroup()

#bhm_adap_gdp <- plm(gdppc_growth ~ lag_gdppc_growth +
#                      temperature_mean + I(temperature_mean*l1_lgdppc) + 
#                      temperature_mean2 + I(temperature_mean2*l1_lgdppc) + 
#                      precip + I(precip*l1_lgdppc) + 
#                      precip2 + I(precip2*l1_lgdppc) +factor(iso3)*year
#                    ,  
#                    data = data_ineq, 
#                    index = c("iso3", "year"), 
#                    model = "within", 
#                    effect = "twoways")
#
#bhm_adap_gdp_simple <- plm(gdppc_growth ~ lag_gdppc_growth +
#                             temperature_mean + I(temperature_mean*l1_lgdppc) + 
#                             temperature_mean2 + I(temperature_mean2*l1_lgdppc) + 
#                             precip + I(precip*l1_lgdppc) + 
#                             precip2 + I(precip2*l1_lgdppc) 
#                           ,  
#                           data = data_ineq, 
#                           index = c("iso3", "year"), 
#                           model = "within", 
#                           effect = "twoways")
#
#summary(bhm_adap_gdp)
#summary(bhm_adap_gdp_simple)

data_ineq<-inner_join(data_cl, data_ineq)


################################################################################
### models
################################################################################

### id

pan_id<-c('iso3', 'year')

idx_reg<- "iso3"
idx_small<- "iso3 + year"
idx_med<- "iso3 + year + iso3[year] "
idx_big<- "iso3 + year + iso3[year]+iso3[year^2] "


##################################################

### permayen data preprocessed by fra, compare full vs reduced years 
out_var="gdppc_growth"  
f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var, "+") , "temperature_mean + I(temperature_mean*l1_lgdppc) + 
                             temperature_mean2 + I(temperature_mean2*l1_lgdppc) + 
                             precip + I(precip*l1_lgdppc) + 
                             precip2 + I(precip2*l1_lgdppc)   ", "|" , idx_med ))
mod_bhm_adap_full_old<-fixest::feols(f, data_ineq, panel.id=pan_id)
summary(mod_bhm_adap_full_old)

out_var="gdppc_growth"  
f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var, "+") , "temperature_mean + I(temperature_mean*l1_lgdppc) + 
                             temperature_mean2 + I(temperature_mean2*l1_lgdppc) + 
                             precip + I(precip*l1_lgdppc) + 
                             precip2 + I(precip2*l1_lgdppc)  ", "|" , idx_med ))
mod_bhm_adap_small_old<-fixest::feols(f, data_ineq[which(data_ineq$year<=2023 & data_ineq$year>=1990), ], panel.id=pan_id)
summary(mod_bhm_adap_small_old)

# with my vars
data_ineq$TM_2<-(data_ineq$TM)^2
data_ineq$RR_2<-(data_ineq$RR)^2

out_var="gdppc_growth"  
f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var, "+") , "TM + I(TM*l1_lgdppc) + 
                             TM_2 + I(TM_2*l1_lgdppc) + 
                             RR + I(RR*l1_lgdppc) + 
                             RR_2 + I(RR_2*l1_lgdppc)   ", "|" , idx_med ))
mod_bhm_adap_full_new<-fixest::feols(f, data_ineq, panel.id=pan_id)
summary(mod_bhm_adap_full_new)

out_var="gdppc_growth"  
f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var, "+") , "TM + I(TM*l1_lgdppc) + 
                             TM_2 + I(TM_2*l1_lgdppc) + 
                             RR + I(RR*l1_lgdppc) + 
                             RR_2 + I(RR_2*l1_lgdppc)   ", "|" , idx_med ))
mod_bhm_adap_small_new<-fixest::feols(f, data_ineq[which(data_ineq$year<=2023 & data_ineq$year>=1990), ], panel.id=pan_id)
summary(mod_bhm_adap_small_new)

models<-list(mod_bhm_adap_full_old, mod_bhm_adap_small_old, mod_bhm_adap_full_new, mod_bhm_adap_small_new)
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_gdp_bhm_adap_tp_years_comparison.html"))
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"))

#plot 
coefficients(mod_bhm_adap_full_new)

# only t
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_bhm_adap_full_new)["TM"] + (x^2)*coefficients(mod_bhm_adap_full_new)["TM_2"]
plot(x,y)

# t, median level income
mean_income<-as.numeric(summary(data_ineq$l1_lgdppc)[3])
exp(mean_income)
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*as.numeric(coefficients(mod_bhm_adap_full_new)["TM"]) + (x^2)*as.numeric(coefficients(mod_bhm_adap_full_new)["TM_2"]) + 
  x*mean_income*as.numeric(coefficients(mod_bhm_adap_full_new)["I(TM * l1_lgdppc)"]) + (x^2)*mean_income*as.numeric(coefficients(mod_bhm_adap_full_new)["I(TM_2 * l1_lgdppc)"]) 
plot(x,y)


######## plot, with conf intervals
# Extract coefficients from the model
coefficients <- coef(mod_bhm_adap_full_new)[2:5] #t coefs

# Extract the covariance matrix of the coefficients
cov_matrix <- vcov(mod_bhm_adap_full_new, )[2:5, 2:5] #t cov

fixed_l1_lgdppc <- as.numeric(summary(data_ineq$l1_lgdppc, na.rm = TRUE)[3])

# Generate a sequence of temperature values
TM_values <- seq(min(data_ineq$TM, na.rm = TRUE), max(data_ineq$TM, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(TM_values,                # TM
           TM_values * fixed_l1_lgdppc,  # TM * l1_lgdppc
           TM_values^2,              # TM_2
           (TM_values^2) * fixed_l1_lgdppc) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- c("TM",  "I(TM * l1_lgdppc)", "TM_2", "I(TM_2 * l1_lgdppc)")

predicted_gdppc_growth <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- predicted_gdppc_growth - 1.96 * standard_errors
upper_ci <- predicted_gdppc_growth + 1.96 * standard_errors

plot(TM_values, predicted_gdppc_growth, type = "l", col = "blue",
     ylim = range(c(lower_ci, upper_ci)),
     xlab = "Temperature (TM)", ylab = "Predicted GDP per Capita Growth",
     main = paste("Effect of Temperature on GDP Growth\n Fixed l1_lgdppc =", round(fixed_l1_lgdppc, 2)))

# Add the confidence intervals to the plot
lines(TM_values, lower_ci, col = "red", lty = 2)
lines(TM_values, upper_ci, col = "red", lty = 2)

################################################################################


### out var gni
out_var="gr_log_gni_pc"  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, vcov="DK")

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_gni_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_gni_burke_tp_dk.html"))


#plot 
coefficients(mod_3)
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_3)["TM"] + (x^2)*coefficients(mod_3)["TM_2"]
plot(x,y)




### out var gni, a different one
out_var="gr_log_gni_pc_2015 "  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, vcov="DK")
lines(x,seq(0,0,length.out=1000))


#plot 
coefficients(mod_3)
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_3)["TM"] + (x^2)*coefficients(mod_3)["TM_2"]
plot(x,y)


models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "rob_models_gni_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "rob_models_gni_burke_tp_dk.html"))

### out var gdp, a different one
out_var="gr_log_gdp_pc_2015 "  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, vcov="DK")
lines(x,seq(0,0,length.out=1000))

#plot 
coefficients(mod_3)
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_3)["TM"] + (x^2)*coefficients(mod_3)["TM_2"]
plot(x,y)


models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "rob_models_gdp_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "rob_models_gdp_burke_tp_dk.html"))


### out var gdp ppp
out_var="gr_log_gdp_pc_ppp_2021"

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, vcov="DK")
lines(x,seq(0,0,length.out=1000))

#plot 
coefficients(mod_3)
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_3)["TM"] + (x^2)*coefficients(mod_3)["TM_2"]
plot(x,y)


### out var hdi diff
out_var="diff_hdi"  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, vcov="DK")

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_hdi_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_hdi_burke_tp_dk.html"))

#plot 
coefficients(mod_3)[3]
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
#y<-x*coefficients(mod_3)[3] + (x^2)*coefficients(mod_3)[4]
y<-(x^2)*coefficients(mod_3)[4]
plot(x,y)


### out var income index diff
out_var="diff_income_index"  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, "DK")
models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_income_index_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_income_index_burke_tp_dk.html"))

#plot 
coefficients(mod_3)[3]
x<-seq(range(data$TM)[1],range(data$TM)[2], length.out=1000 )
y<-x*coefficients(mod_3)[3] + (x^2)*coefficients(mod_3)[4]
plot(x,y)



### out var health index diff
out_var="diff_health_index"  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)
summary(mod_3, "DK")

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_health_index_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_health_index_burke_tp_dk.html"))

# not significant



### out var edu index diff
out_var="diff_edu_index"  

# simple bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_edu_index_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_edu_index_burke_tp_dk.html"))

# not significant

################################################################################


### out var hdi diff on diff
out_var="diff_hdi"  

# simple bhm on diff
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_2)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_hdi_diff_cl_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_hdi_diff_cl_burke_tp_dk.html"))


### out var gni 
out_var="gr_log_gni_pc"  

# simple bhm on diff
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)


models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_gni_diff_cl_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_gni_diff_cl_burke_tp_dk.html"))


### out var health index 
out_var="diff_health_index"  

# simple bhm on diff
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)

models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_health_index_diff_cl_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_health_index_diff_cl_burke_tp_dk.html"))


### out var edu index 
out_var="diff_edu_index"  

# simple bhm on diff
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_med ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "diff_RR + diff_RR_2+ diff_TM+ diff_TM_2 + diff_TM:TM + diff_RR:RR ", "|" , idx_big ))
mod_3<-fixest::feols(f, data, panel.id=pan_id)
summary(mod_3)


models<-list(mod_0, mod_1, mod_2, mod_3)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_edu_index_diff_cl_burke_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_edu_index_diff_cl_burke_tp_dk.html"))


##################################################

### add extremes to burke 

fvcov_dk<-function(x) vcov(x, "DK")
#vcov_dk<- lapply(models, FUN=fvcov_dk)

#

models_hdi<-list()
out_var="diff_hdi"  

# bhm

f <- as.formula( paste(out_var ,"~", "RR + RR_2+  TM + TM_2", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod

# plot marginal t effect 
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-coefficients(mod)[3] + (2*x)*coefficients(mod)[4]
plot(x,y)
lines(x,seq(0,0,length.out=1000))

# plot t effect 
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-x*coefficients(mod)[3] + (x^2)*coefficients(mod)[4]
plot(x,y)
lines(x,seq(0,0,length.out=1000))

#

# try non linear
# bhm
f <- as.formula( paste(out_var ,"~", "RR + RR_2+  poly(TM, 4, raw = TRUE)", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod
# still only linear


# bhm
f <- as.formula( paste(out_var ,"~", " poly(RR, 4, raw = TRUE) + TM + TM_2", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod
# still no effect rr


#
colnames(data)


# bhm + adap
out_var="diff_income_index"
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 +  lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod

# plot t effect without adap
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-x*coefficients(mod)[3] + (x^2)*coefficients(mod)[4]
plot(x,y)
lines(x,seq(0,0,length.out=1000))

# plot t effect with adap, first quantile hdi
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-x*coefficients(mod)[3] + (x^2)*coefficients(mod)[4] + 
  (x*coefficients(mod)[7] + (x^2)*coefficients(mod)[8])*summary(data$hdi)[2] 
plot(x,y)
lines(x,seq(0,0,length.out=1000))

# plot t effect with adap, second quantile hdi
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-x*coefficients(mod)[3] + (x^2)*coefficients(mod)[4] + 
  (x*coefficients(mod)[7] + (x^2)*coefficients(mod)[8])*summary(data$hdi)[3] 
plot(x,y)
lines(x,seq(0,0,length.out=1000))

# plot t effect with adap, third quantile hdi
coefficients(mod)
x<-seq(range(data$TM, na.rm=TRUE)[1], range(data$TM, na.rm=TRUE)[2],length.out=1000 )
y<-x*coefficients(mod)[3] + (x^2)*coefficients(mod)[4] + 
  (x*coefficients(mod)[7] + (x^2)*coefficients(mod)[8])*summary(data$hdi)[5] 
plot(x,y)
lines(x,seq(0,0,length.out=1000))


#

### extremes

# bhm + SPI
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + SPI ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod


# bhm + SPI + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + SPI  +
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod
# tried also poly 4 for spi, no effect over the linear

f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + SPI  +
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 + lag_hdi:SPI", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")

#

# bhm + SPEI
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + SPEI ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod


# bhm + SPEI + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + SPEI  +
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
models_hdi[[length(models_hdi)+1]]<-mod

#

# bhm + TVAR
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + TVAR ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod


# bhm + TVAR + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + TVAR  +
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 + lag_hdi:TVAR  ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod

# significant only with adap

# 

# bhm + RX
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + RX + RX_2 ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod


# bhm + RX + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + RX + RX_2  +
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 + lag_hdi:RX + lag_hdi:RX_2    ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod

# significant only with adap

# 

# bhm + PET
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + PET  ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod


# bhm + PET + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + PET+
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 + lag_hdi:PET    ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod

# significant only with adap

# 

# bhm + WD
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + WD +WD_2 ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")
#models_hdi[[length(models_hdi)+1]]<-mod


# bhm + WD + adap
f <- as.formula( paste(out_var ,"~", "RR + RR_2 + TM + TM_2 + WD+
                       lag_hdi:RR +lag_hdi:RR_2+ lag_hdi:TM+ lag_hdi:TM_2 + lag_hdi:WD    ", "|" , idx_big ))
mod<-fixest::feols(f, data, panel.id=pan_id)
summary(mod)
summary(mod, vcov="DK")

# no sign


#################################################################################

### hdi , only spi
o <- "diff_hdi"
i <- "iso3 + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+ SPI +
      lag_hdi:TM + lag_hdi:TM_2 + lag_hdi:RR + lag_hdi:RR_2" # also here if we add adap to spi it becomes insignificant 
# f <- as.formula(paste( o, "~", paste0("lag_",o),"+", r, "|" ,i ))  # ar
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod <- fixest::feols(f, data, panel.id=pan_id)
#mod = final_models[[2]]
summary(mod) 
summary(mod, vcov = ~ iso3)
summary(mod, vcov = "DK")

coefs<-mod$coefficients

# fill data
mean_damages<- data.frame(iso3=unique(data$iso3), tm_mean_damage=NA, rr_mean_damage=NA, tm_sd_damage=NA, rr_sd_damage=NA, 
                          spi_mean_damage=NA, spi_sd_damage=NA ,
                          all_mean_damage=NA, all_sd_damage=NA
)

for (r in unique(data$iso3)){
  
  laghdi<-mean(data$lag_hdi[data$iso3 == r], na.rm = TRUE)
  
  
  ### mean on years
  tm<-mean(data$TM[data$iso3 == r], na.rm = TRUE)
  rr<-mean(data$RR[data$iso3 == r], na.rm = TRUE)
  spi<-mean(data$SPI[data$iso3 == r], na.rm = TRUE)
  
  
  # tm damages
  mean_damages[mean_damages$iso3==r, c("tm_mean_damage") ]<-
    tm*coefs["TM"] + (tm^2)*coefs["TM_2"] +
    tm*laghdi*coefs["TM:lag_hdi"] + (tm^2)*laghdi*coefs["TM_2:lag_hdi"]
  
  # rr damages
  mean_damages[mean_damages$iso3==r, c("rr_mean_damage") ]<-
    rr*coefs["RR"] +(rr^2)*coefs["RR_2"] +
    rr*laghdi*coefs["RR:lag_hdi"] +(rr^2)*laghdi*coefs["RR_2:lag_hdi"] 
  
  # spi damages
  mean_damages[mean_damages$iso3==r, c("spi_mean_damage") ]<-
    spi*coefs["SPI"] 
  
  
  
  # sum all
  mean_damages[mean_damages$iso3==r, c("all_mean_damage") ]<-
    mean_damages[mean_damages$iso3==r, c("tm_mean_damage") ]+
    mean_damages[mean_damages$iso3==r, c("rr_mean_damage") ]+
    mean_damages[mean_damages$iso3==r, c("spi_mean_damage") ]
  
  
  
  ### sd on years
  tm<-sd(data$TM[data$iso3 == r], na.rm = TRUE)
  rr<-sd(data$RR[data$iso3 == r], na.rm = TRUE)
  spi<-sd(data$SPI[data$iso3 == r], na.rm = TRUE)
  
  
  # tm damages
  mean_damages[mean_damages$iso3==r, c("tm_sd_damage") ]<-
    tm*coefs["TM"] + (tm^2)*coefs["TM_2"] +
    tm*laghdi*coefs["TM:lag_hdi"] + (tm^2)*laghdi*coefs["TM_2:lag_hdi"]
  
  # rr damages
  mean_damages[mean_damages$iso3==r, c("rr_sd_damage") ]<-
    rr*coefs["RR"] +(rr^2)*coefs["RR_2"] +
    rr*laghdi*coefs["RR:lag_hdi"] +(rr^2)*laghdi*coefs["RR_2:lag_hdi"] 
  
  # spi damages
  mean_damages[mean_damages$iso3==r, c("spi_sd_damage") ]<-
    spi*coefs["SPI"] 
  
  
  # sum all 
  mean_damages[mean_damages$iso3==r, c("all_sd_damage") ]<-
    mean_damages[mean_damages$iso3==r, c("tm_sd_damage") ]+
    mean_damages[mean_damages$iso3==r, c("rr_sd_damage") ]+
    mean_damages[mean_damages$iso3==r, c("spi_sd_damage") ]
  
}


mean_damages<-inner_join(countries, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=tm_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_tm_adap_hdi_mean.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=rr_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_rr_adap_hdi_mean.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=spi_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_spi_adap_hdi_mean.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_all_adap_hdi_mean.png")), g, width=15, height=10)


g<-ggplot(mean_damages)+geom_sf( aes(fill=tm_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_tm_adap_hdi_sd.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=rr_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_rr_adap_hdi_sd.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=spi_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_spi_adap_hdi_sd.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_all_adap_hdi_sd.png")), g, width=15, height=10)

################################################################################

### hdi , only spi
o <- "diff_hdi"
i <- "iso3 + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+ SPI " # also here if we add adap to spi it becomes insignificant 
# f <- as.formula(paste( o, "~", paste0("lag_",o),"+", r, "|" ,i ))  # ar
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod <- fixest::feols(f, data, panel.id=pan_id)
#mod = final_models[[2]]
summary(mod) 
summary(mod, vcov = ~ iso3)
summary(mod, vcov = "DK")

coefs<-mod$coefficients

# fill data
mean_damages<- data.frame(iso3=unique(data$iso3), tm_mean_damage=NA, rr_mean_damage=NA, tm_sd_damage=NA, rr_sd_damage=NA, 
                          spi_mean_damage=NA, spi_sd_damage=NA ,
                          all_mean_damage=NA, all_sd_damage=NA
)

for (r in unique(data$iso3)){
  
  laghdi<-mean(data$lag_hdi[data$iso3 == r], na.rm = TRUE)
  
  
  ### mean on years
  tm<-mean(data$TM[data$iso3 == r], na.rm = TRUE)
  rr<-mean(data$RR[data$iso3 == r], na.rm = TRUE)
  spi<-mean(data$SPI[data$iso3 == r], na.rm = TRUE)
  
  
  # tm damages
  mean_damages[mean_damages$iso3==r, c("tm_mean_damage") ]<-
    tm*coefs["TM"] + (tm^2)*coefs["TM_2"]
  
 #  # rr damages
 #  mean_damages[mean_damages$iso3==r, c("rr_mean_damage") ]<-
 #    rr*coefs["RR"] +(rr^2)*coefs["RR_2"] +
 #   rr*laghdi*coefs["RR:lag_hdi"] +(rr^2)*laghdi*coefs["RR_2:lag_hdi"] 
  
  # spi damages
  mean_damages[mean_damages$iso3==r, c("spi_mean_damage") ]<-
    spi*coefs["SPI"] 
  
  
  
  # sum all
  mean_damages[mean_damages$iso3==r, c("all_mean_damage") ]<-
    mean_damages[mean_damages$iso3==r, c("tm_mean_damage") ]+
   # mean_damages[mean_damages$iso3==r, c("rr_mean_damage") ]+
    mean_damages[mean_damages$iso3==r, c("spi_mean_damage") ]
  
  
  
  ### sd on years
  tm<-sd(data$TM[data$iso3 == r], na.rm = TRUE)
  rr<-sd(data$RR[data$iso3 == r], na.rm = TRUE)
  spi<-sd(data$SPI[data$iso3 == r], na.rm = TRUE)
  
  
  # tm damages
  mean_damages[mean_damages$iso3==r, c("tm_sd_damage") ]<-
    tm*coefs["TM"] + (tm^2)*coefs["TM_2"] 
  
 # # rr damages
 # mean_damages[mean_damages$iso3==r, c("rr_sd_damage") ]<-
 #   rr*coefs["RR"] +(rr^2)*coefs["RR_2"] +
 #   rr*laghdi*coefs["RR:lag_hdi"] +(rr^2)*laghdi*coefs["RR_2:lag_hdi"] 
  
  # spi damages
  mean_damages[mean_damages$iso3==r, c("spi_sd_damage") ]<-
    spi*coefs["SPI"] 
  
  
  # sum all 
  mean_damages[mean_damages$iso3==r, c("all_sd_damage") ]<-
    mean_damages[mean_damages$iso3==r, c("tm_sd_damage") ]+
    # mean_damages[mean_damages$iso3==r, c("rr_sd_damage") ]+
    mean_damages[mean_damages$iso3==r, c("spi_sd_damage") ]
  
}


mean_damages<-inner_join(countries, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=tm_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_tm_hdi_mean.png")), g, width=15, height=10)

# g<-ggplot(mean_damages)+geom_sf( aes(fill=rr_mean_damage))+ scale_fill_gradient2()
# ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_rr_hdi_mean.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=spi_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_spi_hdi_mean.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_all_hdi_mean.png")), g, width=15, height=10)


g<-ggplot(mean_damages)+geom_sf( aes(fill=tm_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_tm_hdi_sd.png")), g, width=15, height=10)

# g<-ggplot(mean_damages)+geom_sf( aes(fill=rr_sd_damage))+ scale_fill_gradient2()
# ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_rr_hdi_sd.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=spi_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_spi_hdi_sd.png")), g, width=15, height=10)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mod_mean_damages_all_hdi_sd.png")), g, width=15, height=10)


