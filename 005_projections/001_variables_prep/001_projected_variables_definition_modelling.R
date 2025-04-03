### trasform projections of gdp in gni, and of mean years of schoooling in expected years of schooling, based on past relations
### for gni, also try to use population 
### for expected years of schooling, try to use mean years of schooling at different ages

### gdp gni -> from world bank, compare pop weighted, ppp, same us dollar price adjustment 
# gdp_ppp_2021*ppp_2021/ppp_2011 = gdp_ppp_2011


rm(list=ls())

source("utils/libraries.R")

gdp_gni<-read_excel("data/gdp_data/gdp_gni_P_Data_Extract_From_World_Development_Indicators.xlsx")
gdp_gni<-reshape2::melt(gdp_gni, id.vars = c("Country Name","Country Code", "Series Name", "Series Code"), variable.name = "year")
gdp_gni$year<-as.numeric(substr(as.character(gdp_gni$year), 1,4))
gdp_gni$value<-as.numeric(gdp_gni$value)
gdp_gni<-gdp_gni[, c(1,2,3,5,6)]
unique(gdp_gni$`Series Name`)

gni<-gdp_gni[which(gdp_gni$`Series Name`== "GNI per capita, PPP (constant 2021 international $)"), ]
colnames(gni)[which(colnames(gni)=="value")]<-"gni"
gni$`Series Name`<-NULL
gdp<-gdp_gni[which(gdp_gni$`Series Name`== "GDP per capita, PPP (constant 2021 international $)"), ]
colnames(gdp)[which(colnames(gdp)=="value")]<-"gdp"
gdp$`Series Name`<-NULL

data<-inner_join(gni, gdp)

oecd_ppp_2021_us_dollar <- read_delim("data/gdp_data/oecd_ppp_2021_us_dollar.csv", 
                                      delim = ";", escape_double = FALSE, col_names = FALSE, 
                                      trim_ws = TRUE)

colnames(oecd_ppp_2021_us_dollar)<-c("Country Name", "PPP_2021")

oecd_ppp_2011_us_dollar <- read_delim("data/gdp_data/oecd_ppp_2011_us_dollar.csv", 
                                      delim = ";", escape_double = FALSE, col_names = FALSE, 
                                      trim_ws = TRUE)

colnames(oecd_ppp_2011_us_dollar)<-c("Country Name", "PPP_2011")

oecd_ppp_2005_us_dollar <- read_delim("data/gdp_data/oecd_ppp_2005_us_dollar.csv", 
                                      delim = ";", escape_double = FALSE, col_names = FALSE, 
                                      trim_ws = TRUE)

colnames(oecd_ppp_2005_us_dollar)<-c("Country Name", "PPP_2005")
ppp<-inner_join(oecd_ppp_2005_us_dollar, oecd_ppp_2021_us_dollar)
ppp<-inner_join(ppp, oecd_ppp_2011_us_dollar)

ppp$PPP_2005<-as.numeric(gsub(",", ".",ppp$PPP_2005 ))
ppp$PPP_2021<-as.numeric(gsub(",", ".",ppp$PPP_2021 ))
ppp$PPP_2011<-as.numeric(gsub(",", ".",ppp$PPP_2011 ))

plot(ppp$PPP_2005, ppp$PPP_2021)

###

plot(data$gni , data$gdp)
abline(a=1,b=1)


plot(log(data$gni) , log(data$gdp))
abline(a=1,b=1)

plot(log(data$gdp), log(data$gni) )
abline(a=1,b=1)



# Call:
#   lm(formula = lgdp ~ lgni, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.43267 -0.02757 -0.00574  0.01575  1.00850 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.003921   0.012357  -0.317    0.751    
# lgni         1.003154   0.001306 768.294   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1039 on 5012 degrees of freedom
# (12010 osservazioni eliminate a causa di valori mancanti)
# Multiple R-squared:  0.9916,	Adjusted R-squared:  0.9916 
# F-statistic: 5.903e+05 on 1 and 5012 DF,  p-value: < 2.2e-16

data$lgni<-log(data$gni)
data$lgdp<-log(data$gdp)
colnames(data)[1:2]<-c("country_name", "iso3")
ggplot(data, aes(x=lgdp, y=lgni, col=iso3, group=iso3))+geom_point()+geom_smooth(method = lm, formula=y ~ x)+theme(legend.position = "none")

# coef_list<-list()
# for (iso in unique(data$iso3)){
#   if ( sum(is.na(data[which(data$iso3 == iso),]$gdp)) ==length(data[which(data$iso3 == iso),]$gdp) | 
#        sum(is.na(data[which(data$iso3 == iso),]$gni)) ==length(data[which(data$iso3 == iso),]$gni) ){
#     
#   }else{
# 
#     data_small<- data[which(data$iso3 == iso),]
#     mod<-lm(lgni~lgdp, data_small )
#     #summary(mod_hdi) 
#     coef_list[[iso]]<-mod$coefficients
#   }
# }
# 
#   
# mod_generic<-lm(lgni~lgdp, data )
# coef_list[["generic"]]<-mod_generic$coefficients
# 
# save(coef_list, file="output/projections/lgni_lgdp_by_country_model_coefs.RData")

# add autoreg comp
data<-data %>%
  arrange(iso3, year)%>%
  group_by(iso3)%>%
  mutate(lag_lgni= dplyr::lag(lgni, 1))


mod_generic<-lm(gni~gdp, data )
summary(mod_generic)
mod_generic<-lm(lgni~lgdp, data )
summary(mod_generic)
mod_generic_lag<-lm(lgni~lgdp + lag_lgni , data )
summary(mod_generic_lag)
coefficients(mod_generic_lag)

#################################################################################

data_hdi <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")

data_hdi$lmys=log(data_hdi$mys)
data_hdi$leys=log(data_hdi$eys)

# for (iso in unique(data_hdi$iso3)){
#   
#   if ( sum(is.na(data_hdi[which(data_hdi$iso3 == iso),]$mys)) ==length(data_hdi[which(data_hdi$iso3 == iso),]$mys) | 
#        sum(is.na(data_hdi[which(data_hdi$iso3 == iso),]$eys)) ==length(data_hdi[which(data_hdi$iso3 == iso),]$eys) ){
#     
#   }else{
#     g<-ggplot(data_hdi[which(data_hdi$iso3 == iso),], aes(x=lmys, y=leys))+geom_smooth(method = "lm", formula=y~poly(x,3))+
#       geom_point()+theme(legend.position = "none")
#     ggsave(filename = paste0("output/projections/try/leys_lmys_model_", iso, ".png"))
#   }
#  
# }

ggplot(data_hdi, aes(x=mys, y=eys, col=iso3, group=iso3))+geom_smooth(method = "lm",formula= y~x )+
  geom_point()+theme(legend.position = "none")


ggplot(data_hdi, aes(x=mys, y=eys))+geom_smooth(method = "lm",formula= y~x )+
  geom_point()+theme(legend.position = "none")

mod_hdi_generic<-feols(eys ~  mys ,data_hdi, panel.id = c("iso3", "year") )
mod_hdi_generic$coefficients #
summary(mod_hdi_generic)


mod_hdi_generic<-feols(eys ~  mys + log(gnipc) ,data_hdi, panel.id = c("iso3", "year") )
mod_hdi_generic$coefficients #
summary(mod_hdi_generic)

mod_hdi_generic_lag<-feols(eys ~ l(mys, 0:5) + l(gnipc, 0:4) + l(le, 0:3),data_hdi, panel.id = c("iso3", "year") )
mod_hdi_generic_lag$coefficients # we use these
summary(mod_hdi_generic_lag)

# check autoreg model
# coef_list<-list()
# for (iso in unique(data_hdi$iso3)){
#   
#   if ( sum(is.na(data_hdi[which(data_hdi$iso3 == iso),]$mys)) ==length(data_hdi[which(data_hdi$iso3 == iso),]$mys) | 
#        sum(is.na(data_hdi[which(data_hdi$iso3 == iso),]$eys)) ==length(data_hdi[which(data_hdi$iso3 == iso),]$eys) ){
#     
#   }else{
#     data_small<- data_hdi[which(data_hdi$iso3 == iso),]
#     mod_hdi<-feols(eys ~ bs(mys ),data_small, panel.id = c("iso3", "year") )
#     #summary(mod_hdi) 
#     coef_list[[iso]]<-mod_hdi$coefficients
#     
#     data_small$pred<-predict(mod_hdi_generic, data_small)
#     
#     g<-ggplot(data_small, aes(x=mys, y=eys), col="black")+
#       geom_point()+
#       geom_point(aes(x=mys, y=pred), col="red")+ggtitle(paste0(iso))
#     ggsave(filename = paste0("output/projections/try/eys_mys_model_ar_", iso, ".png"))
#   }
#   
# }
# mod_hdi_generic<-feols(eys ~  l(eys, 1) + mys + l(mys, 1),data_hdi, panel.id = c("iso3", "year") )
# coef_list[["generic"]]<-mod_hdi_generic$coefficients


# save(coef_list, file="output/projections/eys_mys_ar_by_country_model_coefs.RData")
