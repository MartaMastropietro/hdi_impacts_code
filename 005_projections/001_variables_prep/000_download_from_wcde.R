
#install.packages("wcde")
library(wcde)
?get_wcde

# regions_codes<-wic_locations

### leb 

s5<-get_wcde(indicator="e0", scenario=5)
s4<-get_wcde(indicator="e0", scenario=4)
s3<-get_wcde(indicator="e0", scenario=3)
s2<-get_wcde(indicator="e0", scenario=2)
s1<-get_wcde(indicator="e0", scenario=1)

# s1$leb_ssp1<-s1$e0
# s1<-s1[, -c(1,6)]
# s2$leb_ssp2<-s2$e0
# s2<-s2[, -c(1,6)]
# s3$leb_ssp3<-s3$e0
# s3<-s3[, -c(1,6)]
# s4$leb_ssp4<-s4$e0
# s4<-s4[, -c(1,6)]
# s5$leb_ssp5<-s5$e0
# s5<-s5[, -c(1,6)]
# 
# leb_proj<-inner_join(s1, s2)
# leb_proj<-inner_join(leb_proj, s3)
# leb_proj<-inner_join(leb_proj, s4)
# leb_proj<-inner_join(leb_proj, s5)

leb_proj<-rbind(s1, s2,s3,s4,s5)

leb_proj$scenario[which(leb_proj$scenario==1)]<-"SSP1"
leb_proj$scenario[which(leb_proj$scenario==2)]<-"SSP2"
leb_proj$scenario[which(leb_proj$scenario==3)]<-"SSP3"
leb_proj$scenario[which(leb_proj$scenario==4)]<-"SSP4"
leb_proj$scenario[which(leb_proj$scenario==5)]<-"SSP5"
leb_proj$country_code<-NULL

ggplot(leb_proj[which(leb_proj$name=="Afghanistan" & leb_proj$sex=="Male"),], aes(x = period, y = e0, color = scenario)) +
  geom_point(size = 1)

colnames(leb_proj)<-c("Scenario", "Area", "Period", "Sex", "Years")
# same as the ones we downloaded

#################################################################################
# pop


s5<-get_wcde(indicator="pop",pop_sex =  "both", scenario=5)
s4<-get_wcde(indicator="pop",pop_sex =  "both", scenario=4)
s3<-get_wcde(indicator="pop",pop_sex =  "both", scenario=3)
s2<-get_wcde(indicator="pop",pop_sex =  "both", scenario=2)
s1<-get_wcde(indicator="pop",pop_sex =  "both", scenario=1)
