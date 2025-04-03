### analysis of the bootstrapped coefficients from hdi and components models 

library(readr)
library(ggplot2)
library(dplyr)

# output dir
out_dir<-"output"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics/JUNO/sign_extremes_bootstrap_placebo"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics/JUNO/sign_extremes_bootstrap_placebo/plots"
if(!dir.exists(out_dir)){dir.create(out_dir)}


data_dir<-"output/models_metrics/JUNO/sign_extremes_bootstrap_placebo"

out_variables<-c("diff_hdi","diff_health_index", "diff_edu_index" , "diff_income_index")
extr_variables<-c("TVAR", "HW", "RX" , "PEXT", "WD", "SPI", "SPEI", "PET")
formulations<-c("burke_extr_sq", "burke_extr_sq_adap")


for (out_var in out_variables){
  for (ext in extr_variables){
    for (form in formulations ){
      
      rand_coefs_nonrand<-read_csv(file.path(data_dir,paste0(out_var, "_",ext, "_",form, "_coefs_randomization_nonrandom.csv")))
      rand_coefs_nonrand$type<-"nonrand"
      rand_coefs_overall<-read_csv(file.path(data_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_fullsample.csv")))
      rand_coefs_overall$type<-"overall"
      rand_coefs_withinyear<-read_csv(file.path(data_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinyear.csv")))
      rand_coefs_withinyear$type<-"withinyear"
      rand_coefs_withinregion<-read_csv(file.path(data_dir,paste0(out_var, "_",ext,"_",form,"_coefs_randomization_withinregion.csv")))
      rand_coefs_withinregion$type<-"withinregion"
      
      all_coefs<-rbind(rand_coefs_nonrand, rand_coefs_overall, rand_coefs_withinyear,rand_coefs_withinregion )
      
      ## differentiate by formulation 
      if (form == "burke_extr_sq"){
        
        summ<-all_coefs%>%group_by(type)%>%mutate(m_coef_1=mean(coef_1), q1_coef_1=quantile(coef_1, 0.025), q2_coef_1=quantile(coef_1, 0.975),
                                                  m_coef_2=mean(coef_2), q1_coef_2=quantile(coef_2, 0.025), q2_coef_2=quantile(coef_2, 0.975),
        )
        summ<-unique(summ[, c("type", "m_coef_1", "q1_coef_1", "q2_coef_1", 
                                      "m_coef_2", "q1_coef_2", "q2_coef_2" 
                              )])
        
        g1<-ggplot(summ, aes(x=type, y= m_coef_1)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_1, ymax = q2_coef_1))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        g2<-ggplot(summ, aes(x=type, y= m_coef_2)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_2, ymax = q2_coef_2))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        
        ggpubr::ggarrange(g1, g2)%>%
          ggsave(filename=file.path(out_dir,paste0(out_var, "_",ext, "_",form, "_boot.png")), height=10, width=20)
      }
      
      if (form == "burke_extr_sq_adap"){
        
        summ<-all_coefs%>%group_by(type)%>%mutate(m_coef_1=mean(coef_1), q1_coef_1=quantile(coef_1, 0.025), q2_coef_1=quantile(coef_1, 0.975),
                                                  m_coef_2=mean(coef_2), q1_coef_2=quantile(coef_2, 0.025), q2_coef_2=quantile(coef_2, 0.975),
                                                  m_coef_1_adap=mean(coef_1_adap), q1_coef_1_adap=quantile(coef_1_adap, 0.025), q2_coef_1_adap=quantile(coef_1_adap, 0.975),
                                                  m_coef_2_adap=mean(coef_2_adap), q1_coef_2_adap=quantile(coef_2_adap, 0.025), q2_coef_2_adap=quantile(coef_2_adap, 0.975),
        )
        summ<-unique(summ[, c("type", "m_coef_1", "q1_coef_1", "q2_coef_1", 
                              "m_coef_2", "q1_coef_2", "q2_coef_2" ,
                              "m_coef_1_adap", "q1_coef_1_adap", "q2_coef_1_adap", 
                              "m_coef_2_adap", "q1_coef_2_adap", "q2_coef_2_adap" 
        )])
        
        g1<-ggplot(summ, aes(x=type, y= m_coef_1)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_1, ymax = q2_coef_1))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        g2<-ggplot(summ, aes(x=type, y= m_coef_2)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_2, ymax = q2_coef_2))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        g3<-ggplot(summ, aes(x=type, y= m_coef_1_adap)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_1_adap, ymax = q2_coef_1_adap))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        g4<-ggplot(summ, aes(x=type, y= m_coef_2_adap)) + 
          geom_point() + 
          geom_errorbar(aes(ymin = q1_coef_2_adap, ymax = q2_coef_2_adap))+ 
          theme_bw()+ geom_hline(yintercept=0, col="red")
        
        
        ggpubr::ggarrange(g1, g2, g3, g4, ncol=4)%>%
          ggsave(filename=file.path(out_dir,paste0(out_var, "_",ext, "_",form, "_boot.png")), height=10, width=40)
      }
      
      
      
      
    }
  }
}
