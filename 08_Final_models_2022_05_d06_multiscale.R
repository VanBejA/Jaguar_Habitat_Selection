library(dplyr)
library(MuMIn)
library(survival)
library(tidyverse)
library(coxme)
library(usdm)
library(jtools)
library(ggstatsplot)
library(hrbrthemes)

# upload data -high treecover----------------------------------------------------------

day_data <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/04_final_model/final_model_2021_10_d03/scale.day_may2022.csv', header = T, sep = ',', dec = '.', comment.char = '')

night_data <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/04_final_model/final_model_2021_10_d03/scale.night_may2022.csv', header = T, sep = ',', dec = '.', comment.char = '')


# DAY -GLOBAL -------------------------------------------------------------

female_day <- day_data[day_data$sex == "Female",]
male_day <- day_data[day_data$sex == "Male",]

mod1.female <- coxph(Surv(time_,case_)~  tree5886+ 
                       herva3001+ water5886+ crop5886+ urban5886+ 
                       princ3001+ field5886+
                       popdens5886+ livestock5886 +
                       ovine5886  + strata(id), 
                     data= female_day)

summary(mod1.female)

female_day$predicted <- predict(mod1.female,type="expected")


mod1.male <- coxph(Surv(time_,case_)~  tree5886+ 
                     herva5886+ water5886+ crop5886+ urban5886+ 
                     princ1242+ field724+
                     popdens5886+ livestock5886 +
                     ovine5886  + strata(id), 
                   data= male_day)  

summary(mod1.male)
male_day$predicted <- predict(mod1.male,type="expected")

plot_day <- plot_summs(mod1.male, mod1.female, scale=TRUE,  
                       robust = TRUE, 
                       model.names = c("Males", "Females"),
                       coefs = c("Water" = "water5886",
                                 "Treecover" = "tree5886",
                                 "Flood_area" = "flood5886",
                                 "Shrubland" = "herva5886",
                                 "Crop" ="crop5886", 
                                 "Urban" = "urban5886",
                                 "Principal_road"="princ5886",
                                 "Field_road"= "field5886",
                                 "Pop_dens"="popdens5886",
                                 "Cattle_dens" = "livestock5886", 
                                 "Ovine-Caprine_dens"="ovine5886"))

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_blank(), 
        axis.text=element_text(size=15),
        axis.title=element_text(size=10),
        legend.text = element_text(size = 10))

plot_day + apatheme +  labs(x="\n Beta Estimate \n ", y="",
                            title="Day") + theme_ipsum(base_family = "Calibri",base_size = 14) + theme(axis.title.x = element_text(hjust = 0.5, size=12))


#ggcoefstats(mod1.male)
#ggcoefstats(mod1.female)
#### 95% CI 

confint(mod1.female, level= 0.95)
confint(mod1.male, level= 0.95)



# night-GLOBAL -------------------------------------------------------------

female_night<- night_data[night_data$sex == "Female",]
male_night<- night_data[night_data$sex == "Male",]

mod2.female <- coxph(Surv(time_,case_)~  tree5886+ 
                       herva3001+ water5886+ crop5886+ urban5886+ 
                       princ3001+ field5886+
                       popdens5886+ livestock5886 +
                       ovine5886  + strata(id), 
                     data= female_night)                    

summary(mod2.female)

female_night$predicted <- predict(mod2.female,type="expected")

mod2.male <- coxph(Surv(time_,case_)~  tree5886+ 
                     herva5886+ water5886+ crop5886+ urban5886+ 
                     princ1242+ field2105+
                     popdens5886+ livestock5886 +
                     ovine5886  + strata(id), 
                   data= male_night)  

summary(mod2.male)

male_night$predicted <- predict(mod2.male,type="expected")

?predict

plot_night <- plot_summs(mod2.male, mod2.female, scale=TRUE,  
                         robust = TRUE, 
                         model.names = c("Males", "Females"),
                         coefs = c("Water" = "water5886",
                                   "Treecover" = "tree5886",
                                   "Shrubland" = "herva5886",
                                   "Crop" ="crop5886", 
                                   "Urban" = "urban5886",
                                   "Principal_road"="princ5886",
                                   "Field_road"= "field5886",
                                   "Pop_dens"="popdens5886",
                                   "Cattle_dens" = "livestock5886", 
                                   "Ovine-Caprine_dens"="ovine5886"))

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_blank(), 
        axis.text=element_text(size=15),
        axis.title=element_text(size=10),
        legend.text = element_text(size = 10))

plot_night + apatheme +  labs(x="\n Beta Estimate \n ", y="",
                              title="Night") + theme_ipsum(base_family = "Calibri",base_size = 14) + theme(axis.title.x = element_text(hjust = 0.5, size=12))


#### 95% CI 

confint(mod2.female, level= 0.95)
confint(mod2.male, level= 0.95)



