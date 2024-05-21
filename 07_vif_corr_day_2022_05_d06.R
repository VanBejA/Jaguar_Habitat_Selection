##################################
##### SCALE SELECTION 
##### Vanesa Bejarano Alegre   06.05.2022
##### ##############################

library(dplyr)
library(MuMIn)
library(survival)
library(tidyverse)
library(coxme)
library(usdm)

# upload data -----------------------------------------------------------

scale_day <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/03_VIF_CORRELATION/scale.day_may2022.csv', header = T, sep = ',', dec = '.', comment.char = '')

scale_night <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/03_VIF_CORRELATION/scale.night_may2022.csv', header = T, sep = ',', dec = '.', comment.char = '')


# Correlation - Day -------------------------------------------------------------
scale_day[is.na(scale_day)] <- 0

scale_day.corr <- scale_day[, c(2,7:23)]

resultados <- cor(scale_day.corr, method="pearson")

class(resultados)
resultados <-  as.data.frame(resultados)

write.csv(resultados, "D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/03_VIF_CORRELATION/Pearson_corr_day_may2022.csv")


# VIF ---------------------------------------------------------------------
usdm::vifcor(scale_day.corr)
scale_day.corr$flood5886 <- NULL
scale_day.corr$grass5886 <- NULL
scale_day.corr$pig5886 <- NULL # correlation
scale_day.corr$light5886 <- NULL

usdm::vifcor(scale_day.corr)


scale_day.corr$princ5886 <- NULL
scale_day.corr$princ3001 <- NULL
scale_day.corr$field5886 <- NULL


scale_day.corr$herva5886 <- NULL


usdm::vifcor(scale_day.corr)


# Correlation - night -------------------------------------------------------------
scale_night[is.na(scale_night)] <- 0

scale_night.corr <- scale_night[, c(2,7:23)]

resultados <- cor(scale_night.corr, method="pearson")

class(resultados)
resultados <-  as.data.frame(resultados)

write.csv(resultados, "D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/03_VIF_CORRELATION/Pearson_corr_night_may2022.csv")


# VIF ---------------------------------------------------------------------
usdm::vifcor(scale_night.corr)

scale_night.corr$grass5886 <- NULL
scale_night.corr$flood5886 <- NULL
scale_night.corr$herva5886 <- NULL
scale_night.corr$pig5886 <- NULL # correlation of ovine

usdm::vifcor(scale_night.corr)






