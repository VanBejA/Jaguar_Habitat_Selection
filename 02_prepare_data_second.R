#### Second step - Jaguar data prepare
### Vanesa Bejarano Alegre  -  01_14_2021 -
### 
### Here we reviewed the data individually and see how much information we have about the observed times of the jaguars (before this, we perform a check of the expected hours versus the observed hours in the dataset).
# We delete the data with outliers and save the individual, and clean data for the next process.

rm(list= ls())  #clean all before to start

listpacks <- c("tidyverse","purrr", "amt", "lubridate", "ctmm")

if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load(listpacks)

setwd("D:/PhD_Jaguar/Jaguar01/00_Script_to_submit/Jaguar_Datae")

jaguar <- read.delim(file="D:/PhD_Jaguar/Jaguar01/00_Script_to_submit/Jaguar_Data/data_prepare/00_jaguar01.txt")


###  data prepare 
jaguar$timestamp <- ymd_hms(jaguar$timestampTZ)

jaguartrk <- mk_track(jaguar,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"))

jaguartrk <- jaguartrk %>% arrange(id)

jaguar$distance <- amt::step_lengths(jaguartrk, lonlat = TRUE) 

# Jaguar #1 ---------------------------------------------------------------

##Selected individual data

j1 <- filter(jaguar, individual.local.identifier == "1")


##Seccion 2: resample data
j1$timestamp <- ymd_hms(j1$timestampTZ)

jtrk1 <- mk_track(j1,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight,
                  country=country,crs = CRS("+init=epsg:4326"), 
                  distance=distance)## trk format

step_duration <- 4 # observed hour in previus chek
jag01<- track_resample(jtrk1, hours(step_duration), tolerance = minutes(15)) # data continuity

# We check all data description summary but here because RMd we  didn't
# summarize_sampling_rate(jag01) 

table(jag01$burst_) #continuity 

which(step_lengths(jag01, lonlat=TRUE) > 10000)# Checking for outliers data greater than 10,000 meters

#If there is, we delete it
jag01_deleted <- jag01[-c(253,268,269),]

j1 <-  step_lengths(jag01_deleted,lonlat=TRUE) # step_lenght again.


#how many data to used such as turn angle?

jag01_end <- filter_min_n_burst(jag01_deleted, 1)

jag01_duplicated_burst <- as.data.frame( table(jag01_end$burst_))

jag01_duplicated_burst2 <- subset(jag01_duplicated_burst , Freq >=3)### we need 3 or more continuos point




write.table(jag01_deleted ,file="02_jaguar01_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")



### From here on, the previous procedure is repeated for the other individuals. Below the other individuals but without having the commented steps


# Jaguar #2 ---------------------------------------------------------------

j2 <- filter(jaguar, individual.local.identifier == "2")

##Seccion 2: resample data

# Selected data

jtrk2 <- mk_track(j2,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag02<- track_resample(jtrk2, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag02)

table(jag02$burst_)

which(step_lengths(jag02, lonlat=TRUE) > 10000)


jag02_end <- filter_min_n_burst(jag02, 1)

jag02_duplicated <- as.data.frame( table(jag02_end$burst_))

jag02_duplicated2 <- subset(jag02_duplicated , Freq >=3)


write.table(jag02_deleted ,file="02_jaguar02_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #3 ---------------------------------------------------------------
##
j3 <- filter(jaguar, individual.local.identifier == "3")


##Seccion 2: resample data

# Selected data

jtrk3 <- mk_track(j3,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag03<- track_resample(jtrk3, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag03)

table(jag03$burst_)

which(step_lengths(jag03, lonlat=TRUE) > 10000)


jag03_deleted <- jag03[-c(29,37,60,68,80,89,93,105),]

j3 <-  step_lengths(jag03_deleted,lonlat=TRUE)



#how many data with turn angle

jag03_end <- filter_min_n_burst(jag03_deleted, 1)

jag03_duplicated <- as.data.frame( table(jag03_end$burst_))

jag03_duplicated2 <- subset(jag03_duplicated , Freq >=3)


write.table(jag03_deleted ,file="02_jaguar03_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #4 ---------------------------------------------------------------

##
j4 <- filter(jaguar, individual.local.identifier == "4")


##Seccion 2: resample data

# Selected data

jtrk4 <- mk_track(j4,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag04<- track_resample(jtrk4, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag04)

table(jag04$burst_)

which(step_lengths(jag04, lonlat=TRUE) > 10000)

#how many data with turn angle

jag04_end <- filter_min_n_burst(jag04, 1)

jag04_duplicated <- as.data.frame( table(jag04_end$burst_))

jag04_duplicated2 <- subset(jag04_duplicated , Freq >=3)


write.table(jag04 ,file="02_jaguar04_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#5 ----------------------------------------------------------------

j5 <- filter(jaguar, individual.local.identifier == "5")


##Seccion 2: resample data

# Selected data

jtrk5 <- mk_track(j5,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag05<- track_resample(jtrk5, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag05)

table(jag05$burst_)

which(step_lengths(jag05, lonlat=TRUE) > 10000)


#how many data with turn angle

jag05_end <- filter_min_n_burst(jag05, 1)

jag05_duplicated <- as.data.frame( table(jag05_end$burst_))

jag05_duplicated2 <- subset(jag05_duplicated , Freq >=3)



write.table(jag05 ,file="02_jaguar05_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #6 ---------------------------------------------------------------

j6 <- filter(jaguar, individual.local.identifier == "6")


##Seccion 2: resample data

# Selected data

jtrk6 <- mk_track(j6,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag06<- track_resample(jtrk6, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag06)

table(jag06$burst_)

which(step_lengths(jag06, lonlat=TRUE) > 10000)


jag06_deleted <- jag06[-c(848),]

j6 <-  step_lengths(jag06_deleted,lonlat=TRUE)


#how many data with turn angle

jag06_end <- filter_min_n_burst(jag06_deleted, 1)

jag06_duplicated <- as.data.frame( table(jag06_end$burst_))

jag06_duplicated2 <- subset(jag06_duplicated , Freq >=3)



write.table(jag06_deleted ,file="02_jaguar06_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#7 ----------------------------------------------------------------

##
j7 <- filter(jaguar, individual.local.identifier == "7")


##Seccion 2: resample data

# Selected data

jtrk7 <- mk_track(j7,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag07<- track_resample(jtrk7, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag07)

table(jag07$burst_)

which(step_lengths(jag07, lonlat=TRUE) > 10000)


#how many data with turn angle

jag07_end <- filter_min_n_burst(jag07, 1)

jag07_duplicated <- as.data.frame( table(jag07_end$burst_))

jag07_duplicated2 <- subset(jag07_duplicated , Freq >=3)



write.table(jag07 ,file="02_jaguar07_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#8 ----------------------------------------------------------------

##
j8 <- filter(jaguar, individual.local.identifier == "8")

##Seccion 2: resample data

# Selected data

jtrk8 <- mk_track(j8,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag08<- track_resample(jtrk8, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag08)

table(jag08$burst_)

which(step_lengths(jag08, lonlat=TRUE) > 10000)


jag08_deleted <- jag08[-c(491),]

j8 <-  step_lengths(jag08_deleted,lonlat=TRUE)


#how many data with turn angle

jag08_end <- filter_min_n_burst(jag08_deleted, 1)

jag08_duplicated <- as.data.frame( table(jag08_end$burst_))

jag08_duplicated2 <- subset(jag08_duplicated , Freq >=3)



write.table(jag08_deleted ,file="02_jaguar08_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#9 ----------------------------------------------------------------

##
j9 <- filter(jaguar, individual.local.identifier == "9")

##Seccion 2: resample data

# Selected data

jtrk9 <- mk_track(j9,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag09<- track_resample(jtrk9, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag09)

table(jag09$burst_)

which(step_lengths(jag09, lonlat=TRUE) > 10000)


#how many data with turn angle

jag09_end <- filter_min_n_burst(jag09, 1)

jag09_duplicated <- as.data.frame( table(jag09_end$burst_))

jag09_duplicated2 <- subset(jag09_duplicated , Freq >=3)



write.table(jag09 ,file="02_jaguar09_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#10 ---------------------------------------------------------------

##
j10 <- filter(jaguar, individual.local.identifier == "10")

##Seccion 2: resample data

# Selected data

jtrk10 <- mk_track(j10,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag010<- track_resample(jtrk10, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag010)

table(jag010$burst_)

which(step_lengths(jag010, lonlat=TRUE) > 10000)


jag010_deleted <- jag010[-c(1513),]

j10 <-  step_lengths(jag010_deleted,lonlat=TRUE)


#how many data with turn angle

jag010_end <- filter_min_n_burst(jag010_deleted, 1)

jag010_duplicated <- as.data.frame( table(jag010_end$burst_))

jag010_duplicated2 <- subset(jag010_duplicated , Freq >=3)


write.table(jag010_deleted ,file="02_jaguar010_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#11 ---------------------------------------------------------------
j11 <- filter(jaguar, individual.local.identifier == "11")


##Seccion 2: resample data

# Selected data

jtrk11 <- mk_track(j11,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag011<- track_resample(jtrk11, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag011)

table(jag011$burst_)

which(step_lengths(jag011, lonlat=TRUE) > 10000)


#how many data with turn angle

jag011_end <- filter_min_n_burst(jag011, 1)

jag011_duplicated <- as.data.frame( table(jag011_end$burst_))

jag011_duplicated2 <- subset(jag011_duplicated , Freq >=3)



write.table(jag011,file="02_jaguar011_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#12 ---------------------------------------------------------------
j12 <- filter(jaguar, individual.local.identifier == "12")


##Seccion 2: resample data

# Selected data

jtrk12 <- mk_track(j12,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <-1
jag012<- track_resample(jtrk12, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag012)

table(jag012$burst_)

which(step_lengths(jag012, lonlat=TRUE) > 10000)


#how many data with turn angle

jag012_end <- filter_min_n_burst(jag012, 1)

jag012_duplicated <- as.data.frame( table(jag012_end$burst_))

jag012_duplicated2 <- subset(jag012_duplicated , Freq >=3)



write.table(jag012 ,file="02_jaguar012_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #13 --------------------------------------------------------------
j13 <- filter(jaguar, individual.local.identifier == "13")

##Seccion 2: resample data

# Selected data

jtrk13 <- mk_track(j13,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag013<- track_resample(jtrk13, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag013)

table(jag013$burst_)

which(step_lengths(jag013, lonlat=TRUE) > 10000)


j13 <-  step_lengths(jag013,lonlat=TRUE)


#how many data with turn angle

jag013_end <- filter_min_n_burst(jag013, 1)

jag013_duplicated <- as.data.frame( table(jag013_end$burst_))

jag013_duplicated2 <- subset(jag013_duplicated , Freq >=3)



write.table(jag013 ,file="02_jaguar013_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#14 ---------------------------------------------------------------
j14 <-filter(jaguar, individual.local.identifier == "14")


j14$hour <- hour(j14$timestampTZ) 
j14$SchedDiff <-  ifelse(j14$hour <6 | 16< j14$hour, "1", "2")

##Seccion 2: resample data

# Selected data 14.1

jtrk14 <- mk_track(j14,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j14.1 <- jtrk14 %>%
  filter(SchedDiff==1)

j14.2 <- jtrk14 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag014.1<- track_resample(j14.1, hours(step_duration), tolerance = minutes(15))

###

summarize_sampling_rate(jag014.1)

table(jag014.1$burst_)

which(step_lengths(jag014.1, lonlat=TRUE) > 10000)


j14.1 <-  step_lengths(jag014.1,lonlat=TRUE)


#how many data with turn angle

jag014.1_end <- filter_min_n_burst(jag014.1, 1)

jag014.1_duplicated <- as.data.frame( table(jag014.1_end$burst_))

jag014.1_duplicated2 <- subset(jag014.1_duplicated , Freq >=3)



write.table(jag014.1,file="02_jaguar014.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 14.2

step_duration <- 2
jag014.2<- track_resample(j14.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag014.2)

table(jag014.2$burst_)

which(step_lengths(jag014.2, lonlat=TRUE) > 10000)


j14.2 <-  step_lengths(jag014.2,lonlat=TRUE)


#how many data with turn angle

jag014.2_end <- filter_min_n_burst(jag014.2, 1)

jag014.2_duplicated <- as.data.frame( table(jag014.2_end$burst_))

jag014.2_duplicated2 <- subset(jag014.2_duplicated , Freq >=3)



write.table(jag014.2,file="02_jaguar014.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#15 ---------------------------------------------------------------
j15 <- filter(jaguar, individual.local.identifier == "15")

summary(j15)


j15$hour <- hour(j15$timestampTZ) #create a column
j15$SchedDiff <-  ifelse(j15$hour <10 | 22< j15$hour, "1", "2")

##Seccion 2: resample data

# Selected data 15.1

jtrk15 <- mk_track(j15,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j15.1 <- jtrk15 %>%
  filter(SchedDiff==1)

j15.2 <- jtrk15 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag015.1<- track_resample(j15.1, hours(step_duration), tolerance = minutes(15))

summary(jag015.1)

round15.1 <- round(jag015.1$distance)
mode(round15.1)

###

summarize_sampling_rate(jag015.1)

table(jag015.1$burst_)

which(step_lengths(jag015.1, lonlat=TRUE) > 10000)


j15.1 <-  step_lengths(jag015.1,lonlat=TRUE)



#how many data with turn angle

jag015.1_end <- filter_min_n_burst(jag015.1, 1)

jag015.1_duplicated <- as.data.frame( table(jag015.1_end$burst_))

jag015.1_duplicated2 <- subset(jag015.1_duplicated , Freq >=3)


write.table(jag015.1,file="02_jaguar015.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 15.2

step_duration <- 2
jag015.2<- track_resample(j15.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag015.2)

table(jag015.2$burst_)

which(step_lengths(jag015.2, lonlat=TRUE) > 10000)

jag015.2_deleted <- jag015.2[-c(6,45,60,82,109,390),]

j15.2 <-  step_lengths(jag015.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag015.2_end <- filter_min_n_burst(jag015.2_deleted, 1)

jag015.2_duplicated <- as.data.frame( table(jag015.2_end$burst_))

jag015.2_duplicated2 <- subset(jag015.2_duplicated , Freq >=3)


write.table(jag015.2,file="02_jaguar015.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#16 ---------------------------------------------------------------

j16 <- filter(jaguar, individual.local.identifier == "16")



##Seccion 2: resample data

# Selected data

jtrk16 <- mk_track(j16,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag016<- track_resample(jtrk16, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag016)

table(jag016$burst_)

which(step_lengths(jag016, lonlat=TRUE) > 10000)


jag016_deleted <- jag016[-c(1294),]

j16 <-  step_lengths(jag016_deleted,lonlat=TRUE)


#how many data with turn angle

jag016_end <- filter_min_n_burst(jag016_deleted, 1)

jag016_duplicated <- as.data.frame( table(jag016_end$burst_))

jag016_duplicated2 <- subset(jag016_duplicated , Freq >=3)



write.table(jag016_deleted ,file="02_jaguar016_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar#17 ---------------------------------------------------------------
j17 <- filter(jaguar, individual.local.identifier == "17")

##Seccion 2: resample data

# Selected data

jtrk17 <- mk_track(j17,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag017<- track_resample(jtrk17, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag017)

table(jag017$burst_)

which(step_lengths(jag017, lonlat=TRUE) > 10000)


j17 <-  step_lengths(jag017,lonlat=TRUE)


#how many data with turn angle

jag017_end <- filter_min_n_burst(jag017, 1)

jag017_duplicated <- as.data.frame( table(jag017_end$burst_))

jag017_duplicated2 <- subset(jag017_duplicated , Freq >=3)



write.table(jag017 ,file="02_jaguar017_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#18 ---------------------------------------------------------------
j18 <- filter(jaguar, individual.local.identifier == "18")



##Seccion 2: resample data

# Selected data

jtrk18 <- mk_track(j18,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag018<- track_resample(jtrk18, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag018)

table(jag018$burst_)

which(step_lengths(jag018, lonlat=TRUE) > 10000)


jag018_deleted <- jag018[-c(818, 832),]

j18 <-  step_lengths(jag018_deleted,lonlat=TRUE)


#how many data with turn angle

jag018_end <- filter_min_n_burst(jag018_deleted, 1)

jag018_duplicated <- as.data.frame( table(jag018_end$burst_))

jag018_duplicated2 <- subset(jag018_duplicated , Freq >=3)



write.table(jag018_deleted ,file="02_jaguar018_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#19 ---------------------------------------------------------------

j19 <- filter(jaguar, individual.local.identifier == "19")


##Seccion 2: resample data

# Selected data

jtrk19 <- mk_track(j19,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag019<- track_resample(jtrk19, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag019)

table(jag019$burst_)

which(step_lengths(jag019, lonlat=TRUE) > 10000)


jag019_deleted <- jag019

j19 <-  step_lengths(jag019_deleted,lonlat=TRUE)


#how many data with turn angle

jag019_end <- filter_min_n_burst(jag019_deleted, 1)

jag019_duplicated <- as.data.frame( table(jag019_end$burst_))

jag019_duplicated2 <- subset(jag019_duplicated , Freq >=3)


write.table(jag019_deleted ,file="02_jaguar019_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #20 --------------------------------------------------------------
j20 <- filter(jaguar, individual.local.identifier == "20")

##Seccion 2: resample data

# Selected data

jtrk20 <- mk_track(j20,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <-1
jag020<- track_resample(jtrk20, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag020)

table(jag020$burst_)

which(step_lengths(jag020, lonlat=TRUE) > 10000)


jag020_deleted <- jag020

j20 <-  step_lengths(jag020_deleted,lonlat=TRUE)


#how many data with turn angle

jag020_end <- filter_min_n_burst(jag020_deleted, 1)

jag020_duplicated <- as.data.frame( table(jag020_end$burst_))

jag020_duplicated2 <- subset(jag020_duplicated , Freq >=3)


write.table(jag020_deleted ,file="02_jaguar020_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #21 --------------------------------------------------------------
j21 <- filter(jaguar, individual.local.identifier == "21")


##Seccion 2: resample data

# Selected data

jtrk21 <- mk_track(j21,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag021<- track_resample(jtrk21, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag021)

table(jag021$burst_)

which(step_lengths(jag021, lonlat=TRUE) > 10000)


jag021_deleted <- jag021

j21 <-  step_lengths(jag021_deleted,lonlat=TRUE)


#how many data with turn angle

jag021_end <- filter_min_n_burst(jag021_deleted, 1)

jag021_duplicated <- as.data.frame( table(jag021_end$burst_))

jag021_duplicated2 <- subset(jag021_duplicated , Freq >=3)


write.table(jag021_deleted ,file="02_jaguar021_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #22 --------------------------------------------------------------
j22 <- filter(jaguar, individual.local.identifier == "22")


##Seccion 2: resample data

# Selected data

jtrk22 <- mk_track(j22,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag022<- track_resample(jtrk22, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag022)

table(jag022$burst_)

which(step_lengths(jag022, lonlat=TRUE) > 10000)


jag022_deleted <- jag022

j22 <-  step_lengths(jag022_deleted,lonlat=TRUE)


#how many data with turn angle

jag022_end <- filter_min_n_burst(jag022_deleted, 1)

jag022_duplicated <- as.data.frame( table(jag022_end$burst_))

jag022_duplicated2 <- subset(jag022_duplicated , Freq >=3)


write.table(jag022_deleted ,file="02_jaguar022_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar # 23 -------------------------------------------------------------
j23 <- filter(jaguar, individual.local.identifier == "23")


##Seccion 2: resample data

# Selected data

jtrk23 <- mk_track(j23,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag023<- track_resample(jtrk23, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag023)

table(jag023$burst_)

which(step_lengths(jag023, lonlat=TRUE) > 10000)


jag023_deleted <- jag023

j23 <-  step_lengths(jag023_deleted,lonlat=TRUE)


#how many data with turn angle

jag023_end <- filter_min_n_burst(jag023_deleted, 1)

jag023_duplicated <- as.data.frame( table(jag023_end$burst_))

jag023_duplicated2 <- subset(jag023_duplicated , Freq >=3)


write.table(jag023_deleted ,file="02_jaguar023_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar # 24 -------------------------------------------------------------
j24 <- filter(jaguar, individual.local.identifier == "24")

##Seccion 2: resample data

# Selected data

jtrk24 <- mk_track(j24,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag024<- track_resample(jtrk24, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag024)

table(jag024$burst_)

which(step_lengths(jag024, lonlat=TRUE) > 10000)


jag024_deleted <- jag024

j24 <-  step_lengths(jag024_deleted,lonlat=TRUE)


#how many data with turn angle

jag024_end <- filter_min_n_burst(jag024_deleted, 1)

jag024_duplicated <- as.data.frame( table(jag024_end$burst_))

jag024_duplicated2 <- subset(jag024_duplicated , Freq >=3)



write.table(jag024_deleted ,file="02_jaguar024_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #25 --------------------------------------------------------------
j25 <- filter(jaguar, individual.local.identifier == "15")


j25$hour <- hour(j25$timestampTZ) #create a column
j25$SchedDiff <-  ifelse(j25$hour <9 | 19< j25$hour, "1", "2")

##Seccion 2: resample data

# Selected data 15.1

jtrk25 <- mk_track(j25,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j25.1 <- jtrk25 %>%
  filter(SchedDiff==1)

j25.2 <- jtrk25 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag025.1<- track_resample(j25.1, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag025.1)

table(jag025.1$burst_)

which(step_lengths(jag025.1, lonlat=TRUE) > 10000)


j25.1 <-  step_lengths(jag025.1,lonlat=TRUE)


#how many data with turn angle

jag025.1_end <- filter_min_n_burst(jag025.1, 1)

jag025.1_duplicated <- as.data.frame( table(jag025.1_end$burst_))

jag025.1_duplicated2 <- subset(jag025.1_duplicated , Freq >=3)



write.table(jag025.1,file="02_jaguar025.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 15.2

step_duration <- 2
jag025.2<- track_resample(j25.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag025.2)

table(jag025.2$burst_)

which(step_lengths(jag025.2, lonlat=TRUE) > 10000)

jag025.2_deleted <- jag025.2[-c(6,34,45,63,202,294),]

j25.2 <-  step_lengths(jag025.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag025.2_end <- filter_min_n_burst(jag025.2_deleted, 1)

jag025.2_duplicated <- as.data.frame( table(jag025.2_end$burst_))

jag025.2_duplicated2 <- subset(jag025.2_duplicated , Freq >=3)


write.table(jag025.2,file="02_jaguar025.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 26 -------------------------------------------------------------
j26 <- filter(jaguar, individual.local.identifier == "26")


# Selected data

jtrk26 <- mk_track(j26,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag026<- track_resample(jtrk26, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag026)

table(jag026$burst_)

which(step_lengths(jag026, lonlat=TRUE) > 10000)


jag026_deleted <- jag026

j26 <-  step_lengths(jag026_deleted,lonlat=TRUE)



#how many data with turn angle

jag026_end <- filter_min_n_burst(jag026_deleted, 1)

jag026_duplicated <- as.data.frame( table(jag026_end$burst_))

jag026_duplicated2 <- subset(jag026_duplicated , Freq >=3)



write.table(jag026_deleted ,file="02_jaguar026_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar# 27 --------------------------------------------------------------

j27 <- filter(jaguar, individual.local.identifier == "27")


##Seccion 2: resample data

# Selected data

jtrk27 <- mk_track(j27,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag027<- track_resample(jtrk27, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag027)

table(jag027$burst_)

which(step_lengths(jag027, lonlat=TRUE) > 10000)


jag027_deleted <- jag027

j27 <-  step_lengths(jag027_deleted,lonlat=TRUE)


#how many data with turn angle

jag027_end <- filter_min_n_burst(jag027_deleted, 1)

jag027_duplicated <- as.data.frame( table(jag027_end$burst_))

jag027_duplicated2 <- subset(jag027_duplicated , Freq >=3)



write.table(jag027_deleted ,file="02_jaguar027_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #28 --------------------------------------------------------------

j28 <- filter(jaguar, individual.local.identifier == "28")

##Seccion 2: resample data

# Selected data

jtrk28 <- mk_track(j28,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag028<- track_resample(jtrk28, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag028)

table(jag028$burst_)

which(step_lengths(jag028, lonlat=TRUE) > 10000)


jag028_deleted <- jag028
j28 <-  step_lengths(jag028_deleted,lonlat=TRUE)


#how many data with turn angle

jag028_end <- filter_min_n_burst(jag028_deleted, 1)

jag028_duplicated <- as.data.frame( table(jag028_end$burst_))

jag028_duplicated2 <- subset(jag028_duplicated , Freq >=3)



write.table(jag028_deleted ,file="02_jaguar028_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 29  ------------------------------------------------------------


j29 <- filter(jaguar, individual.local.identifier == "29")


##Seccion 2: resample data

# Selected data

jtrk29 <- mk_track(j29,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag029<- track_resample(jtrk29, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag029)

table(jag029$burst_)

which(step_lengths(jag029, lonlat=TRUE) > 10000)


jag029_deleted <- jag029
j29 <-  step_lengths(jag029_deleted,lonlat=TRUE)


#how many data with turn angle

jag029_end <- filter_min_n_burst(jag029_deleted, 1)

jag029_duplicated <- as.data.frame( table(jag029_end$burst_))

jag029_duplicated2 <- subset(jag029_duplicated , Freq >=3)



write.table(jag029_deleted ,file="02_jaguar029_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar # 30 -------------------------------------------------------------

j30 <- filter(jaguar, individual.local.identifier == "30")

##Seccion 2: resample data

# Selected data

jtrk30 <- mk_track(j30,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag030<- track_resample(jtrk30, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag030)

table(jag030$burst_)

which(step_lengths(jag030, lonlat=TRUE) > 10000)


jag030_deleted <- jag030

j30 <-  step_lengths(jag030_deleted,lonlat=TRUE)


#how many data with turn angle

jag030_end <- filter_min_n_burst(jag030_deleted, 1)

jag030_duplicated <- as.data.frame( table(jag030_end$burst_))

jag030_duplicated2 <- subset(jag030_duplicated , Freq >=3)



write.table(jag030_deleted ,file="02_jaguar030_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


#  Jaguar # 31 ------------------------------------------------------------

j31 <- filter(jaguar, individual.local.identifier == "31")

##Seccion 2: resample data

# Selected data

jtrk31 <- mk_track(j31,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag031<- track_resample(jtrk31, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag031)

table(jag031$burst_)

which(step_lengths(jag031, lonlat=TRUE) > 10000)


jag031_deleted <- jag031
j31 <-  step_lengths(jag031_deleted,lonlat=TRUE)



#how many data with turn angle

jag031_end <- filter_min_n_burst(jag031_deleted, 1)

jag031_duplicated <- as.data.frame( table(jag031_end$burst_))

jag031_duplicated2 <- subset(jag031_duplicated , Freq >=3)



write.table(jag031_deleted ,file="02_jaguar031_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 32 -------------------------------------------------------------

j32 <- filter(jaguar, individual.local.identifier == "32")


##Seccion 2: resample data

# Selected data

jtrk32 <- mk_track(j32,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag032<- track_resample(jtrk32, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag032)

table(jag032$burst_)

which(step_lengths(jag032, lonlat=TRUE) > 10000)


jag032_deleted <- jag032

j32 <-  step_lengths(jag032_deleted,lonlat=TRUE)


#how many data with turn angle

jag032_end <- filter_min_n_burst(jag032_deleted, 1)

jag032_duplicated <- as.data.frame( table(jag032_end$burst_))

jag032_duplicated2 <- subset(jag032_duplicated , Freq >=3)



write.table(jag032_deleted ,file="02_jaguar032_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #33 --------------------------------------------------------------

j33 <- filter(jaguar, individual.local.identifier == "33")


##Seccion 2: resample data

# Selected data

jtrk33 <- mk_track(j33,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag033<- track_resample(jtrk33, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag033)

table(jag033$burst_)

which(step_lengths(jag033, lonlat=TRUE) > 10000)


jag033_deleted <- jag033

j33 <-  step_lengths(jag033_deleted,lonlat=TRUE)


#how many data with turn angle

jag033_end <- filter_min_n_burst(jag033_deleted, 1)

jag033_duplicated <- as.data.frame( table(jag033_end$burst_))

jag033_duplicated2 <- subset(jag033_duplicated , Freq >=3)



write.table(jag033_deleted ,file="02_jaguar033_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #34 --------------------------------------------------------------

j34 <- filter(jaguar, individual.local.identifier == "34")

##Seccion 2: resample data

# Selected data

jtrk34 <- mk_track(j34,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 24
jag034<- track_resample(jtrk34, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag034)

table(jag034$burst_)

which(step_lengths(jag034, lonlat=TRUE) > 10000)


jag034_deleted <- jag034

j34 <-  step_lengths(jag034_deleted,lonlat=TRUE)


#how many data with turn angle

jag034_end <- filter_min_n_burst(jag034_deleted, 1)

jag034_duplicated <- as.data.frame( table(jag034_end$burst_))

jag034_duplicated2 <- subset(jag034_duplicated , Freq >=3)



write.table(jag034_deleted ,file="02_jaguar034_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 35 -------------------------------------------------------------

j35 <- filter(jaguar, individual.local.identifier == "35")


##Seccion 2: resample data

# Selected data

jtrk35 <- mk_track(j35,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 8
jag035<- track_resample(jtrk35, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag035)

table(jag035$burst_)

which(step_lengths(jag035, lonlat=TRUE) > 10000)


jag035_deleted <- jag035[-c(2,13,14),]

j35 <-  step_lengths(jag035_deleted,lonlat=TRUE)



#how many data with turn angle

jag035_end <- filter_min_n_burst(jag035_deleted, 1)

jag035_duplicated <- as.data.frame( table(jag035_end$burst_))

jag035_duplicated2 <- subset(jag035_duplicated , Freq >=3)



write.table(jag035_deleted ,file="02_jaguar035_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 36 -------------------------------------------------------------

j36 <- filter(jaguar, individual.local.identifier == "36")


##Seccion 2: resample data

# Selected data

jtrk36 <- mk_track(j36,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 8
jag036<- track_resample(jtrk36, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag036)

table(jag036$burst_)

which(step_lengths(jag036, lonlat=TRUE) > 10000)


jag036_deleted <- jag036[-c(3,14,15),]

j36 <-  step_lengths(jag036_deleted,lonlat=TRUE)


#how many data with turn angle

jag036_end <- filter_min_n_burst(jag036_deleted, 1)

jag036_duplicated <- as.data.frame( table(jag036_end$burst_))

jag036_duplicated2 <- subset(jag036_duplicated , Freq >=3)



write.table(jag036_deleted ,file="02_jaguar036_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar# 37 --------------------------------------------------------------

j37 <- filter(jaguar, individual.local.identifier == "37")


##Seccion 2: resample data

# Selected data

jtrk37 <- mk_track(j37,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 24
jag037<- track_resample(jtrk37, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag037)

table(jag037$burst_)

which(step_lengths(jag037, lonlat=TRUE) > 10000)


jag037_deleted <- jag037

j37 <-  step_lengths(jag037_deleted,lonlat=TRUE)


#how many data with turn angle

jag037_end <- filter_min_n_burst(jag037_deleted, 1)

jag037_duplicated <- as.data.frame( table(jag037_end$burst_))

jag037_duplicated2 <- subset(jag037_duplicated , Freq >=3)



write.table(jag037_deleted ,file="02_jaguar037_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #38 --------------------------------------------------------------

j38 <- filter(jaguar, individual.local.identifier == "38")


##Seccion 2: resample data

# Selected data

jtrk38 <- mk_track(j38,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 8
jag038<- track_resample(jtrk38, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag038)

table(jag038$burst_)

which(step_lengths(jag038, lonlat=TRUE) > 10000)


jag038_deleted <- jag038[-c(12,15),]

j38 <-  step_lengths(jag038_deleted,lonlat=TRUE)



#how many data with turn angle

jag038_end <- filter_min_n_burst(jag038_deleted, 1)

jag038_duplicated <- as.data.frame( table(jag038_end$burst_))

jag038_duplicated2 <- subset(jag038_duplicated , Freq >=3)



write.table(jag038_deleted ,file="02_jaguar038_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #39 --------------------------------------------------------------

j39 <- filter(jaguar, individual.local.identifier == "39")


##Seccion 2: resample data

# Selected data

jtrk39 <- mk_track(j39,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 8
jag039<- track_resample(jtrk39, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag039)

table(jag039$burst_)

which(step_lengths(jag039, lonlat=TRUE) > 10000)


jag039_deleted <- jag039[-c(21,22,23,35),]

j39 <-  step_lengths(jag039_deleted,lonlat=TRUE)



#how many data with turn angle

jag039_end <- filter_min_n_burst(jag039_deleted, 1)

jag039_duplicated <- as.data.frame( table(jag039_end$burst_))

jag039_duplicated2 <- subset(jag039_duplicated , Freq >=3)



write.table(jag039_deleted ,file="02_jaguar039_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #40 --------------------------------------------------------------

j40 <- filter(jaguar, individual.local.identifier == "40")


##Seccion 2: resample data

# Selected data

jtrk40 <- mk_track(j40,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 8
jag040<- track_resample(jtrk40, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag040)

table(jag040$burst_)

which(step_lengths(jag040, lonlat=TRUE) > 10000)


jag040_deleted <- jag040[-c(6,124),]

j40 <-  step_lengths(jag040_deleted,lonlat=TRUE)


#how many data with turn angle

jag040_end <- filter_min_n_burst(jag040_deleted, 1)

jag040_duplicated <- as.data.frame( table(jag040_end$burst_))

jag040_duplicated2 <- subset(jag040_duplicated , Freq >=3)



write.table(jag040_deleted ,file="02_jaguar040_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #41 --------------------------------------------------------------

j41 <- filter(jaguar, individual.local.identifier == "41")


##Seccion 2: resample data

# Selected data

jtrk41 <- mk_track(j41,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag041<- track_resample(jtrk41, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag041)

table(jag041$burst_)

which(step_lengths(jag041, lonlat=TRUE) > 10000)


jag041_deleted <- jag041
j41 <-  step_lengths(jag041_deleted,lonlat=TRUE)


#how many data with turn angle

jag041_end <- filter_min_n_burst(jag041_deleted, 1)

jag041_duplicated <- as.data.frame( table(jag041_end$burst_))

jag041_duplicated2 <- subset(jag041_duplicated , Freq >=3)



write.table(jag041_deleted ,file="02_jaguar041_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#42 ---------------------------------------------------------------

j42 <- filter(jaguar, individual.local.identifier == "42")


##Seccion 2: resample data

# Selected data

jtrk42 <- mk_track(j42,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 30
jag042<- track_resample(jtrk42, minutes(step_duration), tolerance = minutes(45))

summarize_sampling_rate(jag042)

table(jag042$burst_)

which(step_lengths(jag042, lonlat=TRUE) > 10000)


jag042_deleted <- jag042[-c(4911),]

j42 <-  step_lengths(jag042_deleted,lonlat=TRUE)


#how many data with turn angle

jag042_end <- filter_min_n_burst(jag042_deleted, 1)

jag042_duplicated <- as.data.frame( table(jag042_end$burst_))

jag042_duplicated2 <- subset(jag042_duplicated , Freq >=3)



write.table(jag042_deleted ,file="02_jaguar042_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #43 --------------------------------------------------------------
j43 <- filter(jaguar, individual.local.identifier == "43")



j43$hour <- hour(j43$timestampTZ) #create a column
j43$SchedDiff <-  ifelse(j43$hour <12 | 22< j43$hour, "6", "3")

##Seccion 2: resample data

# Selected data 43.1

jtrk43 <- mk_track(j43,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j43.1 <- jtrk43 %>%
  filter(SchedDiff==6)

j43.2 <- jtrk43 %>%
  filter(SchedDiff==3)

step_duration <- 6
jag043.1<- track_resample(j43.1, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag043.1)

table(jag043.1$burst_)

which(step_lengths(jag043.1, lonlat=TRUE) > 10000)

jag043.1_deleted <- jag043.1[-c(92),]

j43.1 <-  step_lengths(jag043.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag043.1_end <- filter_min_n_burst(jag043.1, 1)

jag043.1_duplicated <- as.data.frame( table(jag043.1_end$burst_))

jag043.1_duplicated2 <- subset(jag043.1_duplicated , Freq >=3)



write.table(jag043.1_deleted,file="02_jaguar043.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 43.2

step_duration <- 3
jag043.2<- track_resample(j43.2, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag043.2)

table(jag043.2$burst_)

which(step_lengths(jag043.2, lonlat=TRUE) > 10000)

jag043.2_deleted <- jag043.2[-c(98),]

j43.2 <-  step_lengths(jag043.2_deleted,lonlat=TRUE)



#how many data with turn angle

jag043.2_end <- filter_min_n_burst(jag043.2_deleted, 1)

jag043.2_duplicated <- as.data.frame( table(jag043.2_end$burst_))

jag043.2_duplicated2 <- subset(jag043.2_duplicated , Freq >=3)



write.table(jag043.2_deleted,file="02_jaguar043.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 44 -------------------------------------------------------------
j44 <- filter(jaguar, individual.local.identifier == "44")


##Seccion 2: resample data

# Selected data

jtrk44 <- mk_track(j44,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag044<- track_resample(jtrk44, hours(step_duration), tolerance = minutes(55))

summarize_sampling_rate(jag044)

table(jag044$burst_)

which(step_lengths(jag044, lonlat=TRUE) > 10000)


jag044_deleted <- jag044[-c(20, 28, 62, 66,88,89,102),]

j44 <-  step_lengths(jag044_deleted,lonlat=TRUE)



#how many data with turn angle

jag044_end <- filter_min_n_burst(jag044_deleted, 1)

jag044_duplicated <- as.data.frame( table(jag044_end$burst_))

jag044_duplicated2 <- subset(jag044_duplicated , Freq >=3)



write.table(jag044_deleted ,file="02_jaguar044_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #45 --------------------------------------------------------------
j45 <- filter(jaguar, individual.local.identifier == "45")

##Seccion 2: resample data

# Selected data

jtrk45 <- mk_track(j45,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag045<- track_resample(jtrk45, hours(step_duration), tolerance = minutes(55))

summarize_sampling_rate(jag045)

table(jag045$burst_)

which(step_lengths(jag045, lonlat=TRUE) > 10000)


jag045_deleted <- jag045

j45 <-  step_lengths(jag045_deleted,lonlat=TRUE)


#how many data with turn angle

jag045_end <- filter_min_n_burst(jag045_deleted, 1)

jag045_duplicated <- as.data.frame( table(jag045_end$burst_))

jag045_duplicated2 <- subset(jag045_duplicated , Freq >=3)



write.table(jag045_deleted ,file="02_jaguar045_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#46 ---------------------------------------------------------------

j46 <- filter(jaguar, individual.local.identifier == "46")

##Seccion 2: resample data

# Selected data

jtrk46 <- mk_track(j46,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag046<- track_resample(jtrk46, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag046)

table(jag046$burst_)

which(step_lengths(jag046, lonlat=TRUE) > 10000)


jag046_deleted <- jag046[-c(334,394,395,402,412,431, 433),]

j46 <-  step_lengths(jag046_deleted,lonlat=TRUE)


#how many data with turn angle

jag046_end <- filter_min_n_burst(jag046_deleted, 1)

jag046_duplicated <- as.data.frame( table(jag046_end$burst_))

jag046_duplicated2 <- subset(jag046_duplicated , Freq >=3)



write.table(jag046_deleted ,file="02_jaguar046_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar# 47 ---------------------------------------------------------------
j47 <- filter(jaguar, individual.local.identifier == "47")


##Seccion 2: resample data

# Selected data

jtrk47 <- mk_track(j47,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 10
jag047<- track_resample(jtrk47, hours(step_duration), tolerance = minutes(55))

summarize_sampling_rate(jag047)

table(jag047$burst_)

which(step_lengths(jag047, lonlat=TRUE) > 10000)


jag047_deleted <- jag047[-c(136, 215, 216, 250, 296, 351, 378, 447, 451),]

j47 <-  step_lengths(jag047_deleted,lonlat=TRUE)


#how many data with turn angle

jag047_end <- filter_min_n_burst(jag047_deleted, 1)

jag047_duplicated <- as.data.frame( table(jag047_end$burst_))

jag047_duplicated2 <- subset(jag047_duplicated , Freq >=3)



write.table(jag047_deleted ,file="02_jaguar047_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 48 -------------------------------------------------------------
j48 <- filter(jaguar, individual.local.identifier == "48")


##Seccion 2: resample data

# Selected data

jtrk48 <- mk_track(j48,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 10
jag048<- track_resample(jtrk48, hours(step_duration), tolerance = minutes(55))

summarize_sampling_rate(jag048)

table(jag048$burst_)

which(step_lengths(jag048, lonlat=TRUE) > 10000)


jag048_deleted <- jag048[-c(8, 10, 12, 16, 18, 26, 33, 36, 40, 42, 45),]

j48 <-  step_lengths(jag048_deleted,lonlat=TRUE)


#how many data with turn angle

jag048_end <- filter_min_n_burst(jag048_deleted, 1)

jag048_duplicated <- as.data.frame( table(jag048_end$burst_))

jag048_duplicated2 <- subset(jag048_duplicated , Freq >=3)



write.table(jag048_deleted ,file="02_jaguar048_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 49 -------------------------------------------------------------

j49 <- filter(jaguar, individual.local.identifier == "49")


##Seccion 2: resample data

# Selected data

jtrk49 <- mk_track(j49,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag049<- track_resample(jtrk49, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag049)

table(jag049$burst_)

which(step_lengths(jag049, lonlat=TRUE) > 10000)


jag049_deleted <- jag049[-c(94,129,284,291),]

j49 <-  step_lengths(jag049_deleted,lonlat=TRUE)


#how many data with turn angle

jag049_end <- filter_min_n_burst(jag049_deleted, 1)

jag049_duplicated <- as.data.frame( table(jag049_end$burst_))

jag049_duplicated2 <- subset(jag049_duplicated , Freq >=3)



write.table(jag049_deleted ,file="02_jaguar049_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #50 --------------------------------------------------------------

j50 <- filter(jaguar, individual.local.identifier == "50")


##Seccion 2: resample data

# Selected data

jtrk50 <- mk_track(j50,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag050<- track_resample(jtrk50, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag050)

table(jag050$burst_)

which(step_lengths(jag050, lonlat=TRUE) > 10000)


jag050_deleted <- jag050

j50 <-  step_lengths(jag050_deleted,lonlat=TRUE)



#how many data with turn angle

jag050_end <- filter_min_n_burst(jag050_deleted, 1)

jag050_duplicated <- as.data.frame( table(jag050_end$burst_))

jag050_duplicated2 <- subset(jag050_duplicated , Freq >=3)



write.table(jag050_deleted ,file="02_jaguar050_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 51 -------------------------------------------------------------

j51 <- filter(jaguar, individual.local.identifier == "51")


##Seccion 2: resample data

# Selected data

jtrk51 <- mk_track(j51,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag051<- track_resample(jtrk51, hours(step_duration), tolerance = minutes(55))

summarize_sampling_rate(jag051)

table(jag051$burst_)

which(step_lengths(jag051, lonlat=TRUE) > 10000)


jag051_deleted <- jag051[-c(328,668,674),]

j51 <-  step_lengths(jag051_deleted,lonlat=TRUE)


#how many data with turn angle

jag051_end <- filter_min_n_burst(jag051_deleted, 1)

jag051_duplicated <- as.data.frame( table(jag051_end$burst_))

jag051_duplicated2 <- subset(jag051_duplicated , Freq >=3)



write.table(jag051_deleted ,file="02_jaguar051_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 52 -------------------------------------------------------------

j52 <- filter(jaguar, individual.local.identifier == "52")


##Seccion 2: resample data

# Selected data

jtrk52 <- mk_track(j52,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag052<- track_resample(jtrk52, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag052)

table(jag052$burst_)

which(step_lengths(jag052, lonlat=TRUE) > 10000)


jag052_deleted <- jag052

j52 <-  step_lengths(jag052_deleted,lonlat=TRUE)


#how many data with turn angle

jag052_end <- filter_min_n_burst(jag052_deleted, 1)

jag052_duplicated <- as.data.frame( table(jag052_end$burst_))

jag052_duplicated2 <- subset(jag052_duplicated , Freq >=3)



write.table(jag052_deleted ,file="02_jaguar052_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 53 -------------------------------------------------------------
j53 <- filter(jaguar, individual.local.identifier == "53")



##Seccion 2: resample data

# Selected data

jtrk53 <- mk_track(j53,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag053<- track_resample(jtrk53, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag053)

table(jag053$burst_)

which(step_lengths(jag053, lonlat=TRUE) > 10000)


jag053_deleted <- jag053[-c(178,218),]

j53 <-  step_lengths(jag053_deleted,lonlat=TRUE)


#how many data with turn angle

jag053_end <- filter_min_n_burst(jag053_deleted, 1)

jag053_duplicated <- as.data.frame( table(jag053_end$burst_))

jag053_duplicated2 <- subset(jag053_duplicated , Freq >=3)



write.table(jag053_deleted ,file="02_jaguar053_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 54 -------------------------------------------------------------
j54 <- filter(jaguar, individual.local.identifier == "54")


##Seccion 2: resample data

# Selected data

jtrk54 <- mk_track(j54,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag054<- track_resample(jtrk54, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag054)

table(jag054$burst_)

which(step_lengths(jag054, lonlat=TRUE) > 10000)


jag054_deleted <- jag054
j54 <-  step_lengths(jag054_deleted,lonlat=TRUE)


#how many data with turn angle

jag054_end <- filter_min_n_burst(jag054_deleted, 1)

jag054_duplicated <- as.data.frame( table(jag054_end$burst_))

jag054_duplicated2 <- subset(jag054_duplicated , Freq >=3)



write.table(jag054_deleted ,file="02_jaguar054_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 55 -------------------------------------------------------------

j55 <- filter(jaguar, individual.local.identifier == "55")



##Seccion 2: resample data

# Selected data

jtrk55 <- mk_track(j55,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag055<- track_resample(jtrk55, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag055)

table(jag055$burst_)

which(step_lengths(jag055, lonlat=TRUE) > 10000)


jag055_deleted <- jag055

j55 <-  step_lengths(jag055_deleted,lonlat=TRUE)


#how many data with turn angle

jag055_end <- filter_min_n_burst(jag055_deleted, 1)

jag055_duplicated <- as.data.frame( table(jag055_end$burst_))

jag055_duplicated2 <- subset(jag055_duplicated , Freq >=3)



write.table(jag055_deleted ,file="02_jaguar055_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 56 -------------------------------------------------------------
j56 <- filter(jaguar, individual.local.identifier == "56")


##Seccion 2: resample data

# Selected data

jtrk56 <- mk_track(j56,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag056<- track_resample(jtrk56, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag056)

table(jag056$burst_)

which(step_lengths(jag056, lonlat=TRUE) > 10000)


jag056_deleted <- jag056

j56 <-  step_lengths(jag056_deleted,lonlat=TRUE)



#how many data with turn angle

jag056_end <- filter_min_n_burst(jag056_deleted, 1)

jag056_duplicated <- as.data.frame( table(jag056_end$burst_))

jag056_duplicated2 <- subset(jag056_duplicated , Freq >=3)



write.table(jag056_deleted ,file="02_jaguar056_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar# 57 --------------------------------------------------------------
j57 <- filter(jaguar, individual.local.identifier == "57")


##Seccion 2: resample data

# Selected data

jtrk57 <- mk_track(j57,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag057<- track_resample(jtrk57, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag057)

table(jag057$burst_)

which(step_lengths(jag057, lonlat=TRUE) > 10000)


jag057_deleted <- jag057

j57 <-  step_lengths(jag057_deleted,lonlat=TRUE)


#how many data with turn angle

jag057_end <- filter_min_n_burst(jag057_deleted, 1)

jag057_duplicated <- as.data.frame( table(jag057_end$burst_))

jag057_duplicated2 <- subset(jag057_duplicated , Freq >=3)



write.table(jag057_deleted ,file="02_jaguar057_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 58 -------------------------------------------------------------
j58 <- filter(jaguar, individual.local.identifier == "58")


##Seccion 2: resample data

# Selected data

jtrk58 <- mk_track(j58,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 24
jag058<- track_resample(jtrk58, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag058)

table(jag058$burst_)

which(step_lengths(jag058, lonlat=TRUE) > 10000)


jag058_deleted <- jag058[-c(34,41,70,78),]

j58 <-  step_lengths(jag058_deleted,lonlat=TRUE)


#how many data with turn angle

jag058_end <- filter_min_n_burst(jag058_deleted, 1)

jag058_duplicated <- as.data.frame( table(jag058_end$burst_))

jag058_duplicated2 <- subset(jag058_duplicated , Freq >=3)



write.table(jag058_deleted ,file="02_jaguar058_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 59 -------------------------------------------------------------
j59 <- filter(jaguar, individual.local.identifier == "59")


##Seccion 2: resample data

# Selected data

jtrk59 <- mk_track(j59,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag059<- track_resample(jtrk59, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag059)

table(jag059$burst_)

which(step_lengths(jag059, lonlat=TRUE) > 10000)


jag059_deleted <- jag059[-c(79,180,250,279,281,301,321,331,335,378,379),]

j59 <-  step_lengths(jag059_deleted,lonlat=TRUE)



#how many data with turn angle

jag059_end <- filter_min_n_burst(jag059_deleted, 1)

jag059_duplicated <- as.data.frame( table(jag059_end$burst_))

jag059_duplicated2 <- subset(jag059_duplicated , Freq >=3)



write.table(jag059_deleted ,file="02_jaguar059_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar # 60 -------------------------------------------------------------

j60 <- filter(jaguar, individual.local.identifier == "60")


##Seccion 2: resample data

# Selected data

jtrk60 <- mk_track(j60,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag060<- track_resample(jtrk60, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag060)

table(jag060$burst_)

which(step_lengths(jag060, lonlat=TRUE) > 10000)


jag060_deleted <- jag060[-c(599),]

j60 <-  step_lengths(jag060_deleted,lonlat=TRUE)


#how many data with turn angle

jag060_end <- filter_min_n_burst(jag060_deleted, 1)

jag060_duplicated <- as.data.frame( table(jag060_end$burst_))

jag060_duplicated2 <- subset(jag060_duplicated , Freq >=3)



write.table(jag060_deleted ,file="02_jaguar060_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #61 --------------------------------------------------------------
j61 <- filter(jaguar, individual.local.identifier == "61")


##Seccion 2: resample data

# Selected data

jtrk61 <- mk_track(j61,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag061<- track_resample(jtrk61, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag061)

table(jag061$burst_)

which(step_lengths(jag061, lonlat=TRUE) > 10000)


jag061_deleted <- jag061[-c(66),]

j61 <-  step_lengths(jag061_deleted,lonlat=TRUE)


#how many data with turn angle

jag061_end <- filter_min_n_burst(jag061_deleted, 1)

jag061_duplicated <- as.data.frame( table(jag061_end$burst_))

jag061_duplicated2 <- subset(jag061_duplicated , Freq >=3)



write.table(jag061_deleted ,file="02_jaguar061_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #62 --------------------------------------------------------------
j62 <- filter(jaguar, individual.local.identifier == "62")



##Seccion 2: resample data

# Selected data

jtrk62 <- mk_track(j62,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 12
jag062<- track_resample(jtrk62, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag062)

table(jag062$burst_)

which(step_lengths(jag062, lonlat=TRUE) > 10000)


jag062_deleted <- jag062[-c(11,12,13,16,20,21,23,30,33,35,44,45,47,57,62,68,70,85,86,87, 97,98, 99,105,107),]

j62 <-  step_lengths(jag062_deleted,lonlat=TRUE)


#how many data with turn angle

jag062_end <- filter_min_n_burst(jag062_deleted, 1)

jag062_duplicated <- as.data.frame( table(jag062_end$burst_))

jag062_duplicated2 <- subset(jag062_duplicated , Freq >=3)



write.table(jag062_deleted ,file="02_jaguar062_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #63 --------------------------------------------------------------
j63 <- filter(jaguar, individual.local.identifier == "63")


##Seccion 2: resample data

# Selected data

jtrk63 <- mk_track(j63,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag063<- track_resample(jtrk63, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag063)

table(jag063$burst_)

which(step_lengths(jag063, lonlat=TRUE) > 10000)


jag063_deleted <- jag063[-c(84,104,407,505,516,535,565,608,614,618,700,708,779),]

j63 <-  step_lengths(jag063_deleted,lonlat=TRUE)



#how many data with turn angle

jag063_end <- filter_min_n_burst(jag063_deleted, 1)

jag063_duplicated <- as.data.frame( table(jag063_end$burst_))

jag063_duplicated2 <- subset(jag063_duplicated , Freq >=3)



write.table(jag063_deleted ,file="02_jaguar063_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar#64 ---------------------------------------------------------------
j64 <- filter(jaguar, individual.local.identifier == "64")


##Seccion 2: resample data

# Selected data

jtrk64 <- mk_track(j64,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag064<- track_resample(jtrk64, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag064)

table(jag064$burst_)

which(step_lengths(jag064, lonlat=TRUE) > 10000)


jag064_deleted <- jag064[-c(879, 1020, 1023, 1025 ,1027, 1034, 1039, 1044, 1047, 1049, 1050, 1054, 1074, 1075, 1085, 1087,1101,1102 ,1103, 1110, 1111, 1113, 1117, 1119, 1121, 1124, 1125, 1134, 1137, 1167, 1168, 1175,1177, 1180, 1181, 1190, 1192, 1196, 1197, 1201, 1203, 1205, 1206, 1211, 1212, 1216, 1217, 1218,1219, 1226, 1227, 1237, 1239, 1242, 1243, 1246, 1248, 1249, 1250, 1255, 1257, 1260, 1265, 1269, 1270),]

j64 <-  step_lengths(jag064_deleted,lonlat=TRUE)



#how many data with turn angle

jag064_end <- filter_min_n_burst(jag064_deleted, 1)

jag064_duplicated <- as.data.frame( table(jag064_end$burst_))

jag064_duplicated2 <- subset(jag064_duplicated , Freq >=3)



write.table(jag064_deleted ,file="02_jaguar064_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #65 --------------------------------------------------------------

j65 <- filter(jaguar, individual.local.identifier == "65")



##Seccion 2: resample data

# Selected data

jtrk65 <- mk_track(j65,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag065<- track_resample(jtrk65, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag065)

table(jag065$burst_)

which(step_lengths(jag065, lonlat=TRUE) > 10000)


jag065_deleted <- jag065[-c(148,464,465),]

j65 <-  step_lengths(jag065_deleted,lonlat=TRUE)


#how many data with turn angle

jag065_end <- filter_min_n_burst(jag065_deleted, 1)

jag065_duplicated <- as.data.frame( table(jag065_end$burst_))

jag065_duplicated2 <- subset(jag065_duplicated , Freq >=3)



write.table(jag065_deleted ,file="02_jaguar065_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar#66 ---------------------------------------------------------------

j66 <- filter(jaguar, individual.local.identifier == "66")


##Seccion 2: resample data

# Selected data

jtrk66 <- mk_track(j66,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag066<- track_resample(jtrk66, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag066)

table(jag066$burst_)

which(step_lengths(jag066, lonlat=TRUE) > 10000)


jag066_deleted <- jag066[-c(1,33,44),]

j66 <-  step_lengths(jag066_deleted,lonlat=TRUE)



#how many data with turn angle

jag066_end <- filter_min_n_burst(jag066_deleted, 1)

jag066_duplicated <- as.data.frame( table(jag066_end$burst_))

jag066_duplicated2 <- subset(jag066_duplicated , Freq >=3)



write.table(jag066_deleted ,file="02_jaguar066_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #67 --------------------------------------------------------------
j67 <- filter(jaguar, individual.local.identifier == "67")


##Seccion 2: resample data

# Selected data

jtrk67 <- mk_track(j67,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 7
jag067<- track_resample(jtrk67, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag067)

table(jag067$burst_)

which(step_lengths(jag067, lonlat=TRUE) > 10000)


jag067_deleted <- jag067[-c(27),]

j67 <-  step_lengths(jag067_deleted,lonlat=TRUE)



#how many data with turn angle

jag067_end <- filter_min_n_burst(jag067_deleted, 1)

jag067_duplicated <- as.data.frame( table(jag067_end$burst_))

jag067_duplicated2 <- subset(jag067_duplicated , Freq >=3)



write.table(jag067_deleted ,file="02_jaguar067_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #68 --------------------------------------------------------------
j68 <- filter(jaguar, individual.local.identifier == "68")


j68$hour <- hour(j68$timestampTZ) #create a column
j68$SchedDiff <-  ifelse(j68$hour <5 | 11< j68$hour, "1", "0.5")

##Seccion 2: resample data

# Selected data 68.1

jtrk68 <- mk_track(j68,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j68.1 <- jtrk68 %>%
  filter(SchedDiff==1)

j68.2 <- jtrk68 %>%
  filter(SchedDiff==0.5)

step_duration <- 1
jag068.1<- track_resample(j68.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag068.1)

table(jag068.1$burst_)

which(step_lengths(jag068.1, lonlat=TRUE) > 10000)

jag068.1_deleted <- jag068.1[-c(406),]

j68.1 <-  step_lengths(jag068.1_deleted,lonlat=TRUE)



#how many data with turn angle

jag068.1_end <- filter_min_n_burst(jag068.1, 1)

jag068.1_duplicated <- as.data.frame( table(jag068.1_end$burst_))

jag068.1_duplicated2 <- subset(jag068.1_duplicated , Freq >=3)



write.table(jag068.1_deleted,file="02_jaguar068.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 68.2

step_duration <- 30
jag068.2<- track_resample(j68.2, minutes(step_duration), tolerance = minutes(35))


###

summarize_sampling_rate(jag068.2)

table(jag068.2$burst_)

which(step_lengths(jag068.2, lonlat=TRUE) > 10000)

jag068.2_deleted <- jag068.2[-c(444),]

j68.2 <-  step_lengths(jag068.2_deleted,lonlat=TRUE)



#how many data with turn angle

jag068.2_end <- filter_min_n_burst(jag068.2_deleted, 1)

jag068.2_duplicated <- as.data.frame( table(jag068.2_end$burst_))

jag068.2_duplicated2 <- subset(jag068.2_duplicated , Freq >=3)



write.table(jag068.2_deleted,file="02_jaguar068.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #69 --------------------------------------------------------------
j69 <- filter(jaguar, individual.local.identifier == "69")


j69$hour <- hour(j69$timestampTZ) #create a column
j69$SchedDiff <-  ifelse(j69$hour <10 | 22< j69$hour, "1", "2")

##Seccion 2: resample data

# Selected data 69.1

jtrk69 <- mk_track(j69,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j69.1 <- jtrk69 %>%
  filter(SchedDiff==1)

j69.2 <- jtrk69 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag069.1<- track_resample(j69.1, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag069.1)

table(jag069.1$burst_)

which(step_lengths(jag069.1, lonlat=TRUE) > 10000)

jag069.1_deleted <- jag069.1

j69.1 <-  step_lengths(jag069.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag069.1_end <- filter_min_n_burst(jag069.1, 1)

jag069.1_duplicated <- as.data.frame( table(jag069.1_end$burst_))

jag069.1_duplicated2 <- subset(jag069.1_duplicated , Freq >=3)



write.table(jag069.1_deleted,file="02_jaguar069.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 69.2

step_duration <- 2
jag069.2<- track_resample(j69.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag069.2)

table(jag069.2$burst_)

which(step_lengths(jag069.2, lonlat=TRUE) > 10000)

jag069.2_deleted <- jag069.2

j69.2 <-  step_lengths(jag069.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag069.2_end <- filter_min_n_burst(jag069.2_deleted, 1)

jag069.2_duplicated <- as.data.frame( table(jag069.2_end$burst_))

jag069.2_duplicated2 <- subset(jag069.2_duplicated , Freq >=3)



write.table(jag069.2_deleted,file="02_jaguar069.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #70 --------------------------------------------------------------
j70 <- filter(jaguar, individual.local.identifier == "70")


##Seccion 2: resample data

# Selected data

jtrk70 <- mk_track(j70,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag070<- track_resample(jtrk70, hours(step_duration), tolerance = minutes(45))

summarize_sampling_rate(jag070)

table(jag070$burst_)

which(step_lengths(jag070, lonlat=TRUE) > 10000)


jag070_deleted <- jag070[-c(22,115,127,129,197,213,378,386,389,410,413,454,474, 528,530,555,769,777,782,809,812, 827, 831, 846, 848, 851, 862, 880, 903,948,  974,988,997,1019, 1033, 1047, 1060, 1077, 1078,1079),]

j70 <-  step_lengths(jag070_deleted,lonlat=TRUE)



#how many data with turn angle

jag070_end <- filter_min_n_burst(jag070_deleted, 1)

jag070_duplicated <- as.data.frame( table(jag070_end$burst_))

jag070_duplicated2 <- subset(jag070_duplicated , Freq >=3)



write.table(jag070_deleted ,file="02_jaguar070_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #71 --------------------------------------------------------------
j71 <- filter(jaguar, individual.local.identifier == "71")

##Seccion 2: resample data

# Selected data

jtrk71 <- mk_track(j71,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag071<- track_resample(jtrk71, hours(step_duration), tolerance = minutes(45))

summarize_sampling_rate(jag071)

table(jag071$burst_)

which(step_lengths(jag071, lonlat=TRUE) > 10000)


jag071_deleted <- jag071[-c(581, 634, 794, 912),]

j71 <-  step_lengths(jag071_deleted,lonlat=TRUE)


#how many data with turn angle

jag071_end <- filter_min_n_burst(jag071_deleted, 1)

jag071_duplicated <- as.data.frame( table(jag071_end$burst_))

jag071_duplicated2 <- subset(jag071_duplicated , Freq >=3)



write.table(jag071_deleted ,file="02_jaguar071_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #72 --------------------------------------------------------------
j72 <- filter(jaguar, individual.local.identifier == "72")


##Seccion 2: resample data

# Selected data

jtrk72 <- mk_track(j72,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag072<- track_resample(jtrk72, hours(step_duration), tolerance = minutes(45))

summarize_sampling_rate(jag072)

table(jag072$burst_)

which(step_lengths(jag072, lonlat=TRUE) > 10000)


jag072_deleted <- jag072[-c(659, 672, 675, 676, 681, 691, 705),]

j72 <-  step_lengths(jag072_deleted,lonlat=TRUE)


#how many data with turn angle

jag072_end <- filter_min_n_burst(jag072_deleted, 1)

jag072_duplicated <- as.data.frame( table(jag072_end$burst_))

jag072_duplicated2 <- subset(jag072_duplicated , Freq >=3)



write.table(jag072_deleted ,file="02_jaguar072_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #73 --------------------------------------------------------------
j73 <- filter(jaguar, individual.local.identifier == "73")

##Seccion 2: resample data

# Selected data

jtrk73 <- mk_track(j73,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag073<- track_resample(jtrk73, hours(step_duration), tolerance = minutes(45))

summarize_sampling_rate(jag073)

table(jag073$burst_)

which(step_lengths(jag073, lonlat=TRUE) > 10000)


jag073_deleted <- jag073

j73 <-  step_lengths(jag073_deleted,lonlat=TRUE)



#how many data with turn angle

jag073_end <- filter_min_n_burst(jag073_deleted, 1)

jag073_duplicated <- as.data.frame( table(jag073_end$burst_))

jag073_duplicated2 <- subset(jag073_duplicated , Freq >=3)



write.table(jag073_deleted ,file="02_jaguar073_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #74 --------------------------------------------------------------

j74 <- filter(jaguar, individual.local.identifier == "74")


##Seccion 2: resample data

# Selected data

jtrk74 <- mk_track(j74,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag074<- track_resample(jtrk74, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag074)

table(jag074$burst_)

which(step_lengths(jag074, lonlat=TRUE) > 10000)


jag074_deleted <- jag074

j74 <-  step_lengths(jag074_deleted,lonlat=TRUE)


#how many data with turn angle

jag074_end <- filter_min_n_burst(jag074_deleted, 1)

jag074_duplicated <- as.data.frame( table(jag074_end$burst_))

jag074_duplicated2 <- subset(jag074_duplicated , Freq >=3)



write.table(jag074_deleted ,file="02_jaguar074_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #75 --------------------------------------------------------------

j75 <- filter(jaguar, individual.local.identifier == "75")


##Seccion 2: resample data

# Selected data

jtrk75 <- mk_track(j75,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag075<- track_resample(jtrk75, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag075)

table(jag075$burst_)

which(step_lengths(jag075, lonlat=TRUE) > 10000)


jag075_deleted <- jag075

j75 <-  step_lengths(jag075_deleted,lonlat=TRUE)



#how many data with turn angle

jag075_end <- filter_min_n_burst(jag075_deleted, 1)

jag075_duplicated <- as.data.frame( table(jag075_end$burst_))

jag075_duplicated2 <- subset(jag075_duplicated , Freq >=3)



write.table(jag075_deleted ,file="02_jaguar075_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #76 --------------------------------------------------------------

j76 <- filter(jaguar, individual.local.identifier == "76")


##Seccion 2: resample data

# Selected data

jtrk76 <- mk_track(j76,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag076<- track_resample(jtrk76, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag076)

table(jag076$burst_)

which(step_lengths(jag076, lonlat=TRUE) > 10000)


jag076_deleted <- jag076

j76 <-  step_lengths(jag076_deleted,lonlat=TRUE)


#how many data with turn angle

jag076_end <- filter_min_n_burst(jag076_deleted, 1)

jag076_duplicated <- as.data.frame( table(jag076_end$burst_))

jag076_duplicated2 <- subset(jag076_duplicated , Freq >=3)



write.table(jag076_deleted ,file="02_jaguar076_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar#77 ---------------------------------------------------------------

j77 <- filter(jaguar, individual.local.identifier == "77")


##Seccion 2: resample data

# Selected data

jtrk77 <- mk_track(j77,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag077<- track_resample(jtrk77, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag077)

table(jag077$burst_)

which(step_lengths(jag077, lonlat=TRUE) > 10000)


jag077_deleted <- jag077[-c(156,233,264,501,674,769,1087, 1114,1169,1287,1312, 1347,1352),]

j77 <-  step_lengths(jag077_deleted,lonlat=TRUE)



#how many data with turn angle

jag077_end <- filter_min_n_burst(jag077_deleted, 1)

jag077_duplicated <- as.data.frame( table(jag077_end$burst_))

jag077_duplicated2 <- subset(jag077_duplicated , Freq >=3)



write.table(jag077_deleted ,file="02_jaguar077_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #78 --------------------------------------------------------------

j78 <- filter(jaguar, individual.local.identifier == "78")


##Seccion 2: resample data

# Selected data

jtrk78 <- mk_track(j78,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag078<- track_resample(jtrk78, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag078)

table(jag078$burst_)

which(step_lengths(jag078, lonlat=TRUE) > 10000)


jag078_deleted <- jag078

j78 <-  step_lengths(jag078_deleted,lonlat=TRUE)



#how many data with turn angle

jag078_end <- filter_min_n_burst(jag078_deleted, 1)

jag078_duplicated <- as.data.frame( table(jag078_end$burst_))

jag078_duplicated2 <- subset(jag078_duplicated , Freq >=3)



write.table(jag078_deleted ,file="02_jaguar078_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #79 --------------------------------------------------------------
j79 <- filter(jaguar, individual.local.identifier == "79")


j79$hour <- hour(j79$timestampTZ) #create a column
j79$SchedDiff <-  ifelse(j79$hour <10| 21< j79$hour, "1", "2")

##Seccion 2: resample data

# Selected data 79.1

jtrk79 <- mk_track(j79,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j79.1 <- jtrk79 %>%
  filter(SchedDiff==1)

j79.2 <- jtrk79 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag079.1<- track_resample(j79.1, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag079.1)

table(jag079.1$burst_)

which(step_lengths(jag079.1, lonlat=TRUE) > 10000)

jag079.1_deleted <- jag079.1

j79.1 <-  step_lengths(jag079.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag079.1_end <- filter_min_n_burst(jag079.1, 1)

jag079.1_duplicated <- as.data.frame( table(jag079.1_end$burst_))

jag079.1_duplicated2 <- subset(jag079.1_duplicated , Freq >=3)



write.table(jag079.1_deleted,file="02_jaguar079.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 79.2

step_duration <- 2
jag079.2<- track_resample(j79.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag079.2)

table(jag079.2$burst_)

which(step_lengths(jag079.2, lonlat=TRUE) > 10000)

jag079.2_deleted <- jag079.2[-c(63),]

j79.2 <-  step_lengths(jag079.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag079.2_end <- filter_min_n_burst(jag079.2_deleted, 1)

jag079.2_duplicated <- as.data.frame( table(jag079.2_end$burst_))

jag0792_duplicated2 <- subset(jag079.2_duplicated , Freq >=3)



write.table(jag079.2_deleted,file="02_jaguar079.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #80 --------------------------------------------------------------
j80 <- filter(jaguar, individual.local.identifier == "80")


##Seccion 2: resample data

# Selected data

jtrk80 <- mk_track(j80,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 90
jag080<- track_resample(jtrk80, minutes(step_duration), tolerance = minutes(105))

summarize_sampling_rate(jag080)

table(jag080$burst_)

which(step_lengths(jag080, lonlat=TRUE) > 10000)


jag080_deleted <- jag080

j80 <-  step_lengths(jag080_deleted,lonlat=TRUE)


#how many data with turn angle

jag080_end <- filter_min_n_burst(jag080_deleted, 1)

jag080_duplicated <- as.data.frame( table(jag080_end$burst_))

jag080_duplicated2 <- subset(jag080_duplicated , Freq >=3)



write.table(jag080_deleted ,file="02_jaguar080_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #81 --------------------------------------------------------------
j81 <- filter(jaguar, individual.local.identifier == "81")


j81$hour <- hour(j81$timestampTZ) #create a column
j81$SchedDiff <-  ifelse(j81$hour <13| 19< j81$hour, "1", "2")

##Seccion 2: resample data

# Selected data 81.1

jtrk81 <- mk_track(j81,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j81.1 <- jtrk81 %>%
  filter(SchedDiff==1)

j81.2 <- jtrk81 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag081.1<- track_resample(j81.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag081.1)

table(jag081.1$burst_)

which(step_lengths(jag081.1, lonlat=TRUE) > 10000)

jag081.1_deleted <- jag081.1

j81.1 <-  step_lengths(jag081.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag081.1_end <- filter_min_n_burst(jag081.1, 1)

jag081.1_duplicated <- as.data.frame( table(jag081.1_end$burst_))

jag081.1_duplicated2 <- subset(jag081.1_duplicated , Freq >=3)



write.table(jag081.1_deleted,file="02_jaguar081.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 81.2

step_duration <- 2
jag081.2<- track_resample(j81.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag081.2)

table(jag081.2$burst_)

which(step_lengths(jag081.2, lonlat=TRUE) > 10000)

jag081.2_deleted <- jag081.2

j81.2 <-  step_lengths(jag081.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag081.2_end <- filter_min_n_burst(jag081.2_deleted, 1)

jag081.2_duplicated <- as.data.frame( table(jag081.2_end$burst_))

jag081.2_duplicated2 <- subset(jag081.2_duplicated , Freq >=3)



write.table(jag081.2_deleted,file="02_jaguar081.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #82 --------------------------------------------------------------

j82 <- filter(jaguar, individual.local.identifier == "82")


##Seccion 2: resample data

# Selected data

jtrk82 <- mk_track(j82,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag082<- track_resample(jtrk82, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag082)

table(jag082$burst_)

which(step_lengths(jag082, lonlat=TRUE) > 10000)


jag082_deleted <- jag082[-c(121,157,234,249,333,383,427,437,446,471,512,562,600,636,647,692,694,702,704,723,729,734,749,757,764,786,793,794,798,801,808,811,
                            812,825,827,830,834,847,856,878,879, 882,884,893,926,943,965,981,982,988,1012, 1018,1019,1022,1023),]

j82 <-  step_lengths(jag082_deleted,lonlat=TRUE)

#how many data with turn angle

jag082_end <- filter_min_n_burst(jag082_deleted, 1)

jag082_duplicated <- as.data.frame( table(jag082_end$burst_))

jag082_duplicated2 <- subset(jag082_duplicated , Freq >=3)



write.table(jag082_deleted ,file="02_jaguar082_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #83 --------------------------------------------------------------

j83 <- filter(jaguar, individual.local.identifier == "83")


##Seccion 2: resample data

# Selected data

jtrk83 <- mk_track(j83,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag083<- track_resample(jtrk83, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag083)

table(jag083$burst_)

which(step_lengths(jag083, lonlat=TRUE) > 10000)


jag083_deleted <- jag083[-c(56),]

j83 <-  step_lengths(jag083_deleted,lonlat=TRUE)


#how many data with turn angle

jag083_end <- filter_min_n_burst(jag083_deleted, 1)

jag083_duplicated <- as.data.frame( table(jag083_end$burst_))

jag083_duplicated2 <- subset(jag083_duplicated , Freq >=3)



write.table(jag083_deleted ,file="02_jaguar083_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #84 --------------------------------------------------------------
j84 <- filter(jaguar, individual.local.identifier == "84")



j84$hour <- hour(j84$timestampTZ) #create a column
j84$SchedDiff <-  ifelse(j84$hour <10| 20< j84$hour, "1", "2")

##Seccion 2: resample data

# Selected data 84.1

jtrk84 <- mk_track(j84,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j84.1 <- jtrk84 %>%
  filter(SchedDiff==1)

j84.2 <- jtrk84 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag084.1<- track_resample(j84.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag084.1)

table(jag084.1$burst_)

which(step_lengths(jag084.1, lonlat=TRUE) > 10000)

jag084.1_deleted <- jag084.1

j84.1 <-  step_lengths(jag084.1_deleted,lonlat=TRUE)



#how many data with turn angle

jag084.1_end <- filter_min_n_burst(jag084.1, 1)

jag084.1_duplicated <- as.data.frame( table(jag084.1_end$burst_))

jag084.1_duplicated2 <- subset(jag084.1_duplicated , Freq >=3)



write.table(jag084.1_deleted,file="02_jaguar084.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 84.2

step_duration <- 2
jag084.2<- track_resample(j84.2, hours(step_duration), tolerance = minutes(15))



###

summarize_sampling_rate(jag084.2)

table(jag084.2$burst_)

which(step_lengths(jag084.2, lonlat=TRUE) > 10000)

jag084.2_deleted <- jag084.2[-c(84),]

j84.2 <-  step_lengths(jag084.2_deleted,lonlat=TRUE)



#how many data with turn angle

jag084.2_end <- filter_min_n_burst(jag084.2_deleted, 1)

jag084.2_duplicated <- as.data.frame( table(jag084.2_end$burst_))

jag084.2_duplicated2 <- subset(jag084.2_duplicated , Freq >=3)



write.table(jag084.2_deleted,file="02_jaguar084.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #85 --------------------------------------------------------------

j85 <- filter(jaguar, individual.local.identifier == "85")


##Seccion 2: resample data

# Selected data

jtrk85 <- mk_track(j85,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag085<- track_resample(jtrk85, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag085)

table(jag085$burst_)

which(step_lengths(jag085, lonlat=TRUE) > 10000)


jag085_deleted <- jag085[-c(26,348,367),]

j85 <-  step_lengths(jag085_deleted,lonlat=TRUE)


#how many data with turn angle

jag085_end <- filter_min_n_burst(jag085_deleted, 1)

jag085_duplicated <- as.data.frame( table(jag085_end$burst_))

jag085_duplicated2 <- subset(jag085_duplicated , Freq >=3)



write.table(jag085_deleted ,file="02_jaguar085_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #86 --------------------------------------------------------------
j86 <- filter(jaguar, individual.local.identifier == "86")


j86$hour <- hour(j86$timestampTZ) #create a column
j86$SchedDiff <-  ifelse(j86$hour <11| 21< j86$hour, "1", "2")

##Seccion 2: resample data

# Selected data 86.1

jtrk86 <- mk_track(j86,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j86.1 <- jtrk86 %>%
  filter(SchedDiff==1)

j86.2 <- jtrk86 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag086.1<- track_resample(j86.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag086.1)

table(jag086.1$burst_)

which(step_lengths(jag086.1, lonlat=TRUE) > 10000)

jag086.1_deleted <- jag086.1

j86.1 <-  step_lengths(jag086.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag086.1_end <- filter_min_n_burst(jag086.1, 1)

jag086.1_duplicated <- as.data.frame( table(jag086.1_end$burst_))

jag086.1_duplicated2 <- subset(jag086.1_duplicated , Freq >=3)



write.table(jag086.1_deleted,file="02_jaguar086.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 86.2

step_duration <- 2
jag086.2<- track_resample(j86.2, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag086.2)

table(jag086.2$burst_)

which(step_lengths(jag086.2, lonlat=TRUE) > 10000)

jag086.2_deleted <- jag086.2

j86.2 <-  step_lengths(jag086.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag086.2_end <- filter_min_n_burst(jag086.2_deleted, 1)

jag086.2_duplicated <- as.data.frame( table(jag086.2_end$burst_))

jag086.2_duplicated2 <- subset(jag086.2_duplicated , Freq >=3)



write.table(jag086.2_deleted,file="02_jaguar086.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #87 --------------------------------------------------------------
j87<- filter(jaguar, individual.local.identifier == "87")



j87$hour <- hour(j87$timestampTZ) #create a column
j87$SchedDiff <-  ifelse(j87$hour <6| 17< j87$hour, "1", "2")

##Seccion 2: resample data

# Selected data 87.1

jtrk87 <- mk_track(j87,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j87.1 <- jtrk87 %>%
  filter(SchedDiff==1)

j87.2 <- jtrk87 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag087.1<- track_resample(j87.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag087.1)

table(jag087.1$burst_)

which(step_lengths(jag087.1, lonlat=TRUE) > 10000)

jag087.1_deleted <- jag087.1

j87.1 <-  step_lengths(jag087.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag087.1_end <- filter_min_n_burst(jag087.1, 1)

jag087.1_duplicated <- as.data.frame( table(jag087.1_end$burst_))

jag087.1_duplicated2 <- subset(jag087.1_duplicated , Freq >=3)



write.table(jag087.1_deleted,file="02_jaguar087.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 87.2

step_duration <- 2
jag087.2<- track_resample(j87.2, hours(step_duration), tolerance = minutes(15))

###

summarize_sampling_rate(jag087.2)

table(jag087.2$burst_)

which(step_lengths(jag087.2, lonlat=TRUE) > 10000)

jag087.2_deleted <- jag087.2

j87.2 <-  step_lengths(jag087.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag087.2_end <- filter_min_n_burst(jag087.2_deleted, 1)

jag087.2_duplicated <- as.data.frame( table(jag087.2_end$burst_))

jag087.2_duplicated2 <- subset(jag087.2_duplicated , Freq >=3)



write.table(jag087.2_deleted,file="02_jaguar087.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #88 --------------------------------------------------------------
j88<- filter(jaguar, individual.local.identifier == "88")


j88$hour <- hour(j88$timestampTZ) #create a column
j88$SchedDiff <-  ifelse(j88$hour <12| 20< j88$hour, "1", "2")

##Seccion 2: resample data

# Selected data 88.1

jtrk88 <- mk_track(j88,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance, SchedDiff= SchedDiff)

j88.1 <- jtrk88 %>%
  filter(SchedDiff==1)

j88.2 <- jtrk88 %>%
  filter(SchedDiff==2)

step_duration <- 1
jag088.1<- track_resample(j88.1, hours(step_duration), tolerance = minutes(15))


###

summarize_sampling_rate(jag088.1)

table(jag088.1$burst_)

which(step_lengths(jag088.1, lonlat=TRUE) > 10000)

jag088.1_deleted <- jag088.1

j88.1 <-  step_lengths(jag088.1_deleted,lonlat=TRUE)



#how many data with turn angle

jag088.1_end <- filter_min_n_burst(jag088.1, 1)

jag088.1_duplicated <- as.data.frame( table(jag088.1_end$burst_))

jag088.1_duplicated2 <- subset(jag088.1_duplicated , Freq >=3)



write.table(jag088.1_deleted,file="02_jaguar088.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 88.2

step_duration <- 2
jag088.2<- track_resample(j88.2, hours(step_duration), tolerance = minutes(15))

###

summarize_sampling_rate(jag088.2)

table(jag088.2$burst_)

which(step_lengths(jag088.2, lonlat=TRUE) > 10000)

jag088.2_deleted <- jag088.2

j88.2 <-  step_lengths(jag088.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag088.2_end <- filter_min_n_burst(jag088.2_deleted, 1)

jag088.2_duplicated <- as.data.frame( table(jag088.2_end$burst_))

jag088.2_duplicated2 <- subset(jag088.2_duplicated , Freq >=3)



write.table(jag088.2_deleted,file="02_jaguar088.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #89 --------------------------------------------------------------
j89<- filter(jaguar, individual.local.identifier == "89")



#j89$hour <- hour(j89$timestampTZ) #create a column
#j89$SchedDiff <-  ifelse(j89$hour <12| 20< j89$hour, "1", "2")

##Seccion 2: resample data

# Selected data 89.1

jtrk89 <- mk_track(j89,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

j89.1 <- jtrk89[1:1633,]

j89.2 <- jtrk89[1634:2432,]

step_duration <- 1
jag089.1<- track_resample(j89.1, hours(step_duration), tolerance = minutes(15))

###

summarize_sampling_rate(jag089.1)

table(jag089.1$burst_)

which(step_lengths(jag089.1, lonlat=TRUE) > 10000)

jag089.1_deleted <- jag089.1

j89.1 <-  step_lengths(jag089.1_deleted,lonlat=TRUE)


#how many data with turn angle

jag089.1_end <- filter_min_n_burst(jag089.1, 1)

jag089.1_duplicated <- as.data.frame( table(jag089.1_end$burst_))

jag089.1_duplicated2 <- subset(jag089.1_duplicated , Freq >=3)



write.table(jag089.1_deleted,file="02_jaguar089.1_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Selected data 89.2

step_duration <- 210
jag089.2<- track_resample(j89.2, minutes(step_duration), tolerance = minutes(215))

###

summarize_sampling_rate(jag089.2)

table(jag089.2$burst_)

which(step_lengths(jag089.2, lonlat=TRUE) > 10000)

jag089.2_deleted <- jag089.2[-c(19, 22, 78,111,182, 208, 482, 567, 772),]

j89.2 <-  step_lengths(jag089.2_deleted,lonlat=TRUE)


#how many data with turn angle

jag089.2_end <- filter_min_n_burst(jag089.2_deleted, 1)

jag089.2_duplicated <- as.data.frame( table(jag089.2_end$burst_))

jag089.2_duplicated2 <- subset(jag089.2_duplicated , Freq >=3)



write.table(jag089.2_deleted,file="02_jaguar089.2_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #90 --------------------------------------------------------------

j90 <- filter(jaguar, individual.local.identifier == "90")


##Seccion 2: resample data

# Selected data

jtrk90 <- mk_track(j90,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag090<- track_resample(jtrk90, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag090)

table(jag090$burst_)

which(step_lengths(jag090, lonlat=TRUE) > 10000)


jag090_deleted <- jag090[-c(92,123,232,273),]

j90 <-  step_lengths(jag090_deleted,lonlat=TRUE)



#how many data with turn angle

jag090_end <- filter_min_n_burst(jag090_deleted, 1)

jag090_duplicated <- as.data.frame( table(jag090_end$burst_))

jag090_duplicated2 <- subset(jag090_duplicated , Freq >=3)



write.table(jag090_deleted ,file="02_jaguar090_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #91 --------------------------------------------------------------

j91 <- filter(jaguar, individual.local.identifier == "91")

##Seccion 2: resample data

# Selected data

jtrk91 <- mk_track(j91,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag091<- track_resample(jtrk91, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag091)

table(jag091$burst_)

which(step_lengths(jag091, lonlat=TRUE) > 10000)


jag091_deleted <- jag091[-c(5, 11, 40, 41, 44, 45),]

j91 <-  step_lengths(jag091_deleted,lonlat=TRUE)


#how many data with turn angle

jag091_end <- filter_min_n_burst(jag091_deleted, 1)

jag091_duplicated <- as.data.frame( table(jag091_end$burst_))

jag091_duplicated2 <- subset(jag091_duplicated , Freq >=3)



write.table(jag091_deleted ,file="02_jaguar091_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #92 --------------------------------------------------------------

j92 <- filter(jaguar, individual.local.identifier == "92")

##Seccion 2: resample data

# Selected data

jtrk92 <- mk_track(j92,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag092<- track_resample(jtrk92, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag092)

table(jag092$burst_)

which(step_lengths(jag092, lonlat=TRUE) > 10000)


jag092_deleted <- jag092[-c(7,12,14,25,27),]

j92 <-  step_lengths(jag092_deleted,lonlat=TRUE)



#how many data with turn angle

jag092_end <- filter_min_n_burst(jag092_deleted, 1)

jag092_duplicated <- as.data.frame( table(jag092_end$burst_))

jag092_duplicated2 <- subset(jag092_duplicated , Freq >=3)



write.table(jag092_deleted ,file="02_jaguar092_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #93 --------------------------------------------------------------

j93 <- filter(jaguar, individual.local.identifier == "93")


##Seccion 2: resample data

# Selected data

jtrk93 <- mk_track(j93,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag093<- track_resample(jtrk93, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag093)

table(jag093$burst_)

which(step_lengths(jag093, lonlat=TRUE) > 10000)


jag093_deleted <- jag093[-c(844),]

j93 <-  step_lengths(jag093_deleted,lonlat=TRUE)


#how many data with turn angle

jag093_end <- filter_min_n_burst(jag093_deleted, 1)

jag093_duplicated <- as.data.frame( table(jag093_end$burst_))

jag093_duplicated2 <- subset(jag093_duplicated , Freq >=3)



write.table(jag093_deleted ,file="02_jaguar093_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #94 --------------------------------------------------------------

j94 <- filter(jaguar, individual.local.identifier == "94")


##Seccion 2: resample data

# Selected data

jtrk94 <- mk_track(j94,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag094<- track_resample(jtrk94, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag094)

table(jag094$burst_)

which(step_lengths(jag094, lonlat=TRUE) > 10000)


jag094_deleted <- jag094[-c(118),]

j94 <-  step_lengths(jag094_deleted,lonlat=TRUE)


#how many data with turn angle

jag094_end <- filter_min_n_burst(jag094_deleted, 1)

jag094_duplicated <- as.data.frame( table(jag094_end$burst_))

jag094_duplicated2 <- subset(jag094_duplicated , Freq >=3)



write.table(jag094_deleted ,file="02_jaguar094_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #95 --------------------------------------------------------------
j95 <- filter(jaguar, individual.local.identifier == "95")


##Seccion 2: resample data

# Selected data

jtrk95 <- mk_track(j95,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag095<- track_resample(jtrk95, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag095)

table(jag095$burst_)

which(step_lengths(jag095, lonlat=TRUE) > 10000)


jag095_deleted <- jag095

j95 <-  step_lengths(jag095_deleted,lonlat=TRUE)


#how many data with turn angle

jag095_end <- filter_min_n_burst(jag095_deleted, 1)

jag095_duplicated <- as.data.frame( table(jag095_end$burst_))

jag095_duplicated2 <- subset(jag095_duplicated , Freq >=3)



write.table(jag095_deleted ,file="02_jaguar095_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #96 --------------------------------------------------------------
j96 <- filter(jaguar, individual.local.identifier == "96")


##Seccion 2: resample data

# Selected data

jtrk96 <- mk_track(j96,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag096<- track_resample(jtrk96, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag096)

table(jag096$burst_)

which(step_lengths(jag096, lonlat=TRUE) > 10000)


jag096_deleted <- jag096

j96 <-  step_lengths(jag096_deleted,lonlat=TRUE)


#how many data with turn angle

jag096_end <- filter_min_n_burst(jag096_deleted, 1)

jag096_duplicated <- as.data.frame( table(jag096_end$burst_))

jag096_duplicated2 <- subset(jag096_duplicated , Freq >=3)



write.table(jag096_deleted ,file="02_jaguar096_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #97 --------------------------------------------------------------
j97 <- filter(jaguar, individual.local.identifier == "97")


##Seccion 2: resample data

# Selected data

jtrk97 <- mk_track(j97,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag097<- track_resample(jtrk97, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag097)

table(jag097$burst_)

which(step_lengths(jag097, lonlat=TRUE) > 10000)


jag097_deleted <- jag097

j97 <-  step_lengths(jag097_deleted,lonlat=TRUE)


#how many data with turn angle

jag097_end <- filter_min_n_burst(jag097_deleted, 1)

jag097_duplicated <- as.data.frame( table(jag097_end$burst_))

jag097_duplicated2 <- subset(jag097_duplicated , Freq >=3)



write.table(jag097_deleted ,file="02_jaguar097_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #98 --------------------------------------------------------------
j98 <- filter(jaguar, individual.local.identifier == "98")

##Seccion 2: resample data

# Selected data

jtrk98 <- mk_track(j98,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag098<- track_resample(jtrk98, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag098)

table(jag098$burst_)

which(step_lengths(jag098, lonlat=TRUE) > 10000)


jag098_deleted <- jag098

j98 <-  step_lengths(jag098_deleted,lonlat=TRUE)


#how many data with turn angle

jag098_end <- filter_min_n_burst(jag098_deleted, 1)

jag098_duplicated <- as.data.frame( table(jag098_end$burst_))

jag098_duplicated2 <- subset(jag098_duplicated , Freq >=3)



write.table(jag098_deleted ,file="02_jaguar098_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #99 --------------------------------------------------------------
j99 <- filter(jaguar, individual.local.identifier == "99")


##Seccion 2: resample data

# Selected data

jtrk99 <- mk_track(j99,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 2
jag099<- track_resample(jtrk99, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag099)

table(jag099$burst_)

which(step_lengths(jag099, lonlat=TRUE) > 10000)


jag099_deleted <- jag099
j99 <-  step_lengths(jag099_deleted,lonlat=TRUE)


#how many data with turn angle

jag099_end <- filter_min_n_burst(jag099_deleted, 1)

jag099_duplicated <- as.data.frame( table(jag099_end$burst_))

jag099_duplicated2 <- subset(jag099_duplicated , Freq >=3)



write.table(jag099_deleted ,file="02_jaguar099_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #100 -------------------------------------------------------------
j100 <- filter(jaguar, individual.local.identifier == "100")


##Seccion 2: resample data

# Selected data

jtrk100 <- mk_track(j100,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag0100<- track_resample(jtrk100, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0100)

table(jag0100$burst_)

which(step_lengths(jag0100, lonlat=TRUE) > 10000)


jag0100_deleted <- jag0100

j100 <-  step_lengths(jag0100_deleted,lonlat=TRUE)


#how many data with turn angle

jag0100_end <- filter_min_n_burst(jag0100_deleted, 1)

jag0100_duplicated <- as.data.frame( table(jag0100_end$burst_))

jag0100_duplicated2 <- subset(jag0100_duplicated , Freq >=3)



write.table(jag0100_deleted ,file="02_jaguar0100_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #101 -------------------------------------------------------------
j101 <- filter(jaguar, individual.local.identifier == "101")

##Seccion 2: resample data

# Selected data

jtrk101 <- mk_track(j101,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag0101<- track_resample(jtrk101, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0101)

table(jag0101$burst_)

which(step_lengths(jag0101, lonlat=TRUE) > 10000)


jag0101_deleted <- jag0101[-c(127,184, 260, 292, 307, 311, 313, 321),]

j101 <-  step_lengths(jag0101_deleted,lonlat=TRUE)


#how many data with turn angle

jag0101_end <- filter_min_n_burst(jag0101_deleted, 1)

jag0101_duplicated <- as.data.frame( table(jag0101_end$burst_))

jag0101_duplicated2 <- subset(jag0101_duplicated , Freq >=3)



write.table(jag0101_deleted ,file="02_jaguar0101_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #102 -------------------------------------------------------------
j102 <- filter(jaguar, individual.local.identifier == "102")


##Seccion 2: resample data

# Selected data

jtrk102 <- mk_track(j102,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 4
jag0102<- track_resample(jtrk102, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0102)

table(jag0102$burst_)

which(step_lengths(jag0102, lonlat=TRUE) > 10000)


jag0102_deleted <- jag0102[-c(134),]

j102 <-  step_lengths(jag0102_deleted,lonlat=TRUE)


#how many data with turn angle

jag0102_end <- filter_min_n_burst(jag0102_deleted, 1)

jag0102_duplicated <- as.data.frame( table(jag0102_end$burst_))

jag0102_duplicated2 <- subset(jag0102_duplicated , Freq >=3)



write.table(jag0102_deleted ,file="02_jaguar0102_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #103 -------------------------------------------------------------
j103 <- filter(jaguar, individual.local.identifier == "103")


##Seccion 2: resample data

# Selected data

jtrk103 <- mk_track(j103,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 6
jag0103<- track_resample(jtrk103, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0103)

table(jag0103$burst_)

which(step_lengths(jag0103, lonlat=TRUE) > 10000)


jag0103_deleted <- jag0103

j103 <-  step_lengths(jag0103_deleted,lonlat=TRUE)


#how many data with turn angle

jag0103_end <- filter_min_n_burst(jag0103_deleted, 1)

jag0103_duplicated <- as.data.frame( table(jag0103_end$burst_))

jag0103_duplicated2 <- subset(jag0103_duplicated , Freq >=3)



write.table(jag0103_deleted ,file="02_jaguar0103_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #104 -------------------------------------------------------------
j104 <- filter(jaguar, individual.local.identifier == "104")


##Seccion 2: resample data

# Selected data

jtrk104 <- mk_track(j104,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 5
jag0104<- track_resample(jtrk104, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0104)

table(jag0104$burst_)

which(step_lengths(jag0104, lonlat=TRUE) > 10000)


jag0104_deleted <- jag0104[-c(47,65,76),]

j104 <-  step_lengths(jag0104_deleted,lonlat=TRUE)



#how many data with turn angle

jag0104_end <- filter_min_n_burst(jag0104_deleted, 1)

jag0104_duplicated <- as.data.frame( table(jag0104_end$burst_))

jag0104_duplicated2 <- subset(jag0104_duplicated , Freq >=3)



write.table(jag0104_deleted ,file="02_jaguar0104_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #105 -------------------------------------------------------------
j105 <- filter(jaguar, individual.local.identifier == "105")


##Seccion 2: resample data

# Selected data

jtrk105 <- mk_track(j105,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0105<- track_resample(jtrk105, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0105)

table(jag0105$burst_)

which(step_lengths(jag0105, lonlat=TRUE) > 10000)


jag0105_deleted <- jag0105

j105 <-  step_lengths(jag0105_deleted,lonlat=TRUE)



#how many data with turn angle

jag0105_end <- filter_min_n_burst(jag0105_deleted, 1)

jag0105_duplicated <- as.data.frame( table(jag0105_end$burst_))

jag0105_duplicated2 <- subset(jag0105_duplicated , Freq >=3)



write.table(jag0105_deleted ,file="02_jaguar0105_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #106 -------------------------------------------------------------
j106 <- filter(jaguar, individual.local.identifier == "106")


##Seccion 2: resample data

# Selected data

jtrk106 <- mk_track(j106,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0106<- track_resample(jtrk106, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0106)

table(jag0106$burst_)

which(step_lengths(jag0106, lonlat=TRUE) > 10000)


jag0106_deleted <- jag0106

j106 <-  step_lengths(jag0106_deleted,lonlat=TRUE)


#how many data with turn angle

jag0106_end <- filter_min_n_burst(jag0106_deleted, 1)

jag0106_duplicated <- as.data.frame( table(jag0106_end$burst_))

jag0106_duplicated2 <- subset(jag0106_duplicated , Freq >=3)



write.table(jag0106_deleted ,file="02_jaguar0106_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #107 -------------------------------------------------------------
j107 <- filter(jaguar, individual.local.identifier == "107")


##Seccion 2: resample data

# Selected data

jtrk107 <- mk_track(j107,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0107<- track_resample(jtrk107, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0107)

table(jag0107$burst_)

which(step_lengths(jag0107, lonlat=TRUE) > 10000)


jag0107_deleted <- jag0107
j107 <-  step_lengths(jag0107_deleted,lonlat=TRUE)


#how many data with turn angle

jag0107_end <- filter_min_n_burst(jag0107_deleted, 1)

jag0107_duplicated <- as.data.frame( table(jag0107_end$burst_))

jag0107_duplicated2 <- subset(jag0107_duplicated , Freq >=3)



write.table(jag0107_deleted ,file="02_jaguar0107_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #108 -------------------------------------------------------------

j108 <- filter(jaguar, individual.local.identifier == "108")


##Seccion 2: resample data

# Selected data

jtrk108 <- mk_track(j108,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0108<- track_resample(jtrk108, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0108)

table(jag0108$burst_)

which(step_lengths(jag0108, lonlat=TRUE) > 10000)


jag0108_deleted <- jag0108

j108 <-  step_lengths(jag0108_deleted,lonlat=TRUE)


#how many data with turn angle

jag0108_end <- filter_min_n_burst(jag0108_deleted, 1)

jag0108_duplicated <- as.data.frame( table(jag0108_end$burst_))

jag0108_duplicated2 <- subset(jag0108_duplicated , Freq >=3)



write.table(jag0108_deleted ,file="02_jaguar0108_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")
# Jaguar #109 -------------------------------------------------------------

j109 <- filter(jaguar, individual.local.identifier == "109")



##Seccion 2: resample data

# Selected data

jtrk109 <- mk_track(j109,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0109<- track_resample(jtrk109, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0109)

table(jag0109$burst_)

which(step_lengths(jag0109, lonlat=TRUE) > 10000)


jag0109_deleted <- jag0109
j109 <-  step_lengths(jag0109_deleted,lonlat=TRUE)



#how many data with turn angle

jag0109_end <- filter_min_n_burst(jag0109_deleted, 1)

jag0109_duplicated <- as.data.frame( table(jag0109_end$burst_))

jag0109_duplicated2 <- subset(jag0109_duplicated , Freq >=3)



write.table(jag0109_deleted ,file="02_jaguar0109_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #110 -------------------------------------------------------------

j110 <- filter(jaguar, individual.local.identifier == "110")


##Seccion 2: resample data

# Selected data

jtrk110 <- mk_track(j110,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0110<- track_resample(jtrk110, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0110)

table(jag0110$burst_)

which(step_lengths(jag0110, lonlat=TRUE) > 10000)


jag0110_deleted <- jag0110[-c(7,57),]

j110 <-  step_lengths(jag0110_deleted,lonlat=TRUE)


#how many data with turn angle

jag0110_end <- filter_min_n_burst(jag0110_deleted, 1)

jag0110_duplicated <- as.data.frame( table(jag0110_end$burst_))

jag0110_duplicated2 <- subset(jag0110_duplicated , Freq >=3)



write.table(jag0110_deleted ,file="02_jaguar0110_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")



# Jaguar #111 -------------------------------------------------------------
j111 <- filter(jaguar, individual.local.identifier == "111")


##Seccion 2: resample data

# Selected data

jtrk111 <- mk_track(j111,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0111<- track_resample(jtrk111, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0111)

table(jag0111$burst_)

which(step_lengths(jag0111, lonlat=TRUE) > 10000)


jag0111_deleted <- jag0111

j111 <-  step_lengths(jag0111_deleted,lonlat=TRUE)


#how many data with turn angle

jag0111_end <- filter_min_n_burst(jag0111_deleted, 1)

jag0111_duplicated <- as.data.frame( table(jag0111_end$burst_))

jag0111_duplicated2 <- subset(jag0111_duplicated , Freq >=3)



write.table(jag0111_deleted ,file="02_jaguar0111_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #112 -------------------------------------------------------------

j112 <- filter(jaguar, individual.local.identifier == "112")


##Seccion 2: resample data

# Selected data

jtrk112 <- mk_track(j112,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0112<- track_resample(jtrk112, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0112)

table(jag0112$burst_)

which(step_lengths(jag0112, lonlat=TRUE) > 10000)


jag0112_deleted <- jag0112

j112 <-  step_lengths(jag0112_deleted,lonlat=TRUE)


#how many data with turn angle

jag0112_end <- filter_min_n_burst(jag0112_deleted, 1)

jag0112_duplicated <- as.data.frame( table(jag0112_end$burst_))

jag0112_duplicated2 <- subset(jag0112_duplicated , Freq >=3)



write.table(jag0112_deleted ,file="02_jaguar0112_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #113 -------------------------------------------------------------
j113 <- filter(jaguar, individual.local.identifier == "113")


##Seccion 2: resample data

# Selected data

jtrk113 <- mk_track(j113,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0113<- track_resample(jtrk113, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0113)

table(jag0113$burst_)

which(step_lengths(jag0113, lonlat=TRUE) > 10000)


jag0113_deleted <- jag0113

j113 <-  step_lengths(jag0113_deleted,lonlat=TRUE)


#how many data with turn angle

jag0113_end <- filter_min_n_burst(jag0113_deleted, 1)

jag0113_duplicated <- as.data.frame( table(jag0113_end$burst_))

jag0113_duplicated2 <- subset(jag0113_duplicated , Freq >=3)



write.table(jag0113_deleted ,file="02_jaguar0113_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #114 -------------------------------------------------------------
j114 <- filter(jaguar, individual.local.identifier == "114")

##Seccion 2: resample data

# Selected data

jtrk114 <- mk_track(j114,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0114<- track_resample(jtrk114, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0114)

table(jag0114$burst_)

which(step_lengths(jag0114, lonlat=TRUE) > 10000)


jag0114_deleted <- jag0114[-c(124,125,129,132,136,138,140,141,142,144,145,146,  148,149,153,155,158,162,166,169, 173,174,177, 182,184,187,190,192,195,198,201,  204,208,213,214,218,221,223,227,229,231,234,235,239,245,246,250,251,259,  262,  265,267,275,277,278,279, 281, 283, 287, 289,292,295, 296,300,301,306,311,  312,  318,320,321, 322, 323,324, 331,333,346,347,348,350,361, 362,  366,  368,  369,  370, 376, 377, 383,384, 399, 400, 407,  408, 409,  410,411 , 412,  417,  419,  423,  424, 425 , 426 , 430 , 433 , 441 , 445,446,  447 , 449 , 450, 453 , 454 , 462 ,465 , 466 , 467,  471 , 472 , 473, 47 , 503, 505 , 511,512,  519,  521,526 , 528 , 530 , 531 , 548,  551,  556 , 558 ,561, 563 , 565,  567,  608 , 611 , 614 , 615,620  ,623 , 639 , 641 , 647,648,653,657,661,663,  668,  671,  675,  677,  680 , 681,685,  687,  691,  692,  696,  698,  699,  701,  702,  703,  713,  714,  719,  721,  722,  724,727,  728,  730,  734,  739,  742,  753,  754,  768,  770,  773,  775 , 779 , 780 , 781 , 782,783,  784,  788,  789,  792,  793,  796,  798,  800,  801,  810,  811 , 814 , 816 , 821,  822, 826,  828,  829 , 830,  834,  836,  838 , 843,  845,  847 , 851,  854,  855 , 856 , 858 , 859, 873 , 875,  878,  883,  884,  887,  888,  889,  890,  893,  894,  897,  899,  901,  905,  906, 908 , 909 , 910 , 913 , 917 , 918 , 919,  920 , 932  ,933 , 936,  937,  939,  941,  942 , 944, 949,  952,  953,  954 , 955,  956 , 976,  977,  988,  989,  991,  992,  997,  999, 1012, 1016, 1020, 1021, 1029, 1030, 1035, 1036, 1041, 1042, 1062, 1063, 1066, 1067, 1071, 1072, 1075, 1076,1087, 1088, 1438, 1439, 1440, 1441 ,1452, 1454 ,1473, 1474, 1478, 1479, 1481, 1482, 1488, 1489, 1504, 1505, 1515, 1516, 1518, 1519, 1526 ,1527, 1528, 1529, 1530, 1531, 1535, 1536, 1538, 1539,1543, 1545, 1556, 1557, 1563, 1566, 1574, 1575 ,1577, 1578, 1580, 1581, 1592, 1593, 1603, 1604,1606, 1607, 1612, 1613 ,1615, 1616, 1617, 1621, 1629, 1630, 1632, 1633),]

j114 <-  step_lengths(jag0114_deleted,lonlat=TRUE)

summary(j114)


#how many data with turn angle

jag0114_end <- filter_min_n_burst(jag0114_deleted, 1)

jag0114_duplicated <- as.data.frame( table(jag0114_end$burst_))

jag0114_duplicated2 <- subset(jag0114_duplicated , Freq >=3)



write.table(jag0114_deleted ,file="02_jaguar0114_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar # 115 ------------------------------------------------------------

j115 <- filter(jaguar, individual.local.identifier == "115")


##Seccion 2: resample data

# Selected data

jtrk115 <- mk_track(j115,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 3
jag0115<- track_resample(jtrk115, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0115)

table(jag0115$burst_)

which(step_lengths(jag0115, lonlat=TRUE) > 10000)


jag0115_deleted <- jag0115

j115 <-  step_lengths(jag0115_deleted,lonlat=TRUE)



#how many data with turn angle

jag0115_end <- filter_min_n_burst(jag0115_deleted, 1)

jag0115_duplicated <- as.data.frame( table(jag0115_end$burst_))

jag0115_duplicated2 <- subset(jag0115_duplicated , Freq >=3)



write.table(jag0115_deleted ,file="02_jaguar0115_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")


# Jaguar #116 -------------------------------------------------------------
j116 <- filter(jaguar, individual.local.identifier == "116")



##Seccion 2: resample data

# Selected data

jtrk116 <- mk_track(j116,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag0116<- track_resample(jtrk116, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0116)

table(jag0116$burst_)

which(step_lengths(jag0116, lonlat=TRUE) > 10000)


jag0116_deleted <- jag0116

j116 <-  step_lengths(jag0116_deleted,lonlat=TRUE)


#how many data with turn angle

jag0116_end <- filter_min_n_burst(jag0116_deleted, 1)

jag0116_duplicated <- as.data.frame( table(jag0116_end$burst_))

jag0116_duplicated2 <- subset(jag0116_duplicated , Freq >=3)



write.table(jag0116_deleted ,file="02_jaguar0116_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")

# Jaguar #117 -------------------------------------------------------------
j117 <- filter(jaguar, individual.local.identifier == "117")


##Seccion 2: resample data

# Selected data

jtrk117 <- mk_track(j117,.x=location.long, .y=location.lat, .t=timestamp, id=individual.local.identifier,sex=sex,age=age, weight=weight, season=season,country=country,crs = CRS("+init=epsg:4326"), distance=distance)

step_duration <- 1
jag0117<- track_resample(jtrk117, hours(step_duration), tolerance = minutes(15))

summarize_sampling_rate(jag0117)

table(jag0117$burst_)

which(step_lengths(jag0117, lonlat=TRUE) > 10000)


jag0117_deleted <- jag0117

j117 <-  step_lengths(jag0117_deleted,lonlat=TRUE)


#how many data with turn angle

jag0117_end <- filter_min_n_burst(jag0117_deleted, 1)

jag0117_duplicated <- as.data.frame( table(jag0117_end$burst_))

jag0117_duplicated2 <- subset(jag0117_duplicated , Freq >=3)



write.table(jag0117_deleted ,file="02_jaguar0117_dist_clean.txt",row.names = F,quote=F,col.names=T,sep="\t")



