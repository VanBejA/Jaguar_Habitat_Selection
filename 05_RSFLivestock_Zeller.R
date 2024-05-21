####################################
### Zeller approach - livestock
### Vanesa Bejarano Alegre 
### 27-04-2021
######################################################
# Loading packages

rm(list= ls())  #clean all before to start

listpacks <- c("raster","tidyverse", "move", "amt")

if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load(listpacks)

## Set work directory

setwd("D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock")
jaguar <- read.csv('02_jaguar.csv', header = T, sep = ',', dec = '.', comment.char = '')
### ###############################
# Group prepare -----------------------------------------------------------
## Prepare avial.pts for all the groups
## Agroup
### filter group individuals
j43 <- filter(jaguar, individual.local.identifier== "43")
j43$timestampTZ <- as.POSIXct(j43$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Hermosillo")
j64 <- filter(jaguar, individual.local.identifier== "64")
j64$timestampTZ <- as.POSIXct(j64$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Hermosillo")
Agroup_jag <- bind_rows(j43, j64)

### Then we converted in move and track object

Agroup_mov <- move(x=Agroup_jag$X, y=Agroup_jag$Y, time= as.POSIXct(Agroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Hermosillo"), data= Agroup_jag, prefer_proj=CRS("+init=epsg:32612"), animal=Agroup_jag$individual.local.identifier)

Agroup_track <- amt::mk_track(as.data.frame(Agroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32612"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Agroup_track <-  Agroup_track  %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Agroup_track$each.Day <- cut(Agroup_track$t_, breaks="day", labels=F)

Agroup_track_day <- Agroup_track[Agroup_track$tod_ == "day",]

Agroup_track_night <- Agroup_track[Agroup_track$tod_ == "night",]


## Bgroup
## ### filter group individuals
j49 <- filter(jaguar, individual.local.identifier== "49")
j49$timestampTZ <- as.POSIXct(j49$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Merida")

Bgroup_jag <- j49
### Then we converted in move and track object

Bgroup_mov <- move(x=Bgroup_jag$X, y=Bgroup_jag$Y, time= as.POSIXct(Bgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Merida"), data= Bgroup_jag, prefer_proj=CRS("+init=epsg:32616"), animal=Bgroup_jag$individual.local.identifier)

Bgroup_track <- amt::mk_track(as.data.frame(Bgroup_mov), X, Y, .t=timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32616"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Bgroup_track<-  Bgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Bgroup_track$each.Day <- cut(Bgroup_track$t_, breaks="day", labels=F)

Bgroup_track_day <- Bgroup_track[Bgroup_track$tod_ == "day",]

Bgroup_track_night <- Bgroup_track[Bgroup_track$tod_ == "night",]

## Cgroup
## ### filter group individuals
j44 <- filter(jaguar, individual.local.identifier== "44")
j44$timestampTZ <- as.POSIXct(j44$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Mexico_City")


j46 <- filter(jaguar, individual.local.identifier== "46")
j46$timestampTZ <- as.POSIXct(j46$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Mexico_City")

j47 <- filter(jaguar, individual.local.identifier== "47")
j47$timestampTZ <- as.POSIXct(j47$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Mexico_City")

j48 <- filter(jaguar, individual.local.identifier== "48")
j48$timestampTZ <- as.POSIXct(j48$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Mexico_City")

Cgroup_jag <- bind_rows(j44,j46,j47,j48)

### Then we converted in move and track object
Cgroup_mov <- move(x=Cgroup_jag$X, y=Cgroup_jag$Y, time= as.POSIXct(Cgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Mexico_City"), data= Cgroup_jag, prefer_proj=CRS("+init=epsg:32615"), animal=Cgroup_jag$individual.local.identifier)

Cgroup_track <- amt::mk_track(as.data.frame(Cgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32615"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Cgroup_track  <-  Cgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Cgroup_track$each.Day <- cut(Cgroup_track$t_, breaks="day", labels=F)

Cgroup_track_day <- Cgroup_track[Cgroup_track$tod_ == "day",]

Cgroup_track_night <- Cgroup_track[Cgroup_track$tod_ == "night",]

## Dgroup
## ### filter group individuals
j26 <- filter(jaguar, individual.local.identifier== "26")
j26$timestampTZ <- as.POSIXct(j26$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Costa_Rica")

Dgroup_jag <- j26

### Then we converted in move and track object

Dgroup_mov <- move(x=Dgroup_jag$X, y=Dgroup_jag$Y, time= as.POSIXct(Dgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Costa_Rica"), data= Dgroup_jag, prefer_proj=CRS("+init=epsg:32616"), animal=Dgroup_jag$individual.local.identifier)

Dgroup_track <- amt::mk_track(as.data.frame(Dgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32616"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Dgroup_track <-  Dgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Dgroup_track$each.Day <- cut(Dgroup_track$t_, breaks="day", labels=F)

Dgroup_track_day <- Dgroup_track[Dgroup_track$tod_ == "day",]

Dgroup_track_night <- Dgroup_track[Dgroup_track$tod_ == "night",]

## Egroup
## ### filter group individuals
j93 <- filter(jaguar, individual.local.identifier== "93")
j93$timestampTZ <- as.POSIXct(j93$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j94 <- filter(jaguar, individual.local.identifier== "94")
j94$timestampTZ <- as.POSIXct(j94$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j95 <- filter(jaguar, individual.local.identifier== "95")
j95$timestampTZ <- as.POSIXct(j95$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j96 <- filter(jaguar, individual.local.identifier== "96")
j96$timestampTZ <- as.POSIXct(j96$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j97 <- filter(jaguar, individual.local.identifier== "97")
j97$timestampTZ <- as.POSIXct(j97$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j98 <- filter(jaguar, individual.local.identifier== "98")
j98$timestampTZ <- as.POSIXct(j98$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")

j99 <- filter(jaguar, individual.local.identifier== "99")
j99$timestampTZ <- as.POSIXct(j99$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus")


Egroup_jag <- bind_rows(j93,j94,j95,j96,j97,j98,j99)

### Then we converted in move and track object
Egroup_mov <- move(x=Egroup_jag$X, y=Egroup_jag$Y, time= as.POSIXct(Egroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Manaus"), data= Egroup_jag, prefer_proj=CRS("+init=epsg:32720"), animal=Egroup_jag$individual.local.identifier)

Egroup_track <- amt::mk_track(as.data.frame(Egroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32720"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Egroup_track <-  Egroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Egroup_track$each.Day <- cut(Egroup_track$t_, breaks="day", labels=F)

Egroup_track_day <- Egroup_track[Egroup_track$tod_ == "day",]

Egroup_track_night <- Egroup_track[Egroup_track$tod_ == "night",]

## Fgroup
## ### filter group individuals

j24 <- filter(jaguar, individual.local.identifier== "24")
j24$timestampTZ <- as.POSIXct(j24$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Belem")

Fgroup_jag <- j24

### Then we converted in move and track object
Fgroup_mov <- move(x=Fgroup_jag$X, y=Fgroup_jag$Y, time= as.POSIXct(Fgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Belem"), data= Fgroup_jag, prefer_proj=CRS("+init=epsg:32722"), animal=Fgroup_jag$individual.local.identifier)

Fgroup_track <- amt::mk_track(as.data.frame(Fgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Fgroup_track <-  Fgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Fgroup_track$each.Day <- cut(Fgroup_track$t_, breaks="day", labels=F)

Fgroup_track_day <- Fgroup_track[Fgroup_track$tod_ == "day",]

Fgroup_track_night <- Fgroup_track[Fgroup_track$tod_ == "night",]

## Ggroup
## ### filter group individuals
j20 <- filter(jaguar, individual.local.identifier== "20")
j20$timestampTZ <- as.POSIXct(j20$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Fortaleza")

j50 <- filter(jaguar, individual.local.identifier== "50")
j50$timestampTZ <- as.POSIXct(j50$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Fortaleza")

Ggroup_jag <- bind_rows(j20,j50)

### Then we converted in move and track object
Ggroup_mov <- move(x=Ggroup_jag$X, y=Ggroup_jag$Y, time= as.POSIXct(Ggroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Fortaleza"), data= Ggroup_jag, prefer_proj=CRS("+init=epsg:32723"), animal=Ggroup_jag$individual.local.identifier)

Ggroup_track <- amt::mk_track(as.data.frame(Ggroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32723"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Ggroup_track <-  Ggroup_track %>% time_of_day(solar.dep = 18, include.crepuscule = FALSE)

Ggroup_track$each.Day <- cut(Ggroup_track$t_, breaks="day", labels=F)

Ggroup_track_day <- Ggroup_track[Ggroup_track$tod_ == "day",]

Ggroup_track_night <- Ggroup_track[Ggroup_track$tod_ == "night",]

## Hgroup
## ### filter group individuals

j89 <- filter(jaguar, individual.local.identifier== "89")
j89$timestampTZ <- as.POSIXct(j89$timestampTZ,format="%Y-%m-%d %H:%M", tz="Etc/GMT+3")

Hgroup_jag <- j89

### Then we converted in move and track object
Hgroup_mov <- move(x=Hgroup_jag$X, y=Hgroup_jag$Y, time= as.POSIXct(Hgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M", tz="Etc/GMT+3"), data= Hgroup_jag, prefer_proj=CRS("+init=epsg:32722"), animal=Hgroup_jag$individual.local.identifier)

Hgroup_track <- amt::mk_track(as.data.frame(Hgroup_mov), X, Y, .t=timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Hgroup_track <-  Hgroup_track %>% time_of_day(solar.dep=18, include.crepuscule = FALSE) 

Hgroup_track$each.Day <- cut(Hgroup_track$t_, breaks="day", labels=F)

Hgroup_track_day <- Hgroup_track[Hgroup_track$tod_ == "day",]

Hgroup_track_night <- Hgroup_track[Hgroup_track$tod_ == "night",]

## Igroup
## ### filter group individuals
j17 <- filter(jaguar, individual.local.identifier== "17")
j17$timestampTZ <- as.POSIXct(j17$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j65 <- filter(jaguar, individual.local.identifier== "65")
j65$timestampTZ <- as.POSIXct(j65$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")


j82 <- filter(jaguar, individual.local.identifier== "82")
j82$timestampTZ <- as.POSIXct(j82$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")
j82 <-  j82[-c(1239),] #duplicated

j85 <- filter(jaguar, individual.local.identifier== "85")
j85$timestampTZ <- as.POSIXct(j85$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")
Igroup_jag <- bind_rows(j17,j65,j82,j85)

Igroup_mov <- move(x=Igroup_jag$X, y=Igroup_jag$Y, time= as.POSIXct(Igroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Cuiaba"), data= Igroup_jag, prefer_proj=CRS("+init=epsg:32722"), animal=Igroup_jag$individual.local.identifier)

### Then we converted in move and track object
Igroup_track <- amt::mk_track(as.data.frame(Igroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Igroup_track <-  Igroup_track %>% time_of_day(solar.dep = 18, include.crepuscule = FALSE)

Igroup_track$each.Day <- cut(Igroup_track$t_, breaks="day", labels=F)

Igroup_track_day <- Igroup_track[Igroup_track$tod_ == "day",]

Igroup_track_night <- Igroup_track[Igroup_track$tod_ == "night",]

## Jgroup
## ### filter group individuals
j12 <- filter(jaguar, individual.local.identifier== "12")
j12$timestampTZ <- as.POSIXct(j12$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j13 <- filter(jaguar, individual.local.identifier== "13")
j13$timestampTZ <- as.POSIXct(j13$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")
j13 <- j13[-c(1349),]

j18 <- filter(jaguar, individual.local.identifier== "18")
j18$timestampTZ <- as.POSIXct(j18$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")
j18 <- j18[-c(1430),]

j22 <- filter(jaguar, individual.local.identifier== "22")
j22$timestampTZ <- as.POSIXct(j22$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")
j22 <- j22[-c(2742),]

j23 <- filter(jaguar, individual.local.identifier== "23")
j23$timestampTZ <- as.POSIXct(j23$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j41 <- filter(jaguar, individual.local.identifier== "41")
j41$timestampTZ <- as.POSIXct(j41$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")
j41 <- j41[-c(1174),]

j52 <- filter(jaguar, individual.local.identifier== "52")
j52$timestampTZ <- as.POSIXct(j52$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j81 <- filter(jaguar, individual.local.identifier== "81")
j81$timestampTZ <- as.POSIXct(j81$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")
j81 <- j81[-c(9206),]

j88 <- filter(jaguar, individual.local.identifier== "88")
j88$timestampTZ <- as.POSIXct(j88$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j91 <- filter(jaguar, individual.local.identifier== "91")
j91$timestampTZ <- as.POSIXct(j91$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j92 <- filter(jaguar, individual.local.identifier== "92")
j92$timestampTZ <- as.POSIXct(j92$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j116 <- filter(jaguar, individual.local.identifier== "116")
j116$timestampTZ <- as.POSIXct(j116$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j117 <- filter(jaguar, individual.local.identifier== "117")
j117$timestampTZ <- as.POSIXct(j117$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Jgroup_jag <- bind_rows(j12, j13,j18,j22,j23,j41, j52, j81, j88, j91, j92, j116, j117)

### Then we converted in move and track object
Jgroup_mov <- move(x=Jgroup_jag$X, y=Jgroup_jag$Y, time= as.POSIXct(Jgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="America/Cuiaba"), data= Jgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Jgroup_jag$individual.local.identifier)

Jgroup_track <- amt::mk_track(as.data.frame(Jgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Jgroup_track <-  Jgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Jgroup_track$each.Day <- cut(Jgroup_track$t_, breaks="day", labels=F)

Jgroup_track_day <- Jgroup_track[Jgroup_track$tod_ == "day",]

Jgroup_track_night <- Jgroup_track[Jgroup_track$tod_ == "night",]

## Kgroup
## ### filter group individuals
j27 <- filter(jaguar, individual.local.identifier== "27")
j27$timestampTZ <- as.POSIXct(j27$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j30 <- filter(jaguar, individual.local.identifier== "30")
j30$timestampTZ <- as.POSIXct(j30$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j31 <- filter(jaguar, individual.local.identifier== "31")
j31$timestampTZ <- as.POSIXct(j31$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j32 <- filter(jaguar, individual.local.identifier== "32")
j32$timestampTZ <- as.POSIXct(j32$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j33 <- filter(jaguar, individual.local.identifier== "33")
j33$timestampTZ <- as.POSIXct(j33$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j53 <- filter(jaguar, individual.local.identifier== "53")
j53$timestampTZ <- as.POSIXct(j53$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j55 <- filter(jaguar, individual.local.identifier== "55")
j55$timestampTZ <- as.POSIXct(j55$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j59 <- filter(jaguar, individual.local.identifier== "59")
j59$timestampTZ <- as.POSIXct(j59$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j60 <- filter(jaguar, individual.local.identifier== "60")
j60$timestampTZ <- as.POSIXct(j60$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j61 <- filter(jaguar, individual.local.identifier== "61")
j61$timestampTZ <- as.POSIXct(j61$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Kgroup_jag <- bind_rows(j27, j30,j31,j32,j33,j53,j55,j59,j60,j61)

### Then we converted in move and track object
Kgroup_mov <- move(x=Kgroup_jag$X, y=Kgroup_jag$Y, time= as.POSIXct(Kgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Kgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Kgroup_jag$individual.local.identifier)

Kgroup_track <- amt::mk_track(as.data.frame(Kgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Kgroup_track <-  Kgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Kgroup_track$each.Day <- cut(Kgroup_track$t_, breaks="day", labels=F)

Kgroup_track_day <- Kgroup_track[Kgroup_track$tod_ == "day",]

Kgroup_track_night <- Kgroup_track[Kgroup_track$tod_ == "night",]

## Lgroup
## ### filter group individuals
j28 <- filter(jaguar, individual.local.identifier== "28")
j28$timestampTZ <- as.POSIXct(j28$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j29 <- filter(jaguar, individual.local.identifier== "29")
j29$timestampTZ <- as.POSIXct(j29$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j54 <- filter(jaguar, individual.local.identifier== "54")
j54$timestampTZ <- as.POSIXct(j54$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j56 <- filter(jaguar, individual.local.identifier== "56")
j56$timestampTZ <- as.POSIXct(j56$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Lgroup_jag <- bind_rows(j28, j29, j54, j56)

### Then we converted in move and track object
Lgroup_mov <- move(x=Lgroup_jag$X, y=Lgroup_jag$Y, time= as.POSIXct(Lgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Lgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Lgroup_jag$individual.local.identifier)

Lgroup_track <- amt::mk_track(as.data.frame(Lgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Lgroup_track <-  Lgroup_track %>% time_of_day(solar.dep = 18, include.crepuscule = FALSE)

Lgroup_track$each.Day <- cut(Lgroup_track$t_, breaks="day", labels=F)

Lgroup_track_day <- Lgroup_track[Lgroup_track$tod_ == "day",]

Lgroup_track_night <- Lgroup_track[Lgroup_track$tod_ == "night",]

## Mgroup
## ### filter group individuals
j105 <- filter(jaguar, individual.local.identifier== "105")
j105$timestampTZ <- as.POSIXct(j105$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j106 <- filter(jaguar, individual.local.identifier== "106")
j106$timestampTZ <- as.POSIXct(j106$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j107 <- filter(jaguar, individual.local.identifier== "107")
j107$timestampTZ <- as.POSIXct(j107$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j108 <- filter(jaguar, individual.local.identifier== "108")
j108$timestampTZ <- as.POSIXct(j108$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j109 <- filter(jaguar, individual.local.identifier== "109")
j109$timestampTZ <- as.POSIXct(j109$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j110 <- filter(jaguar, individual.local.identifier== "110")
j110$timestampTZ <- as.POSIXct(j110$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j111 <- filter(jaguar, individual.local.identifier== "111")
j111$timestampTZ <- as.POSIXct(j111$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j112 <- filter(jaguar, individual.local.identifier== "112")
j112$timestampTZ <- as.POSIXct(j112$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j113 <- filter(jaguar, individual.local.identifier== "113")
j113$timestampTZ <- as.POSIXct(j113$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j114 <- filter(jaguar, individual.local.identifier== "114")
j114$timestampTZ <- as.POSIXct(j114$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j115 <- filter(jaguar, individual.local.identifier== "115")
j115$timestampTZ <- as.POSIXct(j115$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Mgroup_jag <- bind_rows(j105, j106,j107,j108,j109,j110,j111,j112,j113,j114,j115)

### Then we converted in move and track object
Mgroup_mov <- move(x=Mgroup_jag$X, y=Mgroup_jag$Y, time= as.POSIXct(Mgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Mgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Mgroup_jag$individual.local.identifier)

Mgroup_track <- amt::mk_track(as.data.frame(Mgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Mgroup_track <-  Mgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Mgroup_track$each.Day <- cut(Mgroup_track$t_, breaks="day", labels=F)

Mgroup_track_day <- Mgroup_track[Mgroup_track$tod_ == "day",]

Mgroup_track_night <- Mgroup_track[Mgroup_track$tod_ == "night",]

## Ngroup
## ### filter group individuals

j15 <- filter(jaguar, individual.local.identifier== "15")
j15$timestampTZ <- as.POSIXct(j15$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j19 <- filter(jaguar, individual.local.identifier== "19")
j19$timestampTZ <- as.POSIXct(j19$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j25 <- filter(jaguar, individual.local.identifier== "25")
j25$timestampTZ <- as.POSIXct(j25$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j68 <- filter(jaguar, individual.local.identifier== "68")
j68$timestampTZ <- as.POSIXct(j68$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j69 <- filter(jaguar, individual.local.identifier== "69")
j69$timestampTZ <- as.POSIXct(j69$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j79 <- filter(jaguar, individual.local.identifier== "79")
j79$timestampTZ <- as.POSIXct(j79$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j84 <- filter(jaguar, individual.local.identifier== "84")
j84$timestampTZ <- as.POSIXct(j84$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j86 <- filter(jaguar, individual.local.identifier== "86")
j86$timestampTZ <- as.POSIXct(j86$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j87 <- filter(jaguar, individual.local.identifier== "87")
j87$timestampTZ <- as.POSIXct(j87$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j101 <- filter(jaguar, individual.local.identifier== "101")
j101$timestampTZ <- as.POSIXct(j101$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j102 <- filter(jaguar, individual.local.identifier== "102")
j102$timestampTZ <- as.POSIXct(j102$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")


j104 <- filter(jaguar, individual.local.identifier== "104")
j104$timestampTZ <- as.POSIXct(j104$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Ngroup_jag <- bind_rows(j15,j19,j25,j68,j69,j79,j84,j86,j87,j101,j102, j104)

### Then we converted in move and track object
Ngroup_mov <- move(x=Ngroup_jag$X, y=Ngroup_jag$Y, time= as.POSIXct(Ngroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Ngroup_jag, prefer_proj=CRS("+init=epsg: 32721"), animal=Ngroup_jag$individual.local.identifier)

Ngroup_track <- amt::mk_track(as.data.frame(Ngroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Ngroup_track <-  Ngroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Ngroup_track$each.Day <- cut(Ngroup_track$t_, breaks="day", labels=F)

Ngroup_track_day <- Ngroup_track[Ngroup_track$tod_ == "day",]

Ngroup_track_night <- Ngroup_track[Ngroup_track$tod_ == "night",]

## Ogroup
## ### filter group individuals
j74 <- filter(jaguar, individual.local.identifier== "74")
j74$timestampTZ <- as.POSIXct(j74$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j75 <- filter(jaguar, individual.local.identifier== "75")
j75$timestampTZ <- as.POSIXct(j75$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Ogroup_jag <- bind_rows(j74, j75)
### Then we converted in move and track object
Ogroup_mov <- move(x=Ogroup_jag$X, y=Ogroup_jag$Y, time= as.POSIXct(Ogroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Ogroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Ogroup_jag$individual.local.identifier)

Ogroup_track <- amt::mk_track(as.data.frame(Ogroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Ogroup_track <-  Ogroup_track %>% time_of_day(solar.dep = 18, include.crepuscule = FALSE)

Ogroup_track$each.Day <- cut(Ogroup_track$t_, breaks="day", labels=F)

Ogroup_track_day <- Ogroup_track[Ogroup_track$tod_ == "day",]

Ogroup_track_night <- Ogroup_track[Ogroup_track$tod_ == "night",]

## Pgroup
## ### filter group individuals
j51 <- filter(jaguar, individual.local.identifier== "51")
j51$timestampTZ <- as.POSIXct(j51$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Pgroup_jag <- j51
### Then we converted in move and track object
Pgroup_mov <- move(x=Pgroup_jag$X, y=Pgroup_jag$Y, time= as.POSIXct(Pgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Pgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Pgroup_jag$individual.local.identifier)


Pgroup_track <- amt::mk_track(as.data.frame(Pgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Pgroup_track <-  Pgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Pgroup_track$each.Day <- cut(Pgroup_track$t_, breaks="day", labels=F)

Pgroup_track_day <- Pgroup_track[Pgroup_track$tod_ == "day",]

Pgroup_track_night <- Pgroup_track[Pgroup_track$tod_ == "night",]

## Qgroup
## ### filter group individuals
j71 <- filter(jaguar, individual.local.identifier== "71")
j71$timestampTZ <- as.POSIXct(j71$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j72 <- filter(jaguar, individual.local.identifier== "72")
j72$timestampTZ <- as.POSIXct(j72$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j73 <- filter(jaguar, individual.local.identifier== "73")
j73$timestampTZ <- as.POSIXct(j73$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Qgroup_jag <- bind_rows(j71, j72, j73)
### Then we converted in move and track object
Qgroup_mov <- move(x=Qgroup_jag$X, y=Qgroup_jag$Y, time= as.POSIXct(Qgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Qgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Qgroup_jag$individual.local.identifier)

Qgroup_track <- amt::mk_track(as.data.frame(Qgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Qgroup_track <-  Qgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Qgroup_track$each.Day <- cut(Qgroup_track$t_, breaks="day", labels=F)

Qgroup_track_day <- Qgroup_track[Qgroup_track$tod_ == "day",]

Qgroup_track_night <- Qgroup_track[Qgroup_track$tod_ == "night",]

## Rgroup
## ### filter group individuals
j76 <- filter(jaguar, individual.local.identifier== "76")
j76$timestampTZ <- as.POSIXct(j76$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j77 <- filter(jaguar, individual.local.identifier== "77")
j77$timestampTZ <- as.POSIXct(j77$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Rgroup_jag <- bind_rows(j76, j77)

### Then we converted in move and track object
Rgroup_mov <- move(x=Rgroup_jag$X, y=Rgroup_jag$Y, time= as.POSIXct(Rgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Rgroup_jag, prefer_proj=CRS("+init=epsg: 32720"), animal=Rgroup_jag$individual.local.identifier)

Rgroup_track <- amt::mk_track(as.data.frame(Rgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32720"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Rgroup_track <-  Rgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Rgroup_track$each.Day <- cut(Rgroup_track$t_, breaks="day", labels=F)

Rgroup_track_day <- Rgroup_track[Rgroup_track$tod_ == "day",]

Rgroup_track_night <- Rgroup_track[Rgroup_track$tod_ == "night",]

## Sgroup
## ### filter group individuals
j16 <- filter(jaguar, individual.local.identifier== "16")
j16$timestampTZ <- as.POSIXct(j16$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j70 <- filter(jaguar, individual.local.identifier== "70")
j70$timestampTZ <- as.POSIXct(j70$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Sgroup_jag <- bind_rows(j16, j70)
### Then we converted in move and track object
Sgroup_mov <- move(x=Sgroup_jag$X, y=Sgroup_jag$Y, time= as.POSIXct(Sgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Sgroup_jag, prefer_proj=CRS("+init=epsg: 32720"), animal=Sgroup_jag$individual.local.identifier)

Sgroup_track <- amt::mk_track(as.data.frame(Sgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=CRS("+init=epsg:32720"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Sgroup_track <-  Sgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Sgroup_track$each.Day <- cut(Sgroup_track$t_, breaks="day", labels=F)

Sgroup_track_day <- Sgroup_track[Sgroup_track$tod_ == "day",]

Sgroup_track_night <- Sgroup_track[Sgroup_track$tod_ == "night",]

## Tgroup
## ### filter group individuals
j34 <- filter(jaguar, individual.local.identifier== "34")
j34$timestampTZ <- as.POSIXct(j34$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j37 <- filter(jaguar, individual.local.identifier== "37")
j37$timestampTZ <- as.POSIXct(j37$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j58 <- filter(jaguar, individual.local.identifier== "58")
j58$timestampTZ <- as.POSIXct(j58$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j62 <- filter(jaguar, individual.local.identifier== "62")
j62$timestampTZ <- as.POSIXct(j62$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Tgroup_jag <- bind_rows(j34, j37, j58, j62)
### Then we converted in move and track object
Tgroup_mov <- move(x=Tgroup_jag$X, y=Tgroup_jag$Y, time= as.POSIXct(Tgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Tgroup_jag, prefer_proj=CRS("+init=epsg: 32722"), animal=Tgroup_jag$individual.local.identifier)

Tgroup_track <- amt::mk_track(as.data.frame(Tgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Tgroup_track <-  Tgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Tgroup_track$each.Day <- cut(Tgroup_track$t_, breaks="day", labels=F)

Tgroup_track_day <- Tgroup_track[Tgroup_track$tod_ == "day",]

Tgroup_track_night <- Tgroup_track[Tgroup_track$tod_ == "night",]

## Ugroup
## ### filter group individuals
j39 <- filter(jaguar, individual.local.identifier== "39")
j39$timestampTZ <- as.POSIXct(j39$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j40 <- filter(jaguar, individual.local.identifier== "40")
j40$timestampTZ <- as.POSIXct(j40$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j63 <- filter(jaguar, individual.local.identifier== "63")
j63$timestampTZ <- as.POSIXct(j63$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Ugroup_jag <- bind_rows(j39, j40, j63)

### Then we converted in move and track object
Ugroup_mov <- move(x=Ugroup_jag$X, y=Ugroup_jag$Y, time= as.POSIXct(Ugroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Ugroup_jag, prefer_proj=CRS("+init=epsg: 32722"), animal=Ugroup_jag$individual.local.identifier)

Ugroup_track <- amt::mk_track(as.data.frame(Ugroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Ugroup_track <-  Ugroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Ugroup_track$each.Day <- cut(Ugroup_track$t_, breaks="day", labels=F)
Ugroup_track_day <- Ugroup_track[Ugroup_track$tod_ == "day",]

Ugroup_track_night <- Ugroup_track[Ugroup_track$tod_ == "night",]

## Vgroup
## ### filter group individuals
j1 <- filter(jaguar, individual.local.identifier== "1")
j1$timestampTZ <- as.POSIXct(j1$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j3 <- filter(jaguar, individual.local.identifier== "3")
j3$timestampTZ <- as.POSIXct(j3$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j4 <- filter(jaguar, individual.local.identifier== "4")
j4$timestampTZ <- as.POSIXct(j4$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j5 <- filter(jaguar, individual.local.identifier== "5")
j5$timestampTZ <- as.POSIXct(j5$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j6 <- filter(jaguar, individual.local.identifier== "6")
j6$timestampTZ <- as.POSIXct(j6$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j7 <- filter(jaguar, individual.local.identifier== "7")
j7$timestampTZ <- as.POSIXct(j7$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j9 <- filter(jaguar, individual.local.identifier== "9")
j9$timestampTZ <- as.POSIXct(j9$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j10 <- filter(jaguar, individual.local.identifier== "10")
j10$timestampTZ <- as.POSIXct(j10$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

j11 <- filter(jaguar, individual.local.identifier== "11")
j11$timestampTZ <- as.POSIXct(j11$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4")

Vgroup_jag <- bind_rows(j1,j3,j4,j5,j6,j7,j9,j10,j11)

### Then we converted in move and track object
Vgroup_mov <- move(x=Vgroup_jag$X, y=Vgroup_jag$Y, time= as.POSIXct(Vgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+4"), data= Vgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Vgroup_jag$individual.local.identifier)

Vgroup_track <- amt::mk_track(as.data.frame(Vgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Vgroup_track <-  Vgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Vgroup_track$each.Day <- cut(Vgroup_track$t_, breaks="day", labels=F)
Vgroup_track_day <- Vgroup_track[Vgroup_track$tod_ == "day",]

Vgroup_track_night <- Vgroup_track[Vgroup_track$tod_ == "night",]

## Wgroup
## ### filter group individuals
j2 <- filter(jaguar, individual.local.identifier== "2")
j2$timestampTZ <- as.POSIXct(j2$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j8 <- filter(jaguar, individual.local.identifier== "8")
j8$timestampTZ <- as.POSIXct(j8$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j21 <- filter(jaguar, individual.local.identifier== "21")
j21$timestampTZ <- as.POSIXct(j21$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j78 <- filter(jaguar, individual.local.identifier== "78")
j78$timestampTZ <- as.POSIXct(j78$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Wgroup_jag <- bind_rows(j2, j8, j21)

### Then we converted in move and track object
Wgroup_mov <- move(x=Wgroup_jag$X, y=Wgroup_jag$Y, time= as.POSIXct(Wgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Wgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Wgroup_jag$individual.local.identifier)

Wgroup_track <- amt::mk_track(as.data.frame(Wgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Wgroup_track <-  Wgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Wgroup_track$each.Day <- cut(Wgroup_track$t_, breaks="day", labels=F)

Wgroup_track_day <- Wgroup_track[Wgroup_track$tod_ == "day",]

Wgroup_track_night <- Wgroup_track[Wgroup_track$tod_ == "night",]

## Xgroup
## ### filter group individuals
j83 <- filter(jaguar, individual.local.identifier== "83")
j83$timestampTZ <- as.POSIXct(j83$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Xgroup_jag <- j83

### Then we converted in move and track object
Xgroup_mov <- move(x=Xgroup_jag$X, y=Xgroup_jag$Y, time= as.POSIXct(Xgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Xgroup_jag, prefer_proj=CRS("+init=epsg:32722"), animal=Xgroup_jag$individual.local.identifier)

Xgroup_track <- amt::mk_track(as.data.frame(Xgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32722"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Xgroup_track <-  Xgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Xgroup_track$each.Day <- cut(Xgroup_track$t_, breaks="day", labels=F)

Xgroup_track_day <- Xgroup_track[Xgroup_track$tod_ == "day",]

Xgroup_track_night <- Xgroup_track[Xgroup_track$tod_ == "night",]

## Ygroup
## ### filter group individuals
j42 <- filter(jaguar, individual.local.identifier== "42")
j42$timestampTZ <- as.POSIXct(j42$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")
j42 <- j42[-543,]

j66 <- filter(jaguar, individual.local.identifier== "66")
j66$timestampTZ <- as.POSIXct(j66$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j80 <- filter(jaguar, individual.local.identifier== "80")
j80$timestampTZ <- as.POSIXct(j80$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

j90 <- filter(jaguar, individual.local.identifier== "90")
j90$timestampTZ <- as.POSIXct(j90$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Ygroup_jag <- bind_rows(j42, j66, j80, j90)

### Then we converted in move and track object
Ygroup_mov <- move(x=Ygroup_jag$X, y=Ygroup_jag$Y, time= as.POSIXct(Ygroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Ygroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Ygroup_jag$individual.local.identifier)

Ygroup_track <- amt::mk_track(as.data.frame(Ygroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Ygroup_track <-  Ygroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Ygroup_track$each.Day <- cut(Ygroup_track$t_, breaks="day", labels=F)

Ygroup_track_day <- Ygroup_track[Ygroup_track$tod_ == "day",]

Ygroup_track_night <- Ygroup_track[Ygroup_track$tod_ == "night",]

## Zgroup
## ### filter group individuals
j78 <- filter(jaguar, individual.local.identifier== "78")
j78$timestampTZ <- as.POSIXct(j78$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3")

Zgroup_jag <- bind_rows(j78)

### Then we converted in move and track object
Zgroup_mov <- move(x=Zgroup_jag$X, y=Zgroup_jag$Y, time= as.POSIXct(Zgroup_jag$timestampTZ,format="%Y-%m-%d %H:%M",tz="Etc/GMT+3"), data= Zgroup_jag, prefer_proj=CRS("+init=epsg:32721"), animal=Zgroup_jag$individual.local.identifier)

Zgroup_track <- amt::mk_track(as.data.frame(Zgroup_mov), X, Y, .t= timestampTZ, id= individual.local.identifier, crs=crs("+init=epsg:32721"), sex=sex, season=season, moon.phase=moon.phase, event=event)

Zgroup_track <-  Zgroup_track %>% time_of_day(solar.dep = 18,include.crepuscule = FALSE)

Zgroup_track$each.Day <- cut(Zgroup_track$t_, breaks="day", labels=F)

Zgroup_track_day <- Zgroup_track[Zgroup_track$tod_ == "day",]

Zgroup_track_night <- Zgroup_track[Zgroup_track$tod_ == "night",]

# Check all the data if is OK ---------------------------------------------
plot(Agroup_track) # ok

plot(Bgroup_track$x_,Bgroup_track$y_)#ok

plot(Cgroup_mov)
plot(Cgroup_track)


plot(Cgroup_track$x_, Cgroup_track$y_)#ok

plot(Dgroup_mov)
plot(Dgroup_track)#ok

plot(Egroup_mov)
plot(Egroup_track$x_,Egroup_track$y_)



plot(Fgroup_mov)
plot(Fgroup_track)


plot(Fgroup_track$x_,Fgroup_track$y_)#ok

plot(Ggroup_mov)
plot(Ggroup_track)#ok

plot(Hgroup_mov)
plot(Hgroup_track)#ok

plot(Igroup_mov)
plot(Igroup_track$x_,Igroup_track$y_)#ok

plot(Jgroup_mov)#ok

plot(Kgroup_mov)#ok
plot(Kgroup_track_day$x_, Kgroup_track_day$y_)

plot(Lgroup_mov)#ok

plot(Mgroup_mov)#ok

plot(Ngroup_mov)#ok

plot(Ogroup_mov)#ok

plot(Pgroup_mov)#ok

plot(Qgroup_mov)# outlier
plot(Qgroup_track$x_, Qgroup_track$y_) #outlier

point_outlier <- which(Qgroup_track$x_ > 4e+5)
Qgroup_track <-  Qgroup_track[- point_outlier,]

point_outlier <- which(Qgroup_mov$X > 4e+05)
Qgroup_mov <-  Qgroup_mov[- point_outlier,]


plot(Qgroup_track$x_, Qgroup_track$y_)#ok

plot(Qgroup_track_day$x_, Qgroup_track_day$y_)#ok

plot(Qgroup_track_night$x_, Qgroup_track_night$y_)
point_outlier <- which(Qgroup_track_night$x_ > 4e+5)
Qgroup_track_night <-  Qgroup_track_night[- point_outlier,]
plot(Qgroup_track_night$x_, Qgroup_track_night$y_)#ok


plot(Rgroup_mov)#ok
plot(Rgroup_track)#ok

plot(Sgroup_mov)# ouliers
plot(Sgroup_track$x_, Sgroup_track$y_)#outliers

point_outlier <- which(Sgroup_track$x_ < 6e+05)
Sgroup_track <-  Sgroup_track[- point_outlier,]

plot(Sgroup_track$x_, Sgroup_track$y_)#ok

plot(Sgroup_track_day$x_, Sgroup_track_day$y_)#outliers
point_outlier <- which(Sgroup_track_day$x_ < 6e+05)
Sgroup_track_day <-  Sgroup_track_day[- point_outlier,]
plot(Sgroup_track_day$x_, Sgroup_track_day$y_)#ok

plot(Sgroup_track_night$x_, Sgroup_track_night$y_)#outliers
point_outlier <- which(Sgroup_track_night$x_ < 6e+05)
Sgroup_track_night <-  Sgroup_track_night[- point_outlier,]
plot(Sgroup_track_night$x_, Sgroup_track_night$y_)#ok


plot(Tgroup_mov)#ok

plot(Ugroup_mov)# outliers
plot(Ugroup_track$x_, Ugroup_track$y_)#outliers

point_outlier <- which(Ugroup_track$x_ > 4e+05)
Ugroup_track <-  Ugroup_track[- point_outlier,]

plot(Ugroup_track$x_, Ugroup_track$y_)#OK

plot(Ugroup_track_day$x_, Ugroup_track_day$y_)#ok

plot(Ugroup_track_night$x_, Ugroup_track_night$y_)#outliers
point_outlier <- which(Ugroup_track_night$x_ > 4e+05)
Ugroup_track_night <-  Ugroup_track_night[- point_outlier,]
plot(Ugroup_track_night$x_, Ugroup_track_night$y_)

plot(Vgroup_mov)#ok

plot(Wgroup_mov)#ok

plot(Xgroup_mov)
plot(Xgroup_track$x_, Xgroup_track$y_)


points(Xgroup_track$x_, Xgroup_track$y_)#ok

plot(Ygroup_mov)
plot(Zgroup_mov)

# Remove innecessry elements ----------------------------------------------
rm(j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,j13,j15,
   j16,j17,j18,j19,j20,j21,j22,j23,j24,j25,j26,j27,j28,
   j29,j30,j31,j32,j33,j34,j37,j39,j40,j41,
   j42,j43,j44,j46,j47,j48,j49,j50,j51,j52,j53,j54,
   j55,j56,j58,j59,j60,j61,j62,j63,j64,j65,j66,
   j68,j69,j70,j71,j72,j73,j74,j75,j76,j77,j78,j79,j80,
   j81,j82,j83,j84,j85,j86,j87,j88,j89,j90,j91,j92,j93,
   j94,j95,j96,j97,j98,j99,j101,j102,j104,j105,
   j106,j107,j108,j109,j110,j111,j112,j113,j114,j115,j116,
   j117)

## Upload the environmental bases by group
#### We work in groups since A to Y for env. bases and jaguars

# A group -----------------------------------------------------------------

A_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Agroup_livestock.tif")


A_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_527_m.tif")

A_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_724_m.tif")

A_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_1242_m.tif")

A_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_1545_m.tif")

A_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_2105_m.tif")

A_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_3001_m.tif")

A_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Agroup_livestock_5886_m.tif")


A_Alllivestock_Scale <- stack( A_livestock527, A_livestock724, A_livestock1242, A_livestock1545, A_livestock2105, A_livestock3001, A_livestock5886)


### Day

avail <- data.frame(raster::extract(A_Alllivestock_Scale, coords(Agroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Agroup_track_day), id=Agroup_track_day$id, 
                sex=Agroup_track_day$sex, season=Agroup_track_day$season)

livestock <- raster::extract(A_livestock, coords(Agroup_track_day),  
                                   method="simple", buffer=30,fun=mean, 
                                   na.rm=T)

used <- cbind(livestock, coords(Agroup_track_day), id=Agroup_track_day$id, 
              sex= Agroup_track_day$sex, season=Agroup_track_day$season)



rsf_Alivest_day<- used
rsf_Alivest_day$livestock527  <- avail$Agroup_livestock_527_m
rsf_Alivest_day$livestock724  <- avail$Agroup_livestock_724_m
rsf_Alivest_day$livestock1242 <- avail$Agroup_livestock_1242_m
rsf_Alivest_day$livestock1545 <- avail$Agroup_livestock_1545_m 
rsf_Alivest_day$livestock2105 <- avail$Agroup_livestock_2105_m
rsf_Alivest_day$livestock3001 <- avail$Agroup_livestock_3001_m
rsf_Alivest_day$livestock5886 <- avail$Agroup_livestock_5886_m 


write.csv(rsf_Alivest_day, "rsf_Alivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(A_Alllivestock_Scale, coords(Agroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Agroup_track_night), id=Agroup_track_night$id, 
                sex=Agroup_track_night$sex, season=Agroup_track_night$season)

livestock <- raster::extract(A_livestock, coords(Agroup_track_night),  
                                   method="simple", buffer=30,fun=mean, 
                                   na.rm=T)

used <- cbind(livestock, coords(Agroup_track_night), id=Agroup_track_night$id, 
              sex= Agroup_track_night$sex, season=Agroup_track_night$season)

rsf_Alivest_night <- used
rsf_Alivest_night$livestock527   <- avail$Agroup_livestock_527_m
rsf_Alivest_night$livestock724   <- avail$Agroup_livestock_724_m
rsf_Alivest_night$livestock1242  <- avail$Agroup_livestock_1242_m
rsf_Alivest_night$livestock1545  <- avail$Agroup_livestock_1545_m  
rsf_Alivest_night$livestock2105 <- avail$Agroup_livestock_2105_m
rsf_Alivest_night$livestock3001 <- avail$Agroup_livestock_3001_m
rsf_Alivest_night$livestock5886 <- avail$Agroup_livestock_5886_m 



write.csv(rsf_Alivest_night, "rsf_Alivest_night.csv")

rm(used, avail, livestock)

# Remove Agroup data useless ----------------------------------------------

### Rasters
rm(A_Alllivestock_Scale, A_livestock, A_livestock1242, A_livestock1545,
   A_livestock2105, A_livestock3001, A_livestock527, A_livestock5886,A_livestock724)

rm(Agroup_jag,Agroup_mov,Agroup_track, Agroup_track_day, Agroup_track_night)

gc()

# B group -----------------------------------------------------------------

B_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Bgroup_livestock.tif")

B_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_527_m.tif")

B_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_724_m.tif")

B_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_1242_m.tif")

B_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_1545_m.tif")

B_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_2105_m.tif")

B_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_3001_m.tif")

B_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Bgroup_livestock_5886_m.tif")


B_Alllivestock_Scale <- stack( B_livestock527, B_livestock724, B_livestock1242, B_livestock1545, B_livestock2105, B_livestock3001, B_livestock5886)


### Day

avail <- data.frame(raster::extract(B_Alllivestock_Scale, coords(Bgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Bgroup_track_day), id=Bgroup_track_day$id, 
                sex=Bgroup_track_day$sex, season=Bgroup_track_day$season)

livestock <- raster::extract(B_livestock, coords(Bgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Bgroup_track_day), id=Bgroup_track_day$id, 
              sex= Bgroup_track_day$sex, season=Bgroup_track_day$season)



rsf_Blivest_day<- used
rsf_Blivest_day$livestock527  <- avail$Bgroup_livestock_527_m
rsf_Blivest_day$livestock724  <- avail$Bgroup_livestock_724_m
rsf_Blivest_day$livestock1242 <- avail$Bgroup_livestock_1242_m
rsf_Blivest_day$livestock1545 <- avail$Bgroup_livestock_1545_m 
rsf_Blivest_day$livestock2105 <- avail$Bgroup_livestock_2105_m
rsf_Blivest_day$livestock3001 <- avail$Bgroup_livestock_3001_m
rsf_Blivest_day$livestock5886 <- avail$Bgroup_livestock_5886_m 


write.csv(rsf_Blivest_day, "rsf_Blivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(B_Alllivestock_Scale, coords(Bgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Bgroup_track_night), id=Bgroup_track_night$id, 
                sex=Bgroup_track_night$sex, season=Bgroup_track_night$season)

livestock <- raster::extract(B_livestock, coords(Bgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Bgroup_track_night), id=Bgroup_track_night$id, 
              sex= Bgroup_track_night$sex, season=Bgroup_track_night$season)

rsf_Blivest_night <- used
rsf_Blivest_night$livestock527   <- avail$Bgroup_livestock_527_m
rsf_Blivest_night$livestock724   <- avail$Bgroup_livestock_724_m
rsf_Blivest_night$livestock1242  <- avail$Bgroup_livestock_1242_m
rsf_Blivest_night$livestock1545  <- avail$Bgroup_livestock_1545_m  
rsf_Blivest_night$livestock2105 <- avail$Bgroup_livestock_2105_m
rsf_Blivest_night$livestock3001 <- avail$Bgroup_livestock_3001_m
rsf_Blivest_night$livestock5886 <- avail$Bgroup_livestock_5886_m 



write.csv(rsf_Blivest_night, "rsf_Blivest_night.csv")

rm(used, avail, livestock)

# Remove Bgroup data useless ----------------------------------------------

### Rasters
rm(B_Alllivestock_Scale, B_livestock, B_livestock1242, B_livestock1545,
     B_livestock2105, B_livestock3001, B_livestock527, B_livestock5886,B_livestock724)

rm(Bgroup_jag,Bgroup_mov,Bgroup_track, Bgroup_track_day, Bgroup_track_night)

gc()

# C group -----------------------------------------------------------------

C_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Cgroup_livestock.tif")

C_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_527_m.tif")

C_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_724_m.tif")

C_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_1242_m.tif")

C_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_1545_m.tif")

C_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_2105_m.tif")

C_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_3001_m.tif")

C_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Cgroup_livestock_5886_m.tif")


C_Alllivestock_Scale <- stack( C_livestock527, C_livestock724, C_livestock1242, C_livestock1545, C_livestock2105, C_livestock3001, C_livestock5886)


### Day

avail <- data.frame(raster::extract(C_Alllivestock_Scale, coords(Cgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Cgroup_track_day), id=Cgroup_track_day$id, 
                sex=Cgroup_track_day$sex, season=Cgroup_track_day$season)

livestock <- raster::extract(C_livestock, coords(Cgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Cgroup_track_day), id=Cgroup_track_day$id, 
              sex= Cgroup_track_day$sex, season=Cgroup_track_day$season)



rsf_Clivest_day<- used
rsf_Clivest_day$livestock527  <- avail$Cgroup_livestock_527_m
rsf_Clivest_day$livestock724  <- avail$Cgroup_livestock_724_m
rsf_Clivest_day$livestock1242 <- avail$Cgroup_livestock_1242_m
rsf_Clivest_day$livestock1545 <- avail$Cgroup_livestock_1545_m 
rsf_Clivest_day$livestock2105 <- avail$Cgroup_livestock_2105_m
rsf_Clivest_day$livestock3001 <- avail$Cgroup_livestock_3001_m
rsf_Clivest_day$livestock5886 <- avail$Cgroup_livestock_5886_m 


write.csv(rsf_Clivest_day, "rsf_Clivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(C_Alllivestock_Scale, coords(Cgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Cgroup_track_night), id=Cgroup_track_night$id, 
                sex=Cgroup_track_night$sex, season=Cgroup_track_night$season)

livestock <- raster::extract(C_livestock, coords(Cgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Cgroup_track_night), id=Cgroup_track_night$id, 
              sex= Cgroup_track_night$sex, season=Cgroup_track_night$season)

rsf_Clivest_night <- used
rsf_Clivest_night$livestock527   <- avail$Cgroup_livestock_527_m
rsf_Clivest_night$livestock724   <- avail$Cgroup_livestock_724_m
rsf_Clivest_night$livestock1242  <- avail$Cgroup_livestock_1242_m
rsf_Clivest_night$livestock1545  <- avail$Cgroup_livestock_1545_m  
rsf_Clivest_night$livestock2105 <- avail$Cgroup_livestock_2105_m
rsf_Clivest_night$livestock3001 <- avail$Cgroup_livestock_3001_m
rsf_Clivest_night$livestock5886 <- avail$Cgroup_livestock_5886_m 



write.csv(rsf_Clivest_night, "rsf_Clivest_night.csv")

rm(used, avail, livestock)

# Remove Cgroup data useless ----------------------------------------------

### Rasters
rm(C_Alllivestock_Scale, C_livestock, C_livestock1242, C_livestock1545,
     C_livestock2105, C_livestock3001, C_livestock527, C_livestock5886,C_livestock724)

rm(Cgroup_jag,Cgroup_mov,Cgroup_track, Cgroup_track_day, Cgroup_track_night)

gc()

# D group -----------------------------------------------------------------

D_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Dgroup_livestock.tif")

D_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_527_m.tif")

D_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_724_m.tif")

D_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_1242_m.tif")

D_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_1545_m.tif")

D_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_2105_m.tif")

D_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_3001_m.tif")

D_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Dgroup_livestock_5886_m.tif")


D_Alllivestock_Scale <- stack( D_livestock527, D_livestock724, D_livestock1242, D_livestock1545, D_livestock2105, D_livestock3001, D_livestock5886)


### Day

avail <- data.frame(raster::extract(D_Alllivestock_Scale, coords(Dgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Dgroup_track_day), id=Dgroup_track_day$id, 
                sex=Dgroup_track_day$sex, season=Dgroup_track_day$season)

livestock <- raster::extract(D_livestock, coords(Dgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Dgroup_track_day), id=Dgroup_track_day$id, 
              sex= Dgroup_track_day$sex, season=Dgroup_track_day$season)



rsf_Dlivest_day<- used
rsf_Dlivest_day$livestock527  <- avail$Dgroup_livestock_527_m
rsf_Dlivest_day$livestock724  <- avail$Dgroup_livestock_724_m
rsf_Dlivest_day$livestock1242 <- avail$Dgroup_livestock_1242_m
rsf_Dlivest_day$livestock1545 <- avail$Dgroup_livestock_1545_m 
rsf_Dlivest_day$livestock2105 <- avail$Dgroup_livestock_2105_m
rsf_Dlivest_day$livestock3001 <- avail$Dgroup_livestock_3001_m
rsf_Dlivest_day$livestock5886 <- avail$Dgroup_livestock_5886_m 


write.csv(rsf_Dlivest_day, "rsf_Dlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(D_Alllivestock_Scale, coords(Dgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Dgroup_track_night), id=Dgroup_track_night$id, 
                sex=Dgroup_track_night$sex, season=Dgroup_track_night$season)

livestock <- raster::extract(D_livestock, coords(Dgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Dgroup_track_night), id=Dgroup_track_night$id, 
              sex= Dgroup_track_night$sex, season=Dgroup_track_night$season)

rsf_Dlivest_night <- used
rsf_Dlivest_night$livestock527   <- avail$Dgroup_livestock_527_m
rsf_Dlivest_night$livestock724   <- avail$Dgroup_livestock_724_m
rsf_Dlivest_night$livestock1242  <- avail$Dgroup_livestock_1242_m
rsf_Dlivest_night$livestock1545  <- avail$Dgroup_livestock_1545_m  
rsf_Dlivest_night$livestock2105 <- avail$Dgroup_livestock_2105_m
rsf_Dlivest_night$livestock3001 <- avail$Dgroup_livestock_3001_m
rsf_Dlivest_night$livestock5886 <- avail$Dgroup_livestock_5886_m 



write.csv(rsf_Dlivest_night, "rsf_Dlivest_night.csv")

rm(used, avail, livestock)

# Remove Dgroup data useless ----------------------------------------------

### Rasters
rm(D_Alllivestock_Scale, D_livestock, D_livestock1242, D_livestock1545,
     D_livestock2105, D_livestock3001, D_livestock527, D_livestock5886,D_livestock724)

rm(Dgroup_jag,Dgroup_mov,Dgroup_track, Dgroup_track_day, Dgroup_track_night)

gc()

# E group -----------------------------------------------------------------

E_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Egroup_livestock.tif")

E_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_527_m.tif")

E_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_724_m.tif")

E_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_1242_m.tif")

E_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_1545_m.tif")

E_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_2105_m.tif")

E_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_3001_m.tif")

E_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Egroup_livestock_5886_m.tif")


E_Alllivestock_Scale <- stack( E_livestock527, E_livestock724, E_livestock1242, E_livestock1545, E_livestock2105, E_livestock3001, E_livestock5886)


### Day

avail <- data.frame(raster::extract(E_Alllivestock_Scale, coords(Egroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Egroup_track_day), id=Egroup_track_day$id, 
                sex=Egroup_track_day$sex, season=Egroup_track_day$season)

livestock <- raster::extract(E_livestock, coords(Egroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Egroup_track_day), id=Egroup_track_day$id, 
              sex= Egroup_track_day$sex, season=Egroup_track_day$season)



rsf_Elivest_day<- used
rsf_Elivest_day$livestock527  <- avail$Egroup_livestock_527_m
rsf_Elivest_day$livestock724  <- avail$Egroup_livestock_724_m
rsf_Elivest_day$livestock1242 <- avail$Egroup_livestock_1242_m
rsf_Elivest_day$livestock1545 <- avail$Egroup_livestock_1545_m 
rsf_Elivest_day$livestock2105 <- avail$Egroup_livestock_2105_m
rsf_Elivest_day$livestock3001 <- avail$Egroup_livestock_3001_m
rsf_Elivest_day$livestock5886 <- avail$Egroup_livestock_5886_m 


write.csv(rsf_Elivest_day, "rsf_Elivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(E_Alllivestock_Scale, coords(Egroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Egroup_track_night), id=Egroup_track_night$id, 
                sex=Egroup_track_night$sex, season=Egroup_track_night$season)

livestock <- raster::extract(E_livestock, coords(Egroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Egroup_track_night), id=Egroup_track_night$id, 
              sex= Egroup_track_night$sex, season=Egroup_track_night$season)

rsf_Elivest_night <- used
rsf_Elivest_night$livestock527   <- avail$Egroup_livestock_527_m
rsf_Elivest_night$livestock724   <- avail$Egroup_livestock_724_m
rsf_Elivest_night$livestock1242  <- avail$Egroup_livestock_1242_m
rsf_Elivest_night$livestock1545  <- avail$Egroup_livestock_1545_m  
rsf_Elivest_night$livestock2105 <- avail$Egroup_livestock_2105_m
rsf_Elivest_night$livestock3001 <- avail$Egroup_livestock_3001_m
rsf_Elivest_night$livestock5886 <- avail$Egroup_livestock_5886_m 



write.csv(rsf_Elivest_night, "rsf_Elivest_night.csv")

rm(used, avail, livestock)

# Remove Egroup data useless ----------------------------------------------

### Rasters
rm(E_Alllivestock_Scale, E_livestock, E_livestock1242, E_livestock1545,
     E_livestock2105, E_livestock3001, E_livestock527, E_livestock5886,E_livestock724)

rm(Egroup_jag,Egroup_mov,Egroup_track, Egroup_track_day, Egroup_track_night)

gc()

# F group -----------------------------------------------------------------

F_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Fgroup_livestock.tif")

F_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_527_m.tif")

F_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_724_m.tif")

F_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_1242_m.tif")

F_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_1545_m.tif")

F_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_2105_m.tif")

F_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_3001_m.tif")

F_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Fgroup_livestock_5886_m.tif")


F_Alllivestock_Scale <- stack( F_livestock527, F_livestock724, F_livestock1242, F_livestock1545, F_livestock2105, F_livestock3001, F_livestock5886)


### Day

avail <- data.frame(raster::extract(F_Alllivestock_Scale, coords(Fgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Fgroup_track_day), id=Fgroup_track_day$id, 
                sex=Fgroup_track_day$sex, season=Fgroup_track_day$season)

livestock <- raster::extract(F_livestock, coords(Fgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Fgroup_track_day), id=Fgroup_track_day$id, 
              sex= Fgroup_track_day$sex, season=Fgroup_track_day$season)



rsf_Flivest_day<- used
rsf_Flivest_day$livestock527  <- avail$Fgroup_livestock_527_m
rsf_Flivest_day$livestock724  <- avail$Fgroup_livestock_724_m
rsf_Flivest_day$livestock1242 <- avail$Fgroup_livestock_1242_m
rsf_Flivest_day$livestock1545 <- avail$Fgroup_livestock_1545_m 
rsf_Flivest_day$livestock2105 <- avail$Fgroup_livestock_2105_m
rsf_Flivest_day$livestock3001 <- avail$Fgroup_livestock_3001_m
rsf_Flivest_day$livestock5886 <- avail$Fgroup_livestock_5886_m 


write.csv(rsf_Flivest_day, "rsf_Flivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(F_Alllivestock_Scale, coords(Fgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Fgroup_track_night), id=Fgroup_track_night$id, 
                sex=Fgroup_track_night$sex, season=Fgroup_track_night$season)

livestock <- raster::extract(F_livestock, coords(Fgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Fgroup_track_night), id=Fgroup_track_night$id, 
              sex= Fgroup_track_night$sex, season=Fgroup_track_night$season)

rsf_Flivest_night <- used
rsf_Flivest_night$livestock527   <- avail$Fgroup_livestock_527_m
rsf_Flivest_night$livestock724   <- avail$Fgroup_livestock_724_m
rsf_Flivest_night$livestock1242  <- avail$Fgroup_livestock_1242_m
rsf_Flivest_night$livestock1545  <- avail$Fgroup_livestock_1545_m  
rsf_Flivest_night$livestock2105 <- avail$Fgroup_livestock_2105_m
rsf_Flivest_night$livestock3001 <- avail$Fgroup_livestock_3001_m
rsf_Flivest_night$livestock5886 <- avail$Fgroup_livestock_5886_m 



write.csv(rsf_Flivest_night, "rsf_Flivest_night.csv")

rm(used, avail, livestock)

# Remove Fgroup data useless ----------------------------------------------

### Rasters
rm(F_Alllivestock_Scale, F_livestock, F_livestock1242, F_livestock1545,
     F_livestock2105, F_livestock3001, F_livestock527, F_livestock5886,F_livestock724)

rm(Fgroup_jag,Fgroup_mov,Fgroup_track, Fgroup_track_day, Fgroup_track_night)

gc()

# G group -----------------------------------------------------------------

G_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Ggroup_livestock.tif")

G_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_527_m.tif")

G_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_724_m.tif")

G_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_1242_m.tif")

G_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_1545_m.tif")

G_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_2105_m.tif")

G_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_3001_m.tif")

G_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ggroup_livestock_5886_m.tif")


G_Alllivestock_Scale <- stack( G_livestock527, G_livestock724, G_livestock1242, G_livestock1545, G_livestock2105, G_livestock3001, G_livestock5886)


### Day

avail <- data.frame(raster::extract(G_Alllivestock_Scale, coords(Ggroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ggroup_track_day), id=Ggroup_track_day$id, 
                sex=Ggroup_track_day$sex, season=Ggroup_track_day$season)

livestock <- raster::extract(G_livestock, coords(Ggroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ggroup_track_day), id=Ggroup_track_day$id, 
              sex= Ggroup_track_day$sex, season=Ggroup_track_day$season)



rsf_Glivest_day<- used
rsf_Glivest_day$livestock527  <- avail$Ggroup_livestock_527_m
rsf_Glivest_day$livestock724  <- avail$Ggroup_livestock_724_m
rsf_Glivest_day$livestock1242 <- avail$Ggroup_livestock_1242_m
rsf_Glivest_day$livestock1545 <- avail$Ggroup_livestock_1545_m 
rsf_Glivest_day$livestock2105 <- avail$Ggroup_livestock_2105_m
rsf_Glivest_day$livestock3001 <- avail$Ggroup_livestock_3001_m
rsf_Glivest_day$livestock5886 <- avail$Ggroup_livestock_5886_m 


write.csv(rsf_Glivest_day, "rsf_Glivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(G_Alllivestock_Scale, coords(Ggroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ggroup_track_night), id=Ggroup_track_night$id, 
                sex=Ggroup_track_night$sex, season=Ggroup_track_night$season)

livestock <- raster::extract(G_livestock, coords(Ggroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ggroup_track_night), id=Ggroup_track_night$id, 
              sex= Ggroup_track_night$sex, season=Ggroup_track_night$season)

rsf_Glivest_night <- used
rsf_Glivest_night$livestock527   <- avail$Ggroup_livestock_527_m
rsf_Glivest_night$livestock724   <- avail$Ggroup_livestock_724_m
rsf_Glivest_night$livestock1242  <- avail$Ggroup_livestock_1242_m
rsf_Glivest_night$livestock1545  <- avail$Ggroup_livestock_1545_m  
rsf_Glivest_night$livestock2105 <- avail$Ggroup_livestock_2105_m
rsf_Glivest_night$livestock3001 <- avail$Ggroup_livestock_3001_m
rsf_Glivest_night$livestock5886 <- avail$Ggroup_livestock_5886_m 



write.csv(rsf_Glivest_night, "rsf_Glivest_night.csv")

rm(used, avail, livestock)

# Remove Ggroup data useless ----------------------------------------------

### Rasters
rm(G_Alllivestock_Scale, G_livestock, G_livestock1242, G_livestock1545,
     G_livestock2105, G_livestock3001, G_livestock527, G_livestock5886,G_livestock724)

rm(Ggroup_jag,Ggroup_mov,Ggroup_track, Ggroup_track_day, Ggroup_track_night)

gc()
# H group -----------------------------------------------------------------

H_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Hgroup_livestock.tif")

H_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_527_m.tif")

H_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_724_m.tif")

H_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_1242_m.tif")

H_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_1545_m.tif")

H_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_2105_m.tif")

H_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_3001_m.tif")

H_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Hgroup_livestock_5886_m.tif")


H_Alllivestock_Scale <- stack( H_livestock527, H_livestock724, H_livestock1242, H_livestock1545, H_livestock2105, H_livestock3001, H_livestock5886)


### Day

avail <- data.frame(raster::extract(H_Alllivestock_Scale, coords(Hgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Hgroup_track_day), id=Hgroup_track_day$id, 
                sex=Hgroup_track_day$sex, season=Hgroup_track_day$season)

livestock <- raster::extract(H_livestock, coords(Hgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Hgroup_track_day), id=Hgroup_track_day$id, 
              sex= Hgroup_track_day$sex, season=Hgroup_track_day$season)



rsf_Hlivest_day<- used
rsf_Hlivest_day$livestock527  <- avail$Hgroup_livestock_527_m
rsf_Hlivest_day$livestock724  <- avail$Hgroup_livestock_724_m
rsf_Hlivest_day$livestock1242 <- avail$Hgroup_livestock_1242_m
rsf_Hlivest_day$livestock1545 <- avail$Hgroup_livestock_1545_m 
rsf_Hlivest_day$livestock2105 <- avail$Hgroup_livestock_2105_m
rsf_Hlivest_day$livestock3001 <- avail$Hgroup_livestock_3001_m
rsf_Hlivest_day$livestock5886 <- avail$Hgroup_livestock_5886_m 


write.csv(rsf_Hlivest_day, "rsf_Hlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(H_Alllivestock_Scale, coords(Hgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Hgroup_track_night), id=Hgroup_track_night$id, 
                sex=Hgroup_track_night$sex, season=Hgroup_track_night$season)

livestock <- raster::extract(H_livestock, coords(Hgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Hgroup_track_night), id=Hgroup_track_night$id, 
              sex= Hgroup_track_night$sex, season=Hgroup_track_night$season)

rsf_Hlivest_night <- used
rsf_Hlivest_night$livestock527   <- avail$Hgroup_livestock_527_m
rsf_Hlivest_night$livestock724   <- avail$Hgroup_livestock_724_m
rsf_Hlivest_night$livestock1242  <- avail$Hgroup_livestock_1242_m
rsf_Hlivest_night$livestock1545  <- avail$Hgroup_livestock_1545_m  
rsf_Hlivest_night$livestock2105 <- avail$Hgroup_livestock_2105_m
rsf_Hlivest_night$livestock3001 <- avail$Hgroup_livestock_3001_m
rsf_Hlivest_night$livestock5886 <- avail$Hgroup_livestock_5886_m 



write.csv(rsf_Hlivest_night, "rsf_Hlivest_night.csv")

rm(used, avail, livestock)

# Remove Hgroup data useless ----------------------------------------------

### Rasters
rm(H_Alllivestock_Scale, H_livestock, H_livestock1242, H_livestock1545,
     H_livestock2105, H_livestock3001, H_livestock527, H_livestock5886,H_livestock724)

rm(Hgroup_jag,Hgroup_mov,Hgroup_track, Hgroup_track_day, Hgroup_track_night)

gc()
# I group -----------------------------------------------------------------

I_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Igroup_livestock.tif")

I_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_527_m.tif")

I_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_724_m.tif")

I_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_1242_m.tif")

I_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_1545_m.tif")

I_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_2105_m.tif")

I_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_3001_m.tif")

I_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Igroup_livestock_5886_m.tif")


I_Alllivestock_Scale <- stack( I_livestock527, I_livestock724, I_livestock1242, I_livestock1545, I_livestock2105, I_livestock3001, I_livestock5886)


### Day

avail <- data.frame(raster::extract(I_Alllivestock_Scale, coords(Igroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Igroup_track_day), id=Igroup_track_day$id, 
                sex=Igroup_track_day$sex, season=Igroup_track_day$season)

livestock <- raster::extract(I_livestock, coords(Igroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Igroup_track_day), id=Igroup_track_day$id, 
              sex= Igroup_track_day$sex, season=Igroup_track_day$season)



rsf_Ilivest_day<- used
rsf_Ilivest_day$livestock527  <- avail$Igroup_livestock_527_m
rsf_Ilivest_day$livestock724  <- avail$Igroup_livestock_724_m
rsf_Ilivest_day$livestock1242 <- avail$Igroup_livestock_1242_m
rsf_Ilivest_day$livestock1545 <- avail$Igroup_livestock_1545_m 
rsf_Ilivest_day$livestock2105 <- avail$Igroup_livestock_2105_m
rsf_Ilivest_day$livestock3001 <- avail$Igroup_livestock_3001_m
rsf_Ilivest_day$livestock5886 <- avail$Igroup_livestock_5886_m 


write.csv(rsf_Ilivest_day, "rsf_Ilivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(I_Alllivestock_Scale, coords(Igroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Igroup_track_night), id=Igroup_track_night$id, 
                sex=Igroup_track_night$sex, season=Igroup_track_night$season)

livestock <- raster::extract(I_livestock, coords(Igroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Igroup_track_night), id=Igroup_track_night$id, 
              sex= Igroup_track_night$sex, season=Igroup_track_night$season)

rsf_Ilivest_night <- used
rsf_Ilivest_night$livestock527   <- avail$Igroup_livestock_527_m
rsf_Ilivest_night$livestock724   <- avail$Igroup_livestock_724_m
rsf_Ilivest_night$livestock1242  <- avail$Igroup_livestock_1242_m
rsf_Ilivest_night$livestock1545  <- avail$Igroup_livestock_1545_m  
rsf_Ilivest_night$livestock2105 <- avail$Igroup_livestock_2105_m
rsf_Ilivest_night$livestock3001 <- avail$Igroup_livestock_3001_m
rsf_Ilivest_night$livestock5886 <- avail$Igroup_livestock_5886_m 



write.csv(rsf_Ilivest_night, "rsf_Ilivest_night.csv")

rm(used, avail, livestock)

# Remove Igroup data useless ----------------------------------------------

### Rasters
rm(I_Alllivestock_Scale, I_livestock, I_livestock1242, I_livestock1545,
     I_livestock2105, I_livestock3001, I_livestock527, I_livestock5886,I_livestock724)

rm(Igroup_jag,Igroup_mov,Igroup_track, Igroup_track_day, Igroup_track_night)

gc()

# J group -----------------------------------------------------------------

J_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Jgroup_livestock.tif")

J_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_527_m.tif")

J_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_724_m.tif")

J_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_1242_m.tif")

J_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_1545_m.tif")

J_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_2105_m.tif")

J_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_3001_m.tif")

J_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Jgroup_livestock_5886_m.tif")


J_Alllivestock_Scale <- stack( J_livestock527, J_livestock724, J_livestock1242, J_livestock1545, J_livestock2105, J_livestock3001, J_livestock5886)


### Day

avail <- data.frame(raster::extract(J_Alllivestock_Scale, coords(Jgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Jgroup_track_day), id=Jgroup_track_day$id, 
                sex=Jgroup_track_day$sex, season=Jgroup_track_day$season)

livestock <- raster::extract(J_livestock, coords(Jgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Jgroup_track_day), id=Jgroup_track_day$id, 
              sex= Jgroup_track_day$sex, season=Jgroup_track_day$season)



rsf_Jlivest_day<- used
rsf_Jlivest_day$livestock527  <- avail$Jgroup_livestock_527_m
rsf_Jlivest_day$livestock724  <- avail$Jgroup_livestock_724_m
rsf_Jlivest_day$livestock1242 <- avail$Jgroup_livestock_1242_m
rsf_Jlivest_day$livestock1545 <- avail$Jgroup_livestock_1545_m 
rsf_Jlivest_day$livestock2105 <- avail$Jgroup_livestock_2105_m
rsf_Jlivest_day$livestock3001 <- avail$Jgroup_livestock_3001_m
rsf_Jlivest_day$livestock5886 <- avail$Jgroup_livestock_5886_m 


write.csv(rsf_Jlivest_day, "rsf_Jlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(J_Alllivestock_Scale, coords(Jgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Jgroup_track_night), id=Jgroup_track_night$id, 
                sex=Jgroup_track_night$sex, season=Jgroup_track_night$season)

livestock <- raster::extract(J_livestock, coords(Jgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Jgroup_track_night), id=Jgroup_track_night$id, 
              sex= Jgroup_track_night$sex, season=Jgroup_track_night$season)

rsf_Jlivest_night <- used
rsf_Jlivest_night$livestock527   <- avail$Jgroup_livestock_527_m
rsf_Jlivest_night$livestock724   <- avail$Jgroup_livestock_724_m
rsf_Jlivest_night$livestock1242  <- avail$Jgroup_livestock_1242_m
rsf_Jlivest_night$livestock1545  <- avail$Jgroup_livestock_1545_m  
rsf_Jlivest_night$livestock2105 <- avail$Jgroup_livestock_2105_m
rsf_Jlivest_night$livestock3001 <- avail$Jgroup_livestock_3001_m
rsf_Jlivest_night$livestock5886 <- avail$Jgroup_livestock_5886_m 



write.csv(rsf_Jlivest_night, "rsf_Jlivest_night.csv")

rm(used, avail, livestock)

# Remove Jgroup data useless ----------------------------------------------

### Rasters
rm(J_Alllivestock_Scale, J_livestock, J_livestock1242, J_livestock1545,
     J_livestock2105, J_livestock3001, J_livestock527, J_livestock5886,J_livestock724)

rm(Jgroup_jag,Jgroup_mov,Jgroup_track, Jgroup_track_day, Jgroup_track_night)

gc()

# K group -----------------------------------------------------------------

K_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Kgroup_livestock.tif")

K_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_527_m.tif")

K_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_724_m.tif")

K_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_1242_m.tif")

K_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_1545_m.tif")

K_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_2105_m.tif")

K_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_3001_m.tif")

K_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Kgroup_livestock_5886_m.tif")


K_Alllivestock_Scale <- stack( K_livestock527, K_livestock724, K_livestock1242, K_livestock1545, K_livestock2105, K_livestock3001, K_livestock5886)


### Day

avail <- data.frame(raster::extract(K_Alllivestock_Scale, coords(Kgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Kgroup_track_day), id=Kgroup_track_day$id, 
                sex=Kgroup_track_day$sex, season=Kgroup_track_day$season)

livestock <- raster::extract(K_livestock, coords(Kgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Kgroup_track_day), id=Kgroup_track_day$id, 
              sex= Kgroup_track_day$sex, season=Kgroup_track_day$season)



rsf_Klivest_day<- used
rsf_Klivest_day$livestock527  <- avail$Kgroup_livestock_527_m
rsf_Klivest_day$livestock724  <- avail$Kgroup_livestock_724_m
rsf_Klivest_day$livestock1242 <- avail$Kgroup_livestock_1242_m
rsf_Klivest_day$livestock1545 <- avail$Kgroup_livestock_1545_m 
rsf_Klivest_day$livestock2105 <- avail$Kgroup_livestock_2105_m
rsf_Klivest_day$livestock3001 <- avail$Kgroup_livestock_3001_m
rsf_Klivest_day$livestock5886 <- avail$Kgroup_livestock_5886_m 


write.csv(rsf_Klivest_day, "rsf_Klivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(K_Alllivestock_Scale, coords(Kgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Kgroup_track_night), id=Kgroup_track_night$id, 
                sex=Kgroup_track_night$sex, season=Kgroup_track_night$season)

livestock <- raster::extract(K_livestock, coords(Kgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Kgroup_track_night), id=Kgroup_track_night$id, 
              sex= Kgroup_track_night$sex, season=Kgroup_track_night$season)

rsf_Klivest_night <- used
rsf_Klivest_night$livestock527   <- avail$Kgroup_livestock_527_m
rsf_Klivest_night$livestock724   <- avail$Kgroup_livestock_724_m
rsf_Klivest_night$livestock1242  <- avail$Kgroup_livestock_1242_m
rsf_Klivest_night$livestock1545  <- avail$Kgroup_livestock_1545_m  
rsf_Klivest_night$livestock2105 <- avail$Kgroup_livestock_2105_m
rsf_Klivest_night$livestock3001 <- avail$Kgroup_livestock_3001_m
rsf_Klivest_night$livestock5886 <- avail$Kgroup_livestock_5886_m 



write.csv(rsf_Klivest_night, "rsf_Klivest_night.csv")

rm(used, avail, livestock)

# Remove Kgroup data useless ----------------------------------------------

### Rasters
rm(K_Alllivestock_Scale, K_livestock, K_livestock1242, K_livestock1545,
     K_livestock2105, K_livestock3001, K_livestock527, K_livestock5886,K_livestock724)

rm(Kgroup_jag,Kgroup_mov,Kgroup_track, Kgroup_track_day, Kgroup_track_night)

gc()

# L group -----------------------------------------------------------------

L_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Lgroup_livestock.tif")

L_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_527_m.tif")

L_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_724_m.tif")

L_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_1242_m.tif")

L_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_1545_m.tif")

L_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_2105_m.tif")

L_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_3001_m.tif")

L_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Lgroup_livestock_5886_m.tif")


L_Alllivestock_Scale <- stack( L_livestock527, L_livestock724, L_livestock1242, L_livestock1545, L_livestock2105, L_livestock3001, L_livestock5886)


### Day

avail <- data.frame(raster::extract(L_Alllivestock_Scale, coords(Lgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Lgroup_track_day), id=Lgroup_track_day$id, 
                sex=Lgroup_track_day$sex, season=Lgroup_track_day$season)

livestock <- raster::extract(L_livestock, coords(Lgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Lgroup_track_day), id=Lgroup_track_day$id, 
              sex= Lgroup_track_day$sex, season=Lgroup_track_day$season)



rsf_Llivest_day<- used
rsf_Llivest_day$livestock527  <- avail$Lgroup_livestock_527_m
rsf_Llivest_day$livestock724  <- avail$Lgroup_livestock_724_m
rsf_Llivest_day$livestock1242 <- avail$Lgroup_livestock_1242_m
rsf_Llivest_day$livestock1545 <- avail$Lgroup_livestock_1545_m 
rsf_Llivest_day$livestock2105 <- avail$Lgroup_livestock_2105_m
rsf_Llivest_day$livestock3001 <- avail$Lgroup_livestock_3001_m
rsf_Llivest_day$livestock5886 <- avail$Lgroup_livestock_5886_m 


write.csv(rsf_Llivest_day, "rsf_Llivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(L_Alllivestock_Scale, coords(Lgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Lgroup_track_night), id=Lgroup_track_night$id, 
                sex=Lgroup_track_night$sex, season=Lgroup_track_night$season)

livestock <- raster::extract(L_livestock, coords(Lgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Lgroup_track_night), id=Lgroup_track_night$id, 
              sex= Lgroup_track_night$sex, season=Lgroup_track_night$season)

rsf_Llivest_night <- used
rsf_Llivest_night$livestock527   <- avail$Lgroup_livestock_527_m
rsf_Llivest_night$livestock724   <- avail$Lgroup_livestock_724_m
rsf_Llivest_night$livestock1242  <- avail$Lgroup_livestock_1242_m
rsf_Llivest_night$livestock1545  <- avail$Lgroup_livestock_1545_m  
rsf_Llivest_night$livestock2105 <- avail$Lgroup_livestock_2105_m
rsf_Llivest_night$livestock3001 <- avail$Lgroup_livestock_3001_m
rsf_Llivest_night$livestock5886 <- avail$Lgroup_livestock_5886_m 



write.csv(rsf_Llivest_night, "rsf_Llivest_night.csv")

rm(used, avail, livestock)

# Remove Lgroup data useless ----------------------------------------------

### Rasters
rm(L_Alllivestock_Scale, L_livestock, L_livestock1242, L_livestock1545,
     L_livestock2105, L_livestock3001, L_livestock527, L_livestock5886,L_livestock724)

rm(Lgroup_jag,Lgroup_mov,Lgroup_track, Lgroup_track_day, Lgroup_track_night)

gc()

# M group -----------------------------------------------------------------

M_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Mgroup_livestock.tif")

M_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_527_m.tif")

M_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_724_m.tif")

M_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_1242_m.tif")

M_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_1545_m.tif")

M_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_2105_m.tif")

M_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_3001_m.tif")

M_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Mgroup_livestock_5886_m.tif")


M_Alllivestock_Scale <- stack( M_livestock527, M_livestock724, M_livestock1242, M_livestock1545, M_livestock2105, M_livestock3001, M_livestock5886)


### Day

avail <- data.frame(raster::extract(M_Alllivestock_Scale, coords(Mgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Mgroup_track_day), id=Mgroup_track_day$id, 
                sex=Mgroup_track_day$sex, season=Mgroup_track_day$season)

livestock <- raster::extract(M_livestock, coords(Mgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Mgroup_track_day), id=Mgroup_track_day$id, 
              sex= Mgroup_track_day$sex, season=Mgroup_track_day$season)



rsf_Mlivest_day<- used
rsf_Mlivest_day$livestock527  <- avail$Mgroup_livestock_527_m
rsf_Mlivest_day$livestock724  <- avail$Mgroup_livestock_724_m
rsf_Mlivest_day$livestock1242 <- avail$Mgroup_livestock_1242_m
rsf_Mlivest_day$livestock1545 <- avail$Mgroup_livestock_1545_m 
rsf_Mlivest_day$livestock2105 <- avail$Mgroup_livestock_2105_m
rsf_Mlivest_day$livestock3001 <- avail$Mgroup_livestock_3001_m
rsf_Mlivest_day$livestock5886 <- avail$Mgroup_livestock_5886_m 


write.csv(rsf_Mlivest_day, "rsf_Mlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(M_Alllivestock_Scale, coords(Mgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Mgroup_track_night), id=Mgroup_track_night$id, 
                sex=Mgroup_track_night$sex, season=Mgroup_track_night$season)

livestock <- raster::extract(M_livestock, coords(Mgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Mgroup_track_night), id=Mgroup_track_night$id, 
              sex= Mgroup_track_night$sex, season=Mgroup_track_night$season)

rsf_Mlivest_night <- used
rsf_Mlivest_night$livestock527   <- avail$Mgroup_livestock_527_m
rsf_Mlivest_night$livestock724   <- avail$Mgroup_livestock_724_m
rsf_Mlivest_night$livestock1242  <- avail$Mgroup_livestock_1242_m
rsf_Mlivest_night$livestock1545  <- avail$Mgroup_livestock_1545_m  
rsf_Mlivest_night$livestock2105 <- avail$Mgroup_livestock_2105_m
rsf_Mlivest_night$livestock3001 <- avail$Mgroup_livestock_3001_m
rsf_Mlivest_night$livestock5886 <- avail$Mgroup_livestock_5886_m 



write.csv(rsf_Mlivest_night, "rsf_Mlivest_night.csv")

rm(used, avail, livestock)

# Remove Mgroup data useless ----------------------------------------------

### Rasters
rm(M_Alllivestock_Scale, M_livestock, M_livestock1242, M_livestock1545,
     M_livestock2105, M_livestock3001, M_livestock527, M_livestock5886,M_livestock724)

rm(Mgroup_jag,Mgroup_mov,Mgroup_track, Mgroup_track_day, Mgroup_track_night)

gc()

# N group -----------------------------------------------------------------

N_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Ngroup_livestock.tif")

N_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_527_m.tif")

N_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_724_m.tif")

N_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_1242_m.tif")

N_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_1545_m.tif")

N_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_2105_m.tif")

N_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_3001_m.tif")

N_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ngroup_livestock_5886_m.tif")


N_Alllivestock_Scale <- stack( N_livestock527, N_livestock724, N_livestock1242, N_livestock1545, N_livestock2105, N_livestock3001, N_livestock5886)


### Day

avail <- data.frame(raster::extract(N_Alllivestock_Scale, coords(Ngroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ngroup_track_day), id=Ngroup_track_day$id, 
                sex=Ngroup_track_day$sex, season=Ngroup_track_day$season)

livestock <- raster::extract(N_livestock, coords(Ngroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ngroup_track_day), id=Ngroup_track_day$id, 
              sex= Ngroup_track_day$sex, season=Ngroup_track_day$season)



rsf_Nlivest_day<- used
rsf_Nlivest_day$livestock527  <- avail$Ngroup_livestock_527_m
rsf_Nlivest_day$livestock724  <- avail$Ngroup_livestock_724_m
rsf_Nlivest_day$livestock1242 <- avail$Ngroup_livestock_1242_m
rsf_Nlivest_day$livestock1545 <- avail$Ngroup_livestock_1545_m 
rsf_Nlivest_day$livestock2105 <- avail$Ngroup_livestock_2105_m
rsf_Nlivest_day$livestock3001 <- avail$Ngroup_livestock_3001_m
rsf_Nlivest_day$livestock5886 <- avail$Ngroup_livestock_5886_m 


write.csv(rsf_Nlivest_day, "rsf_Nlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(N_Alllivestock_Scale, coords(Ngroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ngroup_track_night), id=Ngroup_track_night$id, 
                sex=Ngroup_track_night$sex, season=Ngroup_track_night$season)

livestock <- raster::extract(N_livestock, coords(Ngroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ngroup_track_night), id=Ngroup_track_night$id, 
              sex= Ngroup_track_night$sex, season=Ngroup_track_night$season)

rsf_Nlivest_night <- used
rsf_Nlivest_night$livestock527   <- avail$Ngroup_livestock_527_m
rsf_Nlivest_night$livestock724   <- avail$Ngroup_livestock_724_m
rsf_Nlivest_night$livestock1242  <- avail$Ngroup_livestock_1242_m
rsf_Nlivest_night$livestock1545  <- avail$Ngroup_livestock_1545_m  
rsf_Nlivest_night$livestock2105 <- avail$Ngroup_livestock_2105_m
rsf_Nlivest_night$livestock3001 <- avail$Ngroup_livestock_3001_m
rsf_Nlivest_night$livestock5886 <- avail$Ngroup_livestock_5886_m 



write.csv(rsf_Nlivest_night, "rsf_Nlivest_night.csv")

rm(used, avail, livestock)

# Remove Ngroup data useless ----------------------------------------------

### Rasters
rm(N_Alllivestock_Scale, N_livestock, N_livestock1242, N_livestock1545,
     N_livestock2105, N_livestock3001, N_livestock527, N_livestock5886,N_livestock724)

rm(Ngroup_jag,Ngroup_mov,Ngroup_track, Ngroup_track_day, Ngroup_track_night)

gc()

# O group -----------------------------------------------------------------

O_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Ogroup_livestock.tif")

O_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_527_m.tif")

O_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_724_m.tif")

O_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_1242_m.tif")

O_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_1545_m.tif")

O_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_2105_m.tif")

O_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_3001_m.tif")

O_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ogroup_livestock_5886_m.tif")


O_Alllivestock_Scale <- stack( O_livestock527, O_livestock724, O_livestock1242, O_livestock1545, O_livestock2105, O_livestock3001, O_livestock5886)


### Day

avail <- data.frame(raster::extract(O_Alllivestock_Scale, coords(Ogroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ogroup_track_day), id=Ogroup_track_day$id, 
                sex=Ogroup_track_day$sex, season=Ogroup_track_day$season)

livestock <- raster::extract(O_livestock, coords(Ogroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ogroup_track_day), id=Ogroup_track_day$id, 
              sex= Ogroup_track_day$sex, season=Ogroup_track_day$season)



rsf_Olivest_day<- used
rsf_Olivest_day$livestock527  <- avail$Ogroup_livestock_527_m
rsf_Olivest_day$livestock724  <- avail$Ogroup_livestock_724_m
rsf_Olivest_day$livestock1242 <- avail$Ogroup_livestock_1242_m
rsf_Olivest_day$livestock1545 <- avail$Ogroup_livestock_1545_m 
rsf_Olivest_day$livestock2105 <- avail$Ogroup_livestock_2105_m
rsf_Olivest_day$livestock3001 <- avail$Ogroup_livestock_3001_m
rsf_Olivest_day$livestock5886 <- avail$Ogroup_livestock_5886_m 


write.csv(rsf_Olivest_day, "rsf_Olivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(O_Alllivestock_Scale, coords(Ogroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ogroup_track_night), id=Ogroup_track_night$id, 
                sex=Ogroup_track_night$sex, season=Ogroup_track_night$season)

livestock <- raster::extract(O_livestock, coords(Ogroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ogroup_track_night), id=Ogroup_track_night$id, 
              sex= Ogroup_track_night$sex, season=Ogroup_track_night$season)

rsf_Olivest_night <- used
rsf_Olivest_night$livestock527   <- avail$Ogroup_livestock_527_m
rsf_Olivest_night$livestock724   <- avail$Ogroup_livestock_724_m
rsf_Olivest_night$livestock1242  <- avail$Ogroup_livestock_1242_m
rsf_Olivest_night$livestock1545  <- avail$Ogroup_livestock_1545_m  
rsf_Olivest_night$livestock2105 <- avail$Ogroup_livestock_2105_m
rsf_Olivest_night$livestock3001 <- avail$Ogroup_livestock_3001_m
rsf_Olivest_night$livestock5886 <- avail$Ogroup_livestock_5886_m 



write.csv(rsf_Olivest_night, "rsf_Olivest_night.csv")

rm(used, avail, livestock)

# Remove Ogroup data useless ----------------------------------------------

### Rasters
rm(O_Alllivestock_Scale, O_livestock, O_livestock1242, O_livestock1545,
     O_livestock2105, O_livestock3001, O_livestock527, O_livestock5886,O_livestock724)

rm(Ogroup_jag,Ogroup_mov,Ogroup_track, Ogroup_track_day, Ogroup_track_night)

gc()

# P group -----------------------------------------------------------------

P_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Pgroup_livestock.tif")

P_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_527_m.tif")

P_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_724_m.tif")

P_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_1242_m.tif")

P_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_1545_m.tif")

P_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_2105_m.tif")

P_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_3001_m.tif")

P_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Pgroup_livestock_5886_m.tif")


P_Alllivestock_Scale <- stack( P_livestock527, P_livestock724, P_livestock1242, P_livestock1545, P_livestock2105, P_livestock3001, P_livestock5886)


### Day

avail <- data.frame(raster::extract(P_Alllivestock_Scale, coords(Pgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Pgroup_track_day), id=Pgroup_track_day$id, 
                sex=Pgroup_track_day$sex, season=Pgroup_track_day$season)

livestock <- raster::extract(P_livestock, coords(Pgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Pgroup_track_day), id=Pgroup_track_day$id, 
              sex= Pgroup_track_day$sex, season=Pgroup_track_day$season)



rsf_Plivest_day<- used
rsf_Plivest_day$livestock527  <- avail$Pgroup_livestock_527_m
rsf_Plivest_day$livestock724  <- avail$Pgroup_livestock_724_m
rsf_Plivest_day$livestock1242 <- avail$Pgroup_livestock_1242_m
rsf_Plivest_day$livestock1545 <- avail$Pgroup_livestock_1545_m 
rsf_Plivest_day$livestock2105 <- avail$Pgroup_livestock_2105_m
rsf_Plivest_day$livestock3001 <- avail$Pgroup_livestock_3001_m
rsf_Plivest_day$livestock5886 <- avail$Pgroup_livestock_5886_m 


write.csv(rsf_Plivest_day, "rsf_Plivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(P_Alllivestock_Scale, coords(Pgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Pgroup_track_night), id=Pgroup_track_night$id, 
                sex=Pgroup_track_night$sex, season=Pgroup_track_night$season)

livestock <- raster::extract(P_livestock, coords(Pgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Pgroup_track_night), id=Pgroup_track_night$id, 
              sex= Pgroup_track_night$sex, season=Pgroup_track_night$season)

rsf_Plivest_night <- used
rsf_Plivest_night$livestock527   <- avail$Pgroup_livestock_527_m
rsf_Plivest_night$livestock724   <- avail$Pgroup_livestock_724_m
rsf_Plivest_night$livestock1242  <- avail$Pgroup_livestock_1242_m
rsf_Plivest_night$livestock1545  <- avail$Pgroup_livestock_1545_m  
rsf_Plivest_night$livestock2105 <- avail$Pgroup_livestock_2105_m
rsf_Plivest_night$livestock3001 <- avail$Pgroup_livestock_3001_m
rsf_Plivest_night$livestock5886 <- avail$Pgroup_livestock_5886_m 



write.csv(rsf_Plivest_night, "rsf_Plivest_night.csv")

rm(used, avail, livestock)

# Remove Pgroup data useless ----------------------------------------------

### Rasters
rm(P_Alllivestock_Scale, P_livestock, P_livestock1242, P_livestock1545,
     P_livestock2105, P_livestock3001, P_livestock527, P_livestock5886,P_livestock724)

rm(Pgroup_jag,Pgroup_mov,Pgroup_track, Pgroup_track_day, Pgroup_track_night)

gc()

# Q group -----------------------------------------------------------------

Q_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Qgroup_livestock.tif")

Q_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_527_m.tif")

Q_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_724_m.tif")

Q_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_1242_m.tif")

Q_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_1545_m.tif")

Q_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_2105_m.tif")

Q_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_3001_m.tif")

Q_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Qgroup_livestock_5886_m.tif")


Q_Alllivestock_Scale <- stack( Q_livestock527, Q_livestock724, Q_livestock1242, Q_livestock1545, Q_livestock2105, Q_livestock3001, Q_livestock5886)


### Day

avail <- data.frame(raster::extract(Q_Alllivestock_Scale, coords(Qgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Qgroup_track_day), id=Qgroup_track_day$id, 
                sex=Qgroup_track_day$sex, season=Qgroup_track_day$season)

livestock <- raster::extract(Q_livestock, coords(Qgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Qgroup_track_day), id=Qgroup_track_day$id, 
              sex= Qgroup_track_day$sex, season=Qgroup_track_day$season)



rsf_Qlivest_day<- used
rsf_Qlivest_day$livestock527  <- avail$Qgroup_livestock_527_m
rsf_Qlivest_day$livestock724  <- avail$Qgroup_livestock_724_m
rsf_Qlivest_day$livestock1242 <- avail$Qgroup_livestock_1242_m
rsf_Qlivest_day$livestock1545 <- avail$Qgroup_livestock_1545_m 
rsf_Qlivest_day$livestock2105 <- avail$Qgroup_livestock_2105_m
rsf_Qlivest_day$livestock3001 <- avail$Qgroup_livestock_3001_m
rsf_Qlivest_day$livestock5886 <- avail$Qgroup_livestock_5886_m 


write.csv(rsf_Qlivest_day, "rsf_Qlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(Q_Alllivestock_Scale, coords(Qgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Qgroup_track_night), id=Qgroup_track_night$id, 
                sex=Qgroup_track_night$sex, season=Qgroup_track_night$season)

livestock <- raster::extract(Q_livestock, coords(Qgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Qgroup_track_night), id=Qgroup_track_night$id, 
              sex= Qgroup_track_night$sex, season=Qgroup_track_night$season)

rsf_Qlivest_night <- used
rsf_Qlivest_night$livestock527   <- avail$Qgroup_livestock_527_m
rsf_Qlivest_night$livestock724   <- avail$Qgroup_livestock_724_m
rsf_Qlivest_night$livestock1242  <- avail$Qgroup_livestock_1242_m
rsf_Qlivest_night$livestock1545  <- avail$Qgroup_livestock_1545_m  
rsf_Qlivest_night$livestock2105 <- avail$Qgroup_livestock_2105_m
rsf_Qlivest_night$livestock3001 <- avail$Qgroup_livestock_3001_m
rsf_Qlivest_night$livestock5886 <- avail$Qgroup_livestock_5886_m 



write.csv(rsf_Qlivest_night, "rsf_Qlivest_night.csv")

rm(used, avail, livestock)

# Remove Qgroup data useless ----------------------------------------------

### Rasters
rm(Q_Alllivestock_Scale, Q_livestock, Q_livestock1242, Q_livestock1545,
     Q_livestock2105, Q_livestock3001, Q_livestock527, Q_livestock5886,Q_livestock724)

rm(Qgroup_jag,Qgroup_mov,Qgroup_track, Qgroup_track_day, Qgroup_track_night)

gc()

# R group -----------------------------------------------------------------

R_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Rgroup_livestock.tif")

R_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_527_m.tif")

R_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_724_m.tif")

R_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_1242_m.tif")

R_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_1545_m.tif")

R_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_2105_m.tif")

R_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_3001_m.tif")

R_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Rgroup_livestock_5886_m.tif")


R_Alllivestock_Scale <- stack( R_livestock527, R_livestock724, R_livestock1242, R_livestock1545, R_livestock2105, R_livestock3001, R_livestock5886)


### Day

avail <- data.frame(raster::extract(R_Alllivestock_Scale, coords(Rgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Rgroup_track_day), id=Rgroup_track_day$id, 
                sex=Rgroup_track_day$sex, season=Rgroup_track_day$season)

livestock <- raster::extract(R_livestock, coords(Rgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Rgroup_track_day), id=Rgroup_track_day$id, 
              sex= Rgroup_track_day$sex, season=Rgroup_track_day$season)



rsf_Rlivest_day<- used
rsf_Rlivest_day$livestock527  <- avail$Rgroup_livestock_527_m
rsf_Rlivest_day$livestock724  <- avail$Rgroup_livestock_724_m
rsf_Rlivest_day$livestock1242 <- avail$Rgroup_livestock_1242_m
rsf_Rlivest_day$livestock1545 <- avail$Rgroup_livestock_1545_m 
rsf_Rlivest_day$livestock2105 <- avail$Rgroup_livestock_2105_m
rsf_Rlivest_day$livestock3001 <- avail$Rgroup_livestock_3001_m
rsf_Rlivest_day$livestock5886 <- avail$Rgroup_livestock_5886_m 


write.csv(rsf_Rlivest_day, "rsf_Rlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(R_Alllivestock_Scale, coords(Rgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Rgroup_track_night), id=Rgroup_track_night$id, 
                sex=Rgroup_track_night$sex, season=Rgroup_track_night$season)

livestock <- raster::extract(R_livestock, coords(Rgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Rgroup_track_night), id=Rgroup_track_night$id, 
              sex= Rgroup_track_night$sex, season=Rgroup_track_night$season)

rsf_Rlivest_night <- used
rsf_Rlivest_night$livestock527   <- avail$Rgroup_livestock_527_m
rsf_Rlivest_night$livestock724   <- avail$Rgroup_livestock_724_m
rsf_Rlivest_night$livestock1242  <- avail$Rgroup_livestock_1242_m
rsf_Rlivest_night$livestock1545  <- avail$Rgroup_livestock_1545_m  
rsf_Rlivest_night$livestock2105 <- avail$Rgroup_livestock_2105_m
rsf_Rlivest_night$livestock3001 <- avail$Rgroup_livestock_3001_m
rsf_Rlivest_night$livestock5886 <- avail$Rgroup_livestock_5886_m 



write.csv(rsf_Rlivest_night, "rsf_Rlivest_night.csv")

rm(used, avail, livestock)

# Remove Rgroup data useless ----------------------------------------------

### Rasters
rm(R_Alllivestock_Scale, R_livestock, R_livestock1242, R_livestock1545,
     R_livestock2105, R_livestock3001, R_livestock527, R_livestock5886,R_livestock724)

rm(Rgroup_jag,Rgroup_mov,Rgroup_track, Rgroup_track_day, Rgroup_track_night)

gc()

# S group -----------------------------------------------------------------

S_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Sgroup_livestock.tif")

S_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_527_m.tif")

S_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_724_m.tif")

S_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_1242_m.tif")

S_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_1545_m.tif")

S_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_2105_m.tif")

S_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_3001_m.tif")

S_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Sgroup_livestock_5886_m.tif")


S_Alllivestock_Scale <- stack( S_livestock527, S_livestock724, S_livestock1242, S_livestock1545, S_livestock2105, S_livestock3001, S_livestock5886)


### Day

avail <- data.frame(raster::extract(S_Alllivestock_Scale, coords(Sgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Sgroup_track_day), id=Sgroup_track_day$id, 
                sex=Sgroup_track_day$sex, season=Sgroup_track_day$season)

livestock <- raster::extract(S_livestock, coords(Sgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Sgroup_track_day), id=Sgroup_track_day$id, 
              sex= Sgroup_track_day$sex, season=Sgroup_track_day$season)



rsf_Slivest_day<- used
rsf_Slivest_day$livestock527  <- avail$Sgroup_livestock_527_m
rsf_Slivest_day$livestock724  <- avail$Sgroup_livestock_724_m
rsf_Slivest_day$livestock1242 <- avail$Sgroup_livestock_1242_m
rsf_Slivest_day$livestock1545 <- avail$Sgroup_livestock_1545_m 
rsf_Slivest_day$livestock2105 <- avail$Sgroup_livestock_2105_m
rsf_Slivest_day$livestock3001 <- avail$Sgroup_livestock_3001_m
rsf_Slivest_day$livestock5886 <- avail$Sgroup_livestock_5886_m 


write.csv(rsf_Slivest_day, "rsf_Slivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(S_Alllivestock_Scale, coords(Sgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Sgroup_track_night), id=Sgroup_track_night$id, 
                sex=Sgroup_track_night$sex, season=Sgroup_track_night$season)

livestock <- raster::extract(S_livestock, coords(Sgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Sgroup_track_night), id=Sgroup_track_night$id, 
              sex= Sgroup_track_night$sex, season=Sgroup_track_night$season)

rsf_Slivest_night <- used
rsf_Slivest_night$livestock527   <- avail$Sgroup_livestock_527_m
rsf_Slivest_night$livestock724   <- avail$Sgroup_livestock_724_m
rsf_Slivest_night$livestock1242  <- avail$Sgroup_livestock_1242_m
rsf_Slivest_night$livestock1545  <- avail$Sgroup_livestock_1545_m  
rsf_Slivest_night$livestock2105 <- avail$Sgroup_livestock_2105_m
rsf_Slivest_night$livestock3001 <- avail$Sgroup_livestock_3001_m
rsf_Slivest_night$livestock5886 <- avail$Sgroup_livestock_5886_m 



write.csv(rsf_Slivest_night, "rsf_Slivest_night.csv")

rm(used, avail, livestock)

# Remove Sgroup data useless ----------------------------------------------

### Rasters
rm(S_Alllivestock_Scale, S_livestock, S_livestock1242, S_livestock1545,
     S_livestock2105, S_livestock3001, S_livestock527, S_livestock5886,S_livestock724)

rm(Sgroup_jag,Sgroup_mov,Sgroup_track, Sgroup_track_day, Sgroup_track_night)

gc()

# T group -----------------------------------------------------------------

T_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Tgroup_livestock.tif")

T_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_527_m.tif")

T_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_724_m.tif")

T_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_1242_m.tif")

T_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_1545_m.tif")

T_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_2105_m.tif")

T_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_3001_m.tif")

T_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Tgroup_livestock_5886_m.tif")


T_Alllivestock_Scale <- stack( T_livestock527, T_livestock724, T_livestock1242, T_livestock1545, T_livestock2105, T_livestock3001, T_livestock5886)


### Day

avail <- data.frame(raster::extract(T_Alllivestock_Scale, coords(Tgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Tgroup_track_day), id=Tgroup_track_day$id, 
                sex=Tgroup_track_day$sex, season=Tgroup_track_day$season)

livestock <- raster::extract(T_livestock, coords(Tgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Tgroup_track_day), id=Tgroup_track_day$id, 
              sex= Tgroup_track_day$sex, season=Tgroup_track_day$season)



rsf_Tlivest_day<- used
rsf_Tlivest_day$livestock527  <- avail$Tgroup_livestock_527_m
rsf_Tlivest_day$livestock724  <- avail$Tgroup_livestock_724_m
rsf_Tlivest_day$livestock1242 <- avail$Tgroup_livestock_1242_m
rsf_Tlivest_day$livestock1545 <- avail$Tgroup_livestock_1545_m 
rsf_Tlivest_day$livestock2105 <- avail$Tgroup_livestock_2105_m
rsf_Tlivest_day$livestock3001 <- avail$Tgroup_livestock_3001_m
rsf_Tlivest_day$livestock5886 <- avail$Tgroup_livestock_5886_m 


write.csv(rsf_Tlivest_day, "rsf_Tlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(T_Alllivestock_Scale, coords(Tgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Tgroup_track_night), id=Tgroup_track_night$id, 
                sex=Tgroup_track_night$sex, season=Tgroup_track_night$season)

livestock <- raster::extract(T_livestock, coords(Tgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Tgroup_track_night), id=Tgroup_track_night$id, 
              sex= Tgroup_track_night$sex, season=Tgroup_track_night$season)

rsf_Tlivest_night <- used
rsf_Tlivest_night$livestock527   <- avail$Tgroup_livestock_527_m
rsf_Tlivest_night$livestock724   <- avail$Tgroup_livestock_724_m
rsf_Tlivest_night$livestock1242  <- avail$Tgroup_livestock_1242_m
rsf_Tlivest_night$livestock1545  <- avail$Tgroup_livestock_1545_m  
rsf_Tlivest_night$livestock2105 <- avail$Tgroup_livestock_2105_m
rsf_Tlivest_night$livestock3001 <- avail$Tgroup_livestock_3001_m
rsf_Tlivest_night$livestock5886 <- avail$Tgroup_livestock_5886_m 



write.csv(rsf_Tlivest_night, "rsf_Tlivest_night.csv")

rm(used, avail, livestock)

# Remove Tgroup data useless ----------------------------------------------

### Rasters
rm(T_Alllivestock_Scale, T_livestock, T_livestock1242, T_livestock1545,
     T_livestock2105, T_livestock3001, T_livestock527, T_livestock5886,T_livestock724)

rm(Tgroup_jag,Tgroup_mov,Tgroup_track, Tgroup_track_day, Tgroup_track_night)

gc()

# U group -----------------------------------------------------------------

U_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Ugroup_livestock.tif")

U_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_527_m.tif")

U_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_724_m.tif")

U_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_1242_m.tif")

U_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_1545_m.tif")

U_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_2105_m.tif")

U_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_3001_m.tif")

U_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ugroup_livestock_5886_m.tif")


U_Alllivestock_Scale <- stack( U_livestock527, U_livestock724, U_livestock1242, U_livestock1545, U_livestock2105, U_livestock3001, U_livestock5886)


### Day

avail <- data.frame(raster::extract(U_Alllivestock_Scale, coords(Ugroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ugroup_track_day), id=Ugroup_track_day$id, 
                sex=Ugroup_track_day$sex, season=Ugroup_track_day$season)

livestock <- raster::extract(U_livestock, coords(Ugroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ugroup_track_day), id=Ugroup_track_day$id, 
              sex= Ugroup_track_day$sex, season=Ugroup_track_day$season)



rsf_Ulivest_day<- used
rsf_Ulivest_day$livestock527  <- avail$Ugroup_livestock_527_m
rsf_Ulivest_day$livestock724  <- avail$Ugroup_livestock_724_m
rsf_Ulivest_day$livestock1242 <- avail$Ugroup_livestock_1242_m
rsf_Ulivest_day$livestock1545 <- avail$Ugroup_livestock_1545_m 
rsf_Ulivest_day$livestock2105 <- avail$Ugroup_livestock_2105_m
rsf_Ulivest_day$livestock3001 <- avail$Ugroup_livestock_3001_m
rsf_Ulivest_day$livestock5886 <- avail$Ugroup_livestock_5886_m 


write.csv(rsf_Ulivest_day, "rsf_Ulivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(U_Alllivestock_Scale, coords(Ugroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ugroup_track_night), id=Ugroup_track_night$id, 
                sex=Ugroup_track_night$sex, season=Ugroup_track_night$season)

livestock <- raster::extract(U_livestock, coords(Ugroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ugroup_track_night), id=Ugroup_track_night$id, 
              sex= Ugroup_track_night$sex, season=Ugroup_track_night$season)

rsf_Ulivest_night <- used
rsf_Ulivest_night$livestock527   <- avail$Ugroup_livestock_527_m
rsf_Ulivest_night$livestock724   <- avail$Ugroup_livestock_724_m
rsf_Ulivest_night$livestock1242  <- avail$Ugroup_livestock_1242_m
rsf_Ulivest_night$livestock1545  <- avail$Ugroup_livestock_1545_m  
rsf_Ulivest_night$livestock2105 <- avail$Ugroup_livestock_2105_m
rsf_Ulivest_night$livestock3001 <- avail$Ugroup_livestock_3001_m
rsf_Ulivest_night$livestock5886 <- avail$Ugroup_livestock_5886_m 



write.csv(rsf_Ulivest_night, "rsf_Ulivest_night.csv")

rm(used, avail, livestock)

# Remove Ugroup data useless ----------------------------------------------

### Rasters
rm(U_Alllivestock_Scale, U_livestock, U_livestock1242, U_livestock1545,
     U_livestock2105, U_livestock3001, U_livestock527, U_livestock5886,U_livestock724)

rm(Ugroup_jag,Ugroup_mov,Ugroup_track, Ugroup_track_day, Ugroup_track_night)

gc()

# V group -----------------------------------------------------------------

V_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Vgroup_livestock.tif")

V_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_527_m.tif")

V_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_724_m.tif")

V_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_1242_m.tif")

V_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_1545_m.tif")

V_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_2105_m.tif")

V_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_3001_m.tif")

V_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Vgroup_livestock_5886_m.tif")


V_Alllivestock_Scale <- stack( V_livestock527, V_livestock724, V_livestock1242, V_livestock1545, V_livestock2105, V_livestock3001, V_livestock5886)


### Day

avail <- data.frame(raster::extract(V_Alllivestock_Scale, coords(Vgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Vgroup_track_day), id=Vgroup_track_day$id, 
                sex=Vgroup_track_day$sex, season=Vgroup_track_day$season)

livestock <- raster::extract(V_livestock, coords(Vgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Vgroup_track_day), id=Vgroup_track_day$id, 
              sex= Vgroup_track_day$sex, season=Vgroup_track_day$season)



rsf_Vlivest_day<- used
rsf_Vlivest_day$livestock527  <- avail$Vgroup_livestock_527_m
rsf_Vlivest_day$livestock724  <- avail$Vgroup_livestock_724_m
rsf_Vlivest_day$livestock1242 <- avail$Vgroup_livestock_1242_m
rsf_Vlivest_day$livestock1545 <- avail$Vgroup_livestock_1545_m 
rsf_Vlivest_day$livestock2105 <- avail$Vgroup_livestock_2105_m
rsf_Vlivest_day$livestock3001 <- avail$Vgroup_livestock_3001_m
rsf_Vlivest_day$livestock5886 <- avail$Vgroup_livestock_5886_m 


write.csv(rsf_Vlivest_day, "rsf_Vlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(V_Alllivestock_Scale, coords(Vgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Vgroup_track_night), id=Vgroup_track_night$id, 
                sex=Vgroup_track_night$sex, season=Vgroup_track_night$season)

livestock <- raster::extract(V_livestock, coords(Vgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Vgroup_track_night), id=Vgroup_track_night$id, 
              sex= Vgroup_track_night$sex, season=Vgroup_track_night$season)

rsf_Vlivest_night <- used
rsf_Vlivest_night$livestock527   <- avail$Vgroup_livestock_527_m
rsf_Vlivest_night$livestock724   <- avail$Vgroup_livestock_724_m
rsf_Vlivest_night$livestock1242  <- avail$Vgroup_livestock_1242_m
rsf_Vlivest_night$livestock1545  <- avail$Vgroup_livestock_1545_m  
rsf_Vlivest_night$livestock2105 <- avail$Vgroup_livestock_2105_m
rsf_Vlivest_night$livestock3001 <- avail$Vgroup_livestock_3001_m
rsf_Vlivest_night$livestock5886 <- avail$Vgroup_livestock_5886_m 



write.csv(rsf_Vlivest_night, "rsf_Vlivest_night.csv")

rm(used, avail, livestock)

# Remove Vgroup data useless ----------------------------------------------

### Rasters
rm(V_Alllivestock_Scale, V_livestock, V_livestock1242, V_livestock1545,
     V_livestock2105, V_livestock3001, V_livestock527, V_livestock5886,V_livestock724)

rm(Vgroup_jag,Vgroup_mov,Vgroup_track, Vgroup_track_day, Vgroup_track_night)

gc()

# W group -----------------------------------------------------------------

W_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Wgroup_livestock.tif")

W_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_527_m.tif")

W_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_724_m.tif")

W_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_1242_m.tif")

W_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_1545_m.tif")

W_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_2105_m.tif")

W_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_3001_m.tif")

W_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Wgroup_livestock_5886_m.tif")


W_Alllivestock_Scale <- stack( W_livestock527, W_livestock724, W_livestock1242, W_livestock1545, W_livestock2105, W_livestock3001, W_livestock5886)


### Day

avail <- data.frame(raster::extract(W_Alllivestock_Scale, coords(Wgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Wgroup_track_day), id=Wgroup_track_day$id, 
                sex=Wgroup_track_day$sex, season=Wgroup_track_day$season)

livestock <- raster::extract(W_livestock, coords(Wgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Wgroup_track_day), id=Wgroup_track_day$id, 
              sex= Wgroup_track_day$sex, season=Wgroup_track_day$season)



rsf_Wlivest_day<- used
rsf_Wlivest_day$livestock527  <- avail$Wgroup_livestock_527_m
rsf_Wlivest_day$livestock724  <- avail$Wgroup_livestock_724_m
rsf_Wlivest_day$livestock1242 <- avail$Wgroup_livestock_1242_m
rsf_Wlivest_day$livestock1545 <- avail$Wgroup_livestock_1545_m 
rsf_Wlivest_day$livestock2105 <- avail$Wgroup_livestock_2105_m
rsf_Wlivest_day$livestock3001 <- avail$Wgroup_livestock_3001_m
rsf_Wlivest_day$livestock5886 <- avail$Wgroup_livestock_5886_m 


write.csv(rsf_Wlivest_day, "rsf_Wlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(W_Alllivestock_Scale, coords(Wgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Wgroup_track_night), id=Wgroup_track_night$id, 
                sex=Wgroup_track_night$sex, season=Wgroup_track_night$season)

livestock <- raster::extract(W_livestock, coords(Wgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Wgroup_track_night), id=Wgroup_track_night$id, 
              sex= Wgroup_track_night$sex, season=Wgroup_track_night$season)

rsf_Wlivest_night <- used
rsf_Wlivest_night$livestock527   <- avail$Wgroup_livestock_527_m
rsf_Wlivest_night$livestock724   <- avail$Wgroup_livestock_724_m
rsf_Wlivest_night$livestock1242  <- avail$Wgroup_livestock_1242_m
rsf_Wlivest_night$livestock1545  <- avail$Wgroup_livestock_1545_m  
rsf_Wlivest_night$livestock2105 <- avail$Wgroup_livestock_2105_m
rsf_Wlivest_night$livestock3001 <- avail$Wgroup_livestock_3001_m
rsf_Wlivest_night$livestock5886 <- avail$Wgroup_livestock_5886_m 



write.csv(rsf_Wlivest_night, "rsf_Wlivest_night.csv")

rm(used, avail, livestock)

# Remove Wgroup data useless ----------------------------------------------

### Rasters
rm(W_Alllivestock_Scale, W_livestock, W_livestock1242, W_livestock1545,
     W_livestock2105, W_livestock3001, W_livestock527, W_livestock5886,W_livestock724)

rm(Wgroup_jag,Wgroup_mov,Wgroup_track, Wgroup_track_day, Wgroup_track_night)

gc()

# X group -----------------------------------------------------------------

X_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Xgroup_livestock.tif")

X_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_527_m.tif")

X_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_724_m.tif")

X_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_1242_m.tif")

X_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_1545_m.tif")

X_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_2105_m.tif")

X_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_3001_m.tif")

X_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Xgroup_livestock_5886_m.tif")


X_Alllivestock_Scale <- stack( X_livestock527, X_livestock724, X_livestock1242, X_livestock1545, X_livestock2105, X_livestock3001, X_livestock5886)


### Day

avail <- data.frame(raster::extract(X_Alllivestock_Scale, coords(Xgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Xgroup_track_day), id=Xgroup_track_day$id, 
                sex=Xgroup_track_day$sex, season=Xgroup_track_day$season)

livestock <- raster::extract(X_livestock, coords(Xgroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Xgroup_track_day), id=Xgroup_track_day$id, 
              sex= Xgroup_track_day$sex, season=Xgroup_track_day$season)



rsf_Xlivest_day<- used
rsf_Xlivest_day$livestock527  <- avail$Xgroup_livestock_527_m
rsf_Xlivest_day$livestock724  <- avail$Xgroup_livestock_724_m
rsf_Xlivest_day$livestock1242 <- avail$Xgroup_livestock_1242_m
rsf_Xlivest_day$livestock1545 <- avail$Xgroup_livestock_1545_m 
rsf_Xlivest_day$livestock2105 <- avail$Xgroup_livestock_2105_m
rsf_Xlivest_day$livestock3001 <- avail$Xgroup_livestock_3001_m
rsf_Xlivest_day$livestock5886 <- avail$Xgroup_livestock_5886_m 


write.csv(rsf_Xlivest_day, "rsf_Xlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(X_Alllivestock_Scale, coords(Xgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Xgroup_track_night), id=Xgroup_track_night$id, 
                sex=Xgroup_track_night$sex, season=Xgroup_track_night$season)

livestock <- raster::extract(X_livestock, coords(Xgroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Xgroup_track_night), id=Xgroup_track_night$id, 
              sex= Xgroup_track_night$sex, season=Xgroup_track_night$season)

rsf_Xlivest_night <- used
rsf_Xlivest_night$livestock527   <- avail$Xgroup_livestock_527_m
rsf_Xlivest_night$livestock724   <- avail$Xgroup_livestock_724_m
rsf_Xlivest_night$livestock1242  <- avail$Xgroup_livestock_1242_m
rsf_Xlivest_night$livestock1545  <- avail$Xgroup_livestock_1545_m  
rsf_Xlivest_night$livestock2105 <- avail$Xgroup_livestock_2105_m
rsf_Xlivest_night$livestock3001 <- avail$Xgroup_livestock_3001_m
rsf_Xlivest_night$livestock5886 <- avail$Xgroup_livestock_5886_m 



write.csv(rsf_Xlivest_night, "rsf_Xlivest_night.csv")

rm(used, avail, livestock)

# Remove Xgroup data useless ----------------------------------------------

### Rasters
rm(X_Alllivestock_Scale, X_livestock, X_livestock1242, X_livestock1545,
     X_livestock2105, X_livestock3001, X_livestock527, X_livestock5886,X_livestock724)

rm(Xgroup_jag,Xgroup_mov,Xgroup_track, Xgroup_track_day, Xgroup_track_night)

gc()

# Y group -----------------------------------------------------------------

Y_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Ygroup_livestock.tif")

Y_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_527_m.tif")

Y_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_724_m.tif")

Y_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_1242_m.tif")

Y_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_1545_m.tif")

Y_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_2105_m.tif")

Y_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_3001_m.tif")

Y_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Ygroup_livestock_5886_m.tif")


Y_Alllivestock_Scale <- stack( Y_livestock527, Y_livestock724, Y_livestock1242, Y_livestock1545, Y_livestock2105, Y_livestock3001, Y_livestock5886)


### Day

avail <- data.frame(raster::extract(Y_Alllivestock_Scale, coords(Ygroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ygroup_track_day), id=Ygroup_track_day$id, 
                sex=Ygroup_track_day$sex, season=Ygroup_track_day$season)

livestock <- raster::extract(Y_livestock, coords(Ygroup_track_day),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ygroup_track_day), id=Ygroup_track_day$id, 
              sex= Ygroup_track_day$sex, season=Ygroup_track_day$season)



rsf_Ylivest_day<- used
rsf_Ylivest_day$livestock527  <- avail$Ygroup_livestock_527_m
rsf_Ylivest_day$livestock724  <- avail$Ygroup_livestock_724_m
rsf_Ylivest_day$livestock1242 <- avail$Ygroup_livestock_1242_m
rsf_Ylivest_day$livestock1545 <- avail$Ygroup_livestock_1545_m 
rsf_Ylivest_day$livestock2105 <- avail$Ygroup_livestock_2105_m
rsf_Ylivest_day$livestock3001 <- avail$Ygroup_livestock_3001_m
rsf_Ylivest_day$livestock5886 <- avail$Ygroup_livestock_5886_m 


write.csv(rsf_Ylivest_day, "rsf_Ylivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(Y_Alllivestock_Scale, coords(Ygroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Ygroup_track_night), id=Ygroup_track_night$id, 
                sex=Ygroup_track_night$sex, season=Ygroup_track_night$season)

livestock <- raster::extract(Y_livestock, coords(Ygroup_track_night),  
                               method="simple", buffer=30,fun=mean, 
                               na.rm=T)

used <- cbind(livestock, coords(Ygroup_track_night), id=Ygroup_track_night$id, 
              sex= Ygroup_track_night$sex, season=Ygroup_track_night$season)

rsf_Ylivest_night <- used
rsf_Ylivest_night$livestock527   <- avail$Ygroup_livestock_527_m
rsf_Ylivest_night$livestock724   <- avail$Ygroup_livestock_724_m
rsf_Ylivest_night$livestock1242  <- avail$Ygroup_livestock_1242_m
rsf_Ylivest_night$livestock1545  <- avail$Ygroup_livestock_1545_m  
rsf_Ylivest_night$livestock2105 <- avail$Ygroup_livestock_2105_m
rsf_Ylivest_night$livestock3001 <- avail$Ygroup_livestock_3001_m
rsf_Ylivest_night$livestock5886 <- avail$Ygroup_livestock_5886_m 



write.csv(rsf_Ylivest_night, "rsf_Ylivest_night.csv")

rm(used, avail, livestock)

# Remove Ygroup data useless ----------------------------------------------

### Rasters
rm(Y_Alllivestock_Scale, Y_livestock, Y_livestock1242, Y_livestock1545,
     Y_livestock2105, Y_livestock3001, Y_livestock527, Y_livestock5886,Y_livestock724)

rm(Ygroup_jag,Ygroup_mov,Ygroup_track, Ygroup_track_day, Ygroup_track_night)

gc()
# Z group -----------------------------------------------------------------

Z_livestock<- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/Zgroup_livestock.tif")

Z_livestock527 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_527_m.tif")

Z_livestock724 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_724_m.tif")

Z_livestock1242 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_1242_m.tif")

Z_livestock1545 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_1545_m.tif")

Z_livestock2105 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_2105_m.tif")

Z_livestock3001 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_3001_m.tif")

Z_livestock5886 <- raster("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales/smoothed_layers_livestock/Zgroup_livestock_5886_m.tif")


Z_Alllivestock_Scale <- stack( Z_livestock527, Z_livestock724, Z_livestock1242, Z_livestock1545, Z_livestock2105, Z_livestock3001, Z_livestock5886)


### Day

avail <- data.frame(raster::extract(Z_Alllivestock_Scale, coords(Zgroup_track_day),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Zgroup_track_day), id=Zgroup_track_day$id, 
                sex=Zgroup_track_day$sex, season=Zgroup_track_day$season)

livestock <- raster::extract(Z_livestock, coords(Zgroup_track_day),  
                             method="simple", buffer=30,fun=mean, 
                             na.rm=T)

used <- cbind(livestock, coords(Zgroup_track_day), id=Zgroup_track_day$id, 
              sex= Zgroup_track_day$sex, season=Zgroup_track_day$season)



rsf_Zlivest_day<- used
rsf_Zlivest_day$livestock527  <- avail$Zgroup_livestock_527_m
rsf_Zlivest_day$livestock724  <- avail$Zgroup_livestock_724_m
rsf_Zlivest_day$livestock1242 <- avail$Zgroup_livestock_1242_m
rsf_Zlivest_day$livestock1545 <- avail$Zgroup_livestock_1545_m 
rsf_Zlivest_day$livestock2105 <- avail$Zgroup_livestock_2105_m
rsf_Zlivest_day$livestock3001 <- avail$Zgroup_livestock_3001_m
rsf_Zlivest_day$livestock5886 <- avail$Zgroup_livestock_5886_m 


write.csv(rsf_Zlivest_day, "rsf_Zlivest_day.csv")


rm(used, avail, livestock)

### Night

avail <- data.frame(raster::extract(Z_Alllivestock_Scale, coords(Zgroup_track_night),  method="simple", fun=mean, na.rm=T))

avail  <- cbind(avail, coords(Zgroup_track_night), id=Zgroup_track_night$id, 
                sex=Zgroup_track_night$sex, season=Zgroup_track_night$season)

livestock <- raster::extract(Z_livestock, coords(Zgroup_track_night),  
                             method="simple", buffer=30,fun=mean, 
                             na.rm=T)

used <- cbind(livestock, coords(Zgroup_track_night), id=Zgroup_track_night$id, 
              sex= Zgroup_track_night$sex, season=Zgroup_track_night$season)

rsf_Zlivest_night <- used
rsf_Zlivest_night$livestock527   <- avail$Zgroup_livestock_527_m
rsf_Zlivest_night$livestock724   <- avail$Zgroup_livestock_724_m
rsf_Zlivest_night$livestock1242  <- avail$Zgroup_livestock_1242_m
rsf_Zlivest_night$livestock1545  <- avail$Zgroup_livestock_1545_m  
rsf_Zlivest_night$livestock2105 <- avail$Zgroup_livestock_2105_m
rsf_Zlivest_night$livestock3001 <- avail$Zgroup_livestock_3001_m
rsf_Zlivest_night$livestock5886 <- avail$Zgroup_livestock_5886_m 



write.csv(rsf_Zlivest_night, "rsf_Zlivest_night.csv")

rm(used, avail, livestock)

# Remove Zgroup data useless ----------------------------------------------

### Rasters
rm(Z_Alllivestock_Scale, Z_livestock, Z_livestock1242, Z_livestock1545,
   Z_livestock2105, Z_livestock3001, Z_livestock527, Z_livestock5886,Z_livestock724)

rm(Zgroup_jag,Zgroup_mov,Zgroup_track, Zgroup_track_day, Zgroup_track_night)

gc()

