### First draft of Pareto scale
### Vanesa F. Bejarano Alegre
### 
## Load package and dataset
rm(list= ls())  #clean all before to start

listpacks <- c("tidyverse","POT", "zoo")
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load(listpacks)


setwd("D:/PhD_Jaguar/Jaguar01/00_Script_to_submit/Jaguar_Data/01_Output_cleaning")

# Jaguars group A ---------------------------------------------------------
# ### J43.1
j43.1 <- read.delim(file="02_jaguar043.1_dist_clean.txt")

#Jaguar 43.1  
#It does NOT have a big value that we must eliminate
j43.1 <- j43.1[-270,]# outliers

#Pareto estimations
paretohrt <- fitgpd(j43.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# 43.1 schedule is 6 hours, we will take from the same 12 and 24
# every 2 periods = 12
#Sum of the distances
a=2
jagdist <- rollapply(j43.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4 periods = 24
##Sum of the distances
a=4
jagdist <- rollapply(j43.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##j43.2

j43.2 <- read.delim(file="02_jaguar043.2_dist_clean.txt")

#Jaguar 43.2  is every 3 hours therefore only 3,12,24 we considered
# high value
j43.2 <- j43.2[-285,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j43.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j43.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4 period = 12
#Sum of the distancess
a=4
jagdist <- rollapply(j43.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# a cada 8 periodos =24
#Sum of the distancess
a=8
jagdist <- rollapply(j43.2$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#####64

j64 <- read.delim(file="02_jaguar064_dist_clean.txt")

#Jaguar 64  is every 3 hours therefore only 3,12,24 we considered
# high value
j64 <- j64[-1281,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j64$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j64$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4 period = 12
#Sum of the distancess
a=4
jagdist <- rollapply(j64$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# a cada 8 periodos =24
#Sum of the distancess
a=8
jagdist <- rollapply(j64$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group B -----------------------------------------------------------------
#####j49

j49 <- read.delim(file="02_jaguar049_dist_clean.txt")

#Jaguar 49 es cada 2 horas por lo tanto solo se considerara 2,4,10,12,24
# high value
j49 <- j49[-975,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j49$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# a cada 2 periodos =4
#Sum of the distancess
a=2
jagdist <- rollapply(j49$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos=6

#Sum of the distancess
a=3
jagdist <- rollapply(j49$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos=8

#Sum of the distancess
a=4
jagdist <- rollapply(j49$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# a cada 5 periodos =10
#Sum of the distancess
a=5
jagdist <- rollapply(j49$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 periodos =12

#Sum of the distancess
a=6
jagdist <- rollapply(j49$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12 periodos =24

#Sum of the distancess
a=12
jagdist <- rollapply(j49$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group C ----------------------------------------------------------
##44

j44 <- read.delim(file="02_jaguar044_dist_clean.txt")

#Jaguar 44  
# high value
j44 <- j44[-97,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j44$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =10

#Sum of the distancess
a=2
jagdist <- rollapply(j44$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##45

j45 <- read.delim(file="02_jaguar045_dist_clean.txt")

#Jaguar 45  
# high value
j45 <- j45[-39,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j45$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =10

#Sum of the distancess
a=2
jagdist <- rollapply(j45$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##46

j46 <- read.delim(file="02_jaguar046_dist_clean.txt")

#Jaguar 46  
# high value
j46 <- j46[-436,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j46$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =10 

#Sum of the distancess
a=2
jagdist <- rollapply(j46$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##47

j47 <- read.delim(file="02_jaguar047_dist_clean.txt")

#Jaguar 47  
# high value
j47 <- j47[-463,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j47$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

##48

j48 <- read.delim(file="02_jaguar048_dist_clean.txt")

#Jaguar 48  
# high value
j48 <- j48[-39,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j48$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# Jaguar group D ----------------------------------------------------------
##26

j26 <- read.delim(file="02_jaguar026_dist_clean.txt")

#Jaguar 26  
#Damos una chequeada para verificar todas las columnas
# high value
j26 <- j26[-5923,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j26$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos = 4

#Sum of the distancess
a=2
jagdist <- rollapply(j26$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos = 6

#Sum of the distancess
a=3
jagdist <- rollapply(j26$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 4 periodos=8

#Sum of the distancess
a=4
jagdist <- rollapply(j26$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 5 periodos =10

#Sum of the distancess
a=5
jagdist <- rollapply(j26$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 periodos=12

#Sum of the distancess
a=6
jagdist <- rollapply(j26$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12 periodos =24

#Sum of the distancess
a=12
jagdist <- rollapply(j26$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group E ----------------------------------------------------------
##93

j93 <- read.delim(file="02_jaguar093_dist_clean.txt")

#Jaguar 93  
# high value
j93 <- j93[-1023,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j93$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos=12


#Sum of the distancess
a=2
jagdist <- rollapply(j93$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos=24

#Sum of the distancess
a=4
jagdist <- rollapply(j93$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##94

j94 <- read.delim(file="02_jaguar094_dist_clean.txt")

#Jaguar 94  
# high value
j94 <- j94[-515,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j94$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =12

#Sum of the distancess
a=2
jagdist <- rollapply(j94$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos= 24

#Sum of the distancess
a=4
jagdist <- rollapply(j94$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##95

j95 <- read.delim(file="02_jaguar095_dist_clean.txt")

#Jaguar 95  
# high value
j95 <- j95[-61,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j95$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos=4

#Sum of the distancess
a=2
jagdist <- rollapply(j95$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos= 6

#Sum of the distancess
a=3
jagdist <- rollapply(j95$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos= 8

#Sum of the distancess
a=4
jagdist <- rollapply(j95$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5 periodos = 10 

#Sum of the distancess
a=5
jagdist <- rollapply(j95$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 periodos =12

#Sum of the distancess
a=6
jagdist <- rollapply(j95$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12 periodos=24

#Sum of the distancess
a=12
jagdist <- rollapply(j95$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##96

j96 <- read.delim(file="02_jaguar096_dist_clean.txt")

#Jaguar 96  
# high value
j96 <- j96[-491,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j96$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos=12
#Sum of the distancess
a=2
jagdist <- rollapply(j96$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos = 24

#Sum of the distancess
a=4
jagdist <- rollapply(j96$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##97

j97 <- read.delim(file="02_jaguar097_dist_clean.txt")

#Jaguar 97  
# high value
j97 <- j97[-294,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j97$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos = 12

#Sum of the distancess
a=2
jagdist <- rollapply(j97$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos = 24

#Sum of the distancess
a=4
jagdist <- rollapply(j97$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##98

j98 <- read.delim(file="02_jaguar098_dist_clean.txt")

#Jaguar 98  
# high value
j98 <- j98[-784,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j98$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos = 10

#Sum of the distancess
a=2
jagdist <- rollapply(j98$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##99

j99 <- read.delim(file="02_jaguar099_dist_clean.txt")

#Jaguar 99  
# high value
j99 <- j99[-3698,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j99$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos = 4

#Sum of the distancess
a=2
jagdist <- rollapply(j99$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos= 6

#Sum of the distancess
a=3
jagdist <- rollapply(j99$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos =8

#Sum of the distancess
a=4
jagdist <- rollapply(j99$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5 periodos = 10

#Sum of the distancess
a=5
jagdist <- rollapply(j99$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 periodos=12

#Sum of the distancess
a=6
jagdist <- rollapply(j99$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12 periodos = 24

#Sum of the distancess
a=12
jagdist <- rollapply(j99$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##100

j100 <- read.delim(file="02_jaguar0100_dist_clean.txt")

#Jaguar 100  
# high value
j100 <- j100[-42,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j100$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos = 12

#Sum of the distancess
a=2
jagdist <- rollapply(j100$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodo = 24

#Sum of the distancess
a=4
jagdist <- rollapply(j100$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group F ----------------------------------------------------------

##24

j24 <- read.delim(file="02_jaguar024_dist_clean.txt")

#Jaguar 24  
# high value
j24 <- j24[-135,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j24$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 PERIODOS = 8

#Sum of the distancess
a=2
jagdist <- rollapply(j24$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos =12

#Sum of the distancess
a=3
jagdist <- rollapply(j24$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 periodos =24

#Sum of the distancess
a=6
jagdist <- rollapply(j24$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group G ----------------------------------------------------------

##20

j20 <- read.delim(file="02_jaguar020_dist_clean.txt")

#Jaguar 20  
# high value
j20 <- j20[-5590,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j20$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2H

#Sum of the distancess
a=2
jagdist <- rollapply(j20$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3H

#Sum of the distancess
a=3
jagdist <- rollapply(j20$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4H

#Sum of the distancess
a=4
jagdist <- rollapply(j20$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5H

#Sum of the distancess
a=5
jagdist <- rollapply(j20$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6H

#Sum of the distancess
a=6
jagdist <- rollapply(j20$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7H

#Sum of the distancess
a=7
jagdist <- rollapply(j20$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8H

#Sum of the distancess
a=8
jagdist <- rollapply(j20$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10H

#Sum of the distancess
a=10
jagdist <- rollapply(j20$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12

#Sum of the distancess
a=12
jagdist <- rollapply(j20$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24H

#Sum of the distancess
a=24
jagdist <- rollapply(j20$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##50

j50 <- read.delim(file="02_jaguar050_dist_clean.txt")

#Jaguar 50  
# high value
j50 <- j50[-1356,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j50$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2H

#Sum of the distancess
a=2
jagdist <- rollapply(j50$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3H

#Sum of the distancess
a=3
jagdist <- rollapply(j50$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4H

#Sum of the distancess
a=4
jagdist <- rollapply(j50$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5H

#Sum of the distancess
a=5
jagdist <- rollapply(j50$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6H

#Sum of the distancess
a=6
jagdist <- rollapply(j50$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7H

#Sum of the distancess
a=7
jagdist <- rollapply(j50$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8H

#Sum of the distancess
a=8
jagdist <- rollapply(j50$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10H

#Sum of the distancess
a=10
jagdist <- rollapply(j50$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12H

#Sum of the distancess
a=12
jagdist <- rollapply(j50$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24H

#Sum of the distancess
a=24
jagdist <- rollapply(j50$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group H ----------------------------------------------------------

##89.1

j89.1 <- read.delim(file="02_jaguar089.1_dist_clean.txt")

#Jaguar 89.1  
# high value
j89.1 <- j89.1[-1633,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j89.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2H

#Sum of the distancess
a=2
jagdist <- rollapply(j89.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3H

#Sum of the distancess
a=3
jagdist <- rollapply(j89.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4H

#Sum of the distancess
a=4
jagdist <- rollapply(j89.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5H

#Sum of the distancess
a=5
jagdist <- rollapply(j89.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6H

#Sum of the distancess
a=6
jagdist <- rollapply(j89.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7H

#Sum of the distancess
a=7
jagdist <- rollapply(j89.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8H

#Sum of the distancess
a=8
jagdist <- rollapply(j89.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j89.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12H

#Sum of the distancess
a=12
jagdist <- rollapply(j89.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24H

#Sum of the distancess
a=24
jagdist <- rollapply(j89.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##89.2

j89.2 <- read.delim(file="02_jaguar089.2_dist_clean.txt")

#Jaguar 89.2  
# high value
j89.2 <- j89.2[-790,]# outliers 


#a cada 2 PERIODOS =7

#Sum of the distancess
a=2
jagdist <- rollapply(j89.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# Jaguar group I ----------------------------------------------------------

##17

j17 <- read.delim(file="02_jaguar017_dist_clean.txt")

#Jaguar 17  
# high value
j17 <- j17[-422,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j17$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =10

#Sum of the distancess
a=2
jagdist <- rollapply(j17$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##65

j65 <- read.delim(file="02_jaguar065_dist_clean.txt")

#Jaguar 65  
# high value
j65 <- j65[-660,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j65$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodo =10

#Sum of the distancess
a=2
jagdist <- rollapply(j65$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##67

j67 <- read.delim(file="02_jaguar067_dist_clean.txt")

#Jaguar 67  
# high value
j67 <- j67[-125,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j67$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

##82

j82 <- read.delim(file="02_jaguar082_dist_clean.txt")

#Jaguar 82  
# high value
j82 <- j82[-984,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j82$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos =10

#Sum of the distancess
a=2
jagdist <- rollapply(j82$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##85

j85 <- read.delim(file="02_jaguar085_dist_clean.txt")

#Jaguar 85  
# high value
j85 <- j85[-608,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j85$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 peridos =10

#Sum of the distancess
a=2
jagdist <- rollapply(j85$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group J ----------------------------------------------------------
##12

j12 <- read.delim(file="02_jaguar012_dist_clean.txt")

#Jaguar 12  
# high value
j12 <- j12[-2681,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j12$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2H

#Sum of the distancess
a=2
jagdist <- rollapply(j12$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3H

#Sum of the distancess
a=3
jagdist <- rollapply(j12$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4H

#Sum of the distancess
a=4
jagdist <- rollapply(j12$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5H

#Sum of the distancess
a=5
jagdist <- rollapply(j12$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6H

#Sum of the distancess
a=6
jagdist <- rollapply(j12$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7H

#Sum of the distancess
a=7
jagdist <- rollapply(j12$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8H

#Sum of the distancess
a=8
jagdist <- rollapply(j12$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10H

#Sum of the distancess
a=10
jagdist <- rollapply(j12$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12H

#Sum of the distancess
a=12
jagdist <- rollapply(j12$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24H

#Sum of the distancess
a=24
jagdist <- rollapply(j12$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##13

j13 <- read.delim(file="02_jaguar013_dist_clean.txt")

#Jaguar 13  
# high value
j13 <- j13[-5038,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j13$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2H

#Sum of the distancess
a=2
jagdist <- rollapply(j13$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3H

#Sum of the distancess
a=3
jagdist <- rollapply(j13$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4H

#Sum of the distancess
a=4
jagdist <- rollapply(j13$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5

#Sum of the distancess
a=5
jagdist <- rollapply(j13$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6H

#Sum of the distancess
a=6
jagdist <- rollapply(j13$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7H

#Sum of the distancess
a=7
jagdist <- rollapply(j13$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8H

#Sum of the distancess
a=8
jagdist <- rollapply(j13$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10H

#Sum of the distancess
a=10
jagdist <- rollapply(j13$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12H

#Sum of the distancess
a=12
jagdist <- rollapply(j13$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24H

#Sum of the distancess
a=24
jagdist <- rollapply(j13$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##18

j18 <- read.delim(file="02_jaguar018_dist_clean.txt")

#Jaguar 18  
# high value
j18 <- j18[-2300,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j18$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j18$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j18$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j18$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j18$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j18$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j18$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j18$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j18$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j18$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j18$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##22

j22 <- read.delim(file="02_jaguar022_dist_clean.txt")

#Jaguar 22  
# high value
j22 <- j22[-4708,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j22$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j22$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j22$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j22$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j22$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j22$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j22$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j22$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j22$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j22$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j22$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##23

j23 <- read.delim(file="02_jaguar023_dist_clean.txt")

#Jaguar 23  
# high value
j23 <- j23[-572,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j23$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j23$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j23$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j23$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j23$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j23$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j23$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
#
a=8
jagdist <- rollapply(j23$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j23$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j23$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j23$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##41

j41 <- read.delim(file="02_jaguar041_dist_clean.txt")

#Jaguar 41  
# high value
j41 <- j41[-4950,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j41$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j41$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j41$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j41$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j41$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j41$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j41$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j41$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j41$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j41$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j41$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##52

j52 <- read.delim(file="02_jaguar052_dist_clean.txt")

#Jaguar 52  
# high value
j52 <- j52[-615,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j52$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j52$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j52$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j52$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j52$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j52$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7

#Sum of the distancess
a=7
jagdist <- rollapply(j52$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j52$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j52$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j52$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j52$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##81.1

j81.1 <- read.delim(file="02_jaguar081.1_dist_clean.txt")

#Jaguar 81.1  
# high value
j81.1 <- j81.1[-8881,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j81.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j81.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j81.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j81.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j81.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j81.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j81.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j81.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j81.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j81.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j81.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##81.2

j81.2 <- read.delim(file="02_jaguar081.2_dist_clean.txt")

#Jaguar 81.2  
# high value
j81.2 <- j81.2[-1733,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j81.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2periodos=4

#Sum of the distancess
a=2
jagdist <- rollapply(j81.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 periodos = 6

#Sum of the distancess
a=3
jagdist <- rollapply(j81.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 periodos =8

#Sum of the distancess
a=4
jagdist <- rollapply(j81.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 5 periodos =10

#Sum of the distancess
a=5
jagdist <- rollapply(j81.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6 periodos = 12

#Sum of the distancess
a=6
jagdist <- rollapply(j81.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12 periodos =24

#Sum of the distancess
a=12
jagdist <- rollapply(j81.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##88.1

j88.1 <- read.delim(file="02_jaguar088.1_dist_clean.txt")

#Jaguar 88.1  
# high value
j88.1 <- j88.1[-1006,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j88.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j88.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3hr

#Sum of the distancess
a=3
jagdist <- rollapply(j88.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4

#Sum of the distancess
a=4
jagdist <- rollapply(j88.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j88.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j88.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j88.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j88.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j88.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j88.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j88.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##88.2

j88.2 <- read.delim(file="02_jaguar088.2_dist_clean.txt")

#Jaguar 88.2  
# high value
j88.2 <- j88.2[-238,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j88.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 periodos=4

#Sum of the distancess
a=2
jagdist <- rollapply(j88.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 3 per =6

#Sum of the distancess
a=3
jagdist <- rollapply(j88.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 4 per =8

#Sum of the distancess
a=4
jagdist <- rollapply(j88.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 5 peri =10

#Sum of the distancess
a=5
jagdist <- rollapply(j88.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6 peri =12

#Sum of the distancess
a=6
jagdist <- rollapply(j88.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12 period =24

#Sum of the distancess
a=12
jagdist <- rollapply(j88.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##91

j91 <- read.delim(file="02_jaguar091_dist_clean.txt")

#Jaguar 91  
# high value
j91 <- j91[-78,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j91$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per =12

#Sum of the distancess
a=2
jagdist <- rollapply(j91$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 4 perio= 24

#Sum of the distancess
a=4
jagdist <- rollapply(j91$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##92

j92 <- read.delim(file="02_jaguar092_dist_clean.txt")

#Jaguar 92  
# high value
j92 <- j92[-83,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j92$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per=12

#Sum of the distancess
a=2
jagdist <- rollapply(j92$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 4 per =24

#Sum of the distancess
a=4
jagdist <- rollapply(j92$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##116

j116 <- read.delim(file="02_jaguar0116_dist_clean.txt")

#Jaguar 116  
# high value
j116 <- j116[-3340,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j116$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j116$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j116$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 4

#Sum of the distancess
a=4
jagdist <- rollapply(j116$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 5hr

#Sum of the distancess
a=5
jagdist <- rollapply(j116$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j116$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j116$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j116$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 10hr

#Sum of the distancess
a=10
jagdist <- rollapply(j116$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12hr

#Sum of the distancess
a=12
jagdist <- rollapply(j116$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 24hr

#Sum of the distancess
a=24
jagdist <- rollapply(j116$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##117

j117 <- read.delim(file="02_jaguar0117_dist_clean.txt")

#Jaguar 117  
# high value
j117 <- j117[-2820,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j117$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2h

#Sum of the distancess
a=2
jagdist <- rollapply(j117$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 3h

#Sum of the distancess
a=3
jagdist <- rollapply(j117$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 4h

#Sum of the distancess
a=4
jagdist <- rollapply(j117$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 5h

#Sum of the distancess
a=5
jagdist <- rollapply(j117$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6h

#Sum of the distancess
a=6
jagdist <- rollapply(j117$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 7h

#Sum of the distancess
a=7
jagdist <- rollapply(j117$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 8h

#Sum of the distancess
a=8
jagdist <- rollapply(j117$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 10h

#Sum of the distancess
a=10
jagdist <- rollapply(j117$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12h

#Sum of the distancess
a=12
jagdist <- rollapply(j117$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 24h

#Sum of the distancess
a=24
jagdist <- rollapply(j117$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group K ----------------------------------------------------------
##
##27

j27 <- read.delim(file="02_jaguar027_dist_clean.txt")

#Jaguar 27  
# high value
j27 <- j27[-559,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j27$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2per=8

#Sum of the distancess
a=2
jagdist <- rollapply(j27$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3per=12

#Sum of the distancess
a=3
jagdist <- rollapply(j27$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6per=24

#Sum of the distancess
a=6
jagdist <- rollapply(j27$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##30

j30 <- read.delim(file="02_jaguar030_dist_clean.txt")

#Jaguar 30  
# high value
j30 <- j30[-460,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j30$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per=10

#Sum of the distancess
a=2
jagdist <- rollapply(j30$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##31

j31 <- read.delim(file="02_jaguar031_dist_clean.txt")

#Jaguar 31  
# high value
j31 <- j31[-103,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j31$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2per=8

#Sum of the distancess
a=2
jagdist <- rollapply(j31$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3per=12

#Sum of the distancess
a=3
jagdist <- rollapply(j31$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 per=24

#Sum of the distancess
a=6
jagdist <- rollapply(j31$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##32

j32 <- read.delim(file="02_jaguar032_dist_clean.txt")

#Jaguar 32  
# high value
j32 <- j32[-240,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j32$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per=8

#Sum of the distancess
a=2
jagdist <- rollapply(j32$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3per =12

#Sum of the distancess
a=3
jagdist <- rollapply(j32$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6 per = 12

#Sum of the distancess
a=6
jagdist <- rollapply(j32$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##33

j33 <- read.delim(file="02_jaguar033_dist_clean.txt")

#Jaguar 33  
# high value
j33 <- j33[-133,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j33$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per= 8

#Sum of the distancess
a=2
jagdist <- rollapply(j33$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 3 per =12 

#Sum of the distancess
a=3
jagdist <- rollapply(j33$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6 per =24 

#Sum of the distancess
a=6
jagdist <- rollapply(j33$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##53

j53 <- read.delim(file="02_jaguar053_dist_clean.txt")

#Jaguar 53  
# high value
j53 <- j53[-297,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j53$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per = 8 

#Sum of the distancess
a=2
jagdist <- rollapply(j53$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 3 per= 12

#Sum of the distancess
a=3
jagdist <- rollapply(j53$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6 per = 24

#Sum of the distancess
a=6
jagdist <- rollapply(j53$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##55

j55 <- read.delim(file="02_jaguar055_dist_clean.txt")

#Jaguar 55  
# high value
j55 <- j55[-141,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j55$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2per =8

#Sum of the distancess
a=2
jagdist <- rollapply(j55$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale



#a cada 3 per = 12

#Sum of the distancess
a=3
jagdist <- rollapply(j55$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6 per =24

#Sum of the distancess
a=6
jagdist <- rollapply(j55$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##59

j59 <- read.delim(file="02_jaguar059_dist_clean.txt")

#Jaguar 59  
# high value
j59 <- j59[-419,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j59$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

#a cada 2 per =8

#Sum of the distancess
a=2
jagdist <- rollapply(j59$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 3 per =12

#Sum of the distancess
a=3
jagdist <- rollapply(j59$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6per=24

#Sum of the distancess
a=6
jagdist <- rollapply(j59$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##60

j60 <- read.delim(file="02_jaguar060_dist_clean.txt")

#Jaguar 60  
# high value
j60 <- j60[-703,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j60$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j60$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j60$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j60$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##61

j61 <- read.delim(file="02_jaguar061_dist_clean.txt")

#Jaguar 61  
# high value
j61 <- j61[-108,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j61$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j61$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j61$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j61$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group L ----------------------------------------------------------
##
##28

j28 <- read.delim(file="02_jaguar028_dist_clean.txt")

#Jaguar 28  
# high value
j28 <- j28[-205,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j28$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j28$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j28$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j28$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##29

j29 <- read.delim(file="02_jaguar029_dist_clean.txt")

#Jaguar 29  
# high value
j29 <- j29[-67,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j29$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j29$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j29$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j29$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##54

j54 <- read.delim(file="02_jaguar054_dist_clean.txt")

#Jaguar 54  
# high value
j54 <- j54[-126,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j54$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j54$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j54$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j54$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##56

j56 <- read.delim(file="02_jaguar056_dist_clean.txt")

#Jaguar 56  
# high value
j56 <- j56[-109,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j56$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j56$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j56$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j56$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##57

j57 <- read.delim(file="02_jaguar057_dist_clean.txt")

#Jaguar 57  
# high value
j57 <- j57[-28,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j57$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=8

#Sum of the distancess
a=2
jagdist <- rollapply(j57$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 3p=12

#Sum of the distancess
a=3
jagdist <- rollapply(j57$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p=24

#Sum of the distancess
a=6
jagdist <- rollapply(j57$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group M ----------------------------------------------------------
##
##105

j105 <- read.delim(file="02_jaguar0105_dist_clean.txt")

#Jaguar 105  
# high value
j105 <- j105[-2110,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j105$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j105$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j105$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j105$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##106

j106 <- read.delim(file="02_jaguar0106_dist_clean.txt")

#Jaguar 106  
# high value
j106 <- j106[-226,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j106$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j106$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j106$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j106$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##107

j107 <- read.delim(file="02_jaguar0107_dist_clean.txt")

#Jaguar 107  
# high value
j107 <- j107[-287,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j107$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j107$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j107$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j107$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##108

j108 <- read.delim(file="02_jaguar0108_dist_clean.txt")

#Jaguar 108  
# high value
j108 <- j108[-481,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j108$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j108$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j108$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j108$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##109

j109 <- read.delim(file="02_jaguar0109_dist_clean.txt")

#Jaguar 109  
# high value
j109 <- j109[-165,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j109$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j109$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j109$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j109$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##110

j110 <- read.delim(file="02_jaguar0110_dist_clean.txt")

#Jaguar 110  
# high value
j110 <- j110[-163,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j110$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j110$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j110$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j110$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##111

j111 <- read.delim(file="02_jaguar0111_dist_clean.txt")

#Jaguar 111  
# high value
j111 <- j111[-1754,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j111$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j111$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j111$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j111$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##112

j112 <- read.delim(file="02_jaguar0112_dist_clean.txt")

#Jaguar 112  
# high value
j112 <- j112[-197,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j112$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j112$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j112$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j112$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##113

j113 <- read.delim(file="02_jaguar0113_dist_clean.txt")

#Jaguar 113  
# high value
j113 <- j113[-706,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j113$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j113$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j113$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 8h=24

#Sum of the distancess
a=8
jagdist <- rollapply(j113$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##114

j114 <- read.delim(file="02_jaguar0114_dist_clean.txt")

#Jaguar 114  
# high value
j114 <- j114[-1292,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j114$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j114$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j114$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j114$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##115

j115 <- read.delim(file="02_jaguar0115_dist_clean.txt")

#Jaguar 115  
# high value
j115 <- j115[-952,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j115$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=6

#Sum of the distancess
a=2
jagdist <- rollapply(j115$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p=12

#Sum of the distancess
a=4
jagdist <- rollapply(j115$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p=24

#Sum of the distancess
a=8
jagdist <- rollapply(j115$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group N ----------------------------------------------------------
##
##14.1

j14.1 <- read.delim(file="02_jaguar014.1_dist_clean.txt")

#Jaguar 14.1  
# high value
j14.1 <- j14.1[-133,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j14.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j14.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j14.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j14.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j14.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j14.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j14.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j14.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j14.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j14.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j14.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##14.2

j14.2 <- read.delim(file="02_jaguar014.2_dist_clean.txt")

#Jaguar 14.2  
# high value
j14.2 <- j14.2[-52,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j14.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p=4

#Sum of the distancess
a=2
jagdist <- rollapply(j14.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p=6

#Sum of the distancess
a=3
jagdist <- rollapply(j14.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j14.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j14.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j14.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p=24

#Sum of the distancess
a=12
jagdist <- rollapply(j14.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##15.1

j15.1 <- read.delim(file="02_jaguar015.1_dist_clean.txt")

#Jaguar 15.1  
# high value
j15.1 <- j15.1[-813,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j15.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j15.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j15.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j15.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j15.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j15.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j15.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j15.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j15.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j15.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j15.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##15.2

j15.2 <- read.delim(file="02_jaguar015.2_dist_clean.txt")

#Jaguar 15.2  
# high value
j15.2 <- j15.2[-443,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j15.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j15.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j15.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j15.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j15.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j15.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j15.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##19

j19 <- read.delim(file="02_jaguar019_dist_clean.txt")

#Jaguar 19  
# high value
j19 <- j19[-551,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j19$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j19$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j19$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j19$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j19$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j19$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j19$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j19$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j19$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j19$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j19$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##25.1

j25.1 <- read.delim(file="02_jaguar025.1_dist_clean.txt")

#Jaguar 25.1  
# high value
j25.1 <- j25.1[-850,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j25.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j25.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j25.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j25.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j25.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j25.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j25.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j25.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j25.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j25.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j25.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##25.2

j25.2 <- read.delim(file="02_jaguar025.2_dist_clean.txt")

#Jaguar 25.2  
# high value
j25.2 <- j25.2[-336,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j25.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j25.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j25.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j25.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j25.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j25.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j25.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##68.1

j68.1 <- read.delim(file="02_jaguar068.1_dist_clean.txt")

#Jaguar 68.1  
# high value
j68.1 <- j68.1[-463,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j68.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j68.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j68.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j68.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j68.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j68.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j68.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j68.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j68.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j68.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j68.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##68.2

j68.2 <- read.delim(file="02_jaguar068.2_dist_clean.txt")

#Jaguar 68.2  
# high value
j68.2 <- j68.2[-530,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j68.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j68.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j68.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j68.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j68.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j68.2$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j68.2$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j68.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 14p

#Sum of the distancess
a=14
jagdist <- rollapply(j68.2$distance, width=14, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 16p

#Sum of the distancess
a=16
jagdist <- rollapply(j68.2$distance, width=16, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 20p

#Sum of the distancess
a=20
jagdist <- rollapply(j68.2$distance, width=20, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j68.2$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 48p

#Sum of the distancess
a=48
jagdist <- rollapply(j68.2$distance, width=48, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##69.1

j69.1 <- read.delim(file="02_jaguar069.1_dist_clean.txt")

#Jaguar 69.1  
# high value
j69.1 <- j69.1[-2108,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j69.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j69.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j69.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j69.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j69.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j69.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j69.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j69.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j69.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j69.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j69.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##69.2

j69.2 <- read.delim(file="02_jaguar069.2_dist_clean.txt")

#Jaguar 69.2  
# high value
j69.2 <- j69.2[-1196,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j69.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j69.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j69.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j69.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j69.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j69.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j69.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##79.1

j79.1 <- read.delim(file="02_jaguar079.1_dist_clean.txt")

#Jaguar 79.1  
# high value
j79.1 <- j79.1[-1476,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j79.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j79.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j79.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j79.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j79.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j79.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j79.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j79.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j79.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j79.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j79.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##79.2

j79.2 <- read.delim(file="02_jaguar079.2_dist_clean.txt")

#Jaguar 79.2  
# high value
j79.2 <- j79.2[-725,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j79.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j79.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j79.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j79.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j79.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j79.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j79.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##84.1

j84.1 <- read.delim(file="02_jaguar084.1_dist_clean.txt")

#Jaguar 84.1  
# high value
j84.1 <- j84.1[-3304,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j84.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j84.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j84.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j84.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j84.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j84.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j84.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j84.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j84.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j84.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j84.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##84.2

j84.2 <- read.delim(file="02_jaguar084.2_dist_clean.txt")

#Jaguar 84.2  
# high value
j84.2 <- j84.2[-1338,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j84.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j84.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j84.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j84.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j84.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j84.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j84.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##86.1

j86.1 <- read.delim(file="02_jaguar086.1_dist_clean.txt")

#Jaguar 86.1  
# high value
j86.1 <- j86.1[-994,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j86.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j86.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j86.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j86.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j86.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j86.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j86.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j86.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j86.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j86.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j86.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##86.2

j86.2 <- read.delim(file="02_jaguar086.2_dist_clean.txt")

#Jaguar 86.2  
# high value
j86.2 <- j86.2[-330,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j86.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j86.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j86.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j86.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j86.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j86.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j86.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##87.1

j87.1 <- read.delim(file="02_jaguar087.1_dist_clean.txt")

#Jaguar 87.1  
# high value
j87.1 <- j87.1[-266,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j87.1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j87.1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j87.1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j87.1$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j87.1$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j87.1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 7p

#Sum of the distancess
a=7
jagdist <- rollapply(j87.1$distance, width=7, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j87.1$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j87.1$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j87.1$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j87.1$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##87.2

j87.2 <- read.delim(file="02_jaguar087.2_dist_clean.txt")

#Jaguar 87.2  
# high value
j87.2 <- j87.2[-132,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j87.2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j87.2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j87.2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j87.2$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j87.2$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j87.2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j87.2$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##101

j101 <- read.delim(file="02_jaguar0101_dist_clean.txt")

#Jaguar 101  
# high value
j101 <- j101[-333,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j101$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j101$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##102

j102 <- read.delim(file="02_jaguar0102_dist_clean.txt")

#Jaguar 102  
# high value
j102 <- j102[-137,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j102$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j102$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j102$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j102$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##103

j103 <- read.delim(file="02_jaguar0103_dist_clean.txt")

#Jaguar 103  
# high value
j103 <- j103[-15,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j103$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j103$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j103$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##104

j104 <- read.delim(file="02_jaguar0104_dist_clean.txt")

#Jaguar 104  
# high value
j104 <- j104[-110,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j104$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j104$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group O ----------------------------------------------------------
##
##74

j74 <- read.delim(file="02_jaguar074_dist_clean.txt")

#Jaguar 74  
# high value
j74 <- j74[-1257,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j74$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j74$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j74$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j74$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##75

j75 <- read.delim(file="02_jaguar075_dist_clean.txt")

#Jaguar 75  
# high value
j75 <- j75[-1640,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j75$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j75$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j75$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j75$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# Jaguar group P ----------------------------------------------------------
##
##51

j51 <- read.delim(file="02_jaguar051_dist_clean.txt")

#Jaguar 51  
# high value
j51 <- j51[-724,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j51$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j51$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j51$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale



# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j51$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group Q ----------------------------------------------------------
##
##71

j71 <- read.delim(file="02_jaguar071_dist_clean.txt")

#Jaguar 71  
# high value
j71 <- j71[-917,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j71$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j71$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j71$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j71$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j71$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j71$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j71$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##72

j72 <- read.delim(file="02_jaguar072_dist_clean.txt")

#Jaguar 72  
# high value
j72 <- j72[-715,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j72$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j72$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j72$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j72$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j72$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j72$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale



#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j72$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##73

j73 <- read.delim(file="02_jaguar073_dist_clean.txt")

#Jaguar 73  
# high value
j73 <- j73[-619,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j73$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j73$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j73$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j73$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j73$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j73$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j73$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# Jaguar group R ----------------------------------------------------------
##
##76

j76 <- read.delim(file="02_jaguar076_dist_clean.txt")

#Jaguar 76  
# high value
j76 <- j76[-1609,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j76$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j76$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j76$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j76$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##77

j77 <- read.delim(file="02_jaguar077_dist_clean.txt")

#Jaguar 77  
# high value
j77 <- j77[-1362,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j77$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j77$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j77$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j77$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##16

j16 <- read.delim(file="02_jaguar016_dist_clean.txt")

#Jaguar 16  
# high value
j16 <- j16[-3460,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j16$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j16$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j16$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j16$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##70

j70 <- read.delim(file="02_jaguar070_dist_clean.txt")

#Jaguar 70  
# high value
j70 <- j70[-1054,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j70$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j70$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j70$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j70$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j70$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j70$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j70$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group T ----------------------------------------------------------

##34

j34 <- read.delim(file="02_jaguar034_dist_clean.txt")

#Jaguar 34  
# high value
j34 <- j34[-78,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j34$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

##35

j35 <- read.delim(file="02_jaguar035_dist_clean.txt")

#Jaguar 35  
# high value
j35 <- j35[-13,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j35$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j35$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##36

j36 <- read.delim(file="02_jaguar036_dist_clean.txt")

#Jaguar 36  
# high value
j36 <- j36[-27,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j36$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j36$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##37

j37 <- read.delim(file="02_jaguar037_dist_clean.txt")

#Jaguar 37  
# high value
j37 <- j37[-117,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j37$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

##
##38

j38 <- read.delim(file="02_jaguar038_dist_clean.txt")

#Jaguar 38  
# high value
j38 <- j38[-14,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j38$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j38$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##58

j58 <- read.delim(file="02_jaguar058_dist_clean.txt")

#Jaguar 58  
# high value
j58 <- j58[-94,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j58$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

##
##62

j62 <- read.delim(file="02_jaguar062_dist_clean.txt")

#Jaguar 62  
# high value
j62 <- j62[-85,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j62$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j62$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group U ----------------------------------------------------------

##
##39

j39 <- read.delim(file="02_jaguar039_dist_clean.txt")

#Jaguar 39  
# high value
j39 <- j39[-270,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j39$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j39$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##40

j40 <- read.delim(file="02_jaguar040_dist_clean.txt")

#Jaguar 40  
# high value
j40 <- j40[-150,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j40$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale


# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j40$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##63

j63 <- read.delim(file="02_jaguar063_dist_clean.txt")

#Jaguar 63  
# high value
j63 <- j63[-783,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j63$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j63$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j63$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group V ----------------------------------------------------------


##
##1

j1 <- read.delim(file="02_jaguar01_dist_clean.txt")

#Jaguar 1  
# high value
j1 <- j1[-279,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j1$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j1$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j1$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j1$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##3

j3 <- read.delim(file="02_jaguar03_dist_clean.txt")

#Jaguar 3  
# high value
j3 <- j3[-136,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j3$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j3$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j3$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j3$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##4

j4 <- read.delim(file="02_jaguar04_dist_clean.txt")

#Jaguar 4  
# high value
j4 <- j4[-500,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j4$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j4$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j4$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j4$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##5

j5 <- read.delim(file="02_jaguar05_dist_clean.txt")

#Jaguar 5  
# high value
j5 <- j5[-280,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j5$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j5$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j5$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j5$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##6

j6 <- read.delim(file="02_jaguar06_dist_clean.txt")

#Jaguar 6  
# high value
j6 <- j6[-982,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j6$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j6$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j6$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j6$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##7

j7 <- read.delim(file="02_jaguar07_dist_clean.txt")

#Jaguar 7  
# high value
j7 <- j7[-621,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j7$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j7$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j7$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j7$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##9

j9 <- read.delim(file="02_jaguar09_dist_clean.txt")

#Jaguar 9  
# high value
j9 <- j9[-941,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j9$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j9$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j9$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j9$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j9$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j9$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j9$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##10

j10 <- read.delim(file="02_jaguar010_dist_clean.txt")

#Jaguar 10  
# high value
j10 <- j10[-1515,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j10$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j10$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j10$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j10$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j10$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j10$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j10$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##11

j11 <- read.delim(file="02_jaguar011_dist_clean.txt")

#Jaguar 11  
# high value
j11 <- j11[-885,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j11$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j11$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j11$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j11$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j11$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j11$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j11$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group W ----------------------------------------------------------

##
##2

j2 <- read.delim(file="02_jaguar02_dist_clean.txt")

#Jaguar 2  
# high value
j2 <- j2[-146,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j2$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j2$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j2$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j2$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##8

j8 <- read.delim(file="02_jaguar08_dist_clean.txt")

#Jaguar 8  
# high value
j8 <- j8[-538,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j8$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j8$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j8$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j8$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##21

j21 <- read.delim(file="02_jaguar021_dist_clean.txt")

#Jaguar 21  
# high value
j21 <- j21[-326,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j21$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j21$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j21$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale



#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j21$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale
##
##78

j78 <- read.delim(file="02_jaguar078_dist_clean.txt")

#Jaguar 78  
# high value
j78 <- j78[-798,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j78$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j78$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j78$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j78$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group X ----------------------------------------------------------

##
##83

j83 <- read.delim(file="02_jaguar083_dist_clean.txt")

#Jaguar 83  
# high value
j83 <- j83[-110,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j83$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j83$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j83$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j83$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# Jaguar group Y ----------------------------------------------------------

##
##42

j42 <- read.delim(file="02_jaguar042_dist_clean.txt")

#Jaguar 42  
# high value
j42 <- j42[-7665,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j42$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j42$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j42$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j42$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j42$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j42$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 10

#Sum of the distancess
a=10
jagdist <- rollapply(j42$distance, width=10, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j42$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 14

#Sum of the distancess
a=14
jagdist <- rollapply(j42$distance, width=14, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 16

#Sum of the distancess
a=16
jagdist <- rollapply(j42$distance, width=16, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 20

#Sum of the distancess
a=20
jagdist <- rollapply(j42$distance, width=20, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 24

#Sum of the distancess
a=24
jagdist <- rollapply(j42$distance, width=24, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 48

#Sum of the distancess
a=48
jagdist <- rollapply(j42$distance, width=48, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##66

j66 <- read.delim(file="02_jaguar066_dist_clean.txt")

#Jaguar 66  
# high value
j66 <- j66[-50,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j66$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j66$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j66$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j66$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j66$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j66$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j66$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##80

j80 <- read.delim(file="02_jaguar080_dist_clean.txt")

#Jaguar 80  
# high value
j80 <- j80[-479,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j80$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j80$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j80$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


# every 8p

#Sum of the distancess
a=8
jagdist <- rollapply(j80$distance, width=8, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 16

#Sum of the distancess
a=16
jagdist <- rollapply(j80$distance, width=16, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

##
##90

j90 <- read.delim(file="02_jaguar090_dist_clean.txt")

#Jaguar 90  
# high value
j90 <- j90[-314,]# outliers 

#Pareto estimations
paretohrt <- fitgpd(j90$distance[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 
paretohrt.scale

# every 2p

#Sum of the distancess
a=2
jagdist <- rollapply(j90$distance, width=2, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 3p

#Sum of the distancess
a=3
jagdist <- rollapply(j90$distance, width=3, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

# every 4p

#Sum of the distancess
a=4
jagdist <- rollapply(j90$distance, width=4, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 5p

#Sum of the distancess
a=5
jagdist <- rollapply(j90$distance, width=5, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

#a cada 6p

#Sum of the distancess
a=6
jagdist <- rollapply(j90$distance, width=6, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale


#a cada 12p

#Sum of the distancess
a=12
jagdist <- rollapply(j90$distance, width=12, FUN= sum, -1, by = a, reorder=TRUE, na.rm=TRUE)

#Pareto estimations
paretohrt <- fitgpd(jagdist[-1],threshold = 97.5)
# allowable movement distance
paretohrt.scale<-paretohrt$param[[1]] 

paretohrt.scale

