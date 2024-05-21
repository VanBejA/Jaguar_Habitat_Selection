##################################
##### SCALE SELECTION 
##### Vanesa Bejarano Alegre   12.07.2021
##### ##############################
## Bioma = Flooded grasslands and savannas - Pantanal

library(dplyr)
library(MuMIn)
library(survival)
library(tidyverse)
library(coxme)


# Upload all the rsf data -------------------------------------------------

# Landcover night -----------------------------------------------------------

Agroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Agroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Agroup.land$group <- "A"

Bgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Bgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Bgroup.land$group <- "B"

Cgroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Cgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Clandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.land <- rbind(Cgroup.land.some, Cgroup.land.miss)
Cgroup.land$group <- "C"


Dgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Dgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Dgroup.land$group <- "D"

Egroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Egroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Elandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.land <- rbind(Egroup.land.some, Egroup.land.miss)
Egroup.land$group <- "E"

Fgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Fgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Fgroup.land$group <- "F"

Ggroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Ggroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ggroup.land$group <- "G"

Hgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Hgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Hgroup.land$group <- "H"

Igroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Igroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ilandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.land <- rbind(Igroup.land.some, Igroup.land.miss)
Igroup.land$group <- "I"

Jgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Jgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Jgroup.land$group <- "J"

Kgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Kgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Kgroup.land$group <- "K"

Lgroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Lgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Llandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.land <- rbind(Lgroup.land.some, Lgroup.land.miss)
Lgroup.land$group <- "L"

Mgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Mgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Mgroup.land$group <- "M"

Ngroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Ngroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Nlandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.land <- rbind(Ngroup.land.some, Ngroup.land.miss)
Ngroup.land$group <- "N"

Ogroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Ogroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ogroup.land$group <- "O"

Pgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Pgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Pgroup.land$group <- "P"

Qgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Qgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Qgroup.land$group <- "Q"

Rgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Rgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Rgroup.land$group <- "R"

Sgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Sgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Sgroup.land$group <- "S"

Tgroup.land.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Tgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.land.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tlandcover_missing_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.land <- rbind(Tgroup.land.some, Tgroup.land.miss)
Tgroup.land$group <- "T"


Ugroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Ugroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ugroup.land$group <- "U"

Vgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Vgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Vgroup.land$group <- "V"

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Wgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.land <- bind_rows(j02,j08,j21)
Wgroup.land$group <- "W"



Xgroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Xgroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Xgroup.land$group <- "X"


Ygroup.land <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/01_Zeller_landscape/rsf_Ygroup_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ygroup.land$group <- "Y"

Zgroup.land <-  Wgroup[Wgroup$id=="78",]
Zgroup.land$group <- "Z"

landcover.night <- bind_rows(Agroup.land,Bgroup.land,Cgroup.land,
                           Dgroup.land,Egroup.land,Fgroup.land,
                           Ggroup.land,
                           Hgroup.land,Igroup.land,Jgroup.land
                           ,Kgroup.land,Lgroup.land,
                           Mgroup.land,Ngroup.land,Ogroup.land,
                           Pgroup.land,Qgroup.land,
                           Rgroup.land,Sgroup.land,Tgroup.land,
                           Ugroup.land,Vgroup.land,Zgroup.land,
                           Wgroup.land,Xgroup.land,Ygroup.land)

landcover.night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.land, Bgroup.land, Cgroup.land,
   Dgroup.land,Egroup.land,Fgroup.land,Ggroup.land,
   Hgroup.land,Igroup.land,Jgroup.land,Kgroup.land,Lgroup.land,
   Mgroup.land,Ngroup.land,Ogroup.land,Pgroup.land,Qgroup.land,
   Rgroup.land,Sgroup.land,Tgroup.land,Ugroup.land,Vgroup.land,
   Wgroup.land,Xgroup.land,Ygroup.land,Zgroup.land)

rm(Cgroup.land.miss,Cgroup.land.some, Egroup.land.miss, Egroup.land.some,
   Igroup.land.some,Igroup.land.miss,Lgroup.land.miss,Lgroup.land.some,
   Ngroup.land.miss,Ngroup.land.some,Tgroup.land.miss,Tgroup.land.some)


# Grass 527 scale ---------------------------------------------------------
anyNA(landcover.night)
anyNA(landcover.night$grass)
anyNA(landcover.night$grass527)

grass.used <- data.frame(landcover.night$grass)
grass.used$case_ <- 1
grass.used$id  <- landcover.night$id
grass.used$sex  <- landcover.night$sex
grass.used$group  <- landcover.night$group

variable.names(grass.used)

grass.used <- rename(grass.used, grass527= landcover.night.grass)

variable.names(grass.used)


avail.527 <- data.frame(landcover.night$grass527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, grass527= landcover.night.grass527)
avail.527$id <- landcover.night$id
avail.527$sex <- landcover.night$sex
avail.527$group <- landcover.night$group
grass.data <- rbind(grass.used, avail.527)


# Grass 724 scale ---------------------------------------------------------

anyNA(landcover.night$grass724)

grass.724 <- data.frame(landcover.night$grass)

grass.724 <- rename(grass.724, grass724= landcover.night.grass)

avail.724  <- data.frame(landcover.night$grass724)

avail.724<- rename(avail.724, grass724= landcover.night.grass724)

grass.724 <- rbind(grass.724, avail.724)

# Grass 1242 scale --------------------------------------------------------
anyNA(landcover.night$grass1242)

grass.1242 <- data.frame(landcover.night$grass)

grass.1242 <- rename(grass.1242, grass1242= landcover.night.grass)

avail.1242  <- data.frame(landcover.night$grass1242)

avail.1242<- rename(avail.1242, grass1242= landcover.night.grass1242)

grass.1242 <- rbind(grass.1242, avail.1242)

# Grass 1545 scale --------------------------------------------------------
anyNA(landcover.night$grass1545)

grass.1545 <- data.frame(landcover.night$grass)

grass.1545 <- rename(grass.1545, grass1545= landcover.night.grass)

avail.1545 <- data.frame(landcover.night$grass1545)

avail.1545 <- rename(avail.1545, grass1545= landcover.night.grass1545)

grass.1545<- rbind(grass.1545, avail.1545)

# Grass 2105 scale --------------------------------------------------------
anyNA(landcover.night$grass2105)

grass.2105 <- data.frame(landcover.night$grass)

grass.2105 <- rename(grass.2105, grass2105= landcover.night.grass)

avail.2105  <- data.frame(landcover.night$grass2105)

avail.2105<- rename(avail.2105, grass2105= landcover.night.grass2105)

grass.2105 <- rbind(grass.2105, avail.2105)


# Grass 3001 scale --------------------------------------------------------
anyNA(landcover.night$grass3001)

grass.3001 <- data.frame(landcover.night$grass)

grass.3001 <- rename(grass.3001, grass3001= landcover.night.grass)

avail.3001  <- data.frame(landcover.night$grass3001)

avail.3001<- rename(avail.3001, grass3001= landcover.night.grass3001)

grass.3001 <- rbind(grass.3001, avail.3001)

# Grass 5886 scale --------------------------------------------------------
anyNA(landcover.night$grass5886)

grass.5886 <- data.frame(landcover.night$grass)

grass.5886 <- rename(grass.5886, grass5886= landcover.night.grass)

avail.5886  <- data.frame(landcover.night$grass5886)

avail.5886<- rename(avail.5886, grass5886= landcover.night.grass5886)

grass.5886 <- rbind(grass.5886, avail.5886)

# crop 527 scale ---------------------------------------------------------

anyNA(landcover.night$crop)
anyNA(landcover.night$crop527)

crop.used <- data.frame(landcover.night$crop)
crop.used$case_ <- 1
crop.used$id  <- landcover.night$id

variable.names(crop.used)

crop.used <- rename(crop.used, crop527= landcover.night.crop)

variable.names(crop.used)


avail.527 <- data.frame(landcover.night$crop527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, crop527= landcover.night.crop527)
avail.527$id <- landcover.night$id

crop.data <- rbind(crop.used, avail.527)


# crop 724 scale ---------------------------------------------------------

anyNA(landcover.night$crop724)

crop.724 <- data.frame(landcover.night$crop)

crop.724 <- rename(crop.724, crop724= landcover.night.crop)

avail.724  <- data.frame(landcover.night$crop724)

avail.724<- rename(avail.724, crop724= landcover.night.crop724)

crop.724 <- rbind(crop.724, avail.724)

# crop 1242 scale --------------------------------------------------------
anyNA(landcover.night$crop1242)

crop.1242 <- data.frame(landcover.night$crop)

crop.1242 <- rename(crop.1242, crop1242= landcover.night.crop)

avail.1242  <- data.frame(landcover.night$crop1242)

avail.1242<- rename(avail.1242, crop1242= landcover.night.crop1242)

crop.1242 <- rbind(crop.1242, avail.1242)

# crop 1545 scale --------------------------------------------------------
anyNA(landcover.night$crop1545)

crop.1545 <- data.frame(landcover.night$crop)

crop.1545 <- rename(crop.1545, crop1545= landcover.night.crop)

avail.1545 <- data.frame(landcover.night$crop1545)

avail.1545 <- rename(avail.1545, crop1545= landcover.night.crop1545)

crop.1545<- rbind(crop.1545, avail.1545)

# crop 2105 scale --------------------------------------------------------
anyNA(landcover.night$crop2105)

crop.2105 <- data.frame(landcover.night$crop)

crop.2105 <- rename(crop.2105, crop2105= landcover.night.crop)

avail.2105  <- data.frame(landcover.night$crop2105)

avail.2105<- rename(avail.2105, crop2105= landcover.night.crop2105)

crop.2105 <- rbind(crop.2105, avail.2105)


# crop 3001 scale --------------------------------------------------------
anyNA(landcover.night$crop3001)

crop.3001 <- data.frame(landcover.night$crop)

crop.3001 <- rename(crop.3001, crop3001= landcover.night.crop)

avail.3001  <- data.frame(landcover.night$crop3001)

avail.3001<- rename(avail.3001, crop3001= landcover.night.crop3001)

crop.3001 <- rbind(crop.3001, avail.3001)

# crop 5886 scale --------------------------------------------------------
anyNA(landcover.night$crop5886)

crop.5886 <- data.frame(landcover.night$crop)

crop.5886 <- rename(crop.5886, crop5886= landcover.night.crop)

avail.5886  <- data.frame(landcover.night$crop5886)

avail.5886<- rename(avail.5886, crop5886= landcover.night.crop5886)

crop.5886 <- rbind(crop.5886, avail.5886)

# flood 527 scale ---------------------------------------------------------

anyNA(landcover.night$flood)
anyNA(landcover.night$flood527)

flood.used <- data.frame(landcover.night$flood)
flood.used$case_ <- 1
flood.used$id  <- landcover.night$id

variable.names(flood.used)

flood.used <- rename(flood.used, flood527= landcover.night.flood)

variable.names(flood.used)


avail.527 <- data.frame(landcover.night$flood527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, flood527= landcover.night.flood527)
avail.527$id <- landcover.night$id

flood.data <- rbind(flood.used, avail.527)


# flood 724 scale ---------------------------------------------------------

anyNA(landcover.night$flood724)

flood.724 <- data.frame(landcover.night$flood)

flood.724 <- rename(flood.724, flood724= landcover.night.flood)

avail.724  <- data.frame(landcover.night$flood724)

avail.724<- rename(avail.724, flood724= landcover.night.flood724)

flood.724 <- rbind(flood.724, avail.724)

# flood 1242 scale --------------------------------------------------------
anyNA(landcover.night$flood1242)

flood.1242 <- data.frame(landcover.night$flood)

flood.1242 <- rename(flood.1242, flood1242= landcover.night.flood)

avail.1242  <- data.frame(landcover.night$flood1242)

avail.1242<- rename(avail.1242, flood1242= landcover.night.flood1242)

flood.1242 <- rbind(flood.1242, avail.1242)

# flood 1545 scale --------------------------------------------------------
anyNA(landcover.night$flood1545)

flood.1545 <- data.frame(landcover.night$flood)

flood.1545 <- rename(flood.1545, flood1545= landcover.night.flood)

avail.1545 <- data.frame(landcover.night$flood1545)

avail.1545 <- rename(avail.1545, flood1545= landcover.night.flood1545)

flood.1545<- rbind(flood.1545, avail.1545)

# flood 2105 scale --------------------------------------------------------
anyNA(landcover.night$flood2105)

flood.2105 <- data.frame(landcover.night$flood)

flood.2105 <- rename(flood.2105, flood2105= landcover.night.flood)

avail.2105  <- data.frame(landcover.night$flood2105)

avail.2105<- rename(avail.2105, flood2105= landcover.night.flood2105)

flood.2105 <- rbind(flood.2105, avail.2105)


# flood 3001 scale --------------------------------------------------------
anyNA(landcover.night$flood3001)

flood.3001 <- data.frame(landcover.night$flood)

flood.3001 <- rename(flood.3001, flood3001= landcover.night.flood)

avail.3001  <- data.frame(landcover.night$flood3001)

avail.3001<- rename(avail.3001, flood3001= landcover.night.flood3001)

flood.3001 <- rbind(flood.3001, avail.3001)

# flood 5886 scale --------------------------------------------------------
anyNA(landcover.night$flood5886)

flood.5886 <- data.frame(landcover.night$flood)

flood.5886 <- rename(flood.5886, flood5886= landcover.night.flood)

avail.5886  <- data.frame(landcover.night$flood5886)

avail.5886<- rename(avail.5886, flood5886= landcover.night.flood5886)

flood.5886 <- rbind(flood.5886, avail.5886)

# herva 527 scale ---------------------------------------------------------

anyNA(landcover.night$herva)
anyNA(landcover.night$herva527)

herva.used <- data.frame(landcover.night$herva)
herva.used$case_ <- 1
herva.used$id  <- landcover.night$id

variable.names(herva.used)

herva.used <- rename(herva.used, herva527= landcover.night.herva)

variable.names(herva.used)


avail.527 <- data.frame(landcover.night$herva527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, herva527= landcover.night.herva527)
avail.527$id <- landcover.night$id

herva.data <- rbind(herva.used, avail.527)


# herva 724 scale ---------------------------------------------------------

anyNA(landcover.night$herva724)

herva.724 <- data.frame(landcover.night$herva)

herva.724 <- rename(herva.724, herva724= landcover.night.herva)

avail.724  <- data.frame(landcover.night$herva724)

avail.724<- rename(avail.724, herva724= landcover.night.herva724)

herva.724 <- rbind(herva.724, avail.724)

# herva 1242 scale --------------------------------------------------------
anyNA(landcover.night$herva1242)

herva.1242 <- data.frame(landcover.night$herva)

herva.1242 <- rename(herva.1242, herva1242= landcover.night.herva)

avail.1242  <- data.frame(landcover.night$herva1242)

avail.1242<- rename(avail.1242, herva1242= landcover.night.herva1242)

herva.1242 <- rbind(herva.1242, avail.1242)

# herva 1545 scale --------------------------------------------------------
anyNA(landcover.night$herva1545)

herva.1545 <- data.frame(landcover.night$herva)

herva.1545 <- rename(herva.1545, herva1545= landcover.night.herva)

avail.1545 <- data.frame(landcover.night$herva1545)

avail.1545 <- rename(avail.1545, herva1545= landcover.night.herva1545)

herva.1545<- rbind(herva.1545, avail.1545)

# herva 2105 scale --------------------------------------------------------
anyNA(landcover.night$herva2105)

herva.2105 <- data.frame(landcover.night$herva)

herva.2105 <- rename(herva.2105, herva2105= landcover.night.herva)

avail.2105  <- data.frame(landcover.night$herva2105)

avail.2105<- rename(avail.2105, herva2105= landcover.night.herva2105)

herva.2105 <- rbind(herva.2105, avail.2105)


# herva 3001 scale --------------------------------------------------------
anyNA(landcover.night$herva3001)

herva.3001 <- data.frame(landcover.night$herva)

herva.3001 <- rename(herva.3001, herva3001= landcover.night.herva)

avail.3001  <- data.frame(landcover.night$herva3001)

avail.3001<- rename(avail.3001, herva3001= landcover.night.herva3001)

herva.3001 <- rbind(herva.3001, avail.3001)

# herva 5886 scale --------------------------------------------------------
anyNA(landcover.night$herva5886)

herva.5886 <- data.frame(landcover.night$herva)

herva.5886 <- rename(herva.5886, herva5886= landcover.night.herva)

avail.5886  <- data.frame(landcover.night$herva5886)

avail.5886<- rename(avail.5886, herva5886= landcover.night.herva5886)

herva.5886 <- rbind(herva.5886, avail.5886)

# tree 527 scale ---------------------------------------------------------

anyNA(landcover.night$tree)
anyNA(landcover.night$tree527)

tree.used <- data.frame(landcover.night$tree)
tree.used$case_ <- 1
tree.used$id  <- landcover.night$id

variable.names(tree.used)

tree.used <- rename(tree.used, tree527= landcover.night.tree)

variable.names(tree.used)


avail.527 <- data.frame(landcover.night$tree527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, tree527= landcover.night.tree527)
avail.527$id <- landcover.night$id

tree.data <- rbind(tree.used, avail.527)


# tree 724 scale ---------------------------------------------------------

anyNA(landcover.night$tree724)

tree.724 <- data.frame(landcover.night$tree)

tree.724 <- rename(tree.724, tree724= landcover.night.tree)

avail.724  <- data.frame(landcover.night$tree724)

avail.724<- rename(avail.724, tree724= landcover.night.tree724)

tree.724 <- rbind(tree.724, avail.724)

# tree 1242 scale --------------------------------------------------------
anyNA(landcover.night$tree1242)

tree.1242 <- data.frame(landcover.night$tree)

tree.1242 <- rename(tree.1242, tree1242= landcover.night.tree)

avail.1242  <- data.frame(landcover.night$tree1242)

avail.1242<- rename(avail.1242, tree1242= landcover.night.tree1242)

tree.1242 <- rbind(tree.1242, avail.1242)

# tree 1545 scale --------------------------------------------------------
anyNA(landcover.night$tree1545)

tree.1545 <- data.frame(landcover.night$tree)

tree.1545 <- rename(tree.1545, tree1545= landcover.night.tree)

avail.1545 <- data.frame(landcover.night$tree1545)

avail.1545 <- rename(avail.1545, tree1545= landcover.night.tree1545)

tree.1545<- rbind(tree.1545, avail.1545)

# tree 2105 scale --------------------------------------------------------
anyNA(landcover.night$tree2105)

tree.2105 <- data.frame(landcover.night$tree)

tree.2105 <- rename(tree.2105, tree2105= landcover.night.tree)

avail.2105  <- data.frame(landcover.night$tree2105)

avail.2105<- rename(avail.2105, tree2105= landcover.night.tree2105)

tree.2105 <- rbind(tree.2105, avail.2105)


# tree 3001 scale --------------------------------------------------------
anyNA(landcover.night$tree3001)

tree.3001 <- data.frame(landcover.night$tree)

tree.3001 <- rename(tree.3001, tree3001= landcover.night.tree)

avail.3001  <- data.frame(landcover.night$tree3001)

avail.3001<- rename(avail.3001, tree3001= landcover.night.tree3001)

tree.3001 <- rbind(tree.3001, avail.3001)

# tree 5886 scale --------------------------------------------------------
anyNA(landcover.night$tree5886)

tree.5886 <- data.frame(landcover.night$tree)

tree.5886 <- rename(tree.5886, tree5886= landcover.night.tree)

avail.5886  <- data.frame(landcover.night$tree5886)

avail.5886<- rename(avail.5886, tree5886= landcover.night.tree5886)

tree.5886 <- rbind(tree.5886, avail.5886)

# livestock night -----------------------------------------------------------

Agroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Alivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Blivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Clivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ccattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.livestock <- rbind(Cgroup.livestock.some, Cgroup.livestock.miss)


Dgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Dlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Elivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ecattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.livestock<- rbind(Egroup.livestock.some, Egroup.livestock.miss)

Fgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Flivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Glivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Hlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Ilivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Icattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.livestock<- rbind(Igroup.livestock.some, Igroup.livestock.miss)



Jgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Jlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Klivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Llivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lcattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.livestock<- rbind(Lgroup.livestock.some, Lgroup.livestock.miss)


Mgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Mlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Nlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ncattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.livestock<- rbind(Ngroup.livestock.some, Ngroup.livestock.miss)

Ogroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Olivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Plivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Qlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Rlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Slivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.land$id <- as.factor(Sgroup.land$id)
levels(Sgroup.land$id)

Tgroup.livestock.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Tlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.livestock.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tcattle_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.livestock<- rbind(Tgroup.livestock.some, Tgroup.livestock.miss)

Ugroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Ulivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Vlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Wlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.livestock <- bind_rows(j02,j08,j21)


Zgroup.livestock <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Zlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Xgroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Xlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.livestock <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/03_Zeller_livestock/rsf_Ylivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


livest_night$id <- as.factor(livest_night$id)
levels(livest_night$id)



livest_night<- bind_rows(Agroup.livestock,Bgroup.livestock, Cgroup.livestock,
                       Dgroup.livestock,Egroup.livestock,
                       Fgroup.livestock,Ggroup.livestock,
                       Hgroup.livestock,Igroup.livestock,
                       Jgroup.livestock,Kgroup.livestock,Lgroup.livestock,
                       Mgroup.livestock,Ngroup.livestock,
                       Ogroup.livestock,Pgroup.livestock,Qgroup.livestock,
                       Rgroup.livestock,Sgroup.livestock,Zgroup.livestock,                                 Tgroup.livestock,Ugroup.livestock,Vgroup.livestock,
                       Wgroup.livestock,Xgroup.livestock,Ygroup.livestock)

livest_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.livestock, Bgroup.livestock, Cgroup.livestock,
   Dgroup.livestock,Egroup.livestock,Fgroup.livestock,
   Ggroup.livestock,Zgroup.livestock,
   Hgroup.livestock,Igroup.livestock,Jgroup.livestock,
   Kgroup.livestock,Lgroup.livestock,
   Mgroup.livestock,Ngroup.livestock,Ogroup.livestock,
   Pgroup.livestock,Qgroup.livestock,
   Rgroup.livestock,Sgroup.livestock,Tgroup.livestock,
   Ugroup.livestock,Vgroup.livestock,
   Wgroup.livestock,Xgroup.livestock,Ygroup.livestock)

rm(Cgroup.livestock.miss,Cgroup.livestock.some, 
   Egroup.livestock.miss, Egroup.livestock.some,
   Igroup.livestock.some,Igroup.livestock.miss,
   Lgroup.livestock.miss,Lgroup.livestock.some,
   Ngroup.livestock.miss,Ngroup.livestock.some,
   Tgroup.livestock.miss,Tgroup.livestock.some)
# livestock 527 scale ---------------------------------------------------------

anyNA(livest_night$livestock)
anyNA(livest_night$livestock527)
livest_night[is.na(livest_night)] <- 0


livestock.used <- data.frame(livest_night$livestock)
livestock.used$case_ <- 1
livestock.used$id  <- livest_night$id

variable.names(livestock.used)

livestock.used <- rename(livestock.used, livestock527= livest_night.livestock)

variable.names(livestock.used)


avail.527 <- data.frame(livest_night$livestock527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, livestock527= livest_night.livestock527)
avail.527$id <- livest_night$id

livestock.data <- rbind(livestock.used, avail.527)


# livestock 724 scale ---------------------------------------------------------

anyNA(livest_night$livestock724)

livestock.724 <- data.frame(livest_night$livestock)

livestock.724 <- rename(livestock.724, livestock724= livest_night.livestock)

avail.724  <- data.frame(livest_night$livestock724)

avail.724<- rename(avail.724, livestock724= livest_night.livestock724)

livestock.724 <- rbind(livestock.724, avail.724)

# livestock 1242 scale --------------------------------------------------------
anyNA(livest_night$livestock1242)

livestock.1242 <- data.frame(livest_night$livestock)

livestock.1242 <- rename(livestock.1242, livestock1242= livest_night.livestock)

avail.1242  <- data.frame(livest_night$livestock1242)

avail.1242<- rename(avail.1242, livestock1242= livest_night.livestock1242)

livestock.1242 <- rbind(livestock.1242, avail.1242)

# livestock 1545 scale --------------------------------------------------------
anyNA(livest_night$livestock1545)

livestock.1545 <- data.frame(livest_night$livestock)

livestock.1545 <- rename(livestock.1545, livestock1545= livest_night.livestock)

avail.1545 <- data.frame(livest_night$livestock1545)

avail.1545 <- rename(avail.1545, livestock1545= livest_night.livestock1545)

livestock.1545<- rbind(livestock.1545, avail.1545)

# livestock 2105 scale --------------------------------------------------------
anyNA(livest_night$livestock2105)

livestock.2105 <- data.frame(livest_night$livestock)

livestock.2105 <- rename(livestock.2105, livestock2105= livest_night.livestock)

avail.2105  <- data.frame(livest_night$livestock2105)

avail.2105<- rename(avail.2105, livestock2105= livest_night.livestock2105)

livestock.2105 <- rbind(livestock.2105, avail.2105)


# livestock 3001 scale --------------------------------------------------------
anyNA(livest_night$livestock3001)

livestock.3001 <- data.frame(livest_night$livestock)

livestock.3001 <- rename(livestock.3001, livestock3001= livest_night.livestock)

avail.3001  <- data.frame(livest_night$livestock3001)

avail.3001<- rename(avail.3001, livestock3001= livest_night.livestock3001)

livestock.3001 <- rbind(livestock.3001, avail.3001)

# livestock 5886 scale --------------------------------------------------------
anyNA(livest_night$livestock5886)

livestock.5886 <- data.frame(livest_night$livestock)

livestock.5886 <- rename(livestock.5886, livestock5886= livest_night.livestock)

avail.5886  <- data.frame(livest_night$livestock5886)

avail.5886<- rename(avail.5886, livestock5886= livest_night.livestock5886)

livestock.5886 <- rbind(livestock.5886, avail.5886)

# ovine night -----------------------------------------------------------

Agroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Alivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Blivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Clivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Covine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.ovine<- rbind(Cgroup.ovine.some, Cgroup.ovine.miss)

Dgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Dlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Elivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Eovine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.ovine<- rbind(Egroup.ovine.some, Egroup.ovine.miss)

Fgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Flivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Glivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Hlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Ilivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Iovine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.ovine<- rbind(Igroup.ovine.some, Igroup.ovine.miss)

Jgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Jlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Klivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Llivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lovine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.ovine<- rbind(Lgroup.ovine.some, Lgroup.ovine.miss)

Mgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Mlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Nlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Novine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.ovine<- rbind(Ngroup.ovine.some, Ngroup.ovine.miss)

Ogroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Olivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Plivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Qlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Rlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Slivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.ovine.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Tlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.ovine.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tovine_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.ovine<- rbind(Tgroup.ovine.some, Tgroup.ovine.miss)

Ugroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Ulivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Vlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Wlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.ovine <- bind_rows(j02,j08,j21)


Zgroup.ovine <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Zlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Xgroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Xlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.ovine <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/08_Zeller_ovine/rsf_Ylivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')



ovine_night<- bind_rows(Agroup.ovine,Bgroup.ovine,Cgroup.ovine,
                      Dgroup.ovine,Egroup.ovine,
                      Fgroup.ovine,Ggroup.ovine,
                      Hgroup.ovine,Igroup.ovine,
                      Jgroup.ovine,Kgroup.ovine,Lgroup.ovine,
                      Mgroup.ovine,Ngroup.ovine,
                      Ogroup.ovine,Pgroup.ovine,Qgroup.ovine,
                      Rgroup.ovine,Sgroup.ovine,Zgroup.ovine,
                      Tgroup.ovine,Ugroup.ovine,Vgroup.ovine,
                      Wgroup.ovine,Xgroup.ovine,Ygroup.ovine)

ovine_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.ovine,Bgroup.ovine,Cgroup.ovine,Dgroup.ovine,
   Egroup.ovine,Fgroup.ovine,Ggroup.ovine,
   Hgroup.ovine,Igroup.ovine,Jgroup.ovine,Kgroup.ovine,Lgroup.ovine,
   Mgroup.ovine,Ngroup.ovine,Ogroup.ovine,Pgroup.ovine,Qgroup.ovine,
   Rgroup.ovine,Sgroup.ovine,Tgroup.ovine,Ugroup.ovine,Vgroup.ovine,
   Wgroup.ovine,Xgroup.ovine,Ygroup.ovine,Zgroup.ovine)

rm(Cgroup.ovine.miss,Cgroup.ovine.some,Egroup.ovine.miss,Egroup.ovine.some,
   Igroup.ovine.miss,Igroup.ovine.some,Lgroup.ovine.miss,Lgroup.ovine.some,
   Ngroup.ovine.miss,Ngroup.ovine.some,Tgroup.ovine.miss,Tgroup.ovine.some)

# ovine 527 scale ---------------------------------------------------------
ovine_night[is.na(ovine_night)] <- 0
anyNA(ovine_night$ovine)
anyNA(ovine_night$ovine527)
ovine_night[is.na(ovine_night)] <- 0


ovine.used <- data.frame(ovine_night$ovine)
ovine.used$case_ <- 1
ovine.used$id  <- ovine_night$id

variable.names(ovine.used)

ovine.used <- rename(ovine.used, ovine527= ovine_night.ovine)

variable.names(ovine.used)


avail.527 <- data.frame(ovine_night$ovine527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, ovine527= ovine_night.ovine527)
avail.527$id <- ovine_night$id

ovine.data <- rbind(ovine.used, avail.527)


# ovine 724 scale ---------------------------------------------------------

anyNA(ovine_night$ovine724)

ovine.724 <- data.frame(ovine_night$ovine)

ovine.724 <- rename(ovine.724, ovine724= ovine_night.ovine)

avail.724  <- data.frame(ovine_night$ovine724)

avail.724<- rename(avail.724, ovine724= ovine_night.ovine724)

ovine.724 <- rbind(ovine.724, avail.724)

# ovine 1242 scale --------------------------------------------------------
anyNA(ovine_night$ovine1242)

ovine.1242 <- data.frame(ovine_night$ovine)

ovine.1242 <- rename(ovine.1242, ovine1242= ovine_night.ovine)

avail.1242  <- data.frame(ovine_night$ovine1242)

avail.1242<- rename(avail.1242, ovine1242= ovine_night.ovine1242)

ovine.1242 <- rbind(ovine.1242, avail.1242)

# ovine 1545 scale --------------------------------------------------------
anyNA(ovine_night$ovine1545)

ovine.1545 <- data.frame(ovine_night$ovine)

ovine.1545 <- rename(ovine.1545, ovine1545= ovine_night.ovine)

avail.1545 <- data.frame(ovine_night$ovine1545)

avail.1545 <- rename(avail.1545, ovine1545= ovine_night.ovine1545)

ovine.1545<- rbind(ovine.1545, avail.1545)

# ovine 2105 scale --------------------------------------------------------
anyNA(ovine_night$ovine2105)

ovine.2105 <- data.frame(ovine_night$ovine)

ovine.2105 <- rename(ovine.2105, ovine2105= ovine_night.ovine)

avail.2105  <- data.frame(ovine_night$ovine2105)

avail.2105<- rename(avail.2105, ovine2105= ovine_night.ovine2105)

ovine.2105 <- rbind(ovine.2105, avail.2105)


# ovine 3001 scale --------------------------------------------------------
anyNA(ovine_night$ovine3001)

ovine.3001 <- data.frame(ovine_night$ovine)

ovine.3001 <- rename(ovine.3001, ovine3001= ovine_night.ovine)

avail.3001  <- data.frame(ovine_night$ovine3001)

avail.3001<- rename(avail.3001, ovine3001= ovine_night.ovine3001)

ovine.3001 <- rbind(ovine.3001, avail.3001)

# ovine 5886 scale --------------------------------------------------------
anyNA(ovine_night$ovine5886)

ovine.5886 <- data.frame(ovine_night$ovine)

ovine.5886 <- rename(ovine.5886, ovine5886= ovine_night.ovine)

avail.5886  <- data.frame(ovine_night$ovine5886)

avail.5886<- rename(avail.5886, ovine5886= ovine_night.ovine5886)

ovine.5886 <- rbind(ovine.5886, avail.5886)

# pig night -----------------------------------------------------------

Agroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Alivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Blivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Clivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Cpig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.pig<- rbind(Cgroup.pig.some, Cgroup.pig.miss)

Dgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Dlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Elivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Epig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.pig<- rbind(Egroup.pig.some, Egroup.pig.miss)


Fgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Flivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Glivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Hlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Ilivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ipig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.pig<- rbind(Igroup.pig.some, Igroup.pig.miss)

Jgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Jlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Klivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Llivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lpig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.pig<- rbind(Lgroup.pig.some, Lgroup.pig.miss)

Mgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Mlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Nlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Npig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.pig<- rbind(Ngroup.pig.some, Ngroup.pig.miss)

Ogroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Olivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Plivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Qlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Rlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Slivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.pig.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Tlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.pig.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tpig_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.pig<- rbind(Tgroup.pig.some, Tgroup.pig.miss)

Ugroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Ulivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Vlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Wlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.pig <- bind_rows(j02,j08,j21)


Zgroup.pig <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Zlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Xgroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Xlivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.pig <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/07_Zeller_pig/rsf_Ylivest_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

pig_night<- bind_rows(Agroup.pig,Bgroup.pig,Cgroup.pig,
                    Dgroup.pig,Egroup.pig,
                    Fgroup.pig,Ggroup.pig,
                    Hgroup.pig,Igroup.pig,
                    Jgroup.pig,Kgroup.pig,Lgroup.pig,
                    Mgroup.pig,Ngroup.pig,
                    Ogroup.pig,Pgroup.pig,Qgroup.pig,
                    Rgroup.pig,Sgroup.pig,Zgroup.pig,
                    Tgroup.pig,Ugroup.pig,Vgroup.pig,
                    Wgroup.pig,Xgroup.pig,Ygroup.pig)

pig_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.pig,Bgroup.pig,Cgroup.pig,Dgroup.pig,Egroup.pig,
   Fgroup.pig,Ggroup.pig,Zgroup.pig,
   Hgroup.pig,Igroup.pig,Jgroup.pig,Kgroup.pig,Lgroup.pig,
   Mgroup.pig,Ngroup.pig,Ogroup.pig,Pgroup.pig,Qgroup.pig,
   Rgroup.pig,Sgroup.pig,Tgroup.pig,Ugroup.pig,Vgroup.pig,
   Wgroup.pig,Xgroup.pig,Ygroup.pig)

rm(Cgroup.pig.miss,Cgroup.pig.some,Egroup.pig.miss,Egroup.pig.some,
   Igroup.pig.miss,Igroup.pig.some,Lgroup.pig.miss,Lgroup.pig.some,
   Ngroup.pig.miss,Ngroup.pig.some,Tgroup.pig.miss,Tgroup.pig.some)

# pig 527 scale ---------------------------------------------------------


anyNA(pig_night$pig)
anyNA(pig_night$pig527)
pig_night[is.na(pig_night)] <- 0


pig.used <- data.frame(pig_night$pig)
pig.used$case_ <- 1
pig.used$id  <- pig_night$id

variable.names(pig.used)

pig.used <- rename(pig.used, pig527= pig_night.pig)

variable.names(pig.used)


avail.527 <- data.frame(pig_night$pig527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, pig527= pig_night.pig527)
avail.527$id <- pig_night$id

pig.data <- rbind(pig.used, avail.527)


# pig 724 scale ---------------------------------------------------------

anyNA(pig_night$pig724)

pig.724 <- data.frame(pig_night$pig)

pig.724 <- rename(pig.724, pig724= pig_night.pig)

avail.724  <- data.frame(pig_night$pig724)

avail.724<- rename(avail.724, pig724= pig_night.pig724)

pig.724 <- rbind(pig.724, avail.724)

# pig 1242 scale --------------------------------------------------------
anyNA(pig_night$pig1242)

pig.1242 <- data.frame(pig_night$pig)

pig.1242 <- rename(pig.1242, pig1242= pig_night.pig)

avail.1242  <- data.frame(pig_night$pig1242)

avail.1242<- rename(avail.1242, pig1242= pig_night.pig1242)

pig.1242 <- rbind(pig.1242, avail.1242)

# pig 1545 scale --------------------------------------------------------
anyNA(pig_night$pig1545)

pig.1545 <- data.frame(pig_night$pig)

pig.1545 <- rename(pig.1545, pig1545= pig_night.pig)

avail.1545 <- data.frame(pig_night$pig1545)

avail.1545 <- rename(avail.1545, pig1545= pig_night.pig1545)

pig.1545<- rbind(pig.1545, avail.1545)

# pig 2105 scale --------------------------------------------------------
anyNA(pig_night$pig2105)

pig.2105 <- data.frame(pig_night$pig)

pig.2105 <- rename(pig.2105, pig2105= pig_night.pig)

avail.2105  <- data.frame(pig_night$pig2105)

avail.2105<- rename(avail.2105, pig2105= pig_night.pig2105)

pig.2105 <- rbind(pig.2105, avail.2105)


# pig 3001 scale --------------------------------------------------------
anyNA(pig_night$pig3001)

pig.3001 <- data.frame(pig_night$pig)

pig.3001 <- rename(pig.3001, pig3001= pig_night.pig)

avail.3001  <- data.frame(pig_night$pig3001)

avail.3001<- rename(avail.3001, pig3001= pig_night.pig3001)

pig.3001 <- rbind(pig.3001, avail.3001)

# pig 5886 scale --------------------------------------------------------
anyNA(pig_night$pig5886)

pig.5886 <- data.frame(pig_night$pig)

pig.5886 <- rename(pig.5886, pig5886= pig_night.pig)

avail.5886  <- data.frame(pig_night$pig5886)

avail.5886<- rename(avail.5886, pig5886= pig_night.pig5886)

pig.5886 <- rbind(pig.5886, avail.5886)


# popdens night -----------------------------------------------------------

Agroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Apopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Bpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Cpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Cpopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.popdens<- rbind(Cgroup.popdens.some, Cgroup.popdens.miss)


Dgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Dpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Epopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Epopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Epopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.popdens<- rbind(Egroup.popdens.some, Egroup.popdens.miss)

Fgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Fpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Gpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Hpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Ipopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ipopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.popdens<- rbind(Igroup.popdens.some, Igroup.popdens.miss)

Jgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Jpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Kpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Lpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lpopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.popdens<- rbind(Lgroup.popdens.some, Lgroup.popdens.miss)


Mgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Mpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Npopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Npopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.popdens<- rbind(Ngroup.popdens.some, Ngroup.popdens.miss)


Ogroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Opopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Ppopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Qpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Rpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Spopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.popdens.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Tpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.popdens.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tpopdens_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.popdens<- rbind(Tgroup.popdens.some, Tgroup.popdens.miss)


Ugroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Upopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Vpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Wpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.popdens <- bind_rows(j02,j08,j21)


Zgroup.popdens <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Zpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Xgroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Xpopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.popdens <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/06_Zeller_popdens/rsf_Ypopdens_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

popdens_night<- bind_rows(Agroup.popdens,Bgroup.popdens,Cgroup.popdens,
                        Dgroup.popdens,Egroup.popdens,
                        Fgroup.popdens,Ggroup.popdens,
                        Hgroup.popdens,Igroup.popdens,
                        Jgroup.popdens,Kgroup.popdens,Lgroup.popdens,
                        Mgroup.popdens,Ngroup.popdens,
                        Ogroup.popdens,Pgroup.popdens,Qgroup.popdens,
                        Rgroup.popdens,Sgroup.popdens,Zgroup.popdens,
                        Tgroup.popdens,Ugroup.popdens,Vgroup.popdens,
                        Wgroup.popdens,Xgroup.popdens,Ygroup.popdens)

popdens_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.popdens, Bgroup.popdens,Cgroup.popdens,Dgroup.popdens,
   Egroup.popdens,Fgroup.popdens,Ggroup.popdens,
   Hgroup.popdens,Igroup.popdens,Jgroup.popdens,Kgroup.popdens,Lgroup.popdens,
   Mgroup.popdens,Ngroup.popdens,Ogroup.popdens,Pgroup.popdens,Qgroup.popdens,
   Rgroup.popdens,Sgroup.popdens,Tgroup.popdens,Ugroup.popdens,Vgroup.popdens,
   Wgroup.popdens,Xgroup.popdens,Ygroup.popdens)

rm(Cgroup.popdens.miss,Cgroup.popdens.some,Egroup.popdens.miss,Egroup.popdens.some,
   Igroup.popdens.miss,Igroup.popdens.some,Lgroup.popdens.miss,Lgroup.popdens.some,
   Ngroup.popdens.miss,Ngroup.popdens.some,Tgroup.popdens.miss,Tgroup.popdens.some)

# popdens 527 scale ---------------------------------------------------------

anyNA(popdens_night$popdens)
anyNA(popdens_night$popdens527)
popdens_night[is.na(popdens_night)] <- 0


popdens.used <- data.frame(popdens_night$popdens)
popdens.used$case_ <- 1
popdens.used$id  <- popdens_night$id

variable.names(popdens.used)

popdens.used <- rename(popdens.used, popdens527= popdens_night.popdens)

variable.names(popdens.used)


avail.527 <- data.frame(popdens_night$popdens527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, popdens527= popdens_night.popdens527)
avail.527$id <- popdens_night$id

popdens.data <- rbind(popdens.used, avail.527)


# popdens 724 scale ---------------------------------------------------------

anyNA(popdens_night$popdens724)

popdens.724 <- data.frame(popdens_night$popdens)

popdens.724 <- rename(popdens.724, popdens724= popdens_night.popdens)

avail.724  <- data.frame(popdens_night$popdens724)

avail.724<- rename(avail.724, popdens724= popdens_night.popdens724)

popdens.724 <- rbind(popdens.724, avail.724)

# popdens 1242 scale --------------------------------------------------------
anyNA(popdens_night$popdens1242)

popdens.1242 <- data.frame(popdens_night$popdens)

popdens.1242 <- rename(popdens.1242, popdens1242= popdens_night.popdens)

avail.1242  <- data.frame(popdens_night$popdens1242)

avail.1242<- rename(avail.1242, popdens1242= popdens_night.popdens1242)

popdens.1242 <- rbind(popdens.1242, avail.1242)

# popdens 1545 scale --------------------------------------------------------
anyNA(popdens_night$popdens1545)

popdens.1545 <- data.frame(popdens_night$popdens)

popdens.1545 <- rename(popdens.1545, popdens1545= popdens_night.popdens)

avail.1545 <- data.frame(popdens_night$popdens1545)

avail.1545 <- rename(avail.1545, popdens1545= popdens_night.popdens1545)

popdens.1545<- rbind(popdens.1545, avail.1545)

# popdens 2105 scale --------------------------------------------------------
anyNA(popdens_night$popdens2105)

popdens.2105 <- data.frame(popdens_night$popdens)

popdens.2105 <- rename(popdens.2105, popdens2105= popdens_night.popdens)

avail.2105  <- data.frame(popdens_night$popdens2105)

avail.2105<- rename(avail.2105, popdens2105= popdens_night.popdens2105)

popdens.2105 <- rbind(popdens.2105, avail.2105)


# popdens 3001 scale --------------------------------------------------------
anyNA(popdens_night$popdens3001)

popdens.3001 <- data.frame(popdens_night$popdens)

popdens.3001 <- rename(popdens.3001, popdens3001= popdens_night.popdens)

avail.3001  <- data.frame(popdens_night$popdens3001)

avail.3001<- rename(avail.3001, popdens3001= popdens_night.popdens3001)

popdens.3001 <- rbind(popdens.3001, avail.3001)

# popdens 5886 scale --------------------------------------------------------
anyNA(popdens_night$popdens5886)

popdens.5886 <- data.frame(popdens_night$popdens)

popdens.5886 <- rename(popdens.5886, popdens5886= popdens_night.popdens)

avail.5886  <- data.frame(popdens_night$popdens5886)

avail.5886<- rename(avail.5886, popdens5886= popdens_night.popdens5886)

popdens.5886 <- rbind(popdens.5886, avail.5886)

# roadcover night -----------------------------------------------------------

Agroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Aroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Broad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Croad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Croad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.road <- rbind(Cgroup.road.some, Cgroup.road.miss)

Dgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Droad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Eroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Eroad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.road <- rbind(Egroup.road.some, Egroup.road.miss)

Fgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Froad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Groad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Hroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Iroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Iroad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.road <- rbind(Igroup.road.some, Igroup.road.miss)


Jgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Jroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Kroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Lroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lroad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.road <- rbind(Lgroup.road.some, Lgroup.road.miss)


Mgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Mroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Nroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Nroad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.road <- rbind(Ngroup.road.some, Ngroup.road.miss)


Ogroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Oroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Proad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Qroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Rroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Sroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.road.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Troad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.road.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Troad_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.road <- rbind(Tgroup.road.some, Tgroup.road.miss)

Ugroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Uroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Vroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Wroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.road <- bind_rows(j02,j08,j21)


Zgroup.road <-  Wgroup[Wgroup$id=="78",]

Xgroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Xroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.road <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/02_Zeller_road/rsf_Yroad_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

road_night<- bind_rows(Agroup.road,Bgroup.road,Cgroup.road,
                     Dgroup.road,Egroup.road,Fgroup.road,Ggroup.road,
                     Hgroup.road,Igroup.road,Jgroup.road,
                     Kgroup.road,Lgroup.road,
                     Mgroup.road,Ngroup.road,Ogroup.road,
                     Pgroup.road,Qgroup.road,
                     Rgroup.road,Sgroup.road,Tgroup.road,
                     Ugroup.road,Vgroup.road,Zgroup.road,
                     Wgroup.road,Xgroup.road,Ygroup.road)

road_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.road,Bgroup.road,Cgroup.road,Dgroup.road,
   Egroup.road,Fgroup.road,Ggroup.road,
   Hgroup.road,Igroup.road,Jgroup.road,Kgroup.road,Lgroup.road,
   Mgroup.road,Ngroup.road,Ogroup.road,Pgroup.road,Qgroup.road,
   Rgroup.road,Sgroup.road,Tgroup.road,Ugroup.road,Vgroup.road,
   Wgroup.road,Xgroup.road,Ygroup.road)

rm(Cgroup.road.some,Cgroup.road.miss,Egroup.road.miss,Egroup.road.some,
   Igroup.road.miss,Igroup.road.some,Lgroup.road.some,Lgroup.road.miss,
   Ngroup.road.miss,Ngroup.road.some,Tgroup.road.miss,Tgroup.road.some)

# field 527 scale ---------------------------------------------------------
road_night[is.na(road_night)] <- 0
anyNA(road_night$field)
anyNA(road_night$field527)
road_night[is.na(road_night)] <- 0


field.used <- data.frame(road_night$field)
field.used$case_ <- 1
field.used$id  <- road_night$id

variable.names(field.used)

field.used <- rename(field.used, field527= road_night.field)

variable.names(field.used)


avail.527 <- data.frame(road_night$field527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, field527= road_night.field527)
avail.527$id <- road_night$id

field.data <- rbind(field.used, avail.527)


# field 724 scale ---------------------------------------------------------

anyNA(road_night$field724)

field.724 <- data.frame(road_night$field)

field.724 <- rename(field.724, field724= road_night.field)

avail.724  <- data.frame(road_night$field724)

avail.724<- rename(avail.724, field724= road_night.field724)

field.724 <- rbind(field.724, avail.724)

# field 1242 scale --------------------------------------------------------
anyNA(road_night$field1242)

field.1242 <- data.frame(road_night$field)

field.1242 <- rename(field.1242, field1242= road_night.field)

avail.1242  <- data.frame(road_night$field1242)

avail.1242<- rename(avail.1242, field1242= road_night.field1242)

field.1242 <- rbind(field.1242, avail.1242)

# field 1545 scale --------------------------------------------------------
anyNA(road_night$field1545)

field.1545 <- data.frame(road_night$field)

field.1545 <- rename(field.1545, field1545= road_night.field)

avail.1545 <- data.frame(road_night$field1545)

avail.1545 <- rename(avail.1545, field1545= road_night.field1545)

field.1545<- rbind(field.1545, avail.1545)

# field 2105 scale --------------------------------------------------------
anyNA(road_night$field2105)

field.2105 <- data.frame(road_night$field)

field.2105 <- rename(field.2105, field2105= road_night.field)

avail.2105  <- data.frame(road_night$field2105)

avail.2105<- rename(avail.2105, field2105= road_night.field2105)

field.2105 <- rbind(field.2105, avail.2105)


# field 3001 scale --------------------------------------------------------
anyNA(road_night$field3001)

field.3001 <- data.frame(road_night$field)

field.3001 <- rename(field.3001, field3001= road_night.field)

avail.3001  <- data.frame(road_night$field3001)

avail.3001<- rename(avail.3001, field3001= road_night.field3001)

field.3001 <- rbind(field.3001, avail.3001)

# field 5886 scale --------------------------------------------------------
anyNA(road_night$field5886)

field.5886 <- data.frame(road_night$field)

field.5886 <- rename(field.5886, field5886= road_night.field)

avail.5886  <- data.frame(road_night$field5886)

avail.5886<- rename(avail.5886, field5886= road_night.field5886)

field.5886 <- rbind(field.5886, avail.5886)

# princ 527 scale ---------------------------------------------------------

anyNA(road_night$princ)
anyNA(road_night$princ527)

princ.used <- data.frame(road_night$princ)
princ.used$case_ <- 1
princ.used$id  <- road_night$id

variable.names(princ.used)

princ.used <- rename(princ.used, princ527= road_night.princ)

variable.names(princ.used)


avail.527 <- data.frame(road_night$princ527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, princ527= road_night.princ527)
avail.527$id <- road_night$id

princ.data <- rbind(princ.used, avail.527)


# princ 724 scale ---------------------------------------------------------

anyNA(road_night$princ724)

princ.724 <- data.frame(road_night$princ)

princ.724 <- rename(princ.724, princ724= road_night.princ)

avail.724  <- data.frame(road_night$princ724)

avail.724<- rename(avail.724, princ724= road_night.princ724)

princ.724 <- rbind(princ.724, avail.724)

# princ 1242 scale --------------------------------------------------------
anyNA(road_night$princ1242)

princ.1242 <- data.frame(road_night$princ)

princ.1242 <- rename(princ.1242, princ1242= road_night.princ)

avail.1242  <- data.frame(road_night$princ1242)

avail.1242<- rename(avail.1242, princ1242= road_night.princ1242)

princ.1242 <- rbind(princ.1242, avail.1242)

# princ 1545 scale --------------------------------------------------------
anyNA(road_night$princ1545)

princ.1545 <- data.frame(road_night$princ)

princ.1545 <- rename(princ.1545, princ1545= road_night.princ)

avail.1545 <- data.frame(road_night$princ1545)

avail.1545 <- rename(avail.1545, princ1545= road_night.princ1545)

princ.1545<- rbind(princ.1545, avail.1545)

# princ 2105 scale --------------------------------------------------------
anyNA(road_night$princ2105)

princ.2105 <- data.frame(road_night$princ)

princ.2105 <- rename(princ.2105, princ2105= road_night.princ)

avail.2105  <- data.frame(road_night$princ2105)

avail.2105<- rename(avail.2105, princ2105= road_night.princ2105)

princ.2105 <- rbind(princ.2105, avail.2105)


# princ 3001 scale --------------------------------------------------------
anyNA(road_night$princ3001)

princ.3001 <- data.frame(road_night$princ)

princ.3001 <- rename(princ.3001, princ3001= road_night.princ)

avail.3001  <- data.frame(road_night$princ3001)

avail.3001<- rename(avail.3001, princ3001= road_night.princ3001)

princ.3001 <- rbind(princ.3001, avail.3001)

# princ 5886 scale --------------------------------------------------------
anyNA(road_night$princ5886)

princ.5886 <- data.frame(road_night$princ)

princ.5886 <- rename(princ.5886, princ5886= road_night.princ)

avail.5886  <- data.frame(road_night$princ5886)

avail.5886<- rename(avail.5886, princ5886= road_night.princ5886)

princ.5886 <- rbind(princ.5886, avail.5886)

# water night -----------------------------------------------------------

Agroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Awater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Bwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Cwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Cwater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.water<- rbind(Cgroup.water.some, Cgroup.water.miss)

Dgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Dwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Ewater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ewater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.water<- rbind(Egroup.water.some, Egroup.water.miss)


Fgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Fwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Gwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Hwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Iwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Iwater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.water<- rbind(Igroup.water.some, Igroup.water.miss)


Jgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Jwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Kwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Lwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Lwater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.water<- rbind(Lgroup.water.some, Lgroup.water.miss)


Mgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Mwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Nwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Nwater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.water<- rbind(Ngroup.water.some, Ngroup.water.miss)


Ogroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Owater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Pwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Qwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Rwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Swater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.water.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Twater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.water.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Twater_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.water<- rbind(Tgroup.water.some, Tgroup.water.miss)


Ugroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Uwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Vwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Wwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

j02 <- Wgroup[Wgroup$id=="2",]
j08 <- Wgroup[Wgroup$id=="8",]
j21 <- Wgroup[Wgroup$id=="21",]

Wgroup.water <- bind_rows(j02,j08,j21)


Zgroup.water <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Zwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Xgroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Xwater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.water <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/05_Zeller_water/rsf_Ywater_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

water_night<- bind_rows(Agroup.water,Bgroup.water,Cgroup.water,
                      Dgroup.water,Egroup.water,
                      Fgroup.water,Ggroup.water,
                      Hgroup.water,Igroup.water,Zgroup.water,
                      Jgroup.water,Kgroup.water,Lgroup.water,
                      Mgroup.water,Ngroup.water,
                      Ogroup.water,Pgroup.water,Qgroup.water,
                      Rgroup.water,Sgroup.water,
                      Tgroup.water,Ugroup.water,Vgroup.water,
                      Wgroup.water,Xgroup.water,Ygroup.water)

water_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.water, Bgroup.water,Cgroup.water,Dgroup.water,
   Egroup.water,Fgroup.water,Ggroup.water,Zgroup.water,
   Hgroup.water,Igroup.water,Jgroup.water,Kgroup.water,Lgroup.water,
   Mgroup.water,Ngroup.water,Ogroup.water,Pgroup.water,Qgroup.water,
   Rgroup.water,Sgroup.water,Tgroup.water,Ugroup.water,Vgroup.water,
   Wgroup.water,Xgroup.water,Ygroup.water)

rm(Cgroup.water.miss,Cgroup.water.some,Egroup.water.miss,Egroup.water.some,
   Igroup.water.miss,Igroup.water.some,Lgroup.water.miss,Lgroup.water.some,
   Ngroup.water.miss,Ngroup.water.some,Tgroup.water.miss,Tgroup.water.some)

# water 527 scale ---------------------------------------------------------
water_night[is.na(water_night)] <- 0
anyNA(water_night$water)
anyNA(water_night$water527)



water.used <- data.frame(water_night$water)
water.used$case_ <- 1
water.used$id  <- water_night$id

variable.names(water.used)

water.used <- rename(water.used, water527= water_night.water)

variable.names(water.used)


avail.527 <- data.frame(water_night$water527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, water527= water_night.water527)
avail.527$id <- water_night$id

water.data <- rbind(water.used, avail.527)


# water 724 scale ---------------------------------------------------------

anyNA(water_night$water724)

water.724 <- data.frame(water_night$water)

water.724 <- rename(water.724, water724= water_night.water)

avail.724  <- data.frame(water_night$water724)

avail.724<- rename(avail.724, water724= water_night.water724)

water.724 <- rbind(water.724, avail.724)

# water 1242 scale --------------------------------------------------------
anyNA(water_night$water1242)

water.1242 <- data.frame(water_night$water)

water.1242 <- rename(water.1242, water1242= water_night.water)

avail.1242  <- data.frame(water_night$water1242)

avail.1242<- rename(avail.1242, water1242= water_night.water1242)

water.1242 <- rbind(water.1242, avail.1242)

# water 1545 scale --------------------------------------------------------
anyNA(water_night$water1545)

water.1545 <- data.frame(water_night$water)

water.1545 <- rename(water.1545, water1545= water_night.water)

avail.1545 <- data.frame(water_night$water1545)

avail.1545 <- rename(avail.1545, water1545= water_night.water1545)

water.1545<- rbind(water.1545, avail.1545)

# water 2105 scale --------------------------------------------------------
anyNA(water_night$water2105)

water.2105 <- data.frame(water_night$water)

water.2105 <- rename(water.2105, water2105= water_night.water)

avail.2105  <- data.frame(water_night$water2105)

avail.2105<- rename(avail.2105, water2105= water_night.water2105)

water.2105 <- rbind(water.2105, avail.2105)


# water 3001 scale --------------------------------------------------------
anyNA(water_night$water3001)

water.3001 <- data.frame(water_night$water)

water.3001 <- rename(water.3001, water3001= water_night.water)

avail.3001  <- data.frame(water_night$water3001)

avail.3001<- rename(avail.3001, water3001= water_night.water3001)

water.3001 <- rbind(water.3001, avail.3001)

# water 5886 scale --------------------------------------------------------
anyNA(water_night$water5886)

water.5886 <- data.frame(water_night$water)

water.5886 <- rename(water.5886, water5886= water_night.water)

avail.5886  <- data.frame(water_night$water5886)

avail.5886<- rename(avail.5886, water5886= water_night.water5886)

water.5886 <- rbind(water.5886, avail.5886)

# light night -----------------------------------------------------------

Agroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Alight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Blight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Clight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Clight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Cgroup.light <- rbind(Cgroup.light.some, Cgroup.light.miss)

Dgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Dlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Elight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Elight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Egroup.light<- rbind(Egroup.light.some, Egroup.light.miss)

Fgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Flight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Glight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Hlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Ilight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Ilight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Igroup.light<- rbind(Igroup.light.some, Igroup.light.miss)

Jgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Jlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Klight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Llight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Llight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Lgroup.light<- rbind(Lgroup.light.some, Lgroup.light.miss)


Mgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Mlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Nlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Nlight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Ngroup.light<- rbind(Ngroup.light.some, Ngroup.light.miss)

Ogroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Olight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Plight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Qlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Rlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Slight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.light.some <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Tlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.light.miss <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/09_Zeller_missing/rsf_Tlight_night_missing.csv', header = T, sep = ',', dec = '.', comment.char = '')
Tgroup.light<- rbind(Tgroup.light.some, Tgroup.light.miss)

Ugroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Ulight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Vlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Wlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Xgroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Xlight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.light <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/04_Zeller_light/rsf_Ylight_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

light_night<- bind_rows(Agroup.light,Bgroup.light,Cgroup.light,
                      Dgroup.light,Egroup.light,
                      Fgroup.light,Ggroup.light,
                      Hgroup.light,Igroup.light,
                      Jgroup.light,Kgroup.light,Lgroup.light,
                      Mgroup.light,Ngroup.light,
                      Ogroup.light,Pgroup.light,Qgroup.light,
                      Rgroup.light,Sgroup.light,
                      Tgroup.light,Ugroup.light,Vgroup.light,
                      Wgroup.light,Xgroup.light,Ygroup.light)

light_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.light, Bgroup.light, Cgroup.light,
   Dgroup.light,Egroup.light,Fgroup.light,Ggroup.light,
   Hgroup.light,Igroup.light,Jgroup.light,Kgroup.light,Lgroup.light,
   Mgroup.light,Ngroup.light,Ogroup.light,Pgroup.light,Qgroup.light,
   Rgroup.light,Sgroup.light,Tgroup.light,Ugroup.light,Vgroup.light,
   Wgroup.light,Xgroup.light,Ygroup.light)

rm(Cgroup.light.miss,Cgroup.light.some, Egroup.light.miss, Egroup.light.some,
   Igroup.light.some,Igroup.light.miss,Lgroup.light.miss,Lgroup.light.some,
   Ngroup.light.miss,Ngroup.light.some,Tgroup.light.miss,Tgroup.light.some)

# light 527 scale ---------------------------------------------------------

anyNA(light_night$light)
anyNA(light_night$light527)
light_night[is.na(light_night)] <- 0


light.used <- data.frame(light_night$light)
light.used$case_ <- 1
light.used$id  <- light_night$id

variable.names(light.used)

light.used <- rename(light.used, light527= light_night.light)

variable.names(light.used)


avail.527 <- data.frame(light_night$light527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, light527= light_night.light527)
avail.527$id <- light_night$id

light.data <- rbind(light.used, avail.527)


# light 724 scale ---------------------------------------------------------

anyNA(light_night$light724)

light.724 <- data.frame(light_night$light)

light.724 <- rename(light.724, light724= light_night.light)

avail.724  <- data.frame(light_night$light724)

avail.724<- rename(avail.724, light724= light_night.light724)

light.724 <- rbind(light.724, avail.724)

# light 1242 scale --------------------------------------------------------
anyNA(light_night$light1242)

light.1242 <- data.frame(light_night$light)

light.1242 <- rename(light.1242, light1242= light_night.light)

avail.1242  <- data.frame(light_night$light1242)

avail.1242<- rename(avail.1242, light1242= light_night.light1242)

light.1242 <- rbind(light.1242, avail.1242)

# light 1545 scale --------------------------------------------------------
anyNA(light_night$light1545)

light.1545 <- data.frame(light_night$light)

light.1545 <- rename(light.1545, light1545= light_night.light)

avail.1545 <- data.frame(light_night$light1545)

avail.1545 <- rename(avail.1545, light1545= light_night.light1545)

light.1545<- rbind(light.1545, avail.1545)

# light 2105 scale --------------------------------------------------------
anyNA(light_night$light2105)

light.2105 <- data.frame(light_night$light)

light.2105 <- rename(light.2105, light2105= light_night.light)

avail.2105  <- data.frame(light_night$light2105)

avail.2105<- rename(avail.2105, light2105= light_night.light2105)

light.2105 <- rbind(light.2105, avail.2105)


# light 3001 scale --------------------------------------------------------
anyNA(light_night$light3001)

light.3001 <- data.frame(light_night$light)

light.3001 <- rename(light.3001, light3001= light_night.light)

avail.3001  <- data.frame(light_night$light3001)

avail.3001<- rename(avail.3001, light3001= light_night.light3001)

light.3001 <- rbind(light.3001, avail.3001)

# light 5886 scale --------------------------------------------------------
anyNA(light_night$light5886)

light.5886 <- data.frame(light_night$light)

light.5886 <- rename(light.5886, light5886= light_night.light)

avail.5886  <- data.frame(light_night$light5886)

avail.5886<- rename(avail.5886, light5886= light_night.light5886)

light.5886 <- rbind(light.5886, avail.5886)

# urban night -----------------------------------------------------------

Agroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Aurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Bgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Burban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Cgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Curban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Dgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Durban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Egroup.urban<- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Eurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Fgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Furban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ggroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Gurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Hgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Hurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Igroup.urban<- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Iurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Jgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Jurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Kgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Kurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Lgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Lurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Mgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Murban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ngroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Nurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ogroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Ourban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Pgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Purban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Qgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Qurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Rgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Rurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Sgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Surban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Tgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Turban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ugroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Uurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Vgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Vurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Wgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Wurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Zgroup.urban <-  read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Zurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')


Xgroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Xurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

Ygroup.urban <- read.csv('D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/01_RSF_data/10_Urban-rural/rsf_Yurban_night.csv', header = T, sep = ',', dec = '.', comment.char = '')

urban_night<- bind_rows(Agroup.urban,Bgroup.urban,Cgroup.urban,
                      Dgroup.urban,Egroup.urban,
                      Fgroup.urban,Ggroup.urban,
                      Hgroup.urban,Igroup.urban,Zgroup.urban,
                      Jgroup.urban,Kgroup.urban,Lgroup.urban,
                      Mgroup.urban,Ngroup.urban,
                      Ogroup.urban,Pgroup.urban,Qgroup.urban,
                      Rgroup.urban,Sgroup.urban,
                      Tgroup.urban,Ugroup.urban,Vgroup.urban,
                      Wgroup.urban,Xgroup.urban,Ygroup.urban)

urban_night$tod_ <- "night"

### delete all innecessary datafram
rm(Agroup.urban, Bgroup.urban,Cgroup.urban,Dgroup.urban,
   Egroup.urban,Fgroup.urban,Ggroup.urban,Zgroup.urban,
   Hgroup.urban,Igroup.urban,Jgroup.urban,Kgroup.urban,Lgroup.urban,
   Mgroup.urban,Ngroup.urban,Ogroup.urban,Pgroup.urban,Qgroup.urban,
   Rgroup.urban,Sgroup.urban,Tgroup.urban,Ugroup.urban,Vgroup.urban,
   Wgroup.urban,Xgroup.urban,Ygroup.urban)

rm(Cgroup.urban.miss,Cgroup.urban.some,Egroup.urban.miss,Egroup.urban.some,
   Igroup.urban.miss,Igroup.urban.some,Lgroup.urban.miss,Lgroup.urban.some,
   Ngroup.urban.miss,Ngroup.urban.some,Tgroup.urban.miss,Tgroup.urban.some)

# urban 527 scale ---------------------------------------------------------
urban_night[is.na(urban_night)] <- 0
anyNA(urban_night$urban)
anyNA(urban_night$urban527)



urban.used <- data.frame(urban_night$urban)
urban.used$case_ <- 1
urban.used$id  <- urban_night$id

variable.names(urban.used)

urban.used <- rename(urban.used, urban527= urban_night.urban)

variable.names(urban.used)


avail.527 <- data.frame(urban_night$urban527)
avail.527$case_ <- 0

variable.names(avail.527)
avail.527 <- rename(avail.527, urban527= urban_night.urban527)
avail.527$id <- urban_night$id

urban.data <- rbind(urban.used, avail.527)


# urban 724 scale ---------------------------------------------------------

anyNA(urban_night$urban724)

urban.724 <- data.frame(urban_night$urban)

urban.724 <- rename(urban.724, urban724= urban_night.urban)

avail.724  <- data.frame(urban_night$urban724)

avail.724<- rename(avail.724, urban724= urban_night.urban724)

urban.724 <- rbind(urban.724, avail.724)

# urban 1242 scale --------------------------------------------------------
anyNA(urban_night$urban1242)

urban.1242 <- data.frame(urban_night$urban)

urban.1242 <- rename(urban.1242, urban1242= urban_night.urban)

avail.1242  <- data.frame(urban_night$urban1242)

avail.1242<- rename(avail.1242, urban1242= urban_night.urban1242)

urban.1242 <- rbind(urban.1242, avail.1242)

# urban 1545 scale --------------------------------------------------------
anyNA(urban_night$urban1545)

urban.1545 <- data.frame(urban_night$urban)

urban.1545 <- rename(urban.1545, urban1545= urban_night.urban)

avail.1545 <- data.frame(urban_night$urban1545)

avail.1545 <- rename(avail.1545, urban1545= urban_night.urban1545)

urban.1545<- rbind(urban.1545, avail.1545)

# urban 2105 scale --------------------------------------------------------
anyNA(urban_night$urban2105)

urban.2105 <- data.frame(urban_night$urban)

urban.2105 <- rename(urban.2105, urban2105= urban_night.urban)

avail.2105  <- data.frame(urban_night$urban2105)

avail.2105<- rename(avail.2105, urban2105= urban_night.urban2105)

urban.2105 <- rbind(urban.2105, avail.2105)


# urban 3001 scale --------------------------------------------------------
anyNA(urban_night$urban3001)

urban.3001 <- data.frame(urban_night$urban)

urban.3001 <- rename(urban.3001, urban3001= urban_night.urban)

avail.3001  <- data.frame(urban_night$urban3001)

avail.3001<- rename(avail.3001, urban3001= urban_night.urban3001)

urban.3001 <- rbind(urban.3001, avail.3001)

# urban 5886 scale --------------------------------------------------------
anyNA(urban_night$urban5886)

urban.5886 <- data.frame(urban_night$urban)

urban.5886 <- rename(urban.5886, urban5886= urban_night.urban)

avail.5886  <- data.frame(urban_night$urban5886)

avail.5886<- rename(avail.5886, urban5886= urban_night.urban5886)

urban.5886 <- rbind(urban.5886, avail.5886)

# create the dataset - night---------------------------------------------------
## 527 mt scale
scale527 <- grass.data
scale527$crop527 <- crop.data$crop527
scale527$flood527 <- flood.data$flood527
scale527$herva527 <- herva.data$herva527
scale527$tree527 <- tree.data$tree527
scale527$livestock527 <- livestock.data$livestock527
scale527$ovine527 <- ovine.data$ovine527
scale527$pig527 <- pig.data$pig527
scale527$popdens527 <- popdens.data$popdens527
scale527$field527 <- field.data$field527
scale527$princ527 <- princ.data$princ527
scale527$water527 <- water.data$water527
scale527$light527 <- light.data$light527
scale527$urban527 <- urban.data$urban527
scale527$time_ <- 1

## 724 mt scale
scale724 <- grass.724
scale724$id <-  grass.data$id
scale724$sex <-  grass.data$sex
scale724$case_ <- grass.data$case_
scale724$group<- grass.data$group
scale724$crop724 <- crop.724$crop724
scale724$flood724 <- flood.724$flood724
scale724$herva724 <- herva.724$herva724
scale724$tree724 <- tree.724$tree724
scale724$livestock724 <- livestock.724$livestock724
scale724$ovine724 <- ovine.724$ovine724
scale724$pig724 <- pig.724$pig724
scale724$popdens724 <- popdens.724$popdens724
scale724$field724 <- field.724$field724
scale724$princ724 <- princ.724$princ724
scale724$water724 <- water.724$water724
scale724$light724 <- light.724$light724
scale724$urban724 <- urban.724$urban724
scale724$time_ <- 1

#1242 mt scale
scale1242 <- grass.1242
scale1242$id <-  grass.data$id
scale1242$sex <-  grass.data$sex
scale1242$case_ <- grass.data$case_
scale1242$group<- grass.data$group
scale1242$crop1242 <- crop.1242$crop1242
scale1242$flood1242 <- flood.1242$flood1242
scale1242$herva1242 <- herva.1242$herva1242
scale1242$tree1242 <- tree.1242$tree1242
scale1242$livestock1242 <- livestock.1242$livestock1242
scale1242$ovine1242 <- ovine.1242$ovine1242
scale1242$pig1242 <- pig.1242$pig1242
scale1242$popdens1242 <- popdens.1242$popdens1242
scale1242$field1242 <- field.1242$field1242
scale1242$princ1242 <- princ.1242$princ1242
scale1242$water1242 <- water.1242$water1242
scale1242$light1242 <- light.1242$light1242
scale1242$urban1242 <- urban.1242$urban1242
scale1242$time_ <- 1

#1545 mt scale
scale1545 <- grass.1545
scale1545$id <-  grass.data$id
scale1545$sex <-  grass.data$sex
scale1545$case_ <- grass.data$case_
scale1545$group<- grass.data$group
scale1545$crop1545 <- crop.1545$crop1545
scale1545$flood1545 <- flood.1545$flood1545
scale1545$herva1545 <- herva.1545$herva1545
scale1545$tree1545 <- tree.1545$tree1545
scale1545$livestock1545 <- livestock.1545$livestock1545
scale1545$ovine1545 <- ovine.1545$ovine1545
scale1545$pig1545 <- pig.1545$pig1545
scale1545$popdens1545 <- popdens.1545$popdens1545
scale1545$field1545 <- field.1545$field1545
scale1545$princ1545 <- princ.1545$princ1545
scale1545$water1545 <- water.1545$water1545
scale1545$light1545 <- light.1545$light1545
scale1545$urban1545 <- urban.1545$urban1545
scale1545$time_ <- 1

#2105 mt scale
scale2105 <- grass.2105
scale2105$id <-  grass.data$id
scale2105$sex <-  grass.data$sex
scale2105$case_ <- grass.data$case_
scale2105$group<- grass.data$group
scale2105$crop2105 <- crop.2105$crop2105
scale2105$flood2105 <- flood.2105$flood2105
scale2105$herva2105 <- herva.2105$herva2105
scale2105$tree2105 <- tree.2105$tree2105
scale2105$livestock2105 <- livestock.2105$livestock2105
scale2105$ovine2105 <- ovine.2105$ovine2105
scale2105$pig2105 <- pig.2105$pig2105
scale2105$popdens2105 <- popdens.2105$popdens2105
scale2105$field2105 <- field.2105$field2105
scale2105$princ2105 <- princ.2105$princ2105
scale2105$water2105 <- water.2105$water2105
scale2105$light2105 <- light.2105$light2105
scale2105$urban2105 <- urban.2105$urban2105
scale2105$time_ <- 1

#3001 mt scale
scale3001 <- grass.3001
scale3001$id <-  grass.data$id
scale3001$sex <-  grass.data$sex
scale3001$case_ <- grass.data$case_
scale3001$group<- grass.data$group
scale3001$crop3001 <- crop.3001$crop3001
scale3001$flood3001 <- flood.3001$flood3001
scale3001$herva3001 <- herva.3001$herva3001
scale3001$tree3001 <- tree.3001$tree3001
scale3001$livestock3001 <- livestock.3001$livestock3001
scale3001$ovine3001 <- ovine.3001$ovine3001
scale3001$pig3001 <- pig.3001$pig3001
scale3001$popdens3001 <- popdens.3001$popdens3001
scale3001$field3001 <- field.3001$field3001
scale3001$princ3001 <- princ.3001$princ3001
scale3001$water3001 <- water.3001$water3001
scale3001$light3001 <- light.3001$light3001
scale3001$urban3001 <- urban.3001$urban3001
scale3001$time_ <- 1

#5886 mt scale
scale5886 <- grass.5886
scale5886$id <-  grass.data$id
scale5886$sex <-  grass.data$sex
scale5886$case_ <- grass.data$case_
scale5886$group<- grass.data$group
scale5886$crop5886 <- crop.5886$crop5886
scale5886$flood5886 <- flood.5886$flood5886
scale5886$herva5886 <- herva.5886$herva5886
scale5886$tree5886 <- tree.5886$tree5886
scale5886$livestock5886 <- livestock.5886$livestock5886
scale5886$ovine5886 <- ovine.5886$ovine5886
scale5886$pig5886 <- pig.5886$pig5886
scale5886$popdens5886 <- popdens.5886$popdens5886
scale5886$field5886 <- field.5886$field5886
scale5886$princ5886 <- princ.5886$princ5886
scale5886$water5886 <- water.5886$water5886
scale5886$light5886 <- light.5886$light5886
scale5886$urban5886 <- urban.5886$urban5886
scale5886$time_ <- 1

# MODELS - night ------------------------------------------------------------

mod1 <- coxme(Surv(time_, case_) ~  crop527 + 
                herva527 + tree527 +livestock527 + 
                ovine527 +  popdens527 + field527 + 
                princ527 +  water527 + urban527+  (1|id), data= scale527)

mod2 <- coxme(Surv(time_, case_) ~  crop724 + 
                herva724 + tree724 +  livestock724 + 
                ovine724 + popdens724 + field724 + 
                princ724 +  water724 + urban724+ (1|id), data= scale724)

mod3 <- coxme(Surv(time_, case_) ~ crop1242 + 
                herva1242 + tree1242 +  livestock1242 + 
                ovine1242 +  popdens1242 + field1242 + 
                princ1242 +  water1242 + urban1242+  (1|id), data= scale1242)

mod4 <- coxme(Surv(time_, case_) ~ crop1545 + 
                herva1545 + tree1545 + livestock1545 + 
                ovine1545 +  popdens1545 + field1545 + 
                princ1545 +  water1545 + urban1545+ (1|id), data= scale1545)

mod5 <- coxme(Surv(time_, case_) ~  crop2105 + 
                herva2105 + tree2105 +  livestock2105 + 
                ovine2105 +  popdens2105 + field2105 + 
                princ2105 +  water2105 + urban2105+ (1|id), data= scale2105)

mod6 <- coxme(Surv(time_, case_) ~  crop3001 + 
                herva3001 + tree3001 +  livestock3001 + 
                ovine3001 + popdens3001 + field3001 + 
                princ3001 + water3001 +urban3001+  (1|id), data= scale3001)

mod7 <- coxme(Surv(time_, case_) ~  crop5886 + 
                herva5886 + tree5886 + livestock5886 + 
                ovine5886 +  popdens5886 + field5886 + 
                princ5886 +  water5886 + urban5886+ (1|id), data= scale5886)


output.AIC.night <- model.sel(mod1,mod2,mod3,mod4,mod5,mod6,mod7)

output.AIC.night

#write.csv(output.AIC.night, "D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/02_Scale_selection/Scale_selection_2021_10_d03/AIC.night.csv")

#write.csv(scale5886, "D:/PhD_Jaguar/Jaguar01/15_rsf_ZellerApproach/02_Scale_selection/Scale_selection_2021_10_d03/scale5886.night.csv")


# MODELS - night - Male 2021-10-05 --------------------------------------------------
scale527.m <- scale527[scale527$sex== "Male",]
scale724.m <- scale724[scale724$sex== "Male",]
scale1242.m <- scale1242[scale1242$sex== "Male",]
scale1545.m <- scale1545[scale1545$sex== "Male",]
scale2105.m <- scale2105[scale2105$sex== "Male",]
scale3001.m <- scale3001[scale3001$sex== "Male",]
scale5886.m <- scale5886[scale5886$sex== "Male",]

mod1 <- coxme(Surv(time_, case_) ~  crop527 + herva527+
                tree527 +livestock527 + 
                ovine527 +  popdens527 + field527 + 
                princ527 +  water527 + urban527+  (1|id), data= scale527.m)

mod2 <- coxme(Surv(time_, case_) ~ crop724 + herva724 +
                tree724 +  livestock724 + 
                ovine724 +  popdens724 + field724 + 
                princ724 +  water724 + urban724+ (1|id), data= scale724.m)

mod3 <- coxme(Surv(time_, case_) ~ crop1242 + herva1242 +
                tree1242 +  livestock1242 + 
                ovine1242 +  popdens1242 + field1242 + 
                princ1242 +  water1242 + urban1242+ (1|id), data= scale1242.m)

mod4 <- coxme(Surv(time_, case_) ~ crop1545 + herva1545 +
                tree1545 + livestock1545 + 
                ovine1545 +  popdens1545 + field1545 + 
                princ1545 +  water1545 + urban1545+ (1|id), data= scale1545.m)

mod5 <- coxme(Surv(time_, case_) ~  crop2105 + herva2105 +
                tree2105 +  livestock2105 + 
                ovine2105 +  popdens2105 + field2105 + 
                princ2105 +  water2105 + urban2105+ (1|id), data= scale2105.m)

mod6 <- coxme(Surv(time_, case_) ~  crop3001 + herva3001 +
                tree3001 +  livestock3001 + 
                ovine3001 +  popdens3001 + field3001 + 
                princ3001 + water3001 +urban3001+ (1|id), data= scale3001.m)

mod7 <- coxme(Surv(time_, case_) ~  crop5886 + herva5886 +
                tree5886 + livestock5886 + 
                ovine5886 +  popdens5886 + field5886 + 
                princ5886 +  water5886 + urban5886+(1|id), data= scale5886.m)


output.AIC.night <- model.sel(mod1,mod2,mod3,mod4,mod5,mod6,mod7)

output.AIC.night

importance(output.AIC.night)

# MODELS - night - Female 2021-10-05 --------------------------------------------------
scale527.f <- scale527[scale527$sex== "Female",]
scale724.f <- scale724[scale724$sex== "Female",]
scale1242.f <- scale1242[scale1242$sex== "Female",]
scale1545.f <- scale1545[scale1545$sex== "Female",]
scale2105.f <- scale2105[scale2105$sex== "Female",]
scale3001.f <- scale3001[scale3001$sex== "Female",]
scale5886.f <- scale5886[scale5886$sex== "Female",]

mod1 <- coxme(Surv(time_, case_) ~  crop527 + herva527+
                tree527 +livestock527 + 
                ovine527 +  popdens527 + field527 + 
                princ527 +  water527 + urban527+  (1|id), data= scale527.f)

mod2 <- coxme(Surv(time_, case_) ~ crop724 + herva724 +
                tree724 +  livestock724 + 
                ovine724 +  popdens724 + field724 + 
                princ724 +  water724 + urban724+ (1|id), data= scale724.f)

mod3 <- coxme(Surv(time_, case_) ~ crop1242 + herva1242 +
                tree1242 +  livestock1242 + 
                ovine1242 +  popdens1242 + field1242 + 
                princ1242 +  water1242 + urban1242+ (1|id), data= scale1242.f)

mod4 <- coxme(Surv(time_, case_) ~ crop1545 + herva1545 +
                tree1545 + livestock1545 + 
                ovine1545 +  popdens1545 + field1545 + 
                princ1545 +  water1545 + urban1545+ (1|id), data= scale1545.f)

mod5 <- coxme(Surv(time_, case_) ~  crop2105 + herva2105 +
                tree2105 +  livestock2105 + 
                ovine2105 +  popdens2105 + field2105 + 
                princ2105 +  water2105 + urban2105+ (1|id), data= scale2105.f)

mod6 <- coxme(Surv(time_, case_) ~  crop3001 + herva3001 +
                tree3001 +  livestock3001 + 
                ovine3001 +  popdens3001 + field3001 + 
                princ3001 + water3001 +urban3001+ (1|id), data= scale3001.f)

mod7 <- coxme(Surv(time_, case_) ~  crop5886 + herva5886 +
                tree5886 + livestock5886 + 
                ovine5886 +  popdens5886 + field5886 + 
                princ5886 +  water5886 + urban5886+(1|id), data= scale5886.f)


output.AIC.night <- model.sel(mod1,mod2,mod3,mod4,mod5,mod6,mod7)

output.AIC.night

importance(output.AIC.night)

