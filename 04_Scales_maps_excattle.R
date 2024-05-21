####
####Vanesa Bejarano Alegre 21 fev 2021
### Scale script - Livestock: cattle density
### 
require(raster)
require(smoothie)

setwd("E:/Jaguar01_Maps_work_2020_HD/11_livestock_scales")

###Below codes by Kathy Zeller fraction extracted from: https://github.com/kazeller/PathSF-Data-Prep

#create directory for saving smoothed rasters
subDir<-"smoothed_layers_LIVESTOCK"
dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

#Create scale maps
scales<-c(527, 724, 1242, 1545, 2105, 3001, 5886)# define your scales (these are in meters)

### Mgroup is a group from Pantanal  with 11 individuals, I classify them like this for my comfort and organization

Mgroup <- raster("Mgroup_livestock.tif")

Mgroup.out.names <- c("Mgroup_livestock")
names(Mgroup)<-Mgroup.out.names


#Create scale maps

for (i in 1:nlayers(Mgroup)){ #loop through rasters in raster stack
  r<-Mgroup[[i]]
  cellsize<-res(Mgroup[[i]])[1]
  zmat <- as.matrix(r)
  for(j in 1:length(scales)){#loop through scales
    f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r), ny=ncol(r),
                        sigma=scales[j]/cellsize, cellsize=cellsize) # apply the smooth for a scale
    rast.smooth <- r
    values(rast.smooth) <- f
    writeRaster(rast.smooth,paste0("smoothed_layers_LIVESTOCK/",Mgroup.out.names[i],"_", scales[j], "_m.tif"),format="GTiff",overwrite=TRUE) #write it out
  }
}

### End
### REPEAT FOR ALL THE ENVIRONMENTAL 
