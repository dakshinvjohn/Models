#### Prey in all 3 places ####


#### Load libraries ####

library("raster")
library("rgdal")
library("MASS")
library("raster")
library("rgdal")
library("ncdf4")
library("dismo")
library("rgeos")
library("sp")
library("dplyr")
library("readr")
library("mosaic")

#### Load climatic variables ####

climate_roe <- stack(paste0("Desktop/germany/climate_germany/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))
plot(climate_roe[[1]])

#### Data file for roe all ####

roe_all_final <- read.csv("Desktop/prey_combined/prey_sdm/roe/roe_all.csv", sep = ";")
head(roe_all_final)

#### Envir variables for roe ####

all_ext <- readOGR("Desktop/prey_combined/prey_mask/", "prey_mask")
e <- extent(all_ext)
e
studyarea_all <- crop(climate_roe, e)
studyarea_all <- mask(studyarea_all, all_ext)
plot(studyarea_all[[1]])
names(studyarea_all)
names(studyarea_all)<-paste0("bio", 1:19)
writeRaster(studyarea_all, paste0("Desktop/prey_combined/prey_sdm/roe/climate/", names(studyarea_all)), bylayer = TRUE, format ="ascii", overwrite = TRUE)
bio1_all <- raster("Desktop/prey_combined/prey_sdm/roe/climate/bio1.asc")
plot(bio1_all)
res(bio1_all)
extent(bio1_all)
e

new_m <- readOGR("Desktop/prey_combined/prey_mask/new_mask.shp")
writeRaster(new_m, "Desktop/New_m.asc", format = "ascii")
landuse <- brick("Desktop/germany/land_use_2/states.nc", varname = "range")
plot(landuse[[1166]])
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/prey_combined/prey_sdm/red/landuse/states.nc", varname = i)
  landuse <- crop(landuse, e)
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, all_ext)
  landuse <- resample(landuse, studyarea_red)
  writeRaster(landuse, paste0("Desktop/prey_combined/prey_sdm/red/landuse/landuse_red/",i,".asc"), format = "ascii", overwrite=TRUE)
}


primf_roe <- raster ("Desktop/prey_combined/prey_sdm/roe/landuse/primf.asc")
plot(primf_roe)
secd_roe <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdf.asc")
plot(secd_roe)


#### The same for the boar ####

boar_all <- read.csv("Desktop/prey_combined/prey_sdm/boar/boar_all.csv", header = TRUE, sep = ";")

head(boar_all)
tail(boar_all)

studyarea_boar <- crop(climate_boar,e)
studyarea_boar <- mask(studyarea_boar, all_ext)
plot(studyarea_boar[[1]])
names(studyarea_boar)
names(studyarea_boar) <- paste0("bio", 1:19)
writeRaster(studyarea_boar, paste0("Desktop/prey_combined/prey_sdm/boar/climate/", names(studyarea_boar)), bylayer = TRUE, format ="ascii", overwrite = TRUE)
bio1_boar <- raster("Desktop/prey_combined/prey_sdm/boar/climate/bio1.asc")
plot(bio1_boar)
res(bio1_boar)
extent(bio1_boar)
e

landuse <- brick("Desktop/Germany/land_use_2/states.nc", varname = "range")
plot(landuse[[1166]])
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/Germany/land_use_2/states.nc", varname = i)
  landuse <- crop(landuse, e)
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, all_ext)
  landuse <- resample(landuse, studyarea_boar)
  writeRaster(landuse, paste0("Desktop/prey_combined/prey_sdm/boar/landuse/",i,".asc"), format = "ascii", overwrite=TRUE)
}

primf_boar <- raster ("Desktop/prey_combined/prey_sdm/boar/landuse/primf.asc")
plot(primf_boar)
secd_boar <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdf.asc")


#### Combining for roe deer #### 

forest1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/primf.asc")
forest2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdf.asc")
Forests <- mosaic(forest1, forest2, fun = sum)
writeRaster(Forests, "Desktop/prey_combined/prey_sdm/roe/landuse/Forests.asc", overwrite = TRUE)

nonf1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/primn.asc")
nonf2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdn.asc")
Non_forested_land <- mosaic(nonf1,nonf2, fun = sum)
writeRaster(Non_forested_land, "Desktop/prey_combined/prey_sdm/roe/landuse/Non_forested_land.asc" , overwrite =TRUE)

crop1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c4per.asc")
crop2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c4ann.asc")
crop3 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3per.asc")
crop4 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3nfx.asc")
crop5 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3ann.asc")
Crops <- mosaic(crop1,crop2,crop3,crop4,crop5, fun=sum)
writeRaster(Crops, "Desktop/prey_combined/prey_sdm/roe/landuse/Crops.asc", overwrite =TRUE)

#### Do the same for boar deer #### 

forest1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/primf.asc")
forest2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdf.asc")
Forests <- mosaic(forest1, forest2, fun = sum)
writeRaster(Forests, "Desktop/prey_combined/prey_sdm/boar/landuse/Forests.asc", overwrite = TRUE)

nonf1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/primn.asc")
nonf2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdn.asc")
Non_forested_land <- mosaic(nonf1,nonf2, fun = sum)
writeRaster(Non_forested_land, "Desktop/prey_combined/prey_sdm/boar/landuse/Non_forested_land.asc" , overwrite =TRUE)

crop1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c4per.asc")
crop2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c4ann.asc")
crop3 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3per.asc")
crop4 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3nfx.asc")
crop5 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3ann.asc")
Crops <- mosaic(crop1,crop2,crop3,crop4,crop5, fun=sum)
writeRaster(Crops, "Desktop/prey_combined/prey_sdm/boar/landuse/Crops.asc", overwrite =TRUE)

#### Other variables for Roe ####

bio1_all <- raster("Desktop/prey_combined/prey_sdm/roe/climate/bio1.asc")
plot(bio1_all)
roads_all <- readOGR("Desktop/prey_combined/roads_all_1/", "roads_all")
roads_all <- rasterize(roads_all, bio1_all, fun = 'count', background = NA, mask = TRUE, update = FALSE)
roads <- crop(roads_all, bio1_all)
plot(roads_all)
roads_all >- mask(roads_all, all_ext)
roads_all[!is.na(roads_all)] <- 1
roads_all[ is.na(roads_all)] <- 0
plot(roads_all)
roads_all <- roads_all*bio1_all
plot(roads_all)

roads_all[roads_all < 1] <- NA
roads_all <- distance(roads_all)
plot(roads_all)

writeRaster(roads_all, "Desktop/prey_combined/prey_sdm/roads.asc", format = "ascii", overwrite = TRUE)

water_all <- readOGR("Desktop/prey_combined/water_all_1/", "water_all")
water_all <- rasterize(water_all, bio1_all, fun = 'last', background = NA, mask = TRUE, update = FALSE)
plot(water_all)
water_all <- mask(water_all, all_ext)
water_all <- water_all * bio1_all

plot(water_all)
water_all[!is.na(water_all)] <- 1
water_all[ is.na(water_all)] <- 0 
water_all <- distance(water_all)
plot(water_all)
plot(water_all)
writeRaster(water_all, "Desktop/prey_combined/prey_sdm/roe/Water.asc", format = "ascii", overwrite = TRUE)

#### population #### 

popu <- raster("Desktop/prey_combined/pop.gri")
plot(popu)
popu1 <- crop(popu, all_ext)
popu2 <- mask(popu1, all_ext)
res(popu)
popu <- resample(popu, water_all)
writeRaster(pop, "Desktop/prey_combined/Population.asc", format = "ascii", overwrite = TRUE)



#### Red deer for all ####

climate_red <- stack(paste0("Desktop/germany/climate_germany/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))
plot(climate[[1]])

#### load file to clean ####

red_all <- read.csv("Desktop/prey_combined/prey_sdm/red/nl_red.csv", header = TRUE, sep = ";")
head(red_all)
tail(red_all)
red_all_clean <- subset(red_all, (!is.na(latitude)) & (!is.na(longitude)))
cat(nrow(red_all) - nrow(red_all_clean), "had wrong or erroneous coordinates")

### removing some duplicates ####

dups <- duplicated(red_all_clean[c("latitude", "longitude")])
red_all_nondup <- red_all_clean[!dups, ]
cat(nrow(red_all_clean) - nrow(red_all_nondup), "duplicate records were removed")

### converting the data into spatial data for visual inspectionn ####

coordinates(red_all_nondup) <- ~longitude + latitude
plot(climate[[1]])
plot(red_all_nondup, add = TRUE)

#### If need, run the code to remove points that are outside your study area roughly ###

red_all_nondup <- red_all_nondup[which(red_all_nondup$longitude > -40 & red_all_nondup$longitude < 30), ]

#### making 1 occurrence point per pixel ####

cells <- cellFromXY(climate[[1]], red_all_nondup)
dups <- duplicated(cells)
red_all_finalish <- red_all_nondup[!dups, ]
cat(nrow(red_all_nondup) - nrow(red_all_finalish), "points were removed, that is awesome!")

#### plotting the new data on the map ####
plot(climate[[1]])
plot(red_all_finalish, add = TRUE, col = "black")

#### check and a projections #### 
head(red_all_finalish)
proj4string(red_all_finalish)
proj4string(climate)
proj4string(red_all_finalish) <- crs("+init=epsg:4327")
proj4string(red_all_finalish)

#### setting up the bufer polygon around occurrence points ####
### helps to avoid sampling from a broad background ###

red_all_buffer <- buffer(red_all_finalish, 1)

### proj4string(red_all_finalish) <- crs("+init=epsg:4326")
### proj4string(red_all_finalish)

plot(climate[[1]])
plot(red_all_finalish, add = T, col = "black")
plot(red_all_buffer, add = T, pch = 20, col ="red")
plot(red_all_buffer)

write.csv(red_all__finalish, "Desktop/prey_combined/prey_sdm/red/NL_red_1.csv", row.names = FALSE)


#### envir vars for red deer ####

red_all <- read.csv("Desktop/prey_combined/prey_sdm/red/re_deer_all.csv", header = TRUE, sep = ";")

head(red_all)
tail(red_all)
all_extent <- readOGR("Desktop/prey_combined/prey_mask/", "new_mask")
e <- extent(all_extent)
plot(all_extent)
studyarea_red <- crop(climate_red,e)
studyarea_red <- mask(studyarea_red, all_extent)
plot(studyarea_red[[1]])
names(studyarea_red)
names(studyarea_red) <- paste0("bio", 1:19)
writeRaster(studyarea_red, paste0("Desktop/prey_combined/prey_sdm/red/climate/", names(studyarea_red)), bylayer = TRUE, format ="ascii", overwrite = TRUE)
bio1_red <- raster("Desktop/prey_combined/prey_sdm/red/climate/bio1.asc")
plot(bio1_red)
res(bio1_red)
extent(bio1_red)
e

landuse1 <- brick("Desktop/states.nc", varname = "range")
plot(landuse1[[1166]])
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse1 <- brick("Desktop/prey_combined/prey_sdm/red/landuse/states.nc", varname = i)
  landuse1 <- crop(landuse1, e)
  landuse1 <- mean(landuse1[[1150:1166]])
  landuse1 <- resample(landuse1, studyarea_red)
  writeRaster(landuse1, paste0("Desktop/prey_combined/prey_sdm/red/landuse/landuse_red/",i,".asc"), format = "ascii", overwrite=TRUE)
}

primf_red <- raster ("Desktop/prey_combined/prey_sdm/red/landuse/primf.asc")
plot(primf_red)
secd_boar <- raster("Desktop/prey_combined/prey_sdm/red/landuse/secdf.asc")

landuse_list <- list.files("Desktop/prey_combined/prey_sdm/boar/landuse/", pattern = ".asc", full.names = TRUE)
landuse_list <- stack(landuse_list)
plot(landuse_list[[1]])
landuse_list <- resample(landuse_list, bio1_red)
#### Combining for roe deer #### 

forest1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/primf.asc")
forest2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdf.asc")
Forests <- mosaic(forest1, forest2, fun = sum)
writeRaster(Forests, "Desktop/prey_combined/prey_sdm/roe/landuse/Forests.asc", overwrite = TRUE)

nonf1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/primn.asc")
nonf2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdn.asc")
Non_forested_land <- mosaic(nonf1,nonf2, fun = sum)
writeRaster(Non_forested_land, "Desktop/prey_combined/prey_sdm/roe/landuse/Non_forested_land.asc" , overwrite =TRUE)

crop1 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c4per.asc")
crop2 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c4ann.asc")
crop3 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3per.asc")
crop4 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3nfx.asc")
crop5 <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/c3ann.asc")
Crops <- mosaic(crop1,crop2,crop3,crop4,crop5, fun=sum)
writeRaster(Crops, "Desktop/prey_combined/prey_sdm/roe/landuse/Crops.asc", overwrite =TRUE)

#### Do the same for boar deer #### 

forest1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/primf.asc")
forest2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdf.asc")
Forests <- mosaic(forest1, forest2, fun = sum)
writeRaster(Forests, "Desktop/prey_combined/prey_sdm/boar/landuse/Forests.asc", overwrite = TRUE)

nonf1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/primn.asc")
nonf2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdn.asc")
Non_forested_land <- mosaic(nonf1,nonf2, fun = sum)
writeRaster(Non_forested_land, "Desktop/prey_combined/prey_sdm/boar/landuse/Non_forested_land.asc" , overwrite =TRUE)

crop1 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c4per.asc")
crop2 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c4ann.asc")
crop3 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3per.asc")
crop4 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3nfx.asc")
crop5 <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/c3ann.asc")
Crops <- mosaic(crop1,crop2,crop3,crop4,crop5, fun=sum)
writeRaster(Crops, "Desktop/prey_combined/prey_sdm/boar/landuse/Crops.asc", overwrite =TRUE)



#### extraction of null points from all 3 prey species #### 

### ROE 
coordinates(roe_all_final) <- ~longitude + latitude
bio1_roe <- raster("Desktop/prey_combined/prey_sdm/roe/climate/bio1.asc")
bio2_roe <- raster("Desktop/prey_combined/prey_sdm/roe/landuse/secdf.asc")
plot(bio2_roe)
plot(bio1_roe)
plot(is.na(bio1_roe))
extract(bio1_roe, roe_all_final)
extract(is.na(bio1_roe), roe_all_final)
extract(bio2_roe, roe_all_final)
extract(is.na(bio2_roe), roe_all_final)

row.names(roe_all_final)

smp_size <- floor (0.7 * nrow(roe_all_final))
set.seed(1)
train_ind <- sample(seq_len(nrow(roe_all_final)), size = smp_size)
Train <- roe_all_final[train_ind, ]
Test <- roe_all_final[-train_ind, ]
write.csv(Train, "Desktop/prey_combined/prey_sdm/roe/Train_1.csv", row.names=FALSE)
write.csv(Test,"Desktop/prey_combined/prey_sdm/roe/Test_1.csv", row.names = FALSE)


list_var <- list.files("Desktop/prey_combined/prey_sdm/roe/input_vars_roe/", pattern = ".asc", full.names = TRUE)
all_vars <- stack(list_var)
length(which((!is.na(values(subset(all_vars,1))))))
plot(all_vars)
mask_r <- all_vars[["urban"]]
mask_r[!is.na(mask_r)] <- 1L
plot(mask_r)

masked_r <- mask_r * all_vars
names(masked_r) <- names(all_vars)
names(masked_r)

for(i in 1:nlayers(masked_r))
{
  masked_r[[i]] <- masked_r[[i]] - cellStats(masked_r[[i]], min)
  masked_r[[i]] <- masked_r[[i]] / cellStats(masked_r[[i]], max)
}
plot(masked_r)

writeRaster(masked_r,"Desktop/prey_combined/prey_sdm/roe/new_input_roe/", names(masked_r), bylayer = TRUE, format = "ascii", overwrite= TRUE)

roe_all <- read.csv("Desktop/prey_combined/prey_sdm/roe/roe_all.csv", sep = ";")
head(roe_all)
roe_all$species<- NULL
list_asc <- list.files("Desktop/prey_combined/prey_sdm/roe/new_input_roe/", pattern = ".asc", full.names = TRUE)
all_vasr <- stack(list_asc)
roe_all_ras <- rasterize(roe_all, all_vars, 1)
presences <- which(values(roe_all_ras) ==1)
pres.locs <- coordinates(roe_all_ras) [presences, ]
library("MASS")
dens <- kde2d(pres.locs[,1], pres.locs[,2], n = c(nrow(roe_all_ras), ncol(roe_all_ras)), lims = c(extent(all_vars)[1], extent(all_vars)[2], extent(all_vars)[3], extent(all_vars)[4]))
den.ras <- raster(dens, all_vars)
res(all_vars)
den.ras <- resample(den.ras, all_vars, method = 'ngb')
plot(den.ras)
writeRaster(den.ras, "Desktop/prey_combined/prey_sdm/roe/Bias_roe.asc", format = "ascii", overwrite = TRUE)

length(which((!is.na(values(subset(all_vars,1))))))

### BOAR
coordinates(boar_all) <- ~longitude + latitude
bio1_boar <- raster("Desktop/prey_combined/prey_sdm/boar/climate/bio1.asc")
bio2_boar <- raster("Desktop/prey_combined/prey_sdm/boar/landuse/secdf.asc")
boar_all$species <- NULL
extract(bio1_boar, boar_all)
extract(is.na(bio1_boar), boar_all)
extract(bio2_boar, boar_all)
extract(is.na(bio2_boar), boar_all)

smp_size <- floor (0.7 * nrow(boar_all))
set.seed(1)
train_ind <- sample(seq_len(nrow(boar_all)), size = smp_size)
Train <- boar_all[train_ind, ]
Test <- boar_all[-train_ind, ]
write.csv(Train, "Desktop/prey_combined/prey_sdm/boar/Train_1.csv", row.names=FALSE)
write.csv(Test,"Desktop/prey_combined/prey_sdm/boar/Test_1.csv", row.names = FALSE)

list_var <- list.files("Desktop/prey_combined/prey_sdm/boar/input_variables /", pattern = ".asc", full.names = TRUE)
all_vars <- stack(list_var)
length(which((!is.na(values(subset(all_vars,1))))))
plot(all_vars)
mask_r <- all_vars[["urban"]]
mask_r[!is.na(mask_r)] <- 1L
plot(mask_r)

masked_r <- mask_r * all_vars
names(masked_r) <- names(all_vars)
names(masked_r)

for(i in 1:nlayers(masked_r))
{
  masked_r[[i]] <- masked_r[[i]] - cellStats(masked_r[[i]], min)
  masked_r[[i]] <- masked_r[[i]] / cellStats(masked_r[[i]], max)
}
plot(masked_r)

writeRaster(masked_r,"Desktop/prey_combined/prey_sdm/boar/new_input_oar/", names(masked_r), bylayer = TRUE, format = "ascii", overwrite= TRUE)

boar_all <- read.csv("Desktop/prey_combined/prey_sdm/boar/boar_all.csv", sep = ";")
head(boar_all)
boar_all$species<- NULL
list_asc <- list.files("Desktop/prey_combined/prey_sdm/boar/new_input_boar/", pattern = ".asc", full.names = TRUE)
all_vars <- stack(list_asc)
boar_all_ras <- rasterize(boar_all, all_vars, 1)
presences <- which(values(boar_all_ras) ==1)
pres.locs <- coordinates(boar_all_ras) [presences, ]
library("MASS")
dens <- kde2d(pres.locs[,1], pres.locs[,2], n = c(nrow(boar_all_ras), ncol(boar_all_ras)), lims = c(extent(all_vars)[1], extent(all_vars)[2], extent(all_vars)[3], extent(all_vars)[4]))
den.ras <- raster(dens, all_vars)
res(all_vars)
den.ras <- resample(den.ras, all_vars)
plot(den.ras)
writeRaster(den.ras, "Desktop/prey_combined/prey_sdm/boar/Bias_boar.asc", format = "ascii", overwrite = TRUE)

length(which((!is.na(values(subset(all_vars,1))))))

### RED
coordinates(red_all) <- ~longitude + latitude
bio1_red <- raster("Desktop/prey_combined/prey_sdm/red/climate/bio1.asc")
bio2_red <- raster("Desktop/prey_combined/prey_sdm/red/landuse/landuse_red/secdf.asc")
extract(bio1_red, red_all)
extract(is.na(bio1_red), red_all)
extract(bio2_red, red_all)
extract(is.na(bio2_red), red_all)

smp_size <- floor (0.7 * nrow(red_all))
set.seed(1)
train_ind <- sample(seq_len(nrow(red_all)), size = smp_size)
Train <- red_all[train_ind, ]
Test <- red_all[-train_ind, ]
write.csv(Train, "Desktop/prey_combined/prey_sdm/red/Train_1.csv", row.names=FALSE)
write.csv(Test,"Desktop/prey_combined/prey_sdm/red/Test_1.csv", row.names = FALSE)

list_var <- list.files("Desktop/prey_combined/prey_sdm/red/input_vars_red/", pattern = ".asc", full.names = TRUE)
all_vars <- stack(list_var)
length(which((!is.na(values(subset(all_vars,1))))))
plot(all_vars)
mask_r <- all_vars[["urban"]]
mask_r[!is.na(mask_r)] <- 1L
plot(mask_r)

masked_r <- mask_r * all_vars
names(masked_r) <- names(all_vars)
names(masked_r)

for(i in 1:nlayers(masked_r))
{
  masked_r[[i]] <- masked_r[[i]] - cellStats(masked_r[[i]], min)
  masked_r[[i]] <- masked_r[[i]] / cellStats(masked_r[[i]], max)
}
plot(masked_r)

writeRaster(masked_r,"Desktop/prey_combined/prey_sdm/red/new_input_red/", names(masked_r), bylayer = TRUE, format = "ascii", overwrite= TRUE)

red_all <- read.csv("Desktop/prey_combined/prey_sdm/red/re_deer_all.csv", sep = ";")
head(red_all)
red_all$species<- NULL
list_asc <- list.files("Desktop/prey_combined/prey_sdm/red/new_input_red/", pattern = ".asc", full.names = TRUE)
all_vars <- stack(list_asc)
red_all_ras <- rasterize(red_all, all_vars, 1)
presences <- which(values(red_all_ras) ==1)
pres.locs <- coordinates(red_all_ras) [presences, ]
library("MASS")
dens <- kde2d(pres.locs[,1], pres.locs[,2], n = c(nrow(red_all_ras), ncol(red_all_ras)), lims = c(extent(all_vars)[1], extent(all_vars)[2], extent(all_vars)[3], extent(all_vars)[4]))
den.ras <- raster(dens, all_vars)
res(all_vars)
den.ras <- resample(den.ras, all_vars)
plot(den.ras)
writeRaster(den.ras, "Desktop/prey_combined/prey_sdm/red/Bias_red.asc", format = "ascii", overwrite = TRUE)

length(which((!is.na(values(subset(all_vars,1))))))


#### More variables for 3 prey species ####


### BOAR

# roads

bio1_boar <- raster("Desktop/prey_combined/prey_sdm/boar/new_input_boar/_bio1.asc")
proj4string(bio1_boar) <- "+init=epsg:4327"
plot(bio1_boar)
roads <- readOGR("Desktop/prey_combined/roads/", "roads")
roads <- rasterize(roads, bio1_boar, fun = 'count', background = NA, mask = TRUE, update = FALSE)
proj4string(roads) <- "+init=epsg:4327"
roads <- mask(roads, bio1_boar)
plot(roads)
roads
roads <- resample(roads, bio1_boar)
roads[!is.na(roads)] <- 1
roads[ is.na(roads)] <- 0
roads[roads >1] <- NA
roads <- distance(roads)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/boar/Roads_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/boar/Roads_without_dist.asc",format = "ascii", overwrite = TRUE)

# water

water <- readOGR("Desktop/prey_combined/water/", "water")
water <- rasterize(water, bio1_boar, fun = 'count', backgroun = NA, mask = TRUE, update = FALSE)
proj4string(bio1_boar) <- "+init=epsg:4327"
proj4string(water) <- "+init=epsg:4327"
plot(water)
water <- mask(water, bio1_boar)
water <- resample(water, bio1_boar)
water[!is.na(water)] <- 1
water[ is.na(water)] <-0
plot(water)
water[water < 1] <- NA
water <- distance(water)
writeRaster (water, "Desktop/prey_combined/prey_sdm/boar/Water_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/boar/water_without_dist.asc",format = "ascii", overwrite = TRUE)


### RED

# roads

bio1_red <- raster("Desktop/prey_combined/prey_sdm/red/input_vars_red/bio1.asc")
proj4string(bio1_red) <- "+init=epsg:4327"
plot(bio1_red)
roads <- readOGR("Desktop/prey_combined/roads/", "roads")
roads <- rasterize(roads, bio1_red, fun = 'count', background = NA, mask = TRUE, update = FALSE)
proj4string(roads) <- "+init=epsg:4327"
roads <- mask(roads, bio1_red)
plot(roads)
roads <- resample(roads, bio1_red)
roads[!is.na(roads)] <- 1
roads[ is.na(roads)] <- 0
roads[roads >1] <- NA
roads <- distance(roads)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/red/Roads_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/red/Roads_without_dist.asc",format = "ascii", overwrite = TRUE)

# water

water <- readOGR("Desktop/prey_combined/water/", "water")
water <- rasterize(water, bio1_red, fun = 'count', backgroun = NA, mask = TRUE, update = FALSE)
proj4string(bio1_boar) <- "+init=epsg:4327"
proj4string(water) <- "+init=epsg:4327"
plot(water)
water <- mask(water, bio1_red)
water <- resample(water, bio1_red)
water[!is.na(water)] <- 1
water[ is.na(water)] <-0
plot(water)
water[water < 1] <- NA
water <- distance(water)
writeRaster (water, "Desktop/prey_combined/prey_sdm/red/Water_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/red/water_without_dist.asc",format = "ascii", overwrite = TRUE)

### ROE 

# roads

bio1_roe <- raster("Desktop/prey_combined/prey_sdm/roe/new_input_roe/Annual_mean_temp.asc")
proj4string(bio1_roe) <- "+init=epsg:4327"
plot(bio1_roe)
roads <- readOGR("Desktop/prey_combined/roads/", "roads")
roads <- rasterize(roads, bio1_roe, fun = 'count', background = NA, mask = TRUE, update = FALSE)
proj4string(roads) <- "+init=epsg:4327"
roads <- mask(roads, bio1_roe)
plot(roads)
roads <- resample(roads, bio1_roe)
roads[!is.na(roads)] <- 1
roads[ is.na(roads)] <- 0
roads[roads >1] <- NA
roads <- distance(roads)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/roe/Roads_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/roe/Roads_without_dist.asc",format = "ascii", overwrite = TRUE)

# water

water <- readOGR("Desktop/prey_combined/water/", "water")
water <- rasterize(water, bio1_roe, fun = 'count', backgroun = NA, mask = TRUE, update = FALSE)
proj4string(bio1_roe) <- "+init=epsg:4327"
proj4string(water) <- "+init=epsg:4327"
plot(water)
water <- mask(water, bio1_roe)
water <- resample(water, bio1_roe)
water[!is.na(water)] <- 1
water[ is.na(water)] <-0
plot(water)
water[water < 1] <- NA
water <- distance(water)
writeRaster (water, "Desktop/prey_combined/prey_sdm/roe/Water_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/roe/water_without_dist.asc",format = "ascii", overwrite = TRUE)

## NEW WATER

water_1 <- raster("Desktop/prey_combined/roads/new_water.tif")
plot(water_1)
water_1 <- mask(water_1, bio1_roe)
water_1<- resample(water_1, bio1_roe)
water_1[is.na(water_1)] <-0
water_1[water_1 < 1] <- NA
water_1 <- distance(water_1)

writeRaster(water_1, "Desktop/prey_combined/water/Water_dist.asc", format = "ascii", overwrite = TRUE)

#### Suitability for 3 prey ####

### BOAR

boar_suit <- raster("Desktop/prey_combined/prey_sdm/boar/results_boar/results_boar_6/Sus_scrofa.asc")
plot(boar_suit)
boar_suit_2 <- read.csv("Desktop/prey_combined/prey_sdm/boar/results_boar/results_boar_6/maxentResults.csv", sep = ",")
suitability_boar <- boar_suit > boar_suit_2 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_boar)


### RED

red_suit <- raster("Desktop/prey_combined/prey_sdm/red/results_red/results_red_2/Cervus_elaphus.asc")
plot(red_suit)
red_suit_2 <- read.csv("Desktop/prey_combined/prey_sdm/red/results_red/results_red_2/maxentResults.csv", sep = ",")
suitability_red <- red_suit > red_suit_2 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_red)

### ROE

roe_suit <- raster("Desktop/prey_combined/prey_sdm/roe/results_roe/results_roe_3/Capreolus_capreolus.asc")
plot(roe_suit)
roe_suit_2 <- read.csv("Desktop/prey_combined/prey_sdm/roe/results_roe/results_roe_3/maxentResults.csv", sep = ",")
suitability_roe <- roe_suit > roe_suit_2 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_roe)



#####

list_boar <- list.files("Desktop/prey_combined/prey_sdm/boar/new_input_boar/", pattern = ".asc", full.names = TRUE)
list_boar <- stack(list_boar)
plot(list_boar)


#### Adding population to the 3 prey species #### 

france_pops <- raster("Desktop/france_only/FRA_pop/fra_pop.gri")
plot(france_pops)
france_pops <- mask(france_pops, all_extent)
bio1_all <- raster("Desktop/prey_combined/prey_sdm/boar/new_input_boar/_bio1.asc")
france_pops <- resample(france_pops, bio1_all, method = 'ngb' )
france_pops[!is.na(france_pops)] <- 1
france_pops[ is.na(france_pops)] <- 0


germany_pops <- raster("Desktop/germany/DEU_pop/deu_pop.gri")
plot(germany_pops)
germany_pops <- mask(germany_pops, all_extent)
germany_pops <- resample(germany_pops, bio1_all, method = 'ngb')
germany_pops[is.na(germany_pops)] <- 0

nl_pops <- raster("Desktop/netherlands/NLD_pop 2/nld_pop.gri")
plot(nl_pops)
nl_pops <- mask(nl_pops, all_extent)
nl_pops <- resample(nl_pops, bio1_all, method = 'ngb')
nl_pops[ is.na(nl_pops)] <- 0

fr_gr_pops <- mosaic(france_pops, germany_pops, fun = mean, na.rm = TRUE)
plot(fr_gr_pops)
fr_gr_pops <- mask(fr_gr_pops, fr_gr)
writeRaster(fr_gr_pops, "Desktop/france_germany_trial/SDM/Population_1.asc", format = "ascii", overwrite = TRUE)

?trim

## TRIAL STARTS 

fr_gr_pops_1 <- mosaic(france_pops, germany_pops, fun = max, na.rm = TRUE)
plot(fr_gr_pops_1)
fr_gr_pops <- mask(fr_gr_pops_1, fr_gr)

## TRIAL ENDS

fr_gr_nl_pops <- mosaic(fr_gr_pops, nl_pops, fun = mean , na.rm= TRUE)
plot(fr_gr_nl_pops)
fr_gr_nl_pops <- mask(fr_gr_nl_pops, all_extent)
writeRaster(fr_gr_nl_pops, "Desktop/prey_combined/Population_1.asc", format = "ascii", overwrite = TRUE)



#### New roads for 3 models ####

roads_all <- readOGR("Desktop/prey_combined/roads/", "roads")
roads_all
roads <- rasterize(roads_all, bio1_all, fun = 'count', mask = TRUE, background = TRUE, update= FALSE)
plot(roads)
roads
roads_diss <- disaggregate(roads, fact = 5, fun = 'mean')
roads_diss
plot(roads_diss)
roads_diss_2 <- disaggregate(roads, fact = c(100,100))
plot(roads_diss_2)
roads_diss_2

rm(large_df, large_list, large_vector, temp_variables)

roads_new <- raster("Desktop/prey_combined/roads/_new_road_2.tif")
plot(roads_new)
rMask <- bio1_all
plot(rMask)
rMask[!is.na(rMask)] <- 1L
roads_new <- mask(roads_new, rMask)
plot(roads_new)
roads <- resample(roads_new, bio1_all, method = "ngb")
roads_new[!is.na(roads)] <- 1
roads_new[ is.na(roads)] <- 0
roads[roads >1] <- NA
proj4string(roads_new) <- "+init=epsg:4327"
roads <- distance(roads_new)
writeRaster (roads, "Desktop/prey_combined/prey_sdm/roe/Roads_with_distance.asc",format = "ascii", overwrite = TRUE)
writeRaster (roads_new, "Desktop/prey_combined/prey_sdm/roe/Roads_without_dist_1.asc",format = "ascii", overwrite = TRUE)


road_road <- raster("Desktop/prey_combined/roads/comp_new_roads.tif")
plot(road_road)
roads <- resample(road_road, bio1_all)
roads<- mask(roads, all_extent)
roads[!is.na(roads)] <-1
roads[ is.na(roads)] <-0
roads[roads > 1] <- NA
roads <- distance(roads)

roads_new <- raster("Desktop/prey_combined/roads/full_new_1.tif")
plot(roads_new)
roads_new <- mask(roads_new, rMask)

roads_new <- resample(roads_new, bio1_all)

roads_new[!is.na(roads_new)] <- 1
roads_new[ is.na(roads_new)] <- 0

writeRaster(roads_new, "Desktop/prey_combined/Roads_without_dist.asc", format = "ascii", overwrite = TRUE)

roads_new[roads_new > 1] <- NA
proj4string(roads_new) <- "+init=epsg:4327"
roads_new <- distance(roads_new)

writeRaster(roads_new, "Desktop/prey_combined/Roads_with_dist.asc", format = "ascii", overwrite = TRUE)

list_asc <- list.files("Desktop/prey_combined/prey_sdm/roe/new_input_roe/", pattern = ".asc", full.names = TRUE)
roe_vars <- stack(list_asc)
plot(roe_vars)
rMasked <-rMask * roe_vars
plot(rMasked)
names(rMasked) <- names(roe_vars)
plot(rMasked)
nlayers(rMasked)
for(i in 1:nlayers(rMasked)){
  rMasked[[i]] <- rMasked[[i]] - cellStats(rMasked[[i]], min)
  rMasked[[i]] <- rMasked[[i]] / cellStats(rMasked[[i]], max)
}
plot(rMasked)


### Roads_trial 

road <- raster("Desktop/prey_combined/roads/full_new_1.tif")
plot(road)
road <- resample(road, bio1_roe)
road <- mask(road, bio1_roe)
road[is.na(road)] <- 0
road[!is.na(road)] <- 1
road[road < 1] <- NA
road <- distance(road)
writeRaster(road, "Desktop/prey_combined/roads/Roads_dist.asc", format = "ascii", overwrite = TRUE)

### Population with the LOG  transformation ### 

Population_prey <- raster("Desktop/prey_combined/population/Population_1.asc")
plot(Population_prey)
poplation_p <- log1p(Population_prey)
plot(poplation_p)
poplation_p[ is.na(poplation_p)] <- 0
poplation_p <- mask(poplation_p, bio1_red)
writeRaster(poplation_p, "Desktop/prey_combined/population/Population.asc", format = "ascii", overwrite = TRUE)

#### CHECK RANGE ####

boar_var<- list.files("Desktop/prey_combined/prey_sdm/boar/input_boar_new/", pattern = ".asc", full.names = TRUE)
boar_var <- stack(boar_var)
plot(boar_var)

for( i in 1:nlayers(boar_var)) {
  boar_var[[i]] <- boar_var[[i]] - cellStats(boar_var[[i]], min)
  boar_var[[i]] <- boar_var[[i]] / cellStats(boar_var[[i]], max)
}

plot(boar_var)
writeRaster(boar_var,"Desktop/prey_combined/prey_sdm/boar/All_input_boar/", bylayer = TRUE, names(boar_var), format ="ascii", overwrite = TRUE)

##

red_var <- list.files("Desktop/prey_combined/prey_sdm/red/new_input_red/", pattern = ".asc", full.names = TRUE)
red_var <- stack(red_var)
plot(red_var)

for( i in 1:nlayers(red_var)) {
  red_var[[i]] <- red_var[[i]] - cellStats(red_var[[i]], min)
  red_var[[i]] <- red_var[[i]] / cellStats(red_var[[i]], max)
}

plot(red_var)
writeRaster(red_var, "Desktop/prey_combined/prey_sdm/red/All_input_red/", bylayer = TRUE, names(red_var), format= "ascii", overwrite = TRUE)

##

roe_var <- list.files("Desktop/prey_combined/prey_sdm/roe/new_input_roe/", pattern = ".asc", full.names = TRUE)
roe_var <- stack(roe_var)
plot(roe_var)

for( i in 1:nlayers(roe_var)) {
  roe_var[[i]] <- roe_var[[i]] - cellStats(roe_var[[i]], min)
  roe_var[[i]] <- roe_var[[i]] / cellStats(roe_var[[i]], max)
}

plot(roe_var)
writeRaster(roe_var, "Desktop/prey_combined/prey_sdm/roe/All_input_roe/", bylayer = TRUE, names(roe_var), format= "ascii", overwrite = TRUE)




##### Ploting the maxent results and then taking it to qgis ####

pred_map_boar <- raster("Desktop/prey_combined/prey_sdm/boar/results_boar/Results_boar_7/Sus_scrofa.asc")
plot(pred_map_boar)
maxres_boar <- read.csv("Desktop/prey_combined/prey_sdm/boar/results_boar/Results_boar_7/maxentResults.csv", sep = ",")
head(maxres_boar)
pred_map_2_boar <- pred_map_boar > maxres_boar $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2_boar)


pred_map_roe <- raster("Desktop/prey_combined/prey_sdm/roe/results_roe/Results_roe_5/Capreolus_capreolus.asc")
plot(pred_map_roe)
maxres_roe <- read.csv("Desktop/prey_combined/prey_sdm/roe/results_roe/Results_roe_5/maxentResults.csv", sep = ",")
head(maxres_roe)
pred_map_2_roe <- pred_map_roe > maxres_roe $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2_roe)


pred_map_red <- raster("Desktop/prey_combined/prey_sdm/red/results_red/Results_red_5/Cervus_elaphus.asc")
plot(pred_map_red)
maxres_red <- read.csv("Desktop/prey_combined/prey_sdm/red/results_red/Results_red_5/maxentResults.csv", sep = ",")
head(maxres_red)
pred_map_2_red <- pred_map_red > maxres_red $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2_red)

Prey_preds <- mosaic(pred_map_2_boar, pred_map_2_red, pred_map_2_roe, fun = sum, na.rm = TRUE)
plot(Prey_preds)

writeRaster(pred_map_2_boar, "Desktop/Boar_r_pred.asc", format = "ascii")
writeRaster(pred_map_2_roe, "Desktop/roe_r_pred.asc", format = "ascii")
writeRaster(pred_map_2_red, "Desktop/red_r_pred.asc", format = "ascii")



row_all <- list.files("Desktop/prey_combined/prey_sdm/roe/new_input_roe/", pattern = ".asc", full.names = TRUE)
Roe_all_vars <- stack(row_all)


#### ROADS WITH PROXIMITY RASTER ##### 

bio1_all <- raster("Desktop/prey_combined/prey_sdm/boar/All_input_boar/Annual_mean_temp.asc")
plot(bio1_all)
road <- raster("Desktop/prey_combined/roads/new_road_tues.tif")
plot(road)
roads_dist <- aggregate(road, fact = 10, fun = mean)
plot(roads_dist)
roads_dist <- resample(roads_dist, bio1_all)
roads_dist <- mask(roads_dist, bio1_all)
plot(roads_dist)
roads_dist[!is.na(roads_dist)] <- 1
writeRaster(roads_dist, "Desktop/prey_combined/roads/new_road.asc", format = "ascii", overwrite= TRUE)

base_layer1 <- list.files("Desktop/prey_combined/prey_sdm/base_layer/", pattern = ".asc", full.names = TRUE)
base_layer1 <-  stack(base_layer1)
plot(base_layer1)
for (i in 1:nlayers(base_layer1)) {
  base_layer1[[i]] <- base_layer1[[i]] - cellStats(base_layer1[[i]], min)
  base_layer1[[i]] <- base_layer1[[i]] / cellStats(base_layer1[[i]], max)
}
plot(base_layer1)

writeRaster(base_layer1,"Desktop/prey_combined/prey_sdm/base_layer_newroad/", names(base_layer1), bylayer = TRUE, format = "ascii", overwrite = TRUE)

#### FINAL SUITABILITIES #########

nl_suit <- raster("Desktop/netherlands/Boar/Results_NL/Results_NL_1/Sus_scrofa.asc")
plot(log1p(nl_suit))
nl_suit_2 <- read.csv("Desktop/netherlands/Boar/Results_NL/Results_NL_1/maxentResults.csv", sep = ",")
nl_suit_3 <- nl_suit > nl_suit_2$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(nl_suit_3)


roe_suit <- raster("Desktop/prey_combined/prey_sdm/boar/results_boar/RESULT_FINAL/Sus_scrofa.asc")
plot(roe_suit)


wolf_suit <- raster("Desktop/france_germany_trial/SDM/results_germany_france/RESULTS_FR_GR_FINAL_1/Canis_lupus.asc")
plot(log1p(wolf_suit))
plot(wolf_suit)
