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

#### Load and stack clmatic variables #### 

climate_roe <- stack(paste0("Desktop/germany/climate_germany/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))
plot(climate_roe[[1]])
climate_boar <- stack(paste0("Desktop/germany/climate_germany/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))


#### Load occurrence data to start cleaning #### 

roe_NL <- read.csv("Desktop/netherlands/roe.csv", header = TRUE, sep = ";")
head(roe_NL)
tail(roe_NL)

roe_NL_clean <- subset(roe_NL, (!is.na(latitude)) & (!is.na(longitude)))
cat(nrow(roe_NL) - nrow(roe_NL_clean), "had wrong coordinates")                       

dups <- duplicated(roe_NL_clean[c("longitude", "latitude")])
roe_NL_nondup <- roe_NL_clean[!dups, ]
cat(nrow(roe_NL_clean) - nrow(roe_NL_nondup), "diplicates were removed")

coordinates(roe_NL_nondup) <- ~longitude + latitude
plot(climate_roe[[1]])
plot(roe_NL_nondup, add = TRUE)

roe_NL_nondup <- roe_NL_nondup[which(roe_NL_nondup$longitude > -5 & roe_NL_nondup$longitude < 8), ]

cells <- cellFromXY(climate_roe[[1]], roe_NL_nondup)
dups <- duplicated(cells)
roe_NL_final <- roe_NL_nondup[!dups, ]
cat(nrow(roe_NL_nondup) - nrow(roe_NL_final), "points were removed")

plot(climate_roe[[1]])
plot(roe_NL_final, add = TRUE, col = "red")

proj4string(roe_NL_final)
proj4string(climate_roe)
proj4string(roe_NL_final) <- crs("+init=epsg:4326")
proj4string(roe_NL_final)

roe_NL_buffer <- buffer(roe_NL_final, 1)

plot(climate_roe[[1]])
plot(roe_NL_final, add = T, col = "black")
plot(roe_NL_buffer, add = T, pch = 20, col ="red")
plot(roe_NL_buffer)

#### Do the same for wild boar ####

boar_NL <- read.csv("Desktop/netherlands/Boar.csv", header = TRUE, sep = ";")
head(boar_NL)
tail(boar_NL)

boar_NL_clean <- subset(boar_NL, (!is.na(latitude)) & (!is.na(longitude)))
cat(nrow(boar_NL) - nrow(boar_NL_clean), "had wrong coordinates")                       

dups <- duplicated(boar_NL_clean[c("longitude", "latitude")])
boar_NL_nondup <- boar_NL_clean[!dups, ]
cat(nrow(boar_NL_clean) - nrow(boar_NL_nondup), "diplicates were removed")

coordinates(boar_NL_nondup) <- ~longitude + latitude
plot(climate_boar[[1]])
plot(boar_NL_nondup, add = TRUE)

boar_NL_nondup <- boar_NL_nondup[which(boar_NL_nondup$longitude > -5 & boar_NL_nondup$longitude < 8), ]

cells <- cellFromXY(climate_boar[[1]], boar_NL_nondup)
dups <- duplicated(cells)
boar_NL_final <- boar_NL_nondup[!dups, ]
cat(nrow(boar_NL_nondup) - nrow(boar_NL_final), "points were removed")

plot(climate_boar[[1]])
plot(boar_NL_final, add = TRUE, col = "red")

proj4string(boar_NL_final)
proj4string(climate_boar)
proj4string(boar_NL_final) <- crs("+init=epsg:4326")
proj4string(boar_NL_final)

boar_NL_buffer <- buffer(boar_NL_final, 1)

plot(climate_boar[[1]])
plot(boar_NL_final, add = T, col = "black")
plot(boar_NL_buffer, add = T, pch = 20, col ="blue")
plot(boar_NL_buffer)

#### envir vriables roe deer #### 

NL_ext <- readOGR("Desktop/netherlands/NLD_adm/" ,"NLD_adm0")
e <- extent(NL_ext)
e
studyarea_roe <- crop(climate_roe,e)
studyarea_roe <- mask(studyarea_roe, NL_ext)
plot(studyarea_roe[[1]])
names(studyarea_roe)
names(studyarea_roe) <- paste0("bio", 1:19)
writeRaster(studyarea_roe, paste0("Desktop/netherlands/roe/climate/", names(studyarea_roe)), bylayer = TRUE, format ="ascii", overwrite = TRUE)
bio1_roe <- raster("Desktop/netherlands/roe/climate/bio1.asc")
plot(bio1_roe)
res(bio1_roe)
extent(bio1_roe)
e
plot(is.na(bio1_roe))
extract(bio1_roe, roe_NL_final)
extract(is.na(bio1_roe), roe_NL_final)
plot(roe_NL_nondup, add = T, col = "black")


landuse <- brick("Desktop/germany/land_use_2/states.nc", varname = "range")
plot(landuse[[1166]])
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/germany/land_use_2/states.nc", varname = i)
  landuse <- crop(landuse, extent(all_extent))
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, all_extent)
  landuse <- resample(landuse, studyarea_red)
  writeRaster(landuse, paste0("Desktop/prey_combined/prey_sdm/red/landuse/landuse_red/",i,".asc"), format = "ascii", overwrite=TRUE)
}

primf_roe <- raster ("Desktop/netherlands/roe/landuse/primf.asc")
plot(primf_roe)
secd_roe <- raster("Desktop/netherlands/roe/landuse/secdf.asc")
plot(secd_roe)
plot(is.na(secd_roe))
extract(secd_roe, roe_NL_final)
extract(is.na(secd_roe), roe_NL_final)
plot(roe_NL_final, add= T, col = "black")

write.csv(roe_NL_final, "Desktop/netherlands/roe/roe_NL_final.csv", row.names = FALSE)

#### DO the same envir vars for boar #### 

studyarea_boar <- crop(climate_boar,e)
studyarea_boar <- mask(studyarea_boar, NL_ext)
plot(studyarea_boar[[1]])
names(studyarea_boar)
names(studyarea_boar) <- paste0("bio", 1:19)
writeRaster(studyarea_boar, paste0("Desktop/netherlands/Boar/climate/", names(studyarea_boar)), bylayer = TRUE, format ="ascii", overwrite = TRUE)
bio1_boar <- raster("Desktop/netherlands/Boar/climate/bio1.asc")
plot(bio1_boar)
res(bio1_boar)
extent(bio1_boar)
e
plot(is.na(bio1_boar))
extract(bio1_boar, boar_NL_final)
extract(is.na(bio1_boar), boar_NL_final)
plot(boar_NL_nondup, add = T, col = "black")


landuse <- brick("Desktop/Germany/land_use_2/states.nc", varname = "range")
plot(landuse[[1166]])
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/Germany/land_use_2/states.nc", varname = i)
  landuse <- crop(landuse, e)
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, NL_ext)
  landuse <- resample(landuse, studyarea_boar)
  writeRaster(landuse, paste0("Desktop/netherlands/Boar/landuse/",i,".asc"), format = "ascii", overwrite=TRUE)
}

primf_boar <- raster ("Desktop/netherlands/Boar/landuse/primf.asc")
plot(primf_boar)
secd_boar <- raster("Desktop/netherlands/Boar/landuse/secdf.asc")
plot(secd_boar)
plot(is.na(secd_boar))
extract(secd_boar, boar_NL_final)
extract(is.na(secd_boar), boar_NL_final)
plot(boar_NL_final, add= T, col = "black")

write.csv(boar_NL_final, "Desktop/netherlands/Boar/boar_NL_final.csv", row.names = FALSE)



#### Train, test, roe deer #### 

smp_size <- floor (0.7 * nrow(roe_NL_final))
set.seed(1)
train_ind <- sample(seq_len(nrow(roe_NL_final)), size = smp_size)
Train <- roe_NL_final[train_ind, ]
Test <- roe_NL_final[-train_ind, ]
write.csv(Train, "Desktop/netherlands/roe/Train.csv", row.names=FALSE)
write.csv(Test,"Desktop/netherlands/roe/Test.csv", row.names = FALSE)

#### Train, test, boar deer #### 

smp_size <- floor (0.7 * nrow(boar_NL_final))
set.seed(1)
train_ind <- sample(seq_len(nrow(boar_NL_final)), size = smp_size)
Train <- boar_NL_final[train_ind, ]
Test <- boar_NL_final[-train_ind, ]
write.csv(Train, "Desktop/netherlands/Boar/Train_2.csv", row.names=FALSE)
write.csv(Test,"Desktop/netherlands/Boar/Test_2.csv", row.names = FALSE)


#### suitability boar ####

pred_map_roe <- raster("Desktop/netherlands/Boar/Results_NL/Results_NL_1/Sus_scrofa.asc")
plot(pred_map_roe)

maxres_roe_fr <- read.csv("Desktop/netherlands/Boar/Results_NL/Results_NL_1/maxentResults.csv", sep = ",")
head(maxres_roe_fr)
pred_map_2_roe <- pred_map_roe > maxres_roe_fr $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2_roe)
writeRaster(pred_map_2_roe, "Desktop/netherlands/Boar/Boar_NL.asc", format ="ascii", overwrite = TRUE)

#### suitability roe ####

pred_map_roe <- raster("Desktop/netherlands/roe/Results_NL/Results_roe_NL_1/Capreolus_capreolus.asc")
plot(pred_map_boar)

maxres_boar_fr <- read.csv("Desktop/netherlands/roe/Results_NL/Results_roe_NL_1/maxentResults.csv", sep = ",")
head(maxres_boar_fr)
pred_map_2_boar <- pred_map_boar > maxres_boar_fr $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2_boar)

writeRaster(pred_map_2_boar, "Desktop/netherlands/roe/Roe_NL.asc", format = "ascii")

#### population of roe ####

pop_roe_nl
