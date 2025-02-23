### Loading required packages ####

library("raster")
library("rgdal")
library("ncdf4")
library("dismo")
library("rgeos")
library("rgdal")
library("sp")
library("usdm")
library("sdm")

### Load and stack climatic variables ####

climate <- stack(paste0("Desktop/germany/climate_germany/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))
plot(climate[[1]])
v1 <- vifstep(climate)
v2 <- vifcor(climate, th= 0.7)
v1
v2
### Load occurrence data to start cleaning ####

### removing weired coordinates ####
germany <- read.csv("Desktop/germany/germany_letsgo.csv", header = TRUE, sep = ";")
head(germany)
tail(germany)
germany_clean <- subset(germany, (!is.na(latitude)) & (!is.na(longitude)))
cat(nrow(germany) - nrow(germany_clean), "had wrong or erroneous coordinates")

### removing some duplicates ####

dups <- duplicated(germany_clean[c("latitude", "longitude")])
germany_nondup <- germany_clean[!dups, ]
cat(nrow(germany_clean) - nrow(germany_nondup), "duplicate records were removed")

### converting the data into spatial data for visual inspectionn ####

coordinates(germany_nondup) <- ~longitude + latitude
plot(climate[[1]])
plot(germany_nondup, add = TRUE)

#### If need, run the code to remove points that are outside your study area roughly ###
#### germany_nondup <- germany_nondup[which(germany_nondup$longitude > 0 & germany_nondup$longitude <- 30), ]

#### making 1 occurrence point per pixel ####

cells <- cellFromXY(climate[[1]], germany_nondup)
dups <- duplicated(cells)
germany_finalish <- germany_nondup[!dups, ]
cat(nrow(germany_nondup) - nrow(germany_finalish), "points were removed, that is awesome!")

#### plotting the new data on the map ####
plot(climate[[1]])
plot(germany_finalish, add = TRUE, col = "black")

#### check and a projections #### 

proj4string(germany_finalish)
proj4string(climate)
proj4string(germany_finalish) <- crs("+init=epsg:4327")
proj4string(germany_finalish)

#### setting up the bufer polygon around occurrence points ####
### helps to avoid sampling from a broad background ###

germany_buffer <- buffer(germany_finalish, 1)

### proj4string(germany_finalish) <- crs("+init=epsg:4326")
### proj4string(germany_finalish)

plot(climate[[1]])
plot(germany_finalish, add = T, col = "black")
plot(germany_buffer, add = T, pch = 20, col ="red")
plot(germany_buffer)
#### extract the extent and crop all environmenal vriables ####


germany_ext <- readOGR("Desktop/germany/germany_adm/", "DEU_adm0")
e <- extent(germany_ext)
e
studyarea <- mask(studyarea, germany_ext)
plot(studyarea[[1]])
studyarea <- crop(climate, e)

#### check snap function (outwards)  ###
plot(studyarea[[1]])
climate
names(studyarea)
names(studyarea) <- paste0("bio", 1:19)
writeRaster(studyarea, paste0("Desktop/germany/climate_germany/_new_climate/", names(studyarea)), bylayer = TRUE, format = "ascii", overwrite = TRUE)
v1 <- vifstep(studyarea)
v2 <- vifcor(studyarea, th= 0.7)
v1
v2
#### testing the variables ####

bio1 <- raster("Desktop/germany/climate_germany/_new_climate/bio1.asc")
plot(bio1)
res(bio1)
extent(bio1)
e
plot(germany_buffer, add = T, col ="red")
plot(is.na(bio1))
extract(bio1,germany_finalish)
extract(is.na(bio1),germany_finalish)
plot(germany_finalish, add = T, col = "red")

#### do the same for landuse variables ####

landuse <- brick("Desktop/germany/land_use_2/states.nc", varname ="range")
plot(landuse[[1166]])
landuse <- brick("germany/land_use_2/states.nc", varname ="primf")
landuse <- crop(landuse,e)
proj4string(landuse)
plot(is.na(landuse))
landuse <- mean(landuse[[1150:1166]])
landuse <- mean(landuse[[1150:1166]])
landuse <- resample(landuse,studyarea)
landuse <- mask(landuse, germany_ext)
writeRaster(landuse, "germany/land_use_2/", format = "ascii")
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/germany/land_use_2/states.nc", varname = i)
  landuse <- crop(landuse, e)
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, germany_ext)
  landuse <- resample(landuse, studyarea)
  writeRaster(landuse, paste0("Desktop/germany/land_use_2/land_use_2/",i,".asc"), format = "ascii", overwrite=TRUE)
}
primf <- raster("Desktop/germany/land_use_2/land_use_2/primf.asc")
plot(primf)
extent(primf)
e
res(primf)
secdf <- raster("Desktop/germany/land_use_2/land_use_2/secdf.asc")
plot(secdf)
plot(is.na(secdf))
extract(secdf,germany_finalish)
extract(is.na(secdf), germany_finalish)

v1 <- vifstep(landuse)
v2 <- vifcor(landuse, th= 0.7)
v1
v2

write.csv(germany_finalish, "Desktop/germany/Germany_final_2.csv", row.names = FALSE)

#### selecting random background points and removing the non applicable points ####

#### not needed ####
set.seed(1)
background <- sampleRandom(x=studyarea, size = 100, na.rm = T, sp = T)
plot(studyarea[[1]])
plot(background, add = T)
plot(germany_finalish, add =T, col ="red")
### ### 

#### splitting for test and train ####

smp_size <- floor (0.7 * nrow(germany_finalish))
set.seed(1)
train_ind <- sample(seq_len(nrow(germany_finalish)), size = smp_size)
Train <- germany_finalish[train_ind, ]
Test <- germany_finalish[-train_ind, ]
write.csv(Train, "Desktop/germany/Train_4.csv", row.names=FALSE)
write.csv(Test,"Desktop/germany/Test_4.csv", row.names = FALSE)

#### combining a few variables ####

forest1 <- raster("Desktop/germany/new_input_variables/secdf.asc")
forest2 <- raster("Desktop/germany/new_input_variables/primf.asc")
Forests <- mosaic(forest1,forest2, fun = sum) 
writeRaster(Forests, "Desktop/germany/new_input_variables/Forests.asc", overwrite =TRUE)                  
                  
nonf1 <- raster("Desktop/germany/new_input_variables/primn.asc")
nonf2 <- raster("Desktop/germany/new_input_variables/secdn.asc")
Non_forested_land <- mosaic(nonf1,nonf2, fun = sum)
writeRaster(Non_forested_land, "Desktop/germany/new_input_variables/Non_forested_land.asc" , overwrite =TRUE)
                  
crop1 <- raster("Desktop/germany/new_input_variables/c4per.asc")
crop2 <- raster("Desktop/germany/new_input_variables/c4ann.asc")
crop3 <- raster("Desktop/germany/new_input_variables/c3per.asc")
crop4 <- raster("Desktop/germany/new_input_variables/c3nfx.asc")
crop5 <- raster("Desktop/germany/new_input_variables/c3ann.asc")
Crops <- mosaic(crop1,crop2,crop3,crop4,crop5, fun=sum)
writeRaster(Crops, "Desktop/germany/new_input_variables/Crops.asc", overwrite =TRUE)


plot(Crops)
plot(germany_finalish, add = T, col = "black")

pred_map <- raster("Desktop/germany/Results_gemany/results_germany_1/Canis_lupus.asc")
plot(pred_map)

maxres_germany <- read.csv("Desktop/germany/Results_gemany/results_germany_1/maxentResults.csv", sep = ",")
head(maxres_germany)
pred_map_2 <- pred_map > maxres_germany $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2)

writeRaster(pred_map_2, "Desktop/Germany_suitability.asc", format = "ascii")

#### Making the bias file ####

germany_final_occ <- read.csv("Desktop/germany/Germany_final_2.csv", sep = ",")
head(germany_final_occ)
germany_final_occ$species<-NULL
germany_final_occ$optional<-NULL
head(germany_final_occ)

list_asc <- list.files("Desktop/germany/all_input_germany/", pattern = "*.asc", full.names = TRUE)
library(raster)
vaariables <- stack(list_asc)
germany.ras <- rasterize(germany_final_occ, vaariables, 1)

presences <- which(values(germany.ras) == 1)
pres.locs <- coordinates(germany.ras) [presences, ]
kde2d()
library("MASS")
dens <- kde2d(pres.locs [,1], pres.locs[,2], n = c(nrow(germany.ras), ncol(germany.ras)), lims = c(extent(vaariables)[1], extent(vaariables)[2], extent(vaariables)[3], extent(vaariables)[4]))
den.ras <- raster(dens, vaariables)
den.ras2 <- resample(den.ras, vaariables)
plot(den.ras2)
writeRaster(den.ras2, "Desktop/germany/Bias_germany.asc", overwrite = TRUE)
##### background points ####

length(which((!is.na(values(subset(vaariables,1))))))

#### New variables to test in Maxent ####

bio1 <- raster("Desktop/germany/all_input_germany/Annual_mean_temp.asc")
m <- c(-Inf, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
bio <- reclassify(bio1, rclmat)
plot(bio)
freq(bio)
population_germany <- raster()
population_m <- raster("Desktop/germany/DEU_pop/deu_pop.gri")
population_m <- population_m / 1000 
population_m <- crop(population_m, bio)
population_m <- resample(population_m, bio)
plot(population_m)
population_m <- mask(population_m, germany_ext)
writeRaster(population_m, "Desktop/germany/Population_germany", format = "ascii", overwrite = TRUE)
plot(population_m)

water <- readOGR("Desktop/germany/DEU_wat/", "DEU_water_areas_dcw")

water <- rasterize(water, bio1, fun= 'last', background = NA, mask = TRUE, update = FALSE)
head(water)
water <- reclassify(water,rclmat)                   
freq(water)
plot(water, col = "blue")
bio
plot(bio)
water[is.na(water)] <- 0
water <- water * bio
water <- water * 10
plot(water)
freq(water)
###m2 <- c(-Inf,1.5,1, 1.5,Inf,0)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
water <- reclassify(water, rclmat2)
###plot(water)
freq(water)
res(bio)
res(water)
res(population_m)
extent(bio)
extent(water)
extent(population_m)

writeRaster(water, "Desktop/germany/Water.asc", format = "ascii", overwrite =TRUE)


road <- readOGR("Desktop/germany/DEU_rds/" , "DEU_roads")
road <- rasterize(road, bio1, fun= 'last', background = NA, mask = TRUE, update = FALSE)
road <- reclassify(road, rclmat)
road <- crop(road, bio)
road <- resample(road, bio)
road <- road * (-1000)
plot(road)
road[is.na(road)] <- 0 
road <- road + bio
plot(road)
writeRaster(road, "Desktop/germany/Roads.asc", format = "ascii")


#### Analysis of some predictor variables ####

range_land <- raster("Desktop/germany/all_input_germany/range_land.asc")
plot(range_land)
range_land <- range_land / (-70)
plot(range_land)
writeRaster(range_land, "Desktop/germany/all_input_germany/range_land.asc", format = "ascii", overwrite = TRUE)

urban_areas <- raster("Desktop/germany/all_input_germany/urban.asc")
plot(urban_areas)
urban_areas <- urban_areas * (10)
plot(urban_areas)
writeRaster(urban_areas, "Desktop/germany/all_input_germany/Urban_areas.asc", format = "ascii", overwrite = TRUE)

non_forested_land <- raster("Desktop/germany/all_input_germany/non_forested_land.asc")
plot(non_forested_land)
non_forested_land <- non_forested_land / 5
plot(non_forested_land)
writeRaster(non_forested_land,"Desktop/germany/all_input_germany/non_forested_land.asc", format ="ascii", overwrite = TRUE)

Crops <- raster("Desktop/germany/all_input_germany/Crops.asc")
plot(Crops)
Crops <- Crops  / 8
plot(Crops)
Crops <- Crops * 2
plot(Crops)
writeRaster(Crops, "Desktop/germany/all_input_germany/Crops.asc", format = "ascii", overwrite = TRUE)

Pasture <- raster("Desktop/germany/all_input_germany/pastr.asc")
plot(Pasture)
Pasture <- Pasture * 6
plot(Pasture)
writeRaster(Pasture, "Desktop/germany/all_input_germany/Pasture.asc", format ="ascii", overwrite = TRUE)

Forests <- raster("Desktop/germany/all_input_germany/Forests.asc")
plot(Forests)
Forests <- Forests / 15
plot(Forests)
writeRaster(Forests, "Desktop/germany/all_input_germany/Forests.asc", format ="ascii", overwrite =TRUE)

Population <- raster("Desktop/germany/all_input_germany/Population_germany.asc")
plot(Population)
Population <- Population * (-1)
plot(Population)
writeRaster(Population, "Desktop/germany/all_input_germany/Population.asc", format = "ascii", overwrite = TRUE)

#### plotting suitability ####

pred_map <- raster("Desktop/germany/Results_germany_2/Results_germany_4/Canis_lupus.asc")
plot(pred_map)

maxres_germany <- read.csv("Desktop/germany/Results_germany_2/Results_germany_4/maxentResults.csv", sep = ",")
head(maxres_germany)
pred_map_2 <- pred_map > maxres_germany $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(pred_map_2)

writeRaster(pred_map_2,"Desktop/germany/Results_germany_2/Germany_suitability.asc", fomat ="asci", overwrite =TRUE)
