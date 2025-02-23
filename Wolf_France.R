### Loading required packages ####

library("raster")
library("rgdal")
library("ncdf4")
library("dismo")
library("rgeos")
library("sp")
install.packages("MASS")
library("MASS")
### Load and stack climatic variables ####

climate <- stack(paste0("Desktop/france_only/climate/wc2.1_5m_bio/", "wc2.1_5m_bio_", 1:19, ".tif"))
plot(climate[[1]])

### Load occurrence data to start cleaning ####

### removing weird coordinates ####
france <- read.csv("Desktop/france_only/france_only.csv", header = TRUE , sep = ";")
head(france)
france_clean <- subset(france, (!is.na(latitude)) & (!is.na(longitude)))
cat(nrow(france) - nrow(france_clean), "had wrong or erroneous coordinates")

### removing some duplicates ####

dups <- duplicated(france_clean[c("latitude", "longitude")])
france_nondup <- france_clean[!dups, ]
cat(nrow(france_clean) - nrow(france_nondup), "duplicate records were removed")

### write.csv(france_nondup, "Desktop/france_nondup.csv", row.names = FALSE)
#### converting the data into spatial data for visual inspectionn ####

coordinates(france_nondup) <- ~longitude + latitude
plot(climate[[1]])
plot(france_nondup, add = TRUE)

#### If need, run the code to remove points that are outside your study area roughly ###
#### germany_nondup <- germany_nondup[which(germany_nondup$longitude > 0 & germany_nondup$longitude <- 30), ]

#### making 1 occurrence point per pixel ####

cells <- cellFromXY(climate[[1]], france_nondup)
dups <- duplicated(cells)
france_finalish <- france_nondup[!dups, ]
cat(nrow(france_nondup) - nrow(france_finalish), "points were removed, that is awesome!")

#### plotting the new data on the map ####
plot(climate[[1]])
plot(france_finalish, add = TRUE, col = "black")

#### setting up the bufer polygon around occurrence points ####
### helps to avoid sampling from a broad background ###
proj4string(climate)
proj4string(france_finalish)
proj4string(france_finalish)<- crs("+init=epsg:4327")

france_buffer <- buffer(france_finalish, 1)

### proj4string(germany_finalish) <- crs("+init=epsg:4326")
### proj4string(germany_finalish)

plot(climate[[1]])
plot(france_finalish, add = T, col = "black")
plot(france_buffer, add = T, col ="red")
plot(france_buffer)
#### extract the extent and crop all environmenal vriables ####

e <- extent(france_buffer)
e <- c(-4.6,8.5,42.6,50.3)
studyarea1 <- crop(climate[[1]], e)
plot(studyarea1)
fr <- readOGR("Desktop/france_only/FRA_adm/", "FRA_adm0")
studyarea<- mask(studyarea,fr) 
plot(studyarea2)
extent(fr)
studyarea <- crop(climate, e)
e <- extent(fr)

#### check snap function (outwards)  ###

climate
### Optional 
# studyarea <- mask(studyarea,germany_buffer)
###
names(studyarea)
names(studyarea) <- paste0("bio", 1:19)
writeRaster(studyarea, paste0("Desktop/france_only/climate/climate_france/", names(studyarea)), bylayer = TRUE, format = "ascii", overwrite = TRUE)

#### testing the variables ####

bio1 <- raster("Desktop/france_only/n_input_varibales_france/Annual_mean_temp.asc")
plot(bio1)
res(bio1)
extent(bio1)
e
plot(france_buffer, add = T, col ="red")
plot(is.na(bio1))
extract(bio1,france_finalish)
extract(is.na(bio1),france_finalish)
plot(france_finalish, add = T, col = "black")

#### do the same for landuse variables ####

landuse <- brick("Desktop/france_only/landuse_france/states.nc", varname ="range")
plot(landuse[[1166]])
landuse <- brick("Desktop/states.nc", varname ="primf")
landuse <- crop(landuse,e)
landuse <- mean(landuse[[1150:1165]])
landuse <- resample(landuse,studyarea)
landuse <- mask(landuse, fr)
writeRaster(landuse, "Desktop/france_only/landuse_france/primf.asc", format = "ascii")
layers <- c("primf","primn","secdf","secdn","urban","c3ann","c4ann","c3per","c4per","c3nfx","pastr","range","secmb","secma")
for(i in layers)
{
  landuse <- brick("Desktop/france_only/landuse_france/states.nc", varname = i)
  landuse <- crop(landuse, e)
  landuse <- mean(landuse[[1150:1166]])
  landuse <- mask(landuse, fr)
  landuse <- resample(landuse, studyarea)
  writeRaster(landuse, paste0("Desktop/france_only/landuse_france/landuse_france/",i,".asc"), format = "ascii", overwrite=TRUE)
}
primf <- raster("Desktop/france_only/landuse_france/landuse_france/primf.asc")
plot(primf)
res(primf)
extent(primf)
secdf <- raster("Desktop/france_only/input_varibales_france/Forests.asc")
plot(secdf)
secma <- raster("Desktop/france_only/landuse_france/landuse_france/secma.asc")
plot(secma)
e
plot(is.na(secdf))
extract(secdf, france_finalish)
extract(is.na(secdf), france_finalish)

write.csv(france_finalish, "Desktop/france_only/France_final_occ.csv", row.names = FALSE)
#### selecting random background points and removing the non apllicable points ####

#### not needed ####
set.seed(1)
background <- sampleRandom(x=studyarea, size = 100, na.rm = T, sp = T)
plot(studyarea[[1]])
plot(background, add = T)
plot(germany_finalish, add =T, col ="red")
### ### 

#### splitting for test and train ####

smp_size <- floor (0.6 * nrow(france_finalish))
set.seed(123)
train_ind <- sample(seq_len(nrow(france_finalish)), size = smp_size)
Train <- france_finalish[train_ind, ]
Test <- france_finalish[-train_ind, ]
write.csv(Train, "Desktop/france_only/Train_1.csv", row.names=FALSE)
write.csv(Test,"Desktop/france_only//Test_1.csv", row.names = FALSE)

#### Combining the variables ####

crop1 <- raster("Desktop/france_only/n_input_varibales_france/c3ann.asc")
crop2 <- raster("Desktop/france_only/n_input_varibales_france/c3nfx.asc")
crop3 <- raster("Desktop/france_only/n_input_varibales_france/c3per.asc")
crop4 <- raster("Desktop/france_only/n_input_varibales_france/c4ann.asc")
crop5 <- raster("Desktop/france_only/n_input_varibales_france/c4per.asc")
Crops <- mosaic(crop5,crop4,crop3,crop2,crop1, fun= sum)
plot(Crops)
writeRaster(Crops, "Desktop/france_only/n_input_varibales_france/Crops.asc")


forest1 <- raster("Desktop/france_only/n_input_varibales_france/secdf.asc")
froest2 <- raster("Desktop/france_only/n_input_varibales_france/primf.asc")
Forstes <- mosaic(forest1,froest2, fun=sum)
plot(Forstes)
writeRaster(Forstes, "Desktop/france_only/n_input_varibales_france/Forests.asc")


non_f1 <- raster("Desktop/france_only/n_input_varibales_france/secdn.asc")
nonf_2 <- raster("Desktop/france_only/n_input_varibales_france/primn.asc")
Non_forested_land <- mosaic(non_f1,nonf_2, fun=sum)
plot(Non_forested_land)
writeRaster(Non_forested_land, "Desktop/france_only/n_input_varibales_france/Non_forested_land.asc")

pred_france <- raster("Desktop/france_only/results_france/results_france_2/Canis_lupus.asc")
plot(pred_france)

pred_france2 <- read.csv("Desktop/france_only/results_france/results_france_2/maxentResults.csv", sep = ",")
suitability_france <- pred_france > pred_france2 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_france)


pred_france_60 <- raster("Desktop/france_only/results_france/results_france_5/Canis_lupus.asc")
plot(pred_france_60)

pred_france_60_1 <- read.csv("Desktop/france_only/results_france/results_france_5/maxentResults.csv", sep = ",")
suitability_france_1 <- pred_france_60 > pred_france_60_1 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_france_1)

A_france <- freq(suitability_france_1, value = 1) / (freq(suitability_france_1, value = 1) + freq(suitability_france_1, value = 0))
Test_france <- read.csv("Desktop/france_only/Test_1.csv", sep = ",")
head(Test_france)
Test_france$species <- NULL
Test_france$optional <- NULL
head(Test_france)
Test_france_results <- extract(suitability_france_1, Test_france)

B_france <- sum(Test_france_results == 1, na.rm = TRUE)
C_france <- length(Test_france_results)

success <- B_france/C_france
success
binom.test(B_france,C_france, p= A_france, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)


#### Making a bias file ####

France_final_occ <- read.csv("Desktop/france_only/France_final_occ.csv", sep = ",")
head(France_final_occ)
France_final_occ$optional <- NULL
France_final_occ$species <-NULL
head(France_final_occ)

bioa <- raster("Desktop/france_only/n_input_varibales_france/Annual_mean_temp.asc")
biob <- raster("Desktop/france_only/n_input_varibales_france/Annual_ppt.asc")
bio_c <- raster("Desktop/france_only/n_input_varibales_france/Crops.asc")
biod <- raster("Desktop/france_only/n_input_varibales_france/Forests.asc")
bioe <- raster("Desktop/france_only/n_input_varibales_france/Non_forested_land.asc")
biof <- raster("Desktop/france_only/n_input_varibales_france/pastr.asc")
biog <- raster("Desktop/france_only/n_input_varibales_france/range.asc")
bioh <- raster("Desktop/france_only/n_input_varibales_france/urban.asc")
bioi <- raster("Desktop/france_only/n_input_varibales_france/Temp_annual_range.asc")

variables <- stack(bioa,biob,bio_c,biod,bioe,biof,biog,bioh,bioi)
variables

france.ras <- rasterize(France_final_occ, variables, 1)

presences <- which(values(france.ras) == 1)
pres.locs <- coordinates(france.ras) [presences, ]
kde2d()
dens <- kde2d(pres.locs [,1], pres.locs[,2], n = c(nrow(france.ras), ncol(france.ras)), lims = c(extent(variables)[1], extent(variables)[2], extent(variables)[3], extent(variables)[4]))
den.ras <- raster(dens, variables)
den.ras2 <- resample(den.ras, variables)
plot(den.ras2)
writeRaster(den.ras2, "Desktop/france_only/Bias_france.asc", ovewrite = TRUE)

france_final_occ <- read.csv("Desktop/france_only/France_final_occ.csv", sep = ",")
head(france_final_occ)
france_final_occ$species<-NULL
france_final_occ$optional<-NULL
head(france_final_occ)

list_asc <- list.files("Desktop/france_only/aall_input_france/", pattern = ".asc", full.names = TRUE)
list_asc
library(raster)
s <- stack(list_asc)
france.rast <- rasterize(france_final_occ, s, 1)
presences <- which(values(france.rast) == 1)
pres.locs <- coordinates(france.rast) [presences, ]
dens <- kde2d(pres.locs [,1], pres.locs[,2], n = c(nrow(france.rast), ncol(france.rast)), lims = c(extent(s)[1], extent(s)[2], extent(s)[3], extent(s)[4]))
den.ras <- raster(dens, s)
den.ras2 <- resample(den.ras, s)
plot(den.ras2)
writeRaster(den.ras2, "Desktop/france_only/Bias.france_2.asc", format ="ascii", overwrite = TRUE)

#### check how many background points can be used ####

length(which((!is.na(values(subset(s,1))))))

#### Making new variables to test in maxent #### 

library(raster)
library(rgdal)

bio1 <- raster("Desktop/france_only/aall_input_france/Annual_mean_temp.asc")
m <- c(-Inf, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
bio <- reclassify(bio1, rclmat)
plot(bio)
freq(bio)
population_france <- raster()
population_m <- raster("Desktop/france_only/FRA_pop/fra_pop.gri")
population_m <- population_m / 1000
population_m <- crop(population_m, bio)
population_m <- resample(population_m, bio)
plot(population_m)
writeRaster(population_m, "Desktop/france_only/All_new_input_variables/Population.asc", format = "ascii", overwrite = TRUE)
plot(population_m)
poplulation_france <- readOGR(dsn = dsn , "Desktop/france_only/FRA_pop/", "fra_pop")


water <- readOGR("Desktop/france_only/FRA_wat/", "FRA_water_areas_dcw")
water <- rasterize(water, bio1, fun= 'last', background = NA, mask = TRUE, update = FALSE)
water <- reclassify(water,rclmat)                   
freq(water)
plot(water, col = "blue")
water[is.na(water)] <- 0
water <- water + bio
plot(water)
freq(water)
m2 <- c(-Inf,1.5,1, 1.5,Inf,0)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
water <- reclassify(water, rclmat2)
plot(water)
freq(water)
res(bio)
res(water)
res(population_m)
extent(bio)
extent(water)
extent(population_m)

roads <- readOGR("Desktop/france_only/FRA_rds/" , "FRA_roads")
plot(roads)
roads <- rasterize(roads, bio1, fun='last', background=NA, mask=TRUE, update=FALSE)
roads <- reclassify(roads, rclmat)
roads <- crop(roads, e)
extent(roads)
e <- extent(bio)
roads <- resample(roads, bio)
roads <- roads *(-1000)
plot(roads)
roads[is.na(roads)] <- 0 
roads <- road + bio
plot(roads)
writeRaster(roads, "Desktop/france_only/All_new_input_variables/Roads_1.asc", format ="ascii", overwrite = TRUE)

writeRaster(roads,"Desktop/Roads_2.asc", format = "ascii")
writeFormats()
freq(roads)
plot(roads, col = "black")
roads <- roads * -1
plot(roads, col = "black")
roads[is.na(roads)] <- 0
roads <- roads + bio
plot(roads)
roads <- reclassify(roads, rclmat2)
plot(roads)
frequency(roads)

writeRaster(water, "Desktop/france_only/france_water/Water.asc", format = "ascii")
writeRaster(population_m, "Desktop/france_only/france_pop/Populations.asc", format = "ascii")
writeRaster(roads, "Desktop/france_only/france_roads/Roads_1.asc",format = "ascii", overwrite =TRUE)


ex <- extent(bio1)
ex

#### Rasteizations for qgis ####

point_france_raster <- rasterize(France_final_occ, bio1, fun = 'last', background = NA, mask = TRUE, update = FALSE)
writeRaster(point_france_raster, "Desktop/point_france_raster.asc", format = "ascii")

france_shape <- readOGR("Desktop/france_only/FRA_adm/", "FRA_adm1")
france_shape <- rasterize(france_shape, bio1, fun = 'last', background = NA, mask = TRUE, update = FALSE)
writeRaster(france_shape, "Desktop/france_shape_raster.asc", format = "ascii")

new_road <- raster("Desktop/qgis_crap/Try_R_prox_france.gri")
new_road
plot(new_road)
new_road <- reclassify(new_road, rclmat)
new_road <- crop(new_road, bio)
new_road <- resample(new_road, bio)
new_road <- new_road * -1
plot(new_road)
new_road[is.na(new_road)] <- 0 
new_road <- new_road + bio
plot(new_road)
writeRaster(new_road, "Desktop/france_only/france_roads/New_road.asc", format = "ascii")


#### Analysing few input variables #### 

Crops <- raster("Desktop/france_only/aall_input_france/Crops.asc")
plot(Crops)
Crops <- Crops * 10
plot(Crops)
writeRaster(Crops,"Desktop/france_only/All_new_input_variables/Crops.asc", format = "ascii", overwrite =TRUE)

Forests <- raster("Desktop/france_only/aall_input_france/Forests.asc")
plot(Forests)
Forests <- Forests * 10
plot(Forests)
writeRaster(Forests, "Desktop/france_only/All_new_input_variables/Forests.asc", format = "ascii", overwrite = TRUE)

Non_forested_land <- raster("Desktop/france_only/aall_input_france/Non_forested_land.asc")
plot(Non_forested_land)
Non_forested_land <- Non_forested_land * 10
plot(Non_forested_land)
writeRaster(Non_forested_land, "Desktop/france_only/All_new_input_variables/Non_forested_land.asc", format = "ascii", overwrite = TRUE)

Pasture <- raster("Desktop/france_only/aall_input_france/pastr.asc")
plot(Pasture)
Pasture <- Pasture * 10
plot(Pasture)
writeRaster(Pasture, "Desktop/france_only/All_new_input_variables/Pasture.asc", format = "ascii", overwrite = TRUE)

Range_land <- raster("Desktop/france_only/aall_input_france/range.asc")
plot(Range_land)
Range_land <- Range_land / 2
plot(Range_land)
writeRaster(Range_land, "Desktop/france_only/All_new_input_variables/Range_land.asc", format = "ascii", overwrite = TRUE)

Population <- raster("Desktop/france_only/All_new_input_variables/Population.asc")
plot(Population)
Population <- Population /10
plot(Population)
writeRaster(Population, "Desktop/france_only/All_new_input_variables/Population.asc", format = "ascii", overwrite = TRUE)

Roads <- raster("Desktop/france_only/All_new_input_variables/Roads_1.asc")
plot(Roads)
Roads <- Roads * (-1)
plot(Roads)
writeRaster(Roads, "Desktop/france_only/All_new_input_variables/Roads.asc", format = "ascii", overwrite = TRUE)

Urban_areas <- raster("Desktop/france_only/All_new_input_variables/Urban_areas.asc")
plot(Urban_areas)
Urban_areas <- Urban_areas / 5
plot(Urban_areas)
writeRaster(Urban_areas, "Desktop/france_only/All_new_input_variables/Urban_areas.asc", format = "ascii", overwrite = TRUE)

#### plotting suitability ####

pred_france <- raster("Desktop/france_only/results_france_2/Results_france_7/Canis_lupus.asc")
plot(pred_france)

pred_france2 <- read.csv("Desktop/france_only/results_france_2/Results_france_7/maxentResults.csv", sep = ",")
suitability_france <- pred_france > pred_france2 $Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
plot(suitability_france)

writeRaster(suitability_france, "Desktop/france_only/results_france_2/France_suitability.asc", format = "ascii", overwrite = TRUE)
