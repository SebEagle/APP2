setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/")
library(rgdal)
library(raster)
library(RStoolbox)
# raster_data <- brick("LS8/LC81730382015152LGN00_stack_atm_clip.tif")
# setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/trainingdata/git/APP2/")
# masking <- rgdal::readOGR("shapefiles/Proposed buffer area", "azraq_buffer_new")
# plot(raster_data)
# raster_data <- crop(raster_data, masking)
# raster_data <- mask(raster_data, masking)
# 
# writeRaster(raster_data,filename = "/home/sebi/Schreibtisch/Semester_2/APP2_Christian/LS8/subset.tif")

raster_data <- brick("LS8/subset.tif")
#read shapefile
setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/trainingdata/git/APP2/")
td <- rgdal::readOGR("shapefiles", "training_data_cropped")
#td <- rgdal::readOGR("trainingdata", "training_data_220518_mod")
sc <- superClass(raster_data, trainData=td, responseCol = "CLC", model = "rf")
setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/")
writeRaster(sc$map, "classification/class_050618.tif", format="GTiff", overwrite=TRUE)
setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/trainingdata/git/APP2/")
writeRaster(sc$map, "classification/class_050618.tif", format="GTiff", overwrite=TRUE)
#sc1 <- superClass(raster_data, trainData=td, responseCol = "id", model = "svmLinear")

plot(sc$map)

#split training data
sc_2 <- superClass(raster_data, trainData =td, responseCol = "CLC", trainPartition = 0.7)
sc_2$validation$performance$byClass
sc_2$validation$performance$table
sc_2$validation$performance$overall


arg <- list(at = seq(1,3,1), labels = c("forest", "water", "deforestation"))
color <- c("green", "blue", "brown")
plot(sc$map, col = color, axis.arg = arg)


# models <-  list("ada", "randomGLM", "xlm", "lars", "mxnet", "ranger", "svmLinear")
models <-  c("rf", "svmLinear")
for (i in models){
  sc <- superClass(raster_data, trainData =td, model = i, responseCol = "id")
  plot(sc$map, col = color, axis.arg = arg, main = i)
  if (i == 1){
    results <- sc$map
  } else {
    results <- stack(results, sc$map)
  }
}

results <- stack(sc$map, sc1$map)

##validation of classification
#Entropy
plot(RStoolbox::rasterEntropy(results))


#separated validation data
testd <- rgdal::readOGR("vector_data", "validationdata")
sc <- superClass(raster_data, trainData = td, valData = testd, responseCol = "id")

sc_2$validation$performance$byClass

# validation of existing map with reference data
validateMap(sc$map, valData = testd, responseCol = "id", mode = "classification")










setwd("/home/sebi/Schreibtisch/Semester_2/APP2_Christian/")
classification <- raster("classification/class_050618.tif")
classification@data@attributes

Test_map <- classification
Test_map[Test_map==2 | Test_map==3] <- 1
Test_map[Test_map==5 | Test_map==7 | Test_map==8] <- 6
Test_map[Test_map==12 | Test_map==13] <- 6

unique(Test_map)
Test_map[Test_map==4] <- 2
Test_map[Test_map==6] <- 3
Test_map[Test_map==9] <- 4
Test_map[Test_map==10] <- 5
Test_map[Test_map==11] <- 6

unique(Test_map)

plot(Test_map)
writeRaster(Test_map, "classification/class_aggregated.tif", format="GTiff", overwrite=TRUE)

