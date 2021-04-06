#' ---
#' title: "Global Analysis of Protected Areas - Data preparation"
#' author: "RS-eco"
#' ---

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA"
setwd(workdir)

# Set file directory
filedir <- "F:/Data/"

#Automatically install required packages, which are not yet installed
packages <- c("raster", "fasterize", "tidyverse")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Define IUCN category split
iucn_cat2 <- c("I-II", "III-IV", "V-VI", "Not designated", "Total")

# Obtain world map
library(rnaturalearthhires); library(dplyr)
data(countries10)
countries10 <- sf::st_as_sf(countries10)
outline <- sf::st_union(countries10) %>% sf::st_cast("POLYGON")
outline_ter <- sf::st_crop(outline, extent(-180, 180, -56, 84))
plot(sf::st_geometry(outline_ter))

outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
outline_ter_moll <- sf::st_transform(outline_ter, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

########################################

#' ## Environmental data

#' Download Worldclim v2 Data
#download.file("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_bio.zip", 
#              paste(filedir, "Worldclim/wc2.0_30s_bio.zip", sep="/"), mode = "wb")

#' Unzip file
#unzip(paste(filedir, "Worldclim/wc2.0_30s_bio.zip", sep="/"))

# List environmental data files
files <- c(list.files(paste0(filedir, "Worldclim/wc2.0_30s_bio"), pattern="bio", 
                      full.names=T)[c(1,4,7,12,15)])

# Crop and re-project environmental data
lapply(1:length(files), function(x){
  dat <- raster::raster(files[x])
  dat <- raster::crop(dat, extent(-180, 180, -56, 84))
  dat <- raster::mask(dat, outline_ter)
  dat <- raster::projectRaster(dat, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  raster::writeRaster(dat, filename=paste0("extdata/", sub(".tif", "_wm.nc", basename(files[x]))), force_v4=TRUE, compression=9, overwrite=T)
  rm(dat); gc()
})

#' Read Topography data
#"https://data.earthenv.org/topography/slope_1KMmd_GMTEDmd.tif"
(files <- list.files(paste0(filedir, "/EarthEnv/topography"), pattern="mn_GMTED", full.names=T))
bio1 <- raster::raster(paste0(filedir, "/Worldclim/wc2.0_30s_bio/wc2.0_bio_30s_01.tif"))
bio1 <- raster::crop(bio1, extent(-180, 180, -56, 84))
bio1 <- raster::mask(bio1, outline_ter)
dat <- raster::raster(files[1])

#' Mask and re-project topography data
dat <- raster::mask(dat, bio1)
dat <- raster::projectRaster(dat, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
raster::writeRaster(dat, filename=paste0("extdata/", sub(".tif", "_wm.nc", basename(files[1]))), 
                    force_v4=TRUE, compression=9, overwrite=T)
rm(dat); gc()
#' Marspec files and info is available here:
# http://www.esapubs.org/archive/ecol/E094/086/#data

#files <- c("Monthly_Variables_30s.7z", "Sea_Ice_30s.7z", "biogeo01_07_30s.7", "biogeo08_17_30s.7z")

#' Download file
#download.file("http://www.esapubs.org/archive/ecol/E094/086/biogeo08_17_30s.7z",
#              destfile=paste0(filedir, "Marspec/biogeo08_17_30s.7z"))

#' Unzip file
#unzip(paste0(filedir, "Marspec/biogeo08_17_30s.7z"), exdir=paste0(filedir, "Marspec")
# I am not sure if you can unzip .7z files from within R

# Re-project marine data
files <- c(paste0(filedir, "/Marspec/bathymetry_30s/bathy_30s"),
           list.dirs(paste0(filedir, "/Marspec/biogeo08_17_30s"))[c(2,5,6,7,10,11)])
biogeo08 <- raster::raster(paste0(filedir, "Marspec/biogeo08_17_30s/biogeo08_30s"))
biogeo08[biogeo08 <= 400] <- NA
lapply(1:length(files), function(x){
  if(basename(files[x]) == "biogeo08_30s"){
    raster::projectRaster(biogeo08, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", 
                          filename=paste0("extdata/", basename(files[x]), "_wm.nc"), 
                          force_v4=TRUE, compression=9, overwrite=T)
  } else{
    dat <- raster::raster(files[x])
    dat <- raster::setValues(dat, dat[])
    dat <- raster::mask(dat, biogeo08)
    raster::projectRaster(dat, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", 
                          filename=paste0("extdata/", basename(files[x]), "_wm.nc"), 
                          force_v4=TRUE, compression=9, overwrite=T); rm(dat); gc()
  }
})

########################################