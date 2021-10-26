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
packages <- c("terra", "tidyverse")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Define IUCN category split
iucn_cat2 <- c("I-II", "III-IV", "V-VI", "Not designated", "Total")

########################################

#' ## Worldclim data

#' Reclassify data into bins
files <- list.files("extdata", pattern="bio_.*\\_wm.nc", full.names=T)

# Create matrix for relassfication
m <- c(list(as.matrix(data.frame(x=seq(-55, 39, by=1), y=seq(-54, 40, by=1), z=1:95)),
            as.matrix(data.frame(x=seq(-1, 2430, by=30), y=seq(30, 2460, by=30), z=1:82)),
            as.matrix(data.frame(x=seq(-1, 129, by=1), y=seq(0, 130, by=1), z=1:131)),
            as.matrix(data.frame(x=seq(0, 11900, by=100), y=seq(100, 12000, by=100), z=1:120)),
            as.matrix(data.frame(x=seq(0, 247.5, by=2.5), y=seq(2.5, 250, by=2.5), z=1:100))))

# Matrix was manually produced from min/max values shown by reading 
# in the raster-layer and printing the object:
dat <- terra::rast(files[1])
dat

# Reclassify data
lapply(1:length(files), function(x){
  dat <- terra::rast(files[x])
  rc2 <- terra::classify(dat, rcl=m[[x]])
  terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[x]))), 
                     compression=9, overwrite=T)
})

dat <- terra::rast(paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[1]))))
dat

########################################

#' ## Topography data

(files <- list.files("extdata", pattern="mn_GMTED.*\\_wm.nc", full.names=T))
dat <- terra::rast(files[1])

#' Reclassify data into bins
m <- as.matrix(data.frame(x=seq(-500, 9900, by=100), y=seq(-400, 10000, by=100), z=1:105))
rc2 <- terra::classify(dat, rcl=m, right=T, include.lowest=T)
terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[1]))), 
                   compression=9, overwrite=T)

#' Check data
dat <- terra::rast(paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[1]))))
dat

########################################

#' ## Marspec data

# Reclassify
files <- c("extdata/bathy_30s_wm.nc", list.files("extdata", pattern="biogeo.*\\_wm.nc", full.names=T))
m <- c(list(as.matrix(data.frame(x=seq(-12000, -100, by=100), y=seq(-11900, 0, by=100), z=1:120)),
            as.matrix(data.frame(x=seq(-100, 4400, by=100), y=seq(0, 4500, by=100), z=1:46)),
            as.matrix(data.frame(x=seq(-100, 4400, by=100), y=seq(0, 4500, by=100), z=1:46)),
            as.matrix(data.frame(x=seq(-10000, 1990000, by=10000), y=seq(0, 2000000, by=10000), z=1:201)),
            as.matrix(data.frame(x=seq(-500, 3400, by=100), y=seq(-400, 3500, by=100), z=1:40)), 
            as.matrix(data.frame(x=seq(-500, 3400, by=100), y=seq(-400, 3500, by=100), z=1:40)),
            as.matrix(data.frame(x=seq(-10000, 1240000, by=10000), y=seq(0, 1250000, by=10000), z=1:126))))
lapply(1:length(files), function(x){
  dat <- terra::rast(files[x])
  rc2 <- terra::classify(dat, rcl=m[[x]], right=T, include.lowest=T)
  terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[x]))), 
                     compression=9, overwrite=T)
  rm(dat, rc2); gc()
})

#' Check data
dat <- terra::rast(paste0("extdata/", sub("_wm.nc", "_bins.nc", basename(files[1]))))
dat

########################################