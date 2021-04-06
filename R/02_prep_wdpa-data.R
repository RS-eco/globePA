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
packages <- c("sp", "raster", "fasterize", "sf", "tidyverse", "SpaDES.tools")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="https://cloud.r-project.org"); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Define IUCN category split
iucn_cat2 <- c("I-II", "III-IV", "V-VI", "Not designated", "Total")

# Obtain world map
library(rnaturalearthhires)
data(countries10)
countries10 <- sf::st_as_sf(countries10)
outline <- sf::st_union(countries10)
outline_ter <- sf::st_crop(outline, extent(-180, 180, -56, 84))

outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
outline_ter_moll <- sf::st_transform(outline_ter, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(st_geometry(outline_ter_moll))

########################################

#' ## Get WDPA Data and subset by IUCN category for marine and terrestrial separately

#' Download Geodatabase of all PAs
#download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Jan2020-shapefile.zip",
#              destfile=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile.zip"))

# Downloaded 18th of January 2020

#' Unzip shapefile
#unzip(paste0(filedir, "WDPA/WDPA_Jan2020-shapefile.zip"), exdir=paste0(filedir, "WDPA"))

#' Divide protected areas in marine and terrestrial protected areas, always keep Coastal
#' and save new sf object as RDS

wdpa <- sf::st_read(dsn=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile-polygons.shp"))
wdpa_point <- sf::st_read(dsn=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile-points.shp"))

# Marine & Coastal
mpa <- wdpa[wdpa$MARINE %in% c(1,2),]
saveRDS(mpa, "extdata/WDPA_Jan2020_Marine_Total.rds", compress="xz")

mpa_point <- wdpa_point[wdpa_point$MARINE %in% c(1,2),]
saveRDS(mpa_point, "extdata/WDPA_Jan2020_Marine_Point.rds", compress="xz")

# Terrestrial & Coastal
pa <- wdpa[wdpa$MARINE %in% c(0,1),]
saveRDS(pa, "extdata/WDPA_Jan2020_Terrestrial_Total.rds", compress="xz")

pa_point <- wdpa_point[wdpa_point$MARINE %in% c(0,1),]
saveRDS(pa_point, "extdata/WDPA_Jan2020_Terrestrial_Point.rds", compress="xz")

rm(list=ls()); gc()
#' Restart R after this

# Process marine data
mpa <- readRDS("extdata/WDPA_Jan2020_Marine_Total.rds")
wdpa_point <- sf::st_read(dsn=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile-points.shp"))
mpa_point <- wdpa_point[wdpa_point$MARINE %in% c(1,2),]; rm(wdpa_point); gc()
lapply(iucn_cat2[1:4], function(k){
  if(!file.exists(paste0("extdata/WDPA_Jan2020_Marine_", sub(" ", "", k), ".rds"))){
    if(k == "I-II"){
      mpa_sub <- mpa[mpa$IUCN_CAT %in% c("Ia", "Ib", "II"),]; gc()
      mpa_point_sub <- mpa_point[mpa_point$IUCN_CAT %in% c("Ia", "Ib", "II"),]
    } else if(k == "III-IV"){
      mpa_sub <- mpa[mpa$IUCN_CAT %in% c("III", "IV"),]; gc()
      mpa_point_sub <- mpa_point[mpa_point$IUCN_CAT %in% c("III", "IV"),]
    } else if(k == "V-VI"){
      mpa_sub <- mpa[mpa$IUCN_CAT %in% c("V","VI"),]; gc()
      mpa_point_sub <- mpa_point[mpa_point$IUCN_CAT %in% c("V", "VI"),]
    } else if(k == "Not designated"){
      mpa_sub <- mpa[mpa$IUCN_CAT %in% c("Not Reported", "Not Applicable", "Not Assigned"),]; gc()
      mpa_point_sub <- mpa_point[mpa_point$IUCN_CAT %in% c("Not Reported", "Not Applicable", "Not Assigned"),]
    }
    saveRDS(mpa_sub, paste0("extdata/WDPA_Jan2020_Marine_", sub(" ", "", k), ".rds"), compress="xz"); rm(mpa_sub)
    saveRDS(mpa_point_sub, paste0("extdata/WDPA_Jan2020_Marine_point_", sub(" ", "", k), ".rds"), compress="xz"); rm(mpa_point_sub)
    invisible(gc())
  }
}); rm(mpa, mpa_point); gc()

# Process terrestrial data
pa <- readRDS("extdata/WDPA_Jan2020_Terrestrial_Total.rds")
wdpa_point <- sf::st_read(dsn=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile-points.shp"))
pa_point <- wdpa_point[wdpa_point$MARINE %in% c(0,1),]; rm(wdpa_point); gc()
lapply(iucn_cat2[1:4], function(k){
  if(!file.exists(paste0("extdata/WDPA_Jan2020_Terrestrial_", sub(" ", "", k), ".rds"))){
    if(k == "I-II"){
      pa_sub <- pa[pa$IUCN_CAT %in% c("Ia", "Ib", "II"),]; gc()
      pa_point_sub <- pa_point[pa_point$IUCN_CAT %in% c("Ia", "Ib", "II"),]
    } else if(k == "III-IV"){
      pa_sub <- pa[pa$IUCN_CAT %in% c("III", "IV"),]; gc()
      pa_point_sub <- pa_point[pa_point$IUCN_CAT %in% c("III", "IV"),]
    } else if(k == "V-VI"){
      pa_sub <- pa[pa$IUCN_CAT %in% c("V","VI"),]; gc()
      pa_point_sub <- pa_point[pa_point$IUCN_CAT %in% c("V", "VI"),]
    } else if(k == "Not designated"){
      pa_sub <- pa[pa$IUCN_CAT %in% c("Not Reported", "Not Applicable", "Not Assigned"),]; gc()
      pa_point_sub <- pa_point[pa_point$IUCN_CAT %in% c("Not Reported", "Not Applicable", "Not Assigned"),]
    }
    saveRDS(pa_sub, paste0("extdata/WDPA_Jan2020_Terrestrial_", sub(" ", "", k), ".rds"), compress="xz"); rm(pa_sub)
    saveRDS(pa_point_sub, paste0("extdata/WDPA_Jan2020_Marine_point_", sub(" ", "", k), ".rds"), compress="xz"); rm(pa_point_sub)
    invisible(gc())
  }
}); rm(pa, pa_point); gc()

########################################

#' ## Rasterize data according to area covered by each IUCN category

# Get coverage of mpa data
lapply(iucn_cat2, function(k){
  if(!file.exists(paste0("extdata/mpa_cov_", sub(" ", "", k), ".nc"))){
    if(file.exists(paste0("extdata/WDPA_Jan2020_Marine_", sub(" ", "", k), ".rds"))){
      mpa <- readRDS(paste0("extdata/WDPA_Jan2020_Marine_", sub(" ", "", k), ".rds"))
      
      # Specify raster resolution (0.1km)
      r <- raster::raster(nrow=216000, ncol=432000, xmn=-180, xmx=180, ymn=-90, ymx=90,
                          crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      r_sub <- raster::crop(r, as(mpa, "Spatial"))
      tmpdir <- file.path(tempdir(), "splitRaster")
      dir.create(tmpdir)
      r_sub <- SpaDES.tools::splitRaster(r_sub, nx=5, ny=5, path=file.path(tmpdir, "mpa_sub"))
      lapply(1:length(r_sub), function(l){
        if(!file.exists(paste0("data/mpa_cov_", sub(" ", "", k), "_", l, ".nc"))){
          mpa_cov <- fasterize::fasterize(mpa, r_sub[[l]])
          mpa_cov <- raster::aggregate(mpa_cov, fact=10, fun=sum, 
                                       filename=paste0("extdata/mpa_cov_", sub(" ", "", k), "_", l, ".nc"), 
                                       force_v4=TRUE, compression=9, overwrite=T) 
        }
      }); rm(mpa, r_sub); gc()
    }
  }
})

lapply(iucn_cat2, function(k){
  if(!file.exists(paste0("extdata/mpa_cov_", sub(" ", "", k), ".nc"))){
    mpa_files <- list.files(path="extdata", pattern=paste0("mpa_cov_", sub(" ", "", k), "_"), full.names=T)
    if(length(mpa_files) == 25){
      mpa_cov <- lapply(mpa_files, raster)
      mpa_cov <- do.call(raster::merge, c(mpa_cov, tolerance=0.5))
      r <- raster::raster(nrow=21600, ncol=43200, xmn=-180, xmx=180, ymn=-90, ymx=90,
                          crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      mpa_cov <- raster::extend(mpa_cov, r)
      mpa_cov <- raster::projectRaster(mpa_cov, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", 
                                       filename=paste0("extdata/mpa_cov_", sub(" ", "", k), ".nc"), 
                                       force_v4=TRUE, compression=9, overwrite=T); file.remove(mpa_files); rm(mpa_cov); gc()
    }
  }
})

dat <- raster::raster("extdata/mpa_cov_I-II.nc")
dat

lapply(iucn_cat2, function(k){
  if(!file.exists(paste0("extdata/pa_cov_",  sub(" ", "", k), ".nc"))){
    if(file.exists(paste0("extdata/WDPA_Jan2020_Terrestrial_",  sub(" ", "", k), ".rds"))){
      pa <- readRDS(paste0("extdata/WDPA_Jan2020_Terrestrial_",  sub(" ", "", k), ".rds"))
      # Specify raster resolution (1km)
      r <- raster::raster(nrow=216000, ncol=432000, xmn=-180, xmx=180, ymn=-90, ymx=90,
                          crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      r_sub <- raster::crop(r, as(pa, "Spatial"))
      tmpdir <- file.path(tempdir(), "splitRaster")
      dir.create(tmpdir)
      r_sub <- SpaDES.tools::splitRaster(r_sub, nx=5, ny=5, path=file.path(tmpdir, "pa_sub1"))
      lapply(1:length(r_sub), function(l){
        if(!file.exists(paste0("extdata/pa_cov_", sub(" ", "", k), "_", l, ".nc"))){
          pa_cov <- fasterize::fasterize(pa, r_sub[[l]])
          pa_cov <- raster::aggregate(pa_cov, fact=10, fun=sum, 
                                      filename=paste0("extdata/pa_cov_", sub(" ", "", k), "_", l, ".nc"), 
                                      force_v4=TRUE, compression=9, overwrite=T) 
        }
      }); rm(pa, r_sub); gc()
    }
  }
})

lapply(iucn_cat2, function(k){
  if(!file.exists(paste0("extdata/pa_cov_", sub(" ", "", k), ".nc"))){
    pa_files <- list.files(path="extdata", pattern=paste0("pa_cov_", sub(" ", "", k), "_"), full.names=T)
    if(length(pa_files) == 25){
      pa_cov <- lapply(pa_files, raster)
      pa_cov <- do.call(raster::merge, c(pa_cov, tolerance=0.5))
      r <- raster::raster(ncol=21600, nrow=43200, res=0.008333333, 
                          crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      pa_cov <- raster::extend(pa_cov, r)
      pa_cov <- raster::mask(pa_cov, outline_ter)
      pa_cov <- raster::projectRaster(pa_cov, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", 
                                      filename=paste0("extdata/pa_cov_", sub(" ", "", k), ".nc"), 
                                      force_v4=TRUE, compression=9, overwrite=T) ; file.remove(pa_files); rm(pa_cov); gc()
    }
  }
})

dat <- raster::raster("extdata/pa_cov_I-II.nc")
dat <- raster::raster("extdata/pa_cov_Total.nc")
dat

########################################

## Unify PA raster data to one common extent

mpa_files <- c("extdata/mpa_cov_I-II.nc", "extdata/mpa_cov_III-IV.nc","extdata/mpa_cov_V-VI.nc", 
               "extdata/mpa_cov_Notdesignated.nc", "extdata/mpa_cov_total.nc")
r <- raster::raster(nrow=17525, ncol=43222, xmn=-18045106, xmx=18045264, ymn=-9025552, ymx=9025198,
                    crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
r[] <- 0
bathy <- raster::raster("extdata/bathy_30s_wm.nc")

mpa_dat <- lapply(mpa_files, function(x){raster::raster(x)})
mpa_dat <- lapply(mpa_dat, function(x){raster::extend(x, r)})
mpa_dat <- lapply(mpa_dat, function(x){raster::resample(x, r)})
mpa_dat <- lapply(mpa_dat, function(x){raster::cover(x,r)})
mpa_dat <- lapply(mpa_dat, function(x){raster::mask(x,bathy)})
# This might lead to the fact that some variables to not have 100 bins!!!
lapply(1:length(mpa_files), function(x){raster::writeRaster(mpa_dat[[x]], filename=mpa_files[[x]], overwrite=T)})

pa_files <- c("extdata/pa_cov_I-II.nc", "extdata/pa_cov_III-IV.nc", "extdata/pa_cov_V-VI.nc",
              "extdata/pa_cov_Notdesignated.nc", "extdata/pa_cov_total.nc")
r <- raster::raster(nrow=14969, ncol=44012, xmn=-18045016, xmx=18044824, ymn=-6492085, ymx=8776295,
                    crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
r[] <- 0
bio01 <- raster::raster("extdata/wc2.0_bio_30s_01_wm.nc")

pa_dat <- lapply(pa_files, function(x){raster::raster(x)})
pa_dat <- lapply(pa_dat, function(x){raster::extend(x, r)})
pa_dat <- lapply(pa_dat, function(x){raster::resample(x, r)})
pa_dat <- lapply(pa_dat, function(x){raster::cover(x,r)})
pa_dat <- lapply(pa_dat, function(x){raster::mask(x,bio01)})
lapply(1:length(pa_files), function(x){raster::writeRaster(pa_dat[[x]], filename=pa_files[[x]], overwrite=T)})

file.remove(paste0("extdata/WDPA_Jan2020_Terrestrial_",  sub(" ", "", iucn_cat2), ".rds"))
file.remove(paste0("extdata/WDPA_Jan2020_Marine_",  sub(" ", "", iucn_cat2), ".rds"))

########################################