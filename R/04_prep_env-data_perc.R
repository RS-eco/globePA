#' ---
#' title: "Global Analysis of Protected Areas - Data preparation"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA"
setwd(workdir)

#Automatically install required packages, which are not yet installed
packages <- c("raster", "tidyverse", "terra", "binr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Define IUCN category split
iucn_cat2 <- c("I-II", "III-IV", "V-VI", "Not designated", "Total")

########################################

#' ## Environmental data

#' Summarise global temperature and precipitation data
files <- list.files("extdata", pattern="bio_.*\\_wm.nc", full.names=T)
summary_wc <- lapply(files, function(k){
  dat <- terra::rast(k)
  x <- values(dat)
  x <- na.omit(x); gc()
  bins_perc <- binr::bins(x, target.bins=100, max.breaks=100)
  bins_perc_val <- binr::bins.getvals(bins_perc)
  rclmat <- data.frame(var=sub("_wm.nc", "", basename(k)), 
                       x=as.numeric(c(attr(bins_perc_val, "binlo")[1:(length(bins_perc$binct))])), 
                       y=as.numeric(c(attr(bins_perc_val, "binlo")[2:(length(bins_perc$binct))],attr(bins_perc_val, "binhi")[length(bins_perc$binct)])), 
                       z=1:length(bins_perc$binct))
  rclmat[nrow(rclmat),3] <- rclmat[nrow(rclmat),3]+1
  return(rclmat)
})
gc()
summary_wc2 <- dplyr::bind_rows(summary_wc)
saveRDS(summary_wc2, file="data/summary_wc_perc_optim.rds", compress="xz")

#' Reclassify data into bins
files <- list.files("extdata", pattern="bio_.*\\_wm.nc", full.names=T)
m_ee <- readRDS("data/summary_wc_perc_optim.rds")
m_ee[1,2] <- floor(m_ee[1,2])
m_ee[102,2] <- floor(m_ee[102,2])
m_ee[203,2] <- floor(m_ee[203,2])
m <- m_ee %>% group_split(var)

# Reclassify data
lapply(1:length(files), function(x){
  dat <- terra::rast(files[x])
  rc2 <- terra::classify(dat, rcl=as.matrix(m[[x]][,2:4]), right=T, include.lowest=T)
  terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[x]))), 
                     compression=9, overwrite=T); rm(dat); gc()
})

dat <- terra::rast(paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[1]))))
dat

########################################

#' ## Topography data

(files <- list.files("extdata", pattern="GMTEDmn_wm.nc", full.names=T))

#' Summarise topography data
dat <- terra::rast(files[1])
x <- values(dat)
x <- na.omit(x); gc()
bins_perc <- binr::bins(x, target.bins=100, max.breaks=100)
bins_perc_val <- binr::bins.getvals(bins_perc)
rclmat <- data.frame(var=sub("_wm.nc", "", basename(files[1])), 
                     x=as.numeric(c(attr(bins_perc_val, "binlo")[1:(length(bins_perc$binct))])), 
                     y=as.numeric(c(attr(bins_perc_val, "binlo")[2:(length(bins_perc$binct))],attr(bins_perc_val, "binhi")[length(bins_perc$binct)])), 
                     z=1:length(bins_perc$binct))
rclmat[nrow(rclmat),3] <- rclmat[nrow(rclmat),3]+1
saveRDS(rclmat, file="data/summary_earthenv_perc_optim.rds", compress="xz")

#' Reclassify data into bins
(files <- list.files("extdata", pattern="GMTEDmn_wm.nc", full.names=T))
m_ee <- readRDS("data/summary_earthenv_perc_optim.rds")
dat <- terra::rast(files[1])
rc2 <- terra::classify(dat, rcl=as.matrix(m_ee[,2:4]), right=T, include.lowest=T)
terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[1]))), 
                   compression=9, overwrite=T)
rc2

dat <- raster::raster(paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[1]))))
dat

########################################

#' ## Marspec

### Summarise raster stack
files <- c("extdata/bathy_30s_wm.nc", list.files("extdata", pattern="biogeo.*\\_wm.nc", full.names=T))
summary_marspec <- lapply(files, function(k){
  dat <- terra::rast(k)
  x <- values(dat)
  x <- na.omit(x); gc()
  bins_perc <- binr::bins(x, target.bins=100, max.breaks=100)
  bins_perc_val <- binr::bins.getvals(bins_perc)
  rclmat <- data.frame(var=sub("_wm.nc", "", basename(k)),
                       x=as.numeric(c(attr(bins_perc_val, "binlo")[1:(length(bins_perc$binct))])), 
                       y=as.numeric(c(attr(bins_perc_val, "binlo")[2:(length(bins_perc$binct))],attr(bins_perc_val, "binhi")[length(bins_perc$binct)])), 
                       z=1:length(bins_perc$binct))
  rclmat[nrow(rclmat),3] <- rclmat[nrow(rclmat),3]+1
  return(rclmat)
})
summary_marspec2 <- dplyr::bind_rows(summary_marspec)
saveRDS(summary_marspec2, file="data/summary_marspec_perc_optim.rds", compress="xz")
gc()

# Reclassify

files <- c("extdata/bathy_30s_wm.nc", list.files("extdata", pattern="biogeo.*\\_wm.nc", full.names=T))
m_ee <- readRDS("data/summary_marspec_perc_optim.rds")
m_ee[1,2] <- m_ee[1,2]-1
m_ee[185,2] <- m_ee[185,2]-1
m_ee[335,2] <- m_ee[335,2]-1
m_ee[431,2] <- floor(m_ee[431,2])
m_ee[528,2] <- floor(m_ee[528,2])
m <- m_ee %>% group_split(var)

# Reclassify data
lapply(1:length(files), function(x){
  dat <- terra::rast(files[x])
  rc2 <- terra::classify(dat, rcl=as.matrix(m[[x]][,2:4]), right=T, include.lowest=T)
  terra::writeRaster(rc2, filename=paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[x]))), 
                     compression=9, overwrite=T); rm(dat); gc()
})

dat <- terra::rast(paste0("extdata/", sub("_wm.nc", "_perc.nc", basename(files[7]))))
dat

########################################