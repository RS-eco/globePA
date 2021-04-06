#' ---
#' title: "Global Analysis of Protected Areas - Data summary"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#Automatically install required packages, which are not yet installed
packages <- c("raster", "SpaDES.tools", "tidyverse")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA"
setwd(workdir)

########################################

## Prec, temp, elevation summary
rm(list=ls()); gc()
if(!file.exists("data/summary_all_ter_bins.rds")){
  # Worldclim & Earthenv
  wc_files <- list.files("extdata", pattern="wc2.0_bio.*\\_bins.nc", full.names=T)
  ee_files <- list.files("extdata", pattern="mn_GMTED.*\\_bins.nc", full.names=T)
  pa_files <- list.files("extdata/", pattern="^pa_cov.*\\.nc$", full.names=T)
  dat <- lapply(c(wc_files, ee_files, pa_files), function(x) raster::raster(x))
  tmpdir <- file.path(tempdir(), "splitRaster")
  dir.create(tmpdir)
  r_sub <- lapply(dat, function(y) SpaDES.tools::splitRaster(y, nx=7, ny=7, path=file.path(tmpdir, "r_sub")))
  rm(dat); gc()
  sum_dat <- lapply(1:49, function(j){
    if(!file.exists(paste0("data/summary_ter_bins_", j, ".rds"))){
      dat <- as.data.frame(raster::rasterToPoints(stack(lapply(r_sub,'[[', j))))
      colnames(dat) <- sub("pa_cov_", "", sub("_1KMmn_GMTEDmn", "", sub("wc2.0_bio_30s_", "bio", colnames(dat))))
      sum_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), list(~ replace_na(., 0))) %>% 
        group_by(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins) %>% 
        summarise_all(list(~mean(., na.rm=T)))
      n_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), list(~ replace_na(., 0))) %>% 
        group_by(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins) %>% 
        summarise(n=n()); rm(dat); invisible(gc())
      sum_dat <- full_join(sum_dat, n_dat)
      saveRDS(sum_dat, paste0("data/summary_ter_bins_", j, ".rds"), compress="xz")
    } else{
      sum_dat <- readRDS(paste0("data/summary_ter_bins_", j, ".rds"))
    }
    return(sum_dat)
  }); gc()
  sum_dat <- dplyr::bind_rows(sum_dat); gc()
  
  # Turn data into long format and save to file
  n_dat <- sum_dat %>% group_by(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins) %>% 
    tidyr::pivot_longer(c(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins), names_to="path", values_to="var") %>%
    group_by(path, var) %>% summarise(n=sum(n, na.rm=T))
  ter_dat <- sum_dat %>% group_by(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins) %>% 
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-n) %>% tidyr::pivot_longer(c(bio01_bins, bio04_bins, bio07_bins, bio12_bins, bio15_bins, elevation_bins), names_to="path", values_to="var") %>%
    group_by(path, var) %>% summarise_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~sum(., na.rm=T))) %>% full_join(n_dat) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(ter_dat, "data/summary_ind_ter_bins.rds", compress="xz"); rm(n_dat, ter_dat); gc()
  
  n_dat2 <- sum_dat %>% ungroup() %>% 
    dplyr::select(-c(bio04_bins, bio07_bins, bio15_bins)) %>%
    group_by(bio01_bins, bio12_bins, elevation_bins) %>% 
    summarise(n=sum(n, na.rm=T))
  ter_dat2 <- sum_dat %>% ungroup() %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-c(bio04_bins, bio07_bins, bio15_bins, n)) %>%
    group_by(bio01_bins, bio12_bins, elevation_bins) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% full_join(n_dat2) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(ter_dat2, "data/summary_all_ter_bins.rds", compress="xz"); file.remove(paste0("data/summary_ter_bins_", 1:49, ".rds"))
}; rm(list=ls()); gc()

########################################

## SST, salinity, depth summary

if(!file.exists("data/summary_all_mar_bins.rds")){
  # Stack mpa data to MARSPEC data
  files <- c(list.files("extdata/", pattern="bathy_30s_bins.nc$", full.names=T),
             list.files("extdata", pattern="biogeo.*\\_30s_bins.nc$", full.names=T))
  mpa_files <- list.files("extdata/", pattern="^mpa_cov.*\\.nc", full.names=T)
  dat <- lapply(c(files, mpa_files), function(x) raster::raster(x))
  tmpdir <- file.path(tempdir(), "splitRaster")
  dir.create(tmpdir)
  r_sub <- lapply(dat, function(y) SpaDES.tools::splitRaster(y, nx=7, ny=7, path=file.path(tmpdir, "r_sub")))
  rm(dat); gc()
  sum_dat <- lapply(1:49, function(j){
    if(!file.exists(paste0("data/summary_mar_bins_", j, ".rds"))){
      dat <- as.data.frame(raster::rasterToPoints(stack(lapply(r_sub,'[[', j))))
      colnames(dat) <- sub("mpa_cov_", "", sub("30s_", "", colnames(dat)))
      sum_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), funs(replace_na(., 0))) %>% 
        group_by(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, 
                 biogeo16_bins, biogeo17_bins) %>% 
        summarise_all(list(~mean(., na.rm=T)))
      n_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), funs(replace_na(., 0))) %>% 
        group_by(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, 
                 biogeo16_bins, biogeo17_bins) %>%
        summarise(n=n()); rm(dat); invisible(gc())
      sum_dat <- full_join(sum_dat, n_dat)
      saveRDS(sum_dat, paste0("data/summary_mar_bins_", j, ".rds"), compress="xz")
    } else{
      sum_dat <- readRDS(paste0("data/summary_mar_bins_", j, ".rds"))
    }
    return(sum_dat)
  }); rm(r_sub); gc()
  sum_dat <- dplyr::bind_rows(sum_dat)
  
  # Turn data into long format and save to file
  n_dat <- sum_dat %>% group_by(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, biogeo16_bins, biogeo17_bins) %>% 
    tidyr::pivot_longer(c(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, biogeo16_bins, biogeo17_bins), 
                        names_to="path", values_to="var") %>% group_by(path, var) %>%
    summarise(n=sum(n, na.rm=T))
  mar_dat <- sum_dat %>% group_by(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, biogeo16_bins, biogeo17_bins) %>% 
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-n) %>% tidyr::pivot_longer(c(bathy_bins, biogeo08_bins, biogeo11_bins, biogeo12_bins, biogeo13_bins, biogeo16_bins, biogeo17_bins), 
                                              names_to="path", values_to="var") %>%
    group_by(path, var) %>% summarise_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~sum(., na.rm=T))) %>% full_join(n_dat) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(mar_dat, "data/summary_ind_mar_bins.rds", compress="xz")
  
  n_dat2 <- sum_dat %>% ungroup() %>% 
    dplyr::select(-c(biogeo11_bins, biogeo12_bins, biogeo16_bins, biogeo17_bins)) %>% 
    group_by(bathy_bins, biogeo08_bins, biogeo13_bins) %>% 
    summarise(n=sum(n, na.rm=T))
  mar_dat2 <- sum_dat %>% ungroup() %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-c(biogeo11_bins, biogeo12_bins, biogeo16_bins, biogeo17_bins, n)) %>% 
    group_by(bathy_bins, biogeo08_bins, biogeo13_bins) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% full_join(n_dat2) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
 saveRDS(mar_dat2, "data/summary_all_mar_bins.rds", compress="xz"); file.remove(paste0("data/summary_mar_bins_", 1:49, ".rds"))
}

########################################