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

if(!file.exists("data/summary_all_ter_perc.rds")){
  # Worldclim & Earthenv
  wc_files <- list.files("extdata", pattern="wc2.0_bio.*\\_perc.nc$", full.names=T)
  ee_files <- list.files("extdata", pattern="mn_GMTED.*\\_perc.nc$", full.names=T)
  pa_files <- list.files("extdata/", pattern="^pa_cov.*\\.nc$", full.names=T)
  dat <- lapply(c(wc_files, ee_files, pa_files), function(x) raster::raster(x))
  tmpdir <- file.path(tempdir(), "splitRaster")
  dir.create(tmpdir)
  r_sub <- lapply(dat, function(y) SpaDES.tools::splitRaster(y, nx=7, ny=7, path=file.path(tmpdir, "r_sub")))
  rm(dat); gc()
  sum_dat <- lapply(1:49, function(j){
    if(!file.exists(paste0("data/summary_ter_perc_", j, ".rds"))){
      dat <- as.data.frame(raster::rasterToPoints(stack(lapply(r_sub,'[[', j))))
      colnames(dat) <- sub("pa_cov_", "", sub("_1KMmn_GMTEDmn", "", sub("wc2.0_bio_30s_", "bio", colnames(dat))))
      sum_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), funs(replace_na(., 0))) %>% 
        group_by(bio01_perc, bio04_perc, bio07_perc, bio12_perc, bio15_perc, elevation_perc) %>% 
        summarise_all(list(~mean(., na.rm=T)))
      n_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        dplyr::select(-c("I.II", "III.IV", "Notdesignated", "total", "V.VI")) %>% 
        group_by(bio01_perc, bio04_perc, bio07_perc, bio12_perc, bio15_perc, elevation_perc) %>% 
        summarise(n=n()); rm(dat); invisible(gc())
      sum_dat <- full_join(sum_dat, n_dat)
      saveRDS(sum_dat, paste0("data/summary_ter_perc_", j, ".rds"), compress="xz")
    } else{
      sum_dat <- readRDS(paste0("data/summary_ter_perc_", j, ".rds"))
    }
    return(sum_dat)
  })
  sum_dat <- dplyr::bind_rows(sum_dat)
  
  # Turn data into long format and save to file
  n_dat <- sum_dat %>% group_by(bio01_perc, bio04_perc, bio07_perc, bio12_perc, bio15_perc, elevation_perc) %>% 
    select(-c("I.II", "III.IV", "Notdesignated", "total", "V.VI")) %>% 
    tidyr::pivot_longer(c(bio01_perc, bio04_perc, bio07_perc, bio12_perc, bio15_perc, elevation_perc), names_to="path", values_to="var") %>%
    group_by(path, var) %>% summarise(n=sum(n, na.rm=T))
  ter_dat <- sum_dat %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-n) %>% tidyr::pivot_longer(c(bio01_perc, bio04_perc, bio07_perc, bio12_perc, bio15_perc, elevation_perc), names_to="path", values_to="var") %>%
    group_by(path, var) %>% summarise_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~sum(., na.rm=T))) %>% full_join(n_dat) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(ter_dat, "data/summary_ind_ter_perc.rds", compress="xz")
  
  # Subset data by heatmap variables and save to file
  n_dat2 <- sum_dat %>% ungroup() %>% 
    select(-c(bio04_perc, bio07_perc, bio15_perc)) %>%
    group_by(bio01_perc, bio12_perc, elevation_perc) %>% 
    summarise(n=sum(n, na.rm=T))
  ter_dat2 <- sum_dat %>% ungroup() %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    select(-c(bio04_perc, bio07_perc, bio15_perc, n)) %>%
    group_by(bio01_perc, bio12_perc, elevation_perc) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% full_join(n_dat2) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(ter_dat2, "data/summary_all_ter_perc.rds", compress="xz"); file.remove(paste0("data/summary_ter_perc_", 1:49, ".rds"))
}; gc()

########################################

## SST, salinity, depth summary
rm(list=ls());gc()
if(!file.exists("data/summary_all_mar_perc.rds")){
  # Stack mpa data to MARSPEC data
  files <- c(list.files("extdata/", pattern="bathy_30s.*\\_perc.nc$", full.names=T),
             list.files("extdata", pattern="biogeo.*\\_30s_perc.nc$", full.names=T))
  mpa_files <- list.files("extdata/", pattern="^mpa_cov.*\\.nc", full.names=T)
  dat <- lapply(c(files, mpa_files), function(x) raster::raster(x))
  tmpdir <- file.path(tempdir(), "splitRaster")
  dir.create(tmpdir)
  r_sub <- lapply(dat, function(y) SpaDES.tools::splitRaster(y, nx=7, ny=7, 
                                                             path=file.path(tmpdir, "r_sub")))
  rm(dat); gc()
  sum_dat <- lapply(1:49, function(j){
    if(!file.exists(paste0("data/summary_mar_perc_", j, ".rds"))){
      dat <- as.data.frame(raster::rasterToPoints(stack(lapply(r_sub,'[[', j))))
      colnames(dat) <-  sub("mpa_cov_", "", sub("30s_", "", colnames(dat)))
      sum_dat <- dat %>% select(-c(x,y)) %>% 
        mutate_at(c("I.II", "III.IV", "Notdesignated", "total", "V.VI"), funs(replace_na(., 0))) %>% 
        group_by(bathy_perc, biogeo08_perc, biogeo11_perc, biogeo12_perc, biogeo13_perc, 
                 biogeo16_perc, biogeo17_perc) %>% 
        summarise_all(list(~mean(., na.rm=T)))
      n_dat <- dat %>% dplyr::select(-c(x,y)) %>% 
        dplyr::select(-c("I.II", "III.IV", "Notdesignated", "total", "V.VI")) %>% 
        group_by(bathy_perc, biogeo08_perc, biogeo11_perc, biogeo12_perc, biogeo13_perc, 
                 biogeo16_perc, biogeo17_perc) %>%
        summarise(n=n())
      sum_dat <- full_join(sum_dat, n_dat); rm(dat); invisible(gc())
      saveRDS(sum_dat, paste0("data/summary_mar_perc_", j, ".rds"), compress="xz")
    } else{
      sum_dat <- readRDS(paste0("data/summary_mar_perc_", j, ".rds"))
    }
    return(sum_dat)
  }); rm(r_sub); gc()
  sum_dat <- dplyr::bind_rows(sum_dat); gc()
  saveRDS(sum_dat, "data/summary_mar_all.rds", compress="xz"); file.remove(paste0("data/summary_mar_perc_", 1:49, ".rds"))
}; gc()

# Restart R session

if(!file.exists("data/summary_all_mar_perc.rds")){
  sum_dat <- readRDS("data/summary_mar_all.rds")
  # Turn data into long format and save to file
  n_dat <- sum_dat %>% group_by(bathy_perc, biogeo08_perc, biogeo11_perc, biogeo12_perc, biogeo13_perc, biogeo16_perc, biogeo17_perc) %>% 
    select(-c("I.II", "III.IV", "Notdesignated", "total", "V.VI")) %>% 
    tidyr::pivot_longer(c(bathy_perc, biogeo08_perc, biogeo11_perc, biogeo12_perc, biogeo13_perc, biogeo16_perc, biogeo17_perc), 
                        names_to="path", values_to="var") %>% group_by(path, var) %>%
    summarise(n=sum(n, na.rm=T)); gc()
  mar_dat <- sum_dat %>% 
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-n); gc()
  mar_dat_list <- lapply(c("bathy_perc", "biogeo08_perc", "biogeo11_perc", "biogeo12_perc", "biogeo13_perc", "biogeo16_perc", "biogeo17_perc"), function(x){
    sub_dat <- mar_dat %>% ungroup() %>% dplyr::select(c(I.II, III.IV, Notdesignated, total, V.VI), contains(x))
    sub_dat$path <- x
    sub_dat %>% rename(var = contains(x)) %>%
      group_by(path, var) %>% summarise_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~sum(., na.rm=T))) %>% 
      left_join(n_dat) %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))   
  }); rm(mar_dat); gc()
  mar_dat <- bind_rows(mar_dat_list); rm(mar_dat_list); gc()
  saveRDS(mar_dat, "data/summary_ind_mar_perc.rds", compress="xz")
  
  n_dat2 <- sum_dat %>% ungroup() %>% 
    dplyr::select(-c(biogeo11_perc, biogeo12_perc, biogeo16_perc, biogeo17_perc)) %>% 
    group_by(bathy_perc, biogeo08_perc, biogeo13_perc) %>% 
    summarise(n=sum(n, na.rm=T))
  mar_dat2 <- sum_dat %>% ungroup() %>% mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./100*n)) %>% 
    dplyr::select(-c(biogeo11_perc, biogeo12_perc, biogeo16_perc, biogeo17_perc, n)) %>% 
    group_by(bathy_perc, biogeo08_perc, biogeo13_perc) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% full_join(n_dat2) %>%
    mutate_at(c("I.II", "III.IV", "V.VI", "Notdesignated", "total"), list(~ ./n*100))
  saveRDS(mar_dat2, "data/summary_all_mar_perc.rds", compress="xz")
}; gc()
# file.remove("data/summary_mar_all.rds")

########################################