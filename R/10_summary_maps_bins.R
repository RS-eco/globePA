#' ---
#' title: "Create environmental protection maps"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#Automatically install required packages, which are not yet installed
packages <- c("sp", "raster", "tidyverse", "sf", "patchwork", "RStoolbox", "scico")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA/"
setwd(workdir)

# Specify colour scheme
bluered <- rev(scico(255, palette = 'roma'))
redblue <- c("grey", scico(11, palette = 'roma'))

# Obtain world map
library(rnaturalearthhires)
data(countries10)
countries10 <- sf::st_as_sf(countries10)
outline <- sf::st_union(countries10)
outline_ter <- sf::st_crop(outline, extent(-180, 180, -56, 84))

outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
outline_ter_moll <- sf::st_transform(outline_ter, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

####################

# Plot terrestrial protection maps

# Read and prepare data
ter_dat_ind <- readRDS("data/summary_ind_ter_bins.rds")
head(ter_dat_ind)
colnames(ter_dat_ind) <- c("path", "var", "I-II", "III-IV", "V-VI", "Not-designated", "Total",  "n")
ter_dat_ind$sum <- rowSums(ter_dat_ind[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

#####

# How can sum of individual protection be smaller than total?
ter_dat_ind[round(ter_dat_ind$sum, 1) < round(ter_dat_ind$Total,1),]

#####

ter_dat_ind$`I-II` <- ifelse(ter_dat_ind$sum > ter_dat_ind$Total, 
                             ifelse(ter_dat_ind$`I-II` > ter_dat_ind$Total, ter_dat_ind$Total, ter_dat_ind$`I-II`), 
                             ter_dat_ind$`I-II`)
ter_dat_ind$`III-IV` <- ifelse(ter_dat_ind$sum > ter_dat_ind$Total, 
                               ifelse(ter_dat_ind[,c("I-II")] == ter_dat_ind$Total, 0,
                                      ifelse(rowSums(ter_dat_ind[,c("I-II", "III-IV")], na.rm=T) >= ter_dat_ind$Total,
                                             ter_dat_ind$Total-ter_dat_ind$`I-II`,
                                             ter_dat_ind$`III-IV`)), 
                               ter_dat_ind$`III-IV`)
ter_dat_ind$`V-VI` <- ifelse(ter_dat_ind$sum > ter_dat_ind$Total, 
                             ifelse(rowSums(ter_dat_ind[,c("I-II", "III-IV")], na.rm=T) == ter_dat_ind$Total, 0, 
                                    ifelse(rowSums(ter_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= ter_dat_ind$Total,
                                           ter_dat_ind$Total-rowSums(ter_dat_ind[,c("I-II", "III-IV")], na.rm=T),
                                           ter_dat_ind$`V-VI`)), 
                             ter_dat_ind$`V-VI`)
ter_dat_ind$`Not-designated` <- ifelse(ter_dat_ind$sum > ter_dat_ind$Total, 
                                       ifelse(rowSums(ter_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T) == ter_dat_ind$Total, 0,
                                              ifelse(rowSums(ter_dat_ind[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= ter_dat_ind$Total,
                                                     ter_dat_ind$Total-rowSums(ter_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                     ter_dat_ind$`Not-designated`)), 
                                       ter_dat_ind$`Not-designated`)

ter_dat_all <- readRDS("data/summary_all_ter_bins.rds")
colnames(ter_dat_all) <- c("bio01", "bio12", "elevation","I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")
ter_dat_all$sum <- rowSums(ter_dat_all[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

ter_dat_all$`I-II` <- ifelse(ter_dat_all$sum > ter_dat_all$Total, 
                             ifelse(ter_dat_all$`I-II` > ter_dat_all$Total, ter_dat_all$Total, ter_dat_all$`I-II`), 
                             ter_dat_all$`I-II`)
ter_dat_all$`III-IV` <- ifelse(ter_dat_all$sum > ter_dat_all$Total, 
                               ifelse(ter_dat_all[,c("I-II")] == ter_dat_all$Total, 0,
                                      ifelse(rowSums(ter_dat_all[,c("I-II", "III-IV")], na.rm=T) >= ter_dat_all$Total,
                                             ter_dat_all$Total-ter_dat_all$`I-II`,
                                             ter_dat_all$`III-IV`)), 
                               ter_dat_all$`III-IV`)
ter_dat_all$`V-VI` <- ifelse(ter_dat_all$sum > ter_dat_all$Total, 
                             ifelse(rowSums(ter_dat_all[,c("I-II", "III-IV")], na.rm=T) == ter_dat_all$Total, 0, 
                                    ifelse(rowSums(ter_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= ter_dat_all$Total,
                                           ter_dat_all$Total-rowSums(ter_dat_all[,c("I-II", "III-IV")], na.rm=T),
                                           ter_dat_all$`V-VI`)), 
                             ter_dat_all$`V-VI`)
ter_dat_all$`Not-designated` <- ifelse(ter_dat_all$sum > ter_dat_all$Total, 
                                       ifelse(rowSums(ter_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T) == ter_dat_all$Total, 0,
                                              ifelse(rowSums(ter_dat_all[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= ter_dat_all$Total,
                                                     ter_dat_all$Total-rowSums(ter_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                     ter_dat_all$`Not-designated`)), 
                                       ter_dat_all$`Not-designated`)

vars <- c("bio01", "bio12", "elevation", "bio01+bio12", "bio01+elevation", "bio12+elevation")
files <- c("extdata/wc2.0_bio_30s_01_bins.nc", "extdata/wc2.0_bio_30s_12_bins.nc", "extdata/elevation_1KMmn_GMTEDmn_bins.nc")
#dat <- raster::stack(files)
lapply(1:length(vars), function(x){
  if(!file.exists(paste0("extdata/", vars[x], "_protected_bins.nc"))){
    if(x == 1){
      dat_sub <- dat[[x]]
      dat_df <- ter_dat_ind %>% filter(path=="bio01_bins")
    } else if(x == 2){
      dat_sub <- dat[[x]]
      dat_df <- ter_dat_ind %>% filter(path=="bio12_bins")
    } else if(x == 3){
      dat_sub <- dat[[x]]
      dat_df <- ter_dat_ind %>% filter(path=="elevation_bins")
    } else if(x == 4){
      dat_df <- ter_dat_all %>% 
        drop_na() %>% mutate(bio12 = bio12*1000) %>%
        mutate(var = bio01+bio12) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[1]] + (dat[[2]]*1000)
    } else if(x == 5){
      dat_df <- ter_dat_all %>% 
        drop_na() %>% mutate(elevation = elevation*1000) %>%
        mutate(var = bio01+elevation) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[1]] + (dat[[3]]*1000)
    } else if(x == 6){
      dat_df <- ter_dat_all %>% 
        drop_na() %>% mutate(elevation = elevation*1000) %>%
        mutate(var = bio12+elevation) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[2]] + (dat[[3]]*1000)
    }
    
    dat_df <- dat_df %>% ungroup() %>% dplyr::select(var, Total) %>% drop_na()
    summary(dat_df)
    
    # Subsitute values
    dat_sub <- rasterDT::subsDT(dat_sub, dat_df, by="var")
    
    # Save substituted rasters to file
    raster::writeRaster(dat_sub, filename=paste0("extdata/", vars[x], "_protected_bins.nc"), compression=9, overwrite=T)
  }
})

# Load substituted raster files
prot_all <- raster::stack(paste0("extdata/", vars, "_protected_bins.nc"))
#bio1[bio1 == 0] <- NA
prot_all <- prot_all %>% fortify(maxpixels=2000000) %>% tidyr::drop_na(); gc()
colnames(prot_all) <-c("x", "y", "(a) \t Temp", "(b) \t Prec", "(c) \t Elevation",
                       "(d) \t Temp + Prec", "(e) \t Temp + Elevation", "(f) \t Prec + Elevation")
prot_all <- prot_all %>% tidyr::drop_na() %>% gather(var, value, -c(x,y)) %>%
  mutate(perc2 = as.character(cut(value, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                  labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
  mutate(perc2 = factor(if_else(value == 0, "0", perc2),
                        levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

# Plot maps
p1 <- ggplot() + geom_tile(data=prot_all, aes(x=x, y=y, fill=perc2)) + 
  facet_wrap(var~., nrow=3) + 
  scale_fill_manual(values = redblue, name = "% Protected", na.value="transparent",
                    guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 0.25/.pt) +
  coord_sf() + theme_bw() + 
  theme(strip.background=element_blank(), axis.title=element_blank(), 
        strip.text = element_text(size=12, face="bold"), panel.border=element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

# Save figure
ggsave("figures/Figure4_bins.png", p1, width=9, height=6, dpi=1000)

########################################

# Plot marine protection maps

marspec_dat_ind <- readRDS("data/summary_ind_mar_bins.rds")
head(marspec_dat_ind)
colnames(marspec_dat_ind) <- c("path", "var", "I-II", "III-IV", "V-VI", "Not-designated", "Total", "n")

marspec_dat_ind$sum <- rowSums(marspec_dat_ind[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

marspec_dat_ind$`I-II` <- ifelse(marspec_dat_ind$sum > marspec_dat_ind$Total, 
                                 ifelse(marspec_dat_ind$`I-II` > marspec_dat_ind$Total, marspec_dat_ind$Total, marspec_dat_ind$`I-II`), 
                                 marspec_dat_ind$`I-II`)
marspec_dat_ind$`III-IV` <- ifelse(marspec_dat_ind$sum > marspec_dat_ind$Total, 
                                   ifelse(marspec_dat_ind[,c("I-II")] == marspec_dat_ind$Total, 0,
                                          ifelse(rowSums(marspec_dat_ind[,c("I-II", "III-IV")], na.rm=T) >= marspec_dat_ind$Total,
                                                 marspec_dat_ind$Total-marspec_dat_ind$`I-II`,
                                                 marspec_dat_ind$`III-IV`)), 
                                   marspec_dat_ind$`III-IV`)
marspec_dat_ind$`V-VI` <- ifelse(marspec_dat_ind$sum > marspec_dat_ind$Total, 
                                 ifelse(rowSums(marspec_dat_ind[,c("I-II", "III-IV")], na.rm=T) == marspec_dat_ind$Total, 0, 
                                        ifelse(rowSums(marspec_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= marspec_dat_ind$Total,
                                               marspec_dat_ind$Total-rowSums(marspec_dat_ind[,c("I-II", "III-IV")], na.rm=T),
                                               marspec_dat_ind$`V-VI`)), 
                                 marspec_dat_ind$`V-VI`)
marspec_dat_ind$`Not-designated` <- ifelse(marspec_dat_ind$sum > marspec_dat_ind$Total, 
                                           ifelse(rowSums(marspec_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T) == marspec_dat_ind$Total, 0,
                                                  ifelse(rowSums(marspec_dat_ind[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= marspec_dat_ind$Total,
                                                         marspec_dat_ind$Total-rowSums(marspec_dat_ind[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                         marspec_dat_ind$`Not-designated`)), 
                                           marspec_dat_ind$`Not-designated`)

marspec_dat_all <- readRDS("data/summary_all_mar_bins.rds")
colnames(marspec_dat_all) <- c("bathy", "biogeo08", "biogeo13", "I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")

marspec_dat_all$sum <- rowSums(marspec_dat_all[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

marspec_dat_all$`I-II` <- ifelse(marspec_dat_all$sum > marspec_dat_all$Total, 
                                 ifelse(marspec_dat_all$`I-II` > marspec_dat_all$Total, marspec_dat_all$Total, marspec_dat_all$`I-II`), 
                                 marspec_dat_all$`I-II`)
marspec_dat_all$`III-IV` <- ifelse(marspec_dat_all$sum > marspec_dat_all$Total, 
                                   ifelse(marspec_dat_all[,c("I-II")] == marspec_dat_all$Total, 0,
                                          ifelse(rowSums(marspec_dat_all[,c("I-II", "III-IV")], na.rm=T) >= marspec_dat_all$Total,
                                                 marspec_dat_all$Total-marspec_dat_all$`I-II`,
                                                 marspec_dat_all$`III-IV`)), 
                                   marspec_dat_all$`III-IV`)
marspec_dat_all$`V-VI` <- ifelse(marspec_dat_all$sum > marspec_dat_all$Total, 
                                 ifelse(rowSums(marspec_dat_all[,c("I-II", "III-IV")], na.rm=T) == marspec_dat_all$Total, 0, 
                                        ifelse(rowSums(marspec_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= marspec_dat_all$Total,
                                               marspec_dat_all$Total-rowSums(marspec_dat_all[,c("I-II", "III-IV")], na.rm=T),
                                               marspec_dat_all$`V-VI`)), 
                                 marspec_dat_all$`V-VI`)
marspec_dat_all$`Not-designated` <- ifelse(marspec_dat_all$sum > marspec_dat_all$Total, 
                                           ifelse(rowSums(marspec_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T) == marspec_dat_all$Total, 0,
                                                  ifelse(rowSums(marspec_dat_all[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= marspec_dat_all$Total,
                                                         marspec_dat_all$Total-rowSums(marspec_dat_all[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                         marspec_dat_all$`Not-designated`)), 
                                           marspec_dat_all$`Not-designated`)



vars <- c("biogeo13",  "biogeo08", "bathy", "biogeo08+biogeo13",  "bathy+biogeo13", "bathy+biogeo08")
files <- c("extdata/biogeo13_30s_bins.nc", "extdata/biogeo08_30s_bins.nc", "extdata/bathy_30s_bins.nc")
#dat <- stack(files)
lapply(1:length(vars), function(x){
  if(!file.exists(paste0("extdata/", vars[x], "_protected_bins.nc"))){
    if(x == 1){
      dat_sub <- dat[[x]]
      dat_df <- marspec_dat_ind %>% filter(path=="biogeo13_bins")
    } else if(x == 2){
      dat_sub <- dat[[x]]
      dat_df <- marspec_dat_ind %>% filter(path=="biogeo08_bins")
    } else if(x == 3){
      dat_sub <- dat[[x]]
      dat_df <- marspec_dat_ind %>% filter(path=="bathy_bins")
    } else if(x == 4){
      dat_df <- marspec_dat_all %>% 
        drop_na() %>% mutate(biogeo13 = biogeo13*1000) %>%
        mutate(var = biogeo13+biogeo08) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[2]] + (dat[[1]]*1000)
    } else if(x == 5){
      dat_df <- marspec_dat_all %>% 
        drop_na() %>% mutate(biogeo13 = biogeo13*1000) %>%
        mutate(var = biogeo13+bathy) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[3]] + (dat[[1]]*1000)
    } else if(x == 6){
      dat_df <- marspec_dat_all %>% 
        drop_na() %>% mutate(biogeo08 = biogeo08*1000) %>%
        mutate(var = biogeo08+bathy) %>% group_by(var) %>% 
        mutate(prot_cells = n*Total/100) %>% 
        summarise(cells=sum(n, na.rm=T), prot_cells = sum(prot_cells, na.rm=T)) %>%
        mutate(Total = prot_cells/cells*100)
      dat_sub <- dat[[3]] + (dat[[2]]*1000)
    }
    
    dat_df <- dat_df %>% ungroup() %>% dplyr::select(var, Total) %>% drop_na()
    summary(dat_df)
    
    # Subsitute values
    dat_sub <- rasterDT::subsDT(dat_sub, dat_df, by="var")
    
    # Save substituted rasters to file
    raster::writeRaster(dat_sub, filename=paste0("extdata/", vars[x], "_protected_bins.nc"), compression=9, overwrite=T)
  }
})

# Load substituted raster files
prot_all <- raster::stack(paste0("extdata/", vars, "_protected_bins.nc"))
prot_all <- prot_all %>% fortify(maxpixels=2000000) %>% tidyr::drop_na(); gc()
colnames(prot_all) <-c("x", "y", "(a) \t SST", "(b) \t SSS", "(c) \t Bathy",
                       "(d) \t SST + SSS", "(e) \t SST + Bathy", "(f) \t SSS + Bathy")
prot_all <- prot_all %>% tidyr::drop_na() %>% gather(var, value, -c(x,y)) %>%
  mutate(perc2 = as.character(cut(value, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                  labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
  mutate(perc2 = factor(if_else(value == 0, "0", perc2),
                        levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

# Plot maps
p1 <- ggplot() + geom_tile(data=prot_all, aes(x=x, y=y, fill=perc2)) + 
  facet_wrap(var~., nrow=3) + 
  scale_fill_manual(values = redblue, name = "% Protected", na.value="transparent",
                    guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size =0.25/.pt) +
  coord_sf(datum=NA) + theme_bw() + 
  theme(strip.background=element_blank(), axis.title=element_blank(), 
        strip.text = element_text(size=12, face="bold"), panel.border=element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

# Save figure
ggsave("figures/Figure5_bins.png", p1, width=7.8, height=6, dpi=1000)
