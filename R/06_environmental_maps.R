#' ---
#' title: "Global Analysis of Protected Areas - Create environmental data maps"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA"
setwd(workdir)

#Automatically install required packages, which are not yet installed
`%!in%` <- Negate(`%in%`)
if("scico" %!in% installed.packages()[,"Package"]) remotes::install_github("thomasp85/scico")
if("rnaturalearthhires" %!in% installed.packages()[,"Package"]) remotes::install_github("ropensci/rnaturalearthhires/")
packages <- c("sp", "raster", "tidyverse", "patchwork", "RStoolbox", "scico", "rnaturalearthhires")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Specify colour scheme
#bluered <- rev(scico(255, palette = 'roma'))

# Obtain world map from rnaturalearthhires package
data(countries10)
countries10 <- sf::st_as_sf(countries10)
outline <- sf::st_union(countries10)
outline_ter <- sf::st_crop(outline, extent(-180, 180, -56, 84))

outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
outline_ter_moll <- sf::st_transform(outline_ter, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
rm(countries10, outline_ter, outline); invisible(gc())

########################################

# Plot terrestrial maps raw

# Need to read data directly from Worldclim v2 & EarthEnv folder
# as you have specified in the filedir path at the beginning of the script.

# Create maps

bio1 <- raster::raster("extdata/wc2.0_bio_30s_01_wm.nc")
bio1[bio1 == 0] <- NA
bio1 <- bio1 %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantiles <- (0:8)/8                          # how many quantiles we want to map 
colours <- rev(scico(9, palette = 'roma'))    # 7 evenly interpolated colors 
quantile.vals <- quantile(bio1$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bio1$layer)) / diff(range(bio1$layer)) # The values corresponding to the quantiles
p1 <- ggplot() + geom_tile(data=bio1, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Temp (°C)", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(a)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(bio1); invisible(gc())

bio12 <- raster::raster("extdata/wc2.0_bio_30s_12_wm.nc")
bio12[bio12 == 0] <- NA
bio12 <- bio12 %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantile.vals <- quantile(bio12$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bio12$layer)) / diff(range(bio12$layer)) # The values corresponding to the quantiles
p2 <- ggplot() + geom_tile(data=bio12, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Prec (mm)", colours = colours, na.value="transparent", 
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(b)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(bio12); invisible(gc())

elevation <-  raster::raster("extdata/elevation_1KMmn_GMTEDmn_wm.nc")
elevation[elevation == 0] <- NA
elevation <- elevation %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantile.vals <- quantile(elevation$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(elevation$layer)) / diff(range(elevation$layer)) # The values corresponding to the quantiles
p3 <- ggplot() + geom_tile(data=elevation, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Elevation (m)", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(c)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(elevation); invisible(gc())

bio4 <- raster::raster("extdata/wc2.0_bio_30s_04_wm.nc")
bio4[bio4 == 0] <- NA
bio4 <- bio4 %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantile.vals <- quantile(bio4$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bio4$layer)) / diff(range(bio4$layer)) # The values corresponding to the quantiles
p4 <- ggplot() + geom_tile(data=bio4, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Temp\nseasonality", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(a)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(bio4); invisible(gc())

bio7 <- raster::raster("extdata/wc2.0_bio_30s_07_wm.nc")
bio7[bio7 == 0] <- NA
bio7 <- bio7 %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantile.vals <- quantile(bio7$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bio7$layer)) / diff(range(bio7$layer)) # The values corresponding to the quantiles
p5 <- ggplot() + geom_tile(data=bio7, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Temp\nannual range", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(b)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(bio7); invisible(gc())

bio15 <- raster::raster("extdata/wc2.0_bio_30s_15_wm.nc")
bio15[bio15 == 0] <- NA
bio15 <- bio15 %>% fortify(maxpixels=1000000) %>% tidyr::drop_na(); invisible(gc())
quantile.vals <- quantile(bio15$layer, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bio15$layer)) / diff(range(bio15$layer)) # The values corresponding to the quantiles
p6 <- ggplot() + geom_tile(data=bio15, aes(x=x,y=y,fill=layer)) + 
  scale_fill_gradientn(name="Prec\nseasonality", colours = colours, na.value="transparent", 
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_ter_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(c)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(bio15); invisible(gc())

# Join figures
pj <- p1 + p2 + p3 + plot_layout(ncol=1)
ggsave(filename="figures/ter_maps_raw1.png", pj, width=5.5, height=7, dpi=1000)

pj2 <- p4 + p5 + p6 + plot_layout(ncol=1)
ggsave(filename="figures/ter_maps_raw2.png", pj2, width=5.5, height=7, dpi=1000)

########################################

# Plot marine maps raw

# Need to read data directly from MARSPEC files!!!

# Create maps

biogeo13 <- raster::raster("extdata/biogeo13_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantiles <- (0:8)/8                          # how many quantiles we want to map 
colours <- rev(scico(9, palette = 'roma'))    # 7 evenly interpolated colors 
quantile.vals <- quantile(biogeo13$biogeo13_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo13$biogeo13_30s_wm)/100) / diff(range(biogeo13$biogeo13_30s_wm)/100) # The values corresponding to the quantiles
p1 <- ggplot() + geom_tile(data=biogeo13, aes(x=x,y=y,fill=biogeo13_30s_wm/100)) + 
  scale_fill_gradientn(name="SST (°C)", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(a)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'))
#rm(biogeo13); invisible(gc())

biogeo08 <- raster::raster("extdata/biogeo08_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(biogeo08$biogeo08_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo08$biogeo08_30s_wm)/100) / diff(range(biogeo08$biogeo08_30s_wm)/100) # The values corresponding to the quantiles
p2 <- ggplot() + geom_tile(data=biogeo08, aes(x=x,y=y,fill=biogeo08_30s_wm/100)) + 
  scale_fill_gradientn(name="SSS (psu)", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(b)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'))
#rm(biogeo08); invisible(gc)

bathy <- raster::raster("extdata/bathy_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(bathy$bathy_30s_wm, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(bathy$bathy_30s_wm)) / diff(range(bathy$bathy_30s_wm)) # The values corresponding to the quantiles
p3 <- ggplot() + geom_tile(data=bathy, aes(x=x,y=y,fill=bathy_30s_wm)) + 
  scale_fill_gradientn(name="Bathy (m)", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 0), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(c)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'))
#rm(bathy); invisible(gc())

biogeo16 <- raster::raster("extdata/biogeo16_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(biogeo16$biogeo16_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo16$biogeo16_30s_wm/100)) / diff(range(biogeo16$biogeo16_30s_wm/100)) # The values corresponding to the quantiles
p4 <- ggplot() + geom_tile(data=biogeo16, aes(x=x,y=y,fill=biogeo16_30s_wm/100)) + 
  scale_fill_gradientn(name="Annual range\nin SST", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(a)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(biogeo16); invisible(gc)

biogeo17 <- raster::raster("extdata/biogeo17_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(biogeo17$biogeo17_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo17$biogeo17_30s_wm/100)) / diff(range(biogeo17$biogeo17_30s_wm/100)) # The values corresponding to the quantiles
p5 <- ggplot() + geom_tile(data=biogeo17, aes(x=x,y=y,fill=biogeo17_30s_wm/100)) + 
  scale_fill_gradientn(name="Annual variance\nin SST",colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(b)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(biogeo17); invisible(gc)

biogeo11 <- raster::raster("extdata/biogeo11_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(biogeo11$biogeo11_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo11$biogeo11_30s_wm/100)) / diff(range(biogeo11$biogeo11_30s_wm/100)) # The values corresponding to the quantiles
p6 <- ggplot() + geom_tile(data=biogeo11, aes(x=x,y=y,fill=biogeo11_30s_wm/100)) + 
  scale_fill_gradientn(name="Annual range\nin SSS", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(c)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(biogeo11); invisible(gc)

biogeo12 <- raster::raster("extdata/biogeo12_30s_wm.nc") %>% fortify(maxpixels=1000000) %>% tidyr::drop_na()
quantile.vals <- quantile(biogeo12$biogeo12_30s_wm, quantiles, names=F)/100 # the values for each quantile
val.remap <- (quantile.vals - min(biogeo12$biogeo12_30s_wm/100)) / diff(range(biogeo12$biogeo12_30s_wm/100)) # The values corresponding to the quantiles
p7 <- ggplot() + geom_tile(data=biogeo12, aes(x=x,y=y,fill=biogeo12_30s_wm/100)) + 
  scale_fill_gradientn(name="Annual variance\nin SSS", colours = colours, na.value="transparent",
                       values=val.remap, breaks=quantile.vals, labels=round(quantile.vals, 2), 
                       guide = guide_legend(reverse = TRUE)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  coord_sf() + theme_bw() + ggtitle("(d)") + 
  theme(axis.title=element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_blank(), plot.title = element_text(vjust = - 7, hjust=0.07),
        legend.margin=margin(t=0, l=-0.5, unit='cm'),
        legend.key.height=unit(0.5, "cm"))
#rm(biogeo12); invisible(gc)

# Join figures
pm <- p1 + p2 + p3 + plot_layout(ncol=1)
ggsave(filename="figures/mar_maps_raw1.png", pm, width=5.6, height=8, dpi=1000)

pm2 <- p4 + p5 + p6 + p7 + plot_layout(ncol=2)
ggsave(filename="figures/mar_maps_raw2.png", pm2, width=10.5, height=5, dpi=1000)
