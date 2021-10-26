#' ---
#' title: "Global Analysis of Protected Areas - Create environmental protection maps"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA"
setwd(workdir)

# Set file directory
filedir <- "F:/Data/"

#Automatically install required packages, which are not yet installed
`%!in%` <- Negate(`%in%`)
if("scico" %!in% installed.packages()[,"Package"]) remotes::install_github("thomasp85/scico")
if("rnaturalearthhires" %!in% installed.packages()[,"Package"]) remotes::install_github("ropensci/rnaturalearthhires/")
packages <- c("sp", "raster", "tidyverse", "patchwork", "RStoolbox", "rworldmap", "rgeos", "sf", "scico", "rnaturalearthhires")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Specify colour scheme
bluered <- rev(scico(255, palette = 'roma'))

# Obtain world map from rnaturalearthhires package
data(countries10)
countries10 <- sf::st_as_sf(countries10)
outline <- sf::st_union(countries10)
outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

########################################

#' Map of marine and terrestrial PAs as of December 2019 in WGS1984 projection.

# Read rasterized MPA & PA data
r_mpa <- stack("extdata/mpa_cov_total.nc")
r_pa <- stack("extdata/pa_cov_total.nc")

# Plot map
mpa <- fortify(r_mpa, maxpixels=1000000)
colnames(mpa)[3] <- "mpa_cov_total"
mpa$mpa_cov_total[mpa$mpa_cov_total == 0] <- NA
mpa <- mpa %>% drop_na()
mpa$mpa <- 1
pa <- fortify(r_pa, maxpixels=1000000)
colnames(pa)[3] <- "pa_cov_total"
pa$pa_cov_total[pa$pa_cov_total == 0] <- NA
pa <- pa %>% drop_na()
pa$pa <- 1

p1 <- ggplot() + geom_tile(data=mpa, aes(x,y, fill="Marine")) + 
  geom_tile(data=pa, aes(x,y, fill="Terrestrial")) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_manual(name="", values=c(Terrestrial="darkgreen", Marine="blue")) + 
  coord_sf() + theme_bw() + 
  theme(legend.title.align = 0.5, legend.position="bottom",
        axis.title = element_blank(), panel.border=element_blank(),
        plot.title = element_blank())
ggsave(paste0("figures/map_pa_polygons.png"), p1, width=9, height=5, dpi=600)

mpa %>% group_by(x) %>% summarise(mpa=sum(mpa)) %>% 
  ggplot() + geom_histogram(aes(x=x, y=mpa), stat="identity", position="stack", colour=NA) + 
  scale_y_continuous(position = "right", expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  labs(x="Longitude", y="Number of cells with MPAs") + 
  theme_bw()
mpa %>% group_by(y) %>% summarise(mpa=sum(mpa)) %>% 
  ggplot() + geom_histogram(aes(x=y, y=mpa), stat="identity", position="stack", colour=NA) + 
  scale_y_reverse() + scale_x_continuous(expand=c(0,0)) + 
  coord_flip(expand=FALSE) + labs(x="Latitude", y="Number of cells with MPAs") + 
  theme_bw()

mpa_point <- readRDS("extdata/WDPA_Jan2020_Marine_Point.rds")
pa_point <- readRDS("extdata/WDPA_Jan2020_Terrestrial_Point.rds")
mpa_point <- mpa_point %>% dplyr::select(IUCN_CAT, MARINE, REP_AREA, STATUS_YR, ISO3)
pa_point <- pa_point %>% dplyr::select(IUCN_CAT, MARINE, REP_AREA, STATUS_YR, ISO3)
sf::st_crs(mpa_point) <- "+proj=longlat +datum=WGS84 +no_defs"
sf::st_crs(pa_point) <- "+proj=longlat +datum=WGS84 +no_defs"
mpa_point_moll <- sf::st_transform(mpa_point, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pa_point_moll <- sf::st_transform(pa_point, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Add map of point data
p2 <- ggplot() + geom_sf(data=pa_point_moll, aes(colour="Terrestrial"), size = 1/.pt) + 
  geom_sf(data=mpa_point_moll, aes(colour="Marine"), size = 1/.pt) + 
  geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) +
  scale_colour_manual(name="", values=c(Terrestrial="darkgreen", Marine="blue")) + 
  coord_sf() + theme_bw() + 
  theme(legend.position="bottom", axis.title=element_blank(), 
        panel.border = element_blank(), plot.title=element_blank())
ggsave(paste0("figures/map_pa_points.png"), p2, width=9, height=5, dpi=600)

########################################

#' r iucn_sum, fig.height=8, fig.width=10

#' a) Map of protected areas by IUCN category in WGS1984 projection and 
#' b) number and c) total area of terrestrial, coastal and marine protected areas per IUCN category. 
#' Create same map for point data, points are included in the numbers, no?

#Read rasterized MPA & PA data
mpa_files <- c("extdata/mpa_cov_I-II.nc", "extdata/mpa_cov_III-IV.nc","extdata/mpa_cov_V-VI.nc", 
               "extdata/mpa_cov_Notdesignated.nc", "extdata/mpa_cov_total.nc")
r_mpa <- raster::stack(mpa_files)

pa_files <- c("extdata/pa_cov_I-II.nc", "extdata/pa_cov_III-IV.nc", "extdata/pa_cov_V-VI.nc",
              "extdata/pa_cov_Notdesignated.nc", "extdata/pa_cov_total.nc")
r_pa <- raster::stack(pa_files)

#Convert raster to dataframe & remove columns with no information
r_mpa <- fortify(r_mpa, maxpixels=1000000)
r_mpa <- r_mpa[rowSums(r_mpa[,c(3:7)], na.rm=T)!= 0,]
r_mpa <- r_mpa[rowSums(is.na(r_mpa))<ncol(r_mpa)-2,]
r_pa <- fortify(r_pa, maxpixels=1000000)
r_pa <- r_pa[rowSums(r_pa[,c(3:7)], na.rm=T)!= 0,]
r_pa <- r_pa[rowSums(is.na(r_pa))<ncol(r_pa)-2,]

# Turn NAs into 0s
r_mpa[is.na(r_mpa)] <- 0
r_pa[is.na(r_pa)] <- 0 

# Make sure total is not larger than sum of individual categories
colnames(r_mpa)
colnames(r_pa)
r_mpa$sum <- rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV", "mpa_cov_V.VI", "mpa_cov_Notdesignated")], na.rm=T)
r_pa$sum <- rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI", "pa_cov_Notdesignated")], na.rm=T)

sub <- which(r_mpa$sum > r_mpa$mpa_cov_total)
r_sub <- r_mpa[sub,]
head(r_sub)

h1 <- head(r_mpa)
r_mpa$mpa_cov_I.II <- ifelse(r_mpa$sum > r_mpa$mpa_cov_total, 
                             ifelse(r_mpa$mpa_cov_I.II > r_mpa$mpa_cov_total, r_mpa$mpa_cov_total, r_mpa$mpa_cov_I.II), 
                             r_mpa$mpa_cov_I.II)
r_mpa$mpa_cov_III.IV <- ifelse(r_mpa$sum > r_mpa$mpa_cov_total, 
                               ifelse(r_mpa[,c("mpa_cov_I.II")] == r_mpa$mpa_cov_total, 0,
                                      ifelse(rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV")], na.rm=T) >= r_mpa$mpa_cov_total,
                                             r_mpa$mpa_cov_total-r_mpa$mpa_cov_I.II,
                                             r_mpa$mpa_cov_III.IV)), 
                               r_mpa$mpa_cov_III.IV)
r_mpa$mpa_cov_V.VI <- ifelse(r_mpa$sum > r_mpa$mpa_cov_total, 
                             ifelse(rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV")], na.rm=T) == r_mpa$mpa_cov_total, 0, 
                                    ifelse(rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV", "mpa_cov_V.VI")], na.rm=T) >= r_mpa$mpa_cov_total,
                                           r_mpa$mpa_cov_total-rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV")], na.rm=T),
                                           r_mpa$mpa_cov_V.VI)), 
                             r_mpa$mpa_cov_V.VI)
r_mpa$mpa_cov_Notdesignated <- ifelse(r_mpa$sum > r_mpa$mpa_cov_total, 
                                      ifelse(rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV", "mpa_cov_V.VI")], na.rm=T) == r_mpa$mpa_cov_total, 0,
                                             ifelse(rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV", "mpa_cov_V.VI", "mpa_cov_Notdesignated")], na.rm=T) >= r_mpa$mpa_cov_total,
                                                    r_mpa$mpa_cov_total-rowSums(r_mpa[,c("mpa_cov_I.II", "mpa_cov_III.IV", "mpa_cov_V.VI")], na.rm=T),
                                                    r_mpa$mpa_cov_Notdesignated)), 
                                      r_mpa$mpa_cov_Notdesignated)
head(r_mpa)

head(r_pa)
r_pa$pa_cov_I.II <- ifelse(r_pa$sum > r_pa$pa_cov_total, 
                           ifelse(r_pa$pa_cov_I.II > r_pa$pa_cov_total, r_pa$pa_cov_total, r_pa$pa_cov_I.II), 
                           r_pa$pa_cov_I.II)
r_pa$pa_cov_III.IV <- ifelse(r_pa$sum > r_pa$pa_cov_total, 
                             ifelse(r_pa[,c("pa_cov_I.II")] == r_pa$pa_cov_total, 0,
                                    ifelse(rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV")], na.rm=T) >= r_pa$pa_cov_total,
                                           r_pa$pa_cov_total-r_pa$pa_cov_I.II,
                                           r_pa$pa_cov_III.IV)), 
                             r_pa$pa_cov_III.IV)
r_pa$pa_cov_V.VI <- ifelse(r_pa$sum > r_pa$pa_cov_total, 
                           ifelse(rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV")], na.rm=T) == r_pa$pa_cov_total, 0, 
                                  ifelse(rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI")], na.rm=T) >= r_pa$pa_cov_total,
                                         r_pa$pa_cov_total-rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV")], na.rm=T),
                                         r_pa$pa_cov_V.VI)), 
                           r_pa$pa_cov_V.VI)
r_pa$pa_cov_Notdesignated <- ifelse(r_pa$sum > r_pa$pa_cov_total, 
                                    ifelse(rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI")], na.rm=T) == r_pa$pa_cov_total, 0,
                                           ifelse(rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI", "pa_cov_Notdesignated")], na.rm=T) >= r_pa$pa_cov_total,
                                                  r_pa$pa_cov_total-rowSums(r_pa[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI")], na.rm=T),
                                                  r_pa$pa_cov_Notdesignated)), 
                                    r_pa$pa_cov_Notdesignated)
head(r_pa)

r_sub2 <- r_pa[sub,]
r_sub2$sum2 <- rowSums(r_sub2[,c("pa_cov_I.II", "pa_cov_III.IV", "pa_cov_V.VI", "pa_cov_Notdesignated")], na.rm=T)

# Turn data.frame into right format
colnames(r_mpa) <- c("x", "y", "I-II", "III-IV", "V-VI", "Notdesignated", "Total", "Sum")
colnames(r_pa) <- c("x", "y", "I-II", "III-IV", "V-VI", "Notdesignated", "Total", "Sum")

# Calculate dominant protection category per grid cell
r_mpa$dom_cat <- colnames(r_mpa)[c(3,4,5,6)][apply(r_mpa[,-c(1,2,7,8)],1,which.max)]
r_pa$dom_cat <- colnames(r_pa)[c(3,4,5,6)][apply(r_pa[,-c(1,2,7,8)],1,which.max)]
unique(r_mpa$dom_cat)
unique(r_pa$dom_cat)

r_mpa$dom_cat <- factor(r_mpa$dom_cat, levels=c("I-II","III-IV", "V-VI", "Notdesignated"), 
                        labels=c("I-II","III-IV", "V-VI", "Non-designated"))
r_pa$dom_cat <- factor(r_pa$dom_cat, levels=c("I-II","III-IV", "V-VI", "Notdesignated"),
                       c("I-II","III-IV", "V-VI", "Non-designated"))

p1 <-  ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
  geom_tile(data=r_mpa, aes(x,y,fill=dom_cat)) + geom_tile(data=r_pa, aes(x,y,fill=dom_cat)) + 
  scale_fill_manual(name="Protection category", values=c("#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#D43F3AFF")) + 
  theme_bw() + coord_sf() + 
  theme(axis.title = element_blank(), legend.position="bottom", panel.border = element_blank())
ggsave(paste0("figures/map_pa_iucn_polygons.png"), p1, width=9, height=5, dpi=600)

mpa_point <- readRDS("extdata/WDPA_Jan2020_Marine_Point.rds")
pa_point <- readRDS("extdata/WDPA_Jan2020_Terrestrial_Point.rds")
mpa_point <- mpa_point %>% 
  mutate(iucn_cat2=case_when(.$IUCN_CAT %in% c("Ia", "Ib", "II") ~ "I-II",
                             .$IUCN_CAT %in% c("III", "IV") ~ "III-IV", 
                             .$IUCN_CAT %in% c("V", "VI") ~ "V-VI", TRUE ~ "Non-designated")) %>% 
  mutate(iucn_cat2 = factor(iucn_cat2, levels=c("I-II", "III-IV", "V-VI", "Non-designated")))
pa_point <- pa_point %>% 
  mutate(iucn_cat2=case_when(.$IUCN_CAT %in% c("Ia", "Ib", "II") ~ "I-II",
                             .$IUCN_CAT %in% c("III", "IV") ~ "III-IV",
                             .$IUCN_CAT %in% c("V", "VI") ~ "V-VI", TRUE ~ "Non-designated")) %>% 
  mutate(iucn_cat2 = factor(iucn_cat2, levels=c("I-II", "III-IV", "V-VI", "Non-designated")))
sf::st_crs(mpa_point) <- "+proj=longlat +datum=WGS84 +no_defs"
sf::st_crs(pa_point) <- "+proj=longlat +datum=WGS84 +no_defs"
mpa_point_moll <- sf::st_transform(mpa_point, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pa_point_moll <- sf::st_transform(pa_point, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

p2 <- ggplot() + 
  geom_sf(data=pa_point_moll, aes(colour=iucn_cat2), size = 1/.pt) + 
  geom_sf(data=mpa_point_moll, aes(colour=iucn_cat2), size = 1/.pt) + 
  geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) +
  scale_colour_manual(name="Protection category", values=c("#46B8DAFF", "#5CB85CFF", "#EEA236FF", "#D43F3AFF")) + 
  theme_bw() + coord_sf() + 
  theme(legend.position="bottom", axis.title=element_blank(), panel.border = element_blank(),
        plot.title=element_blank())
ggsave(paste0("figures/map_pa_iucn_points.png"), p2, width=9, height=5, dpi=600)

# Create wdpa_data

# Read WDPA file and extract data.frame
if(!file.exists("data/wdpa_data.rds")){
  wdpa_data <- sf::st_read(dsn=paste0(filedir, "WDPA/WDPA_Jan2020-shapefile-polygons.shp"))
  wdpa_data <- wdpa_data %>% data.frame() %>% dplyr::select(-geometry)
  saveRDS(wdpa_data, file="data/wdpa_data.rds", compress="xz")
}

# Load wdpa_data
wdpa_data <- readRDS("data/wdpa_data.rds")
unique(wdpa_data$IUCN_CAT)

#Specify categories
iucn_sum <- wdpa_data %>% 
  mutate(iucn_cat2=case_when(.$IUCN_CAT %in% c("Ia", "Ib", "II") ~ "I-II", 
                             .$IUCN_CAT %in% c("III", "IV") ~ "III-IV",
                             .$IUCN_CAT %in% c("V", "VI") ~ "V-VI", TRUE ~ "Non-designated")) %>% 
  mutate(iucn_cat2 = factor(iucn_cat2, levels=c("I-II", "III-IV", "V-VI", "Non-designated"))) %>% 
  group_by(iucn_cat2, MARINE) %>%
  summarise(area = sum(REP_AREA, na.rm=T), number = n())
iucn_sum$MARINE <- factor(iucn_sum$MARINE, labels=c("Terrestrial", "Coastal", "Marine"))

iucn_sum %>% filter(MARINE == "Terrestrial") %>% 
  mutate(area=area/sum(iucn_sum$area[iucn_sum$MARINE == "Terrestrial"])*100)


iucn_tot <- wdpa_data %>% 
  mutate(iucn_cat2=case_when(.$IUCN_CAT %in% c("Ia", "Ib", "II") ~ "I-II", 
                             .$IUCN_CAT %in% c("III", "IV") ~ "III-IV",
                             .$IUCN_CAT %in% c("V", "VI") ~ "V-VI", TRUE ~ "Non-designated")) %>% 
  mutate(iucn_cat2 = factor(iucn_cat2, levels=c("I-II", "III-IV", "V-VI", "Non-designated"))) %>% 
  group_by(iucn_cat2) %>% summarise(total = round(sum(REP_AREA, na.rm=T)/10000,2), sum = n())
iucn_sum <- left_join(iucn_sum, iucn_tot)

p2 <- ggplot(data=iucn_sum, aes(x=iucn_cat2, y=number, fill=MARINE)) + 
  geom_bar(stat="identity", position="stack") + theme_bw() + 
  scale_fill_manual(name="", values=c("#AEAEAE", "#E6E6E6", "#4D4D4D")) + 
  theme(legend.position = "none") + 
  labs(x="Protection category", y="Number of PAs") +
  geom_text(aes(x=iucn_cat2, y=sum, label=sum), vjust=-0.5) + 
  scale_y_continuous(expand=expansion(mult=c(0,.1)))
p3 <- ggplot(data=iucn_sum, aes(x=iucn_cat2, y=area/10000, fill=MARINE)) + 
  geom_bar(stat="identity", position = "stack") + theme_bw() + 
  geom_text(aes(x=iucn_cat2, y=total, label=total), vjust=-0.5) + 
  scale_fill_manual(name="", values=c("#AEAEAE", "#E6E6E6", "#4D4D4D")) + 
  labs(x="Protection category", y="Area (Mio. ha)") + 
  scale_y_continuous(expand=expansion(mult=c(0,.1)))

p <- {p1 + theme(legend.position="right")} - 
  {p2 + p3 + plot_layout(ncol=2)} + plot_layout(ncol=1, heights=c(3,2))
ggsave(paste0("figures/iucn_sum-1.png"), p, width=8, height=6, dpi=600)

########################################

#Read rasterized MPA & PA data
mpa_files <- c("extdata/mpa_cov_I-II.nc", "extdata/mpa_cov_III-IV.nc","extdata/mpa_cov_V-VI.nc", 
               "extdata/mpa_cov_Notdesignated.nc", "extdata/mpa_cov_total.nc")
r_mpa <- raster::stack(mpa_files)

pa_files <- c("extdata/pa_cov_I-II.nc", "extdata/pa_cov_III-IV.nc", "extdata/pa_cov_V-VI.nc",
              "extdata/pa_cov_Notdesignated.nc", "extdata/pa_cov_total.nc")
r_pa <- raster::stack(pa_files)

#Convert raster to dataframe & remove columns with no information
r_mpa <- fortify(r_mpa, maxpixels=1000000)
r_mpa <- r_mpa[rowSums(r_mpa[,c(3:7)], na.rm=T)!= 0,]
r_mpa <- r_mpa[rowSums(is.na(r_mpa))<ncol(r_mpa)-2,]
r_pa <- fortify(r_pa, maxpixels=1000000)
r_pa <- r_pa[rowSums(r_pa[,c(3:7)], na.rm=T)!= 0,]
r_pa <- r_pa[rowSums(is.na(r_pa))<ncol(r_pa)-2,]
colnames(r_mpa) <- c("x", "y", "I-II", "III-IV", "V-VI", "Non-designated", "All")
colnames(r_pa) <- c("x", "y", "I-II", "III-IV", "V-VI", "Non-designated", "All"); gc()

r_mpa$sum <- rowSums(r_mpa[,c("I-II", "III-IV", "V-VI", "Non-designated")], na.rm=T)

r_mpa$`I-II` <- ifelse(r_mpa$sum > r_mpa$All, 
                       ifelse(r_mpa$`I-II` > r_mpa$All, r_mpa$All, r_mpa$`I-II`), 
                       r_mpa$`I-II`)
r_mpa$`III-IV` <- ifelse(r_mpa$sum > r_mpa$All, 
                         ifelse(r_mpa[,c("I-II")] == r_mpa$All, 0,
                                ifelse(rowSums(r_mpa[,c("I-II", "III-IV")], na.rm=T) >= r_mpa$All,
                                       r_mpa$All-r_mpa$`I-II`,
                                       r_mpa$`III-IV`)), 
                         r_mpa$`III-IV`)
r_mpa$`V-VI` <- ifelse(r_mpa$sum > r_mpa$All, 
                       ifelse(rowSums(r_mpa[,c("I-II", "III-IV")], na.rm=T) == r_mpa$All, 0, 
                              ifelse(rowSums(r_mpa[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= r_mpa$All,
                                     r_mpa$All-rowSums(r_mpa[,c("I-II", "III-IV")], na.rm=T),
                                     r_mpa$`V-VI`)), 
                       r_mpa$`V-VI`)
r_mpa$`Non-designated` <- ifelse(r_mpa$sum > r_mpa$All, 
                                 ifelse(rowSums(r_mpa[,c("I-II", "III-IV", "V-VI")], na.rm=T) == r_mpa$All, 0,
                                        ifelse(rowSums(r_mpa[,c("I-II", "III-IV", "V-VI", "Non-designated")], na.rm=T) >= r_mpa$All,
                                               r_mpa$All-rowSums(r_mpa[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                               r_mpa$`Non-designated`)), 
                                 r_mpa$`Non-designated`)


r_pa$sum <- rowSums(r_pa[,c("I-II", "III-IV", "V-VI", "Non-designated")], na.rm=T)

r_pa$`I-II` <- ifelse(r_pa$sum > r_pa$All, 
                      ifelse(r_pa$`I-II` > r_pa$All, r_pa$All, r_pa$`I-II`), 
                      r_pa$`I-II`)
r_pa$`III-IV` <- ifelse(r_pa$sum > r_pa$All, 
                        ifelse(r_pa[,c("I-II")] == r_pa$All, 0,
                               ifelse(rowSums(r_pa[,c("I-II", "III-IV")], na.rm=T) >= r_pa$All,
                                      r_pa$All-r_pa$`I-II`,
                                      r_pa$`III-IV`)), 
                        r_pa$`III-IV`)
r_pa$`V-VI` <- ifelse(r_pa$sum > r_pa$All, 
                      ifelse(rowSums(r_pa[,c("I-II", "III-IV")], na.rm=T) == r_pa$All, 0, 
                             ifelse(rowSums(r_pa[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= r_pa$All,
                                    r_pa$All-rowSums(r_pa[,c("I-II", "III-IV")], na.rm=T),
                                    r_pa$`V-VI`)), 
                      r_pa$`V-VI`)
r_pa$`Non-designated` <- ifelse(r_pa$sum > r_pa$All, 
                                ifelse(rowSums(r_pa[,c("I-II", "III-IV", "V-VI")], na.rm=T) == r_pa$All, 0,
                                       ifelse(rowSums(r_pa[,c("I-II", "III-IV", "V-VI", "Non-designated")], na.rm=T) >= r_pa$All,
                                              r_pa$All-rowSums(r_pa[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                              r_pa$`Non-designated`)), 
                                r_pa$`Non-designated`)

# Turn 0 values to NA
r_mpa$`I-II`[r_mpa$`I-II` == 0] <- NA
r_mpa$`III-IV`[r_mpa$`III-IV` == 0] <- NA
r_mpa$`V-VI`[r_mpa$`V-VI` == 0] <- NA
r_mpa$`Non-designated`[r_mpa$`Non-designated` == 0] <- NA

r_pa$`I-II`[r_pa$`I-II` == 0] <- NA
r_pa$`III-IV`[r_pa$`III-IV` == 0] <- NA
r_pa$`V-VI`[r_pa$`V-VI` == 0] <- NA
r_pa$`Non-designated`[r_pa$`Non-designated` == 0] <- NA

# Plot perc maps

#p1 <- ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
# geom_tile(data=r_mpa, aes(x,y,fill=`I-II`)) + geom_tile(data=r_pa, aes(x,y,fill=`I-II`)) + 
#  scale_fill_gradientn(name="% protected", colours = bluered, na.value="transparent") + 
#  labs(y="I-II") + theme_bw() + coord_sf() + 
#  theme(axis.title.x = element_blank(), axis.title.y=element_text(size=12, face="bold"),
#        panel.border = element_blank())
#p2 <- ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
#  geom_tile(data=r_mpa, aes(x,y,fill=`III-IV`)) + geom_tile(data=r_pa, aes(x,y,fill=`III-IV`)) + 
#  scale_fill_gradientn(name="% protected", colours = bluered, na.value="transparent") + 
#  labs(y="III-IV") + theme_bw() + coord_sf() + 
#  theme(axis.title.x = element_blank(), axis.title.y=element_text(size=12, face="bold"),
#        panel.border = element_blank())
#p3 <- ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
#  geom_tile(data=r_mpa, aes(x,y,fill=`V-VI`)) + geom_tile(data=r_pa, aes(x,y,fill=`V-VI`)) + 
#  scale_fill_gradientn(name="% protected", colours = bluered, na.value="transparent") + 
#  labs(y="IV-V") + theme_bw() + coord_sf() + 
#  theme(axis.title.x = element_blank(), axis.title.y=element_text(size=12, face="bold"),
#        panel.border = element_blank())
#p4 <- ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
#  geom_tile(data=r_mpa, aes(x,y,fill=`Non-designated`)) + geom_tile(data=r_pa, aes(x,y,fill=`Non-designated`)) + 
#  scale_fill_gradientn(name="% protected", colours = bluered, na.value="transparent") + 
#  labs(y="Non-designated") + theme_bw() + coord_sf() + 
#  theme(axis.title.x = element_blank(), axis.title.y=element_text(size=12, face="bold"),
#        panel.border = element_blank())
#p <- p1 + p2 + p3 + p4 + plot_layout(ncol=1)

r_mpa <- r_mpa %>% dplyr::select(-c(sum, All)) %>% tidyr::gather(prot, perc, -c(x,y))
r_mpa$prot <- factor(r_mpa$prot, levels=c("I-II", "III-IV", "V-VI", "Non-designated"))
r_pa <- r_pa %>% dplyr::select(-c(sum, All)) %>% tidyr::gather(prot, perc, -c(x,y)); gc()
r_pa$prot <- factor(r_pa$prot, levels=c("I-II", "III-IV", "V-VI", "Non-designated"))
ggplot() + geom_sf(data=outline_moll, fill=NA, na.rm=T, color = "black", size = 1/.pt) + 
  geom_tile(data=r_mpa, aes(x,y,fill=perc)) + geom_tile(data=r_pa, aes(x,y,fill=perc)) + 
  facet_wrap(.~prot) + 
  coord_sf() + scale_fill_gradientn(name="% protected", colours = bluered, na.value="transparent") + 
  theme_bw() + theme(axis.title = element_blank(),
                     panel.border = element_blank(), strip.background=element_blank(),
                     strip.text=element_text(size=12, face="bold"),
                     legend.margin=margin(t = 0, l=-0.5, b=0, unit='cm'),
                     plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(paste0("figures/maps_perc_prot.png"), width=11, height=6, dpi=1000)

########################################