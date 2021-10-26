#' ---
#' title: "Global Analysis of Protected Areas - Create figures for method scheme"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

# Set working directory
#workdir <- "C:/Users/admin/Documents/GitHub/globePA"
#setwd(workdir)

# Set file directory
#filedir <- "F:/Data/"
filedir <- "extdata"

#Automatically install required packages, which are not yet installed
packages <- c("sf", "raster", "tidyverse", "fasterize", "RStoolbox", "scico")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Specify colour scheme
bluered <- rev(scico(255, palette = 'roma'))

########################################

#download.file("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Jan2020_DEU-shapefile.zip",
#              destfile=paste0(filedir, "WDPA/WDPA_Jan2020_DEU-shapefile.zip"))
pa_poly <- sf::st_read(dsn=paste0(filedir, "/WDPA/WDPA_Jan2020_DEU-shapefile-polygons.shp"))
pa_bg <- pa_poly[grep(pa_poly$NAME, pattern="Berchtesgaden"),]

plot(st_geometry(pa_bg[4,]))
pa_sub <- pa_bg[4,]

pa_sub_moll <- sf::st_transform(pa_sub, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(st_geometry(pa_sub_moll))

r <- raster::raster(nrow=14969, ncol=44012, xmn=-18045016, xmx=18044824, ymn=-6492085, ymx=8776295,
                    crs=" +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
r_sub <- raster::crop(r, pa_sub_moll)
r_sub[] <- 1

r_sub_high <- raster::disaggregate(r_sub, fact=10)
r_pa_sub_high <- fasterize::fasterize(pa_sub_moll, r_sub_high)
r_pa_sub <- raster::aggregate(r_pa_sub_high, 10, fun=sum)
plot(r_pa_sub)

df_pa_sub <- as.data.frame(raster::rasterToPoints(r_pa_sub)) 
df_pa_all <- tidyr::expand(df_pa_sub, x, y) %>% left_join(df_pa_sub) %>% replace_na(list(layer = 0))

ggplot() + 
  geom_sf(data=pa_sub_moll, fill=NA, colour="black") + 
  geom_tile(data=df_pa_all, aes(x, y), fill="transparent", colour="black") + 
  coord_sf(ndiscr=F) + theme_minimal() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/pa_example_shape.png", dpi=1000, height=3.3, width=3.8)

########################################

ggplot() + 
  geom_tile(data=df_pa_all, aes(x, y, fill = layer)) + 
  geom_sf(data = pa_sub_moll, fill=NA, colour="black") + 
  geom_tile(data=df_pa_all, aes(x, y), fill="transparent", colour="black") + 
  scale_x_continuous() + scale_y_continuous() + 
  scale_fill_gradientn(name="% protected", colors=bluered) + 
  theme_minimal() + coord_sf(ndiscr=F) + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(0.8, 'cm'), #change legend key width
        legend.margin=margin(t = 0, l=-0.5, unit='cm'),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/pa_example_coverage.png", dpi=1000, height=3.3, width=5)

########################################

# Plot terrestrial maps raw of example PA

# Need to read data directly from Worldclim v2 & EarthEnv files!!!

# Create maps
bio1 <- raster::raster("extdata/wc2.0_bio_30s_01_wm.nc")
bio1_sub <- raster::crop(bio1, pa_sub_moll, snap="out")
bio1_sub <- as.data.frame(rasterToPoints(bio1_sub))
colnames(bio1_sub)

# Plot map
ggplot() + geom_tile(data=bio1_sub, aes(x=x, y=y, fill=wc2.0_bio_30s_01_wm)) + 
  scale_fill_gradientn(name="Temp (°C)", colours = bluered, na.value="transparent",
                       values=c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  geom_sf(data = pa_sub_moll, fill=NA, colour="black") + 
  #geom_tile(data=df_pa_all, aes(x, y), fill="transparent", colour="black") + 
  theme_minimal() + coord_sf(ndiscr=F) + 
  theme(axis.title=element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(0.8, 'cm'), #change legend key width
        legend.margin=margin(t = 0, l=-0.5, unit='cm'),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(paste0("figures/pa_example_bio1.png"), height=3.3, width=4.8, dpi=1000)

########################################

# Plot terrestrial maps cut for example PA

# Create maps
bio1 <- raster::raster("extdata/wc2.0_bio_30s_01_perc.nc")
bio1_sub <- raster::crop(bio1, pa_sub_moll, snap="out")

m_ee <- readRDS("data/summary_wc_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- floor(m_ee[1,2])
m_ee[102,2] <- floor(m_ee[102,2])
m_ee[203,2] <- floor(m_ee[203,2])
m <- m_ee %>% group_split(var)

bio1_df <- as.data.frame(m[[1]]) %>% 
  rowwise() %>% mutate(val=paste0("(", round(x, 2), ",", round(y, 2), "]")) %>%
  select(-c(x,y,var)) %>% as.data.frame()

# Subsitute values
bio1_sub <- raster::subs(bio1_sub, bio1_df, by="z")

# Plot map
# Specify colour scheme
bluered2 <- rev(scico(17, palette = 'roma'))
ggplot() + ggR(bio1_sub, geom_raster=T, ggLayer=T) + 
  geom_sf(data = pa_sub_moll, fill=NA, colour="black") + 
  geom_tile(data=df_pa_all, aes(x, y), fill="transparent", colour="black") + 
  scale_fill_manual(name="Temp (°C)", values=bluered2, na.value="transparent", na.translate=F) + 
  scale_x_continuous() + scale_y_continuous() + 
  theme_minimal() + coord_sf(ndiscr=F) + 
  theme(axis.title=element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.title = element_text(size=14),
        legend.text = element_text(size=10),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.margin=margin(t = 0, l=-0.5, unit='cm'),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(paste0("figures/pa_example_bio1_perc.png"), height=3.3, width=4.9, dpi=1000)

########################################

# Create table

perc_prot <- raster::crop(raster::raster("extdata/pa_cov_total.nc"), pa_sub_moll)
perc_prot[is.na(perc_prot)] <- 0
perc_prot <- raster::resample(perc_prot, bio1_sub)
summary(perc_prot)
bio1_dat <- stack(bio1_sub, perc_prot)

df_bio1_sub <- as.data.frame(raster::rasterToPoints(bio1_dat))
df_bio1_sub <- df_bio1_sub %>% left_join(bio1_df, by=c("val"="z"))
df_bio1_sub %>% group_by(val) %>% head(n=8)

########################################

# Plot of temperature

# Read and prepare data
ter_dat <- readRDS("data/summary_ind_ter_perc.rds")
head(ter_dat)
colnames(ter_dat) <- c("path", "var", "I-II", "III-IV",  "V-VI", "Not-designated", "Total", "n")
ter_dat$sum <- rowSums(ter_dat[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

ter_dat$`I-II` <- ifelse(ter_dat$sum > ter_dat$Total, 
                         ifelse(ter_dat$`I-II` > ter_dat$Total, ter_dat$Total, ter_dat$`I-II`), 
                         ter_dat$`I-II`)
ter_dat$`III-IV` <- ifelse(ter_dat$sum > ter_dat$Total, 
                           ifelse(ter_dat[,c("I-II")] == ter_dat$Total, 0,
                                  ifelse(rowSums(ter_dat[,c("I-II", "III-IV")], na.rm=T) >= ter_dat$Total,
                                         ter_dat$Total-ter_dat$`I-II`,
                                         ter_dat$`III-IV`)), 
                           ter_dat$`III-IV`)
ter_dat$`V-VI` <- ifelse(ter_dat$sum > ter_dat$Total, 
                         ifelse(rowSums(ter_dat[,c("I-II", "III-IV")], na.rm=T) == ter_dat$Total, 0, 
                                ifelse(rowSums(ter_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= ter_dat$Total,
                                       ter_dat$Total-rowSums(ter_dat[,c("I-II", "III-IV")], na.rm=T),
                                       ter_dat$`V-VI`)), 
                         ter_dat$`V-VI`)
ter_dat$`Not-designated` <- ifelse(ter_dat$sum > ter_dat$Total, 
                                   ifelse(rowSums(ter_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T) == ter_dat$Total, 0,
                                          ifelse(rowSums(ter_dat[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= ter_dat$Total,
                                                 ter_dat$Total-rowSums(ter_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                 ter_dat$`Not-designated`)), 
                                   ter_dat$`Not-designated`)

ter_dat <- ter_dat %>% dplyr::select(-c(Total, sum)) %>% 
  tidyr::gather(iucn_cat, perc, -c(path, var, n)) %>% 
  mutate(iucn_cat = factor(iucn_cat, levels=c("Not-designated", "V-VI", "III-IV", "I-II"),
                           labels=c("Non-designated", "V-VI", "III-IV", "I-II"))) %>% drop_na()

####################

#' ### Hyper-geometric distribution

# Total area & Total area protected
(tot_sum <- ter_dat %>% group_by(path, var) %>% summarise(sum=sum(n)/n(), prot_cells=sum((perc*n))) %>%
   ungroup() %>% group_by(path) %>% summarise(sum=sum(sum), prot_cells=sum(prot_cells)) %>%
   mutate(prop_prot=prot_cells/sum))

# Global area climate
(clim_area <- ter_dat %>% group_by(path,var) %>% summarise(area_clim=sum(n)/n()) %>% 
    left_join(tot_sum))

# Expected proportion climate  
n_bin <- clim_area %>% group_by(path) %>% summarise(n_bin = n())
(exp_val <- clim_area %>% left_join(n_bin) %>% mutate(prop_clim = area_clim/sum*n_bin) %>% 
    mutate(exp = (prop_clim*prop_prot),
           exp_aichi = 15*prop_clim,
           var_exp = (prop_clim*prop_prot*(1-prop_clim)*(1-prop_prot)/(sum-1))))

####################

m_ee <- readRDS("data/summary_wc_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- floor(m_ee[1,2])
m_ee[102,2] <- floor(m_ee[102,2])
m_ee[203,2] <- floor(m_ee[203,2])
colnames(m_ee) <- c("path", "x", "y", "var")
m_ee$path <- paste0(sub("_30s_", "", sub("wc2.0_", "", m_ee$path)), "_perc")
m_ee$var2 <- round(rowMeans(m_ee[, c("x", "y")]),1)

m <- readRDS("data/summary_earthenv_perc_optim.rds")
colnames(m) <- c("path", "x", "y", "var")
m$path <- "elevation_perc"
m$var2 <- round(rowMeans(m[, c("x", "y")]),0)
m_ee <- rbind(m_ee, m); rm(m)

ter_dat %<>% filter(path %in% c("bio01_perc", "bio12_perc", "elevation_perc")) %>% left_join(exp_val) %>% 
  left_join(as.data.frame(m_ee)) %>% 
  mutate(path = factor(path, levels =  c("bio01_perc", "bio12_perc", "elevation_perc"),
                       labels = c("Annual mean temp. (°C)", "Annual precipitation (mm)", "Elevation (m)")))

###
# NOTE: If some summaries still have NAs after left_join, check if re-classification is correct
###

# goodness of fit test
test <- ter_dat %>% group_by(path, var) %>% summarise(perc=sum(perc), exp=mean(exp)) %>%
  group_by(path) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path) %>% summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
                               df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
                               pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

ter_dat %>% filter(path=="Annual mean temp. (°C)") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc), width= 1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + facet_grid(. ~ path, switch="x", scales="free_x") + 
  geom_text(data=test %>% filter(path=="Annual mean temp. (°C)"), aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test %>% filter(path=="Annual mean temp. (°C)"), aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test %>% filter(path=="Annual mean temp. (°C)"), aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  labs(y="% of area protected") + # Annual mean temperature is too long
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector(ter_dat %>% filter(path=="Annual mean temp. (°C)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01)), limits=c(0,48)) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title.x=element_blank(), strip.background = element_blank(),
                     axis.title.y=element_text(size=12, face="bold"), strip.text = element_text(size=12, face="bold"),
                     axis.text = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
ggsave(paste0("figures/pa_example_bio1_bar.png"), height=3.3, width=3.3, dpi=1000)

########################################

# Plot terrestrial protection map

# Create maps
bio1 <- raster::raster("extdata/wc2.0_bio_30s_01_perc.nc")
bio1 <- raster::crop(bio1, pa_sub_moll, snap="out")
bio1 <- as.data.frame(rasterToPoints(bio1))
colnames(bio1)

bio1 <- bio1 %>% mutate(perc2 = as.character(cut(wc2.0_bio_30s_01_perc, breaks=seq(0,10,1)^2, include.lowest=T,
                                        labels=c("0 - 1", "1 - 4", "4 - 9", "9 - 16", 
                                                 "16 - 25", "25 - 36", "36 - 49", "49 - 64", "64 - 81", "81 - 100")))) %>% drop_na() %>%
  mutate(perc2 = factor(if_else(wc2.0_bio_30s_01_perc == 0, "0", perc2),
                        levels=c("0", "0 - 1", "1 - 4", "4 - 9", "9 - 16", 
                                 "16 - 25", "25 - 36", "36 - 49", "49 - 64", "64 - 81", "81 - 100")))
redblue <- c("grey", scico(11, palette = 'roma'))

ggplot() + geom_tile(data=bio1, aes(x=x, y=y, fill=perc2)) + 
  scale_fill_manual(values = redblue, name = "% Protected", na.value="transparent",
                    drop=F, guide = guide_legend(reverse = TRUE)) + 
  #scale_fill_gradientn(colours = bluered, name = "% protected",
  #                     na.value="transparent") + 
  geom_sf(data = pa_sub_moll, fill=NA, colour="black") + 
  #geom_tile(data=df_pa_all, aes(x, y), fill="transparent", colour="black") + 
  scale_x_continuous() + scale_y_continuous() + 
  theme_minimal() + coord_sf(ndiscr=F) + 
  theme(axis.title=element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.margin=margin(t = 0, l=-0.5, unit='cm'),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave("figures/pa_example_bio1_prot_map.png", height=3.4, width=5.1, dpi=1000)

########################################