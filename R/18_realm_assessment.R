#' ---
#' title: "Global Analysis of Protected Areas - Create bar chart for different realms"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#Automatically install required packages, which are not yet installed
packages <- c("sf", "raster", "exactextractr", "scico", "dplyr", "magrittr", "ggplot2", "patchwork")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Specify colour scheme
redblue <- c("grey", scico(11, palette = 'roma'))
redblue2 <- scico(255, palette = 'roma')

setwd("/home/matt/Documents/globePA")

# Install exactextractr from Github
#remotes::install_github("isciences/exactextractr")

########################################

# Load zoorealms data
#remotes::install_github("RS-eco/geodat")
library(geodat)
data(zoorealms)
zoorealms %<>% st_set_crs(4326)

plot(st_geometry(zoorealms))

# Remove invalid topologies and crop to desired extent
#zoorealms <- st_make_valid(zoorealms)
#zoorealms <- sf::st_crop(zoorealms, xmin=-180, ymin=-56, xmax=180, ymax=84); gc()

zoorealm2 <- lapply(zoorealms$Realm, function(x){
  dat <- zoorealms %>% filter(Realm == x)
  dat <- rgeos::gPolygonize(rgeos::gNode(as(as(dat, "Spatial"), "SpatialLines")))
  dat <- rgeos::gUnaryUnion(dat)
  dat <- sf::st_as_sf(dat)
  #dat <- sf::st_crop(dat, xmin=-180, ymin=-56, xmax=180, ymax=84)
  # Combine the individual polygons to one
  #outlinehigh <- sf::st_geometry(outlinehigh)
  #outlinehigh <- sf::st_cast(outlinehigh, "MULTILINESTRING")
  #outlinehigh <- sf::st_union(outlinehigh)
  return(dat)
})
zoorealm2 <- bind_rows(zoorealm2)
plot(st_geometry(zoorealm2))
zoorealm2$Realm <- zoorealms$Realm

# Re-project realm data
zoorealms <- sf::st_transform(zoorealm2, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
p1 <- ggplot() + geom_sf(data=zoorealms, aes(fill=Realm)) + coord_sf(datum=NA) + 
  scale_fill_manual(values=ggsci::pal_d3("category20")(11)) + 
  theme_bw() + theme(panel.border = element_blank(),
                     legend.position=c(0.04,0.5), legend.key.height=unit(0.4, "cm"))

if(!file.exists("data/ncells_zoorealm_perc.rds")){
  vars <- c("bio01", "bio12", "elevation", "bio01+bio12", "bio01+elevation", "bio12+elevation")
  sum_dat <- lapply(vars, function(i){
    if(!file.exists(paste0("data/ter_realm_pro_cell_", i, ".rds"))){
      if(!file.exists(paste0("extdata/", i, "_layerized.nc"))){
        # Load raster data
        prot_all <- raster::stack(paste0("extdata/", i, "_protected_perc.nc"))
        projection(prot_all) <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
        
        # Turn raster into categories
        prot_all <- raster::cut(prot_all, breaks=c(0,0.001,round(10^(seq(0,2,0.2)),0)), include.lowest=T)
        levels(prot_all) <- list(data.frame(ID = c(1:12),
                                            perc = c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                                                     "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))
        prot_all <- layerize(prot_all, classes=c(1:12), filename=paste0("extdata/", i, "_layerized.nc"),
                             force_v4=TRUE, compression=9, overwrite=T); gc()
      } else{
        prot_all <- raster::stack(paste0("extdata/", i, "_layerized.nc"))
      }
      
      # Sum of defined raster values within the polygon
      zoo_realm_prot_cells <- exact_extract(prot_all, zoorealms, 'sum'); rm(prot_all); gc()
      zoo_realm_prot_cells$realm <- zoorealms$Realm
      zoo_realm_prot_cells$var <- i
      saveRDS(zoo_realm_prot_cells, file=paste0("data/ter_realm_pro_cell_", i, ".rds"))
    } else{
      zoo_realm_prot_cells <- readRDS(paste0("data/ter_realm_pro_cell_", i, ".rds"))
      head(zoo_realm_prot_cells)
    }
    return(zoo_realm_prot_cells)
  })
  sum_dat <- data.table::rbindlist(sum_dat)
  sum_dat <- sum_dat %>% dplyr::select(realm, var, paste0("sum.X", 1:12))
  colnames(sum_dat) <- c("realm", "var", "0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                         "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")
  sum_dat %<>% tidyr::gather(perc, area, -c(realm, var)) %>% tidyr::replace_na(list(area = 0))
  saveRDS(sum_dat, file="data/ncells_zoorealm_perc.rds", compress="xz")
}

ncells_zoorealm <- readRDS("data/ncells_zoorealm_perc.rds")
head(ncells_zoorealm)

ncells_zoorealm %<>% mutate(perc = factor(perc, levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                                                         "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100"))) %>% 
  mutate(var = factor(var, 
                    levels=c("bio01", "bio12", "elevation", "bio01+bio12", "bio01+elevation", "bio12+elevation"),
                    labels=c("Temp", "Prec", "Elevation", "Temp + Prec", "Temp + Elevation", "Prec + Elevation")))

total_area <- ncells_zoorealm %>% filter(var == "Temp") %>% group_by(realm) %>% summarise(total_area=sum(area))

p2 <- ncells_zoorealm %>% left_join(total_area, by="realm") %>% 
  mutate(lab= round((area/total_area),3)*100) %>%
  mutate(lab = case_when(lab >= 1 ~ lab)) %>% # Drop labels for values smaller than 1 %
  ggplot(aes(x=var, y=area/1e+6, fill=perc,
             label = lab)) + 
  geom_bar(width=0.9, stat="identity", position=position_stack(reverse=T)) + 
  geom_text(size=2, position = position_stack(vjust=0.5, reverse=T)) + 
  facet_wrap(. ~ realm, scales="free_y") + theme_bw() + 
  scale_y_continuous(name=expression(Area~(km^2)), limits=c(0,NA), expand=expansion(mult=c(0,.01))) + 
  scale_fill_manual(name="% protected", values=redblue,
                    guide = guide_legend(reverse = TRUE)) + 
  theme(strip.background = element_blank(), strip.text = element_text(size=10, face="bold"),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=12, face="bold"),
        legend.key.height=unit(0.4, "cm"))
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p2))

# Unit of area correct?

p <- p1 / ((p2 & theme(legend.position="none")) + inset_element(leg, left = 0.75, bottom = -0.22, right = 1, top = 0.25)) + plot_layout(heights=c(1,3))
ggsave("figures/ter_realm_summary.png", dpi=1000, width=7, height=9)

########################################

# Load marinerealms data
library(geodat)
data(marinerealms) 
marinerealms %<>% st_set_crs(4326)
marinerealms <- sf::st_make_valid(marinerealms)

# Re-project realm data
marinerealms <- sf::st_transform(marinerealms, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"); gc()

#marinerealms$ID <- cut(marinerealms$Realm, breaks=c(1,2,3,9,10,11,29,30,31), right=F,
#                       labels=c("Inner Baltic Sea", "Black Sea", "NE and NW Atlantic and \n Mediterranean, Arctic \n and North Pacific",
#                                "Mid-tropical North Pacific Ocean", "South-east Pacific", "Mid-Atlantic, Pacific \n and Indian Oceans",
#                                "North West Pacific", "Southern Ocean"))
marinerealms$ID2 <- cut(marinerealms$Realm, breaks=c(1,2,3,6,8,9,10,11,13,17,18,24,26,29,30,31), right=F,
                        labels=c("Inner Baltic Sea", "Black Sea", "NE and Atlantic and \n Mediterranean", "Arctic and N Pacific",
                                 "N Atlantic boreal & \n sub Arctic", "Mid-tropical N Pacific Ocean", "South-east Pacific", 
                                 "Tropical W Atlantic & \n Tropical E Pacific", "Coastal Indian Ocean, \n W Pacific, ...", 
                                 "Mid South Tropical Pacific", "Open Atlantic, Indian & \n Pacific Oceans", "S South America", 
                                 "S Africa, S Australia & \n New Zealand", "North West Pacific", "Southern Ocean"))
marinerealms %<>% group_by(ID2) %>%
  summarise(); gc()

marrealm2 <- lapply(marinerealms$ID2, function(x){
  dat <- marinerealms %>% filter(ID2 == x)
  dat <- rgeos::gPolygonize(rgeos::gNode(as(as(dat, "Spatial"), "SpatialLines")))
  dat <- rgeos::gUnaryUnion(dat)
  dat <- sf::st_as_sf(dat)
  # Combine the individual polygons to one
  #outlinehigh <- sf::st_geometry(outlinehigh)
  #outlinehigh <- sf::st_cast(outlinehigh, "MULTILINESTRING")
  #outlinehigh <- sf::st_union(outlinehigh)
  return(dat)
})
marrealm2 <- bind_rows(marrealm2)
marrealm2$ID2 <- c("Inner Baltic Sea", "Black Sea", "NE and Atlantic and \nMediterranean", "Arctic and N Pacific",
                   "N Atlantic boreal & \nsub Arctic", "Mid-tropical N \nPacific Ocean", "South-east Pacific", 
                   "Tropical W Atlantic & \nTropical E Pacific", "Coastal Indian Ocean, \nW Pacific, ...", 
                   "Mid South Tropical Pacific", "Open Atlantic, Indian & \nPacific Oceans", "S South America", 
                   "S Africa, S Australia & \nNew Zealand", "North West Pacific", "Southern Ocean")

p3 <- ggplot() + geom_sf(data=marrealm2, aes(fill=ID2)) + coord_sf(datum=NA) + 
  scale_fill_manual(name="Realm", values=ggsci::pal_d3("category20")(15)) + 
  theme_bw() + theme(panel.border = element_blank(), legend.position="bottom",
                     legend.margin=margin(t=0,r=0,b=0, l=0, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     legend.title = element_text(size=12, face="bold", 
                                                 angle=90, vjust=0.5, hjust=0.5) #,
                     #legend.key.width = unit(1, "cm")
  ) + 
  guides(fill = guide_legend(nrow = 5))

if(!file.exists("data/ncells_marinerealm_perc.rds")){
  vars <- c("biogeo13",  "biogeo08", "bathy", "biogeo08+biogeo13",  "bathy+biogeo13", "bathy+biogeo08")
  sum_dat <- lapply(vars, function(i){
    if(!file.exists(paste0("data/mar_realm_pro_cell_", i, ".rds"))){
      if(!file.exists(paste0("extdata/", i, "_layerized.nc"))){
        # Load raster data
        prot_all <- raster::raster(paste0("extdata/", i, "_protected_perc.nc"))
        projection(prot_all) <-  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
        
        # Turn raster into categories
        prot_all <- raster::cut(prot_all, breaks=c(0,0.001,round(10^(seq(0,2,0.2)),0)), include.lowest=T); gc()
        levels(prot_all) <- list(data.frame(ID = c(1:12),
                                            perc = c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                                                     "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))
        prot_all <- layerize(prot_all, classes=c(1:12), filename=paste0("extdata/", i, "_layerized.nc"),
                             force_v4=TRUE, compression=9, overwrite=T); gc()
      } else{
        prot_all <- raster::stack(paste0("extdata/", i, "_layerized.nc"))
      }
      
      # Sum of defined raster values within the polygon
      mar_realm_prot_cells <- exact_extract(prot_all, marinerealms, 'sum'); rm(prot_all); gc()
      mar_realm_prot_cells$realm <- marinerealms$ID2
      mar_realm_prot_cells$var <- i
      saveRDS(mar_realm_prot_cells, file=paste0("data/mar_realm_pro_cell_", i, ".rds"))
    } else{
      mar_realm_prot_cells <- readRDS(paste0("data/mar_realm_pro_cell_", i, ".rds"))
    }
    return(mar_realm_prot_cells)
  })
  sum_dat <- data.table::rbindlist(sum_dat)
  sum_dat <- sum_dat %>% dplyr::select(realm, var, paste0("sum.X", 1:12))
  colnames(sum_dat) <- c("realm", "var", "0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                         "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")
  sum_dat %<>% tidyr::gather(perc, area, -c(realm, var)) %>% tidyr::replace_na(list(area = 0))
  saveRDS(sum_dat, file="data/ncells_marinerealm_perc.rds", compress="xz")
}

ncells_marinerealm <- readRDS("data/ncells_marinerealm_perc.rds")
head(ncells_marinerealm)

ncells_marinerealm %<>% 
  mutate(perc = factor(perc, levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", 
                                      "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100"))) %>% 
  mutate(var = factor(var, 
                      levels=c("biogeo13", "biogeo08", "bathy", "biogeo08+biogeo13", "bathy+biogeo13", "bathy+biogeo08"),
                      labels=c("SST", "SSS", "Bathy", "SST + SSS", "SST + Bathy", "SSS + Bathy")))

total_area <- ncells_marinerealm %>% filter(var == "SST") %>% group_by(realm) %>% summarise(total_area=sum(area))

p4 <- ncells_marinerealm %>% left_join(total_area, by="realm") %>% 
  mutate(realm = factor(realm, labels=c("Inner \n Baltic Sea", "Black Sea", "NE and Atlantic \n and Mediterranean", "Arctic and \n N Pacific",
                                        "N Atlantic boreal & \n sub Arctic", "Mid-tropical N \n Pacific Ocean", "South-east \n Pacific", 
                                        "Tropical W Atlantic & \n Tropical E Pacific", "Coast. Indian Ocean,\n W Pacific, ...", 
                                        "Mid South \n Tropical Pacific", "Open Atlantic, Indian\n & Pacific Oceans", "S South \n America", 
                                        "S Africa, S Australia\n & New Zealand", "North West \n Pacific", "Southern Ocean"))) %>%
  mutate(lab= round((area/total_area),3)*100) %>%
  mutate(lab = case_when(lab >= 1 ~ lab)) %>% # Drop labels for values smaller than 1 %
  ggplot(aes(x=var, y=area/1e+6, fill=perc,
             label = lab)) + 
  geom_bar(width=0.9, stat="identity", position=position_stack(reverse=T)) + 
  geom_text(size=2, position = position_stack(vjust=0.5, reverse=T)) + 
  facet_wrap(. ~ realm, scales="free_y", ncol=5) + theme_bw() + 
  scale_y_continuous(name=expression(Area~(km^2)), limits=c(0,NA), expand=expansion(mult=c(0,.01))) + 
  scale_fill_manual(name="% protected", values=redblue) + 
  theme(strip.background = element_blank(), strip.text = element_text(size=6.5),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=12, face="bold"),
        legend.margin=margin(t=0,r=0.4,b=0, l=0, "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position="bottom")
#leg <- ggpubr::as_ggplot(ggpubr::get_legend(p4))

# Unit of area correct?

p <- p3 / ((p4 #& theme(legend.position="none")
) #+ inset_element(leg, left = 0.7, bottom = -0.22, right = 1, top = 0.25))
) + plot_layout(heights=c(2,4))
ggsave("figures/mar_realm_summary.png", dpi=1000, width=7, height=9)
