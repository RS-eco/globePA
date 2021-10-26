#' ---
#' title: "Global Analysis of Protected Areas - Create environmental summary figures"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#Automatically install required packages, which are not yet installed
packages <- c("tidyverse", "patchwork", "ggpubr", "ggpmisc")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) instTotal.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

########################################

# Plot of temperature, precipitation/salinity and elevation/depth

# Read and prepare data
ter_dat <- readRDS("data/summary_ind_ter_bins.rds")
head(ter_dat)
colnames(ter_dat) <- c("path", "var", "I-II", "III-IV", "V-VI", "Not-designated", "Total", "n")
ter_dat$sum <- rowSums(ter_dat[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

#####

# How can sum of individual protection be smaller than total?
ter_dat[round(ter_dat$sum, 1) < round(ter_dat$Total,1),]

#####

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

head(ter_dat)

# Number of cells
ter_dat %>% group_by(path) %>% summarise(no_cells=sum(n))

# Area summary
ter_dat <- ter_dat %>% dplyr::select(-c(Total, sum)) %>% 
  tidyr::gather(iucn_cat, perc, -c(path, var, n)) %>% 
  mutate(iucn_cat = factor(iucn_cat, levels=c("Not-designated", "V-VI", "III-IV", "I-II"),
                           labels=c("Non-designated", "V-VI", "III-IV", "I-II"))) %>% drop_na()


ter_dat %>% filter(path=="bio12_bins", iucn_cat == "I-II") %>% 
  ungroup() %>% dplyr::select(perc) %>% summary()

# Summary
ter_dat %>% group_by(path, var) %>% summarise(sum=sum(perc)) %>% 
  summarise(max(sum))

# Plot frequeny plots of envdata
ter_dat %>% ggplot(aes(x = var, y=perc, fill=iucn_cat)) + 
  geom_area(stat="identity", position="stack") + facet_wrap(.~ path, scales="free") + 
  labs(x="", y="% of area protected ") + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=expansion(mult=c(0,.1)))

###
# NOTE: If some summaries still have NAs after left_join, check if re-classification is correct
###

####################

# Calculate % area coverage per bin

# Total number of cells
(tot_sum <- ter_dat %>% group_by(path, var) %>% summarise(n=sum(n)/4) %>%
    group_by(path) %>% summarise(sum=sum(n)))

# % number of cells per bin
(area_val <- ter_dat %>% group_by(path, var) %>% summarise(n=sum(n)/4) %>% 
  left_join(tot_sum) %>% mutate(perc_area=(n/sum)*100))

# Test if area adds up to 100 %
area_val %>% summarise(sum(perc_area))

# Area per cell
(area_cell <- 860050) #m2

# Number of cells
(number_cells <- unique(tot_sum$sum))

# Total area (m2)
(tot_area <- area_cell*number_cells)

# Total area (km2)
tot_area/1000000

# Total area (10000 km2)
tot_area10000 <- tot_area/(1000000*10000)

# Total area in 10000 km2
area_val %>% summarise(sum(perc_area*tot_area10000))

####################

bio01_dat <- ter_dat %>% filter(path=="bio01_bins") %>% left_join(area_val) %>% 
  left_join(data.frame(x=seq(-55, 39, by=1), y=seq(-54, 40, by=1), z=1:95), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y)))

p1 <- bio01_dat %>% ggplot() + 
  geom_bar(aes(x = var2, y=perc, fill=iucn_cat), stat="identity", position="stack", width=1) +
  geom_line(aes(x=var2, y=perc_area)) + # Area line 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(a)") + 
  labs(x="Annual mean temp. (°C)", y="Area protected (%)") + # Annual mean temperature is too long
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100)) +
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank()) + coord_trans(y="sqrt")

bio12_dat <- ter_dat %>% filter(path=="bio12_bins") %>% left_join(area_val) %>% 
  left_join(data.frame(x=seq(0, 11900, by=100), y=seq(100, 12000, by=100), z=1:120), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y)))

p2 <- bio12_dat %>% ggplot() + 
  geom_bar(aes(x = var2, y=perc, fill=iucn_cat), stat="identity", position="stack", width=100) +
  geom_line(aes(x=var2, y=perc_area)) + # Area line 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(b)") + 
  labs(x="Annual precipitation (mm)") + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100)) + 
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position="none", axis.title.y=element_blank(), 
        panel.grid.minor = element_blank()) + coord_trans(y="sqrt")

p3 <- ter_dat %>% filter(path=="elevation_bins") %>% left_join(area_val) %>% 
  left_join(data.frame(x=seq(-500, 9900, by=100), y=seq(-400, 10000, by=100), z=1:105), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y))) %>% 
  ggplot(aes(x = var2, y=perc, fill=iucn_cat)) + 
  geom_bar(stat="identity", position="stack", width=100) +
  geom_line(aes(x=var2, y=perc_area)) + # Area line 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(c)") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100), 
                     sec.axis = sec_axis(~ .*138.3044, name="Area (10000 km2)", breaks=round(c(0, 1, 5, 15)*138.3044, digits=0))) + 
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  labs(x="Elevation (m)", y="", fill="IUCN") + 
  theme_bw() + theme(legend.position = "bottom", axis.title.y=element_blank(), 
                     panel.grid.minor = element_blank()) +
  coord_trans(y="sqrt")

#########################

# Plot of environmental variables for marine areas

# Read and prepare data

marspec_dat <- readRDS("data/summary_ind_mar_bins.rds")
head(marspec_dat)
colnames(marspec_dat) <- c("path", "var", "I-II", "III-IV", "V-VI", "Not-designated", "Total", "n")

marspec_dat$sum <- rowSums(marspec_dat[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T)

marspec_dat$`I-II` <- ifelse(marspec_dat$sum > marspec_dat$Total, 
                             ifelse(marspec_dat$`I-II` > marspec_dat$Total, marspec_dat$Total, marspec_dat$`I-II`), 
                             marspec_dat$`I-II`)
marspec_dat$`III-IV` <- ifelse(marspec_dat$sum > marspec_dat$Total, 
                               ifelse(marspec_dat[,c("I-II")] == marspec_dat$Total, 0,
                                      ifelse(rowSums(marspec_dat[,c("I-II", "III-IV")], na.rm=T) >= marspec_dat$Total,
                                             marspec_dat$Total-marspec_dat$`I-II`,
                                             marspec_dat$`III-IV`)), 
                               marspec_dat$`III-IV`)
marspec_dat$`V-VI` <- ifelse(marspec_dat$sum > marspec_dat$Total, 
                             ifelse(rowSums(marspec_dat[,c("I-II", "III-IV")], na.rm=T) == marspec_dat$Total, 0, 
                                    ifelse(rowSums(marspec_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T) >= marspec_dat$Total,
                                           marspec_dat$Total-rowSums(marspec_dat[,c("I-II", "III-IV")], na.rm=T),
                                           marspec_dat$`V-VI`)), 
                             marspec_dat$`V-VI`)
marspec_dat$`Not-designated` <- ifelse(marspec_dat$sum > marspec_dat$Total, 
                                       ifelse(rowSums(marspec_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T) == marspec_dat$Total, 0,
                                              ifelse(rowSums(marspec_dat[,c("I-II", "III-IV", "V-VI", "Not-designated")], na.rm=T) >= marspec_dat$Total,
                                                     marspec_dat$Total-rowSums(marspec_dat[,c("I-II", "III-IV", "V-VI")], na.rm=T),
                                                     marspec_dat$`Not-designated`)), 
                                       marspec_dat$`Not-designated`)

###

# Why are some areas 0, but have a value for protection???
# only the case for biogeo08

# => Re-check this is still the case!

###

# Number of cells
marspec_dat %>% group_by(path) %>% summarise(no_cells=sum(n))

marspec_dat <- marspec_dat %>% dplyr::select(-c(Total, sum)) %>% 
  tidyr::gather(iucn_cat,perc, -c(path, var, n)) %>%
  mutate(iucn_cat = factor(iucn_cat, levels=c("Not-designated", "V-VI", "III-IV", "I-II"),
                           labels=c("Non-designated", "V-VI", "III-IV", "I-II"))) %>% 
  group_by(path, var, n, iucn_cat) %>%
  summarise(perc=sum(perc,na.rm=T))

# Summary should be maximum of 100!!!
marspec_dat %>% group_by(path, var) %>% summarise(sum=sum(perc)) %>% 
  summarise(max(sum))

# Plot frequeny plots of envdata
marspec_dat %>% 
  ggplot(aes(x = var, y=perc, fill=iucn_cat)) + 
  geom_area(stat="identity", position="stack") + facet_wrap(.~ path, scales="free") + 
  labs(x="", y="% of area protected") + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=expansion(mult=c(0,.1)))

####################

# Calculate % coverage per bin

# Total number of cells
(tot_sum <- marspec_dat %>% group_by(path, var) %>% summarise(n=sum(n)/4) %>%
   group_by(path) %>% summarise(sum=sum(n)))

# % number of cells per bin
(area_val <- marspec_dat %>% group_by(path, var) %>% summarise(n=sum(n)/4) %>% 
    left_join(tot_sum) %>% mutate(perc_area=(n/sum)*100))

# Test if area adds up to 100 %
area_val %>% summarise(sum(perc_area))

# Area per cell
(area_cell <- 860050) #m2

# Number of cells
(number_cells <- unique(tot_sum$sum))

# Total area (m2)
(tot_area <- area_cell*number_cells)

# Total area (km2)
tot_area/1000000

# Total area (10000 km2)
tot_area10000 <- tot_area/(1000000*10000)

# Total area in 10000 km2
area_val %>% summarise(sum(perc_area*tot_area10000))

####################

p4 <- marspec_dat %>% filter(path=="biogeo13_bins") %>% left_join(area_val) %>%
  left_join(data.frame(x=seq(-500, 3400, by=100), y=seq(-400, 3500, by=100), z=1:40), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y))) %>% mutate(var2 = var2/100) %>% 
  ggplot(aes(x = var2, y=perc, fill=iucn_cat)) + 
  geom_bar(stat="identity", position="stack", width=1) +
  geom_line(aes(x=var2, y=perc_area)) + 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(d)") + 
  theme_bw() + labs(x="Mean annual SST (°C)", y="Area protected (%)") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100)) +
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position="none", panel.grid.minor = element_blank()) + coord_trans(y="sqrt")

p5 <- marspec_dat  %>% filter(path=="biogeo08_bins") %>% left_join(area_val) %>% 
  left_join(data.frame(x=seq(-100, 4400, by=100), y=seq(0, 4500, by=100), z=1:46), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y))) %>% 
  ggplot(aes(x = var2/100, y=perc, fill=iucn_cat)) + 
  geom_bar(stat="identity", position="stack", width=1) +
  geom_line(aes(x=var2/100, y=perc_area)) + 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(e)") + theme_bw() + 
  labs(x="Mean annual SSS (psu)") + scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100)) + 
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position="none", axis.title.y=element_blank(), 
        panel.grid.minor = element_blank()) + coord_trans(y="sqrt")

###
# Why are there values from -1 to 0!!!
###

p6 <- marspec_dat %>% filter(path == "bathy_bins") %>% left_join(area_val) %>%
  left_join(data.frame(x=seq(-12000, -100, by=100), y=seq(-11900, 0, by=100), z=1:120), by=c("var"="z")) %>% 
  mutate(var2=rowMeans(cbind(x,y))) %>% 
  ggplot(aes(x = var2, y=perc, fill=iucn_cat)) + 
  geom_bar(stat="identity", position="stack", width=100) +
  geom_line(aes(x=var2, y=perc_area)) + theme_bw() + 
  geom_text_npc(npcx=0.04,npcy=0.95, label="(f)") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0)), limits=c(0,100), breaks=c(0, 1, 5, 15, 30, 50, 100), 
                     sec.axis = sec_axis(~ .*362.2512, name="Area (10000 km2)", 
                                         breaks=round(c(0, 1, 5, 15, 30)*362.2512, digits=0))) + 
  scale_fill_manual(values=c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"), 
                    guide = guide_legend(reverse = TRUE)) + 
  theme_bw() + labs(x="Bathymetry (m)", y="") + 
  theme(legend.position="none", axis.title.y=element_blank(), 
        panel.grid.minor = element_blank()) + coord_trans(y="sqrt")

p7 <- ggpubr::as_ggplot(ggpubr::get_legend(p3))

p <- p1 + p2 + {p3 + theme(legend.position="none")} + p4 + p5 + p6 + plot_spacer() + p7 + plot_spacer() + 
  plot_layout(ncol=3, heights=c(4,4,1))
ggsave("figures/Figure2_bins.png", p, dpi=1000, width=8, height=5)
