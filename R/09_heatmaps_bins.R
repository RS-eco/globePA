#' ---
#' title: "Global Analysis of Protected Areas - Create climate summary heatmap"
#' author: "RS-eco"
#' ---

#Automatically install required packages, which are not yet installed
rm(list=ls()); invisible(gc())
packages <- c("sp", "raster", "tidyverse", "patchwork", "RStoolbox", "sf", "ggpmisc", "scico")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Set working directory
workdir <- "C:/Users/admin/Documents/GitHub/globePA/"
setwd(workdir)

# Specify colour scheme
redblue <- c("grey", scico(11, palette = 'roma'))
redblue2 <- scico(255, palette = 'roma')

########################################

# Plot terrestrial heatmap

# For the heatmap we need to summarise the data by prec and tmean together!!!
ter_dat <- readRDS("data/summary_all_ter_bins.rds")
colnames(ter_dat)
colnames(ter_dat) <- c("bio01", "bio12", "elevation","I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")
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

dat <- ter_dat %>% 
    left_join(data.frame(bio1_x=seq(-55, 39, by=1), bio1_y=seq(-54, 40, by=1), z=1:95), by=c("bio01"="z")) %>%
    left_join(data.frame(bio12_x=seq(0, 11900, by=100), bio12_y=seq(100, 12000, by=100), z=1:120), by=c("bio12"="z")) %>% 
    left_join(data.frame(alt_x=seq(-500, 9900, by=100), alt_y=seq(-400, 10000, by=100), z=1:105), by=c("elevation"="z")) %>%
    drop_na()

(no_cells <- dat %>% ungroup() %>% summarise(no_cells=sum(n)) %>% as.numeric())

dat_sum1 <- dat %>% group_by(bio1_x, bio1_y, bio12_x, bio12_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))


df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1 <- dat_sum1 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=bio12_x, ymax=bio12_y, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(a)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position="none")

dat_sum2 <- dat %>% group_by(bio1_x, bio1_y, alt_x, alt_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(b)")

p2 <- dat_sum2 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=alt_x, ymax=alt_y, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(b)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none")

dat_sum3 <- dat %>% group_by(bio12_x, bio12_y, alt_x, alt_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(c)")

p3 <- dat_sum3 %>% ggplot() + geom_rect(aes(xmin=bio12_x, xmax=bio12_y, ymin=alt_x, ymax=alt_y, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(c)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none")

area_sum1 <- dat_sum1 %>% ungroup() %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum2 <- dat_sum2 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum3 <- dat_sum3 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)

area_sum_all <- dplyr::bind_rows(list(a=area_sum1, b=area_sum2, c=area_sum3), .id="id")
df <- data.frame(x = 0.04, y = 0.95, text = "(d)")

total_area <- area_sum_all %>% filter(id == "a") %>% summarise(area=sum(area)) %>% unlist()
total_area[1]

p4 <- area_sum_all %>% mutate(lab= (round(area/as.numeric(total_area[1]),3)*100)) %>%
    mutate(lab = case_when(lab >= 1 ~ lab)) %>% # Drop labels for values smaller than 1 %
    ggplot(aes(x=id, y=(area/10000)/1e+6, fill=perc2,
               label = lab)) + 
    geom_bar(width=0.85, stat="identity", position=position_stack(reverse=T)) + 
    geom_text(size=3, position = position_stack(vjust=0.5, reverse=T)) + 
    scale_fill_manual(name="% protected", values=redblue,
                      guide = guide_legend(reverse = TRUE)) + 
    #geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.015, npcy=0.934, label="(d)") + 
    theme_bw() + labs(x="", y=expression(Area~(10000~km^2))) + 
    scale_x_discrete(expand=expansion(mult=c(.4,.4))) +
    scale_y_continuous(expand=expansion(mult=c(0,.01))) + 
    theme(axis.text.x = element_text(size=12, face="bold"))

########################################

# Plot marine heatmap

# For the heatmap we need to summarise the data by prec and tmean together!!!

marspec_dat <- readRDS("data/summary_all_mar_bins.rds")
head(marspec_dat)
colnames(marspec_dat) <- c("bathy", "biogeo08", "biogeo13", "I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")

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

dat <- marspec_dat %>% 
    left_join(data.frame(biogeo13_x=seq(-500, 3400, by=100), biogeo13_y=seq(-400, 3500, by=100), z=1:40), by=c("biogeo13"="z")) %>%
    left_join(data.frame(biogeo08_x=seq(-100, 4400, by=100), biogeo08_y=seq(0, 4500, by=100), z=1:46), by=c("biogeo08"="z")) %>% 
    left_join(data.frame(bathy_x=seq(-12000, -100, by=100), bathy_y=seq(-11900, 0, by=100), z=1:120), by=c("bathy"="z")) %>%
    drop_na()

(no_cells <- dat %>% ungroup() %>% summarise(no_cells=sum(n)) %>% as.numeric())

dat_sum4 <- dat %>% group_by(biogeo13_x, biogeo13_y, biogeo08_x, biogeo08_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))
df <- data.frame(x = 0.04, y = 0.95, text = "(e)")

p5 <- dat_sum4 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                            ymin=biogeo08_x/100, ymax=biogeo08_y/100, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(e)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none")

dat_sum5 <- dat %>% group_by(biogeo13_x, biogeo13_y, bathy_x, bathy_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(f)")

p6 <- dat_sum5 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, ymin=bathy_x, 
                                            ymax=bathy_y, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(f)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none")

dat_sum6 <- dat %>% group_by(biogeo08_x, biogeo08_y, bathy_x, bathy_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(g)")

p7 <- dat_sum6 %>% ggplot() + geom_rect(aes(xmin=biogeo08_x/100, xmax=biogeo08_y/100, 
                                            ymin=bathy_x, ymax=bathy_y, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(g)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none")

area_sum4 <- dat_sum4 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum5 <- dat_sum5 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum6 <- dat_sum6 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)

area_sum_all <- dplyr::bind_rows(list(e=area_sum4, f=area_sum5, g=area_sum6), .id="id")
df <- data.frame(x = 0.04, y = 0.95, text = "(h)")

total_area <- area_sum_all %>% filter(id == "e") %>% summarise(area=sum(area)) %>% unlist()
total_area[1]

p8 <- area_sum_all %>% mutate(lab= (round(area/as.numeric(total_area[1]),3)*100)) %>%
    mutate(lab = case_when(lab >= 1 ~ lab)) %>% # Drop labels for values smaller than 1 %
    ggplot(aes(x=id, y=(area/10000)/1e+6, fill=perc2,
               label = lab)) + 
    geom_bar(width=0.85, stat="identity", position=position_stack(reverse=T)) + 
    geom_text(size=3, position = position_stack(vjust=0.5, reverse=T)) + 
    scale_fill_manual(name="% protected", values=redblue, 
                      guide = guide_legend(reverse = TRUE)) + 
    #geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.015, npcy=0.934, label="(h)") + 
    theme_bw() + labs(x="", y=expression(Area~(10000~km^2))) + 
    scale_x_discrete(expand=expansion(mult=c(.4,.4))) +
    scale_y_continuous(expand=expansion(mult=c(0,.01))) + 
    theme(axis.text.x = element_text(size=12, face="bold"))

########################################

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(nrow=2)
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(nrow=2, guides = 'collect')
ggsave(paste0("figures/Figure3_bins.png"), width=12, height=7, dpi=1000)

########################################

# Plot terrestrial heatmap of available climate space, protected climate space and proportion protected

# For the heatmap we need to summarise the data by prec and tmean together!!!
ter_dat <- readRDS("data/summary_all_ter_bins.rds")
colnames(ter_dat) <- c("bio01", "bio12", "elevation","I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")
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

dat <- ter_dat %>% 
    left_join(data.frame(bio1_x=seq(-55, 39, by=1), bio1_y=seq(-54, 40, by=1), z=1:95), by=c("bio01"="z")) %>%
    left_join(data.frame(bio12_x=seq(0, 11900, by=100), bio12_y=seq(100, 12000, by=100), z=1:120), by=c("bio12"="z")) %>% 
    left_join(data.frame(alt_x=seq(-500, 9900, by=100), alt_y=seq(-400, 10000, by=100), z=1:105), by=c("elevation"="z")) %>%
    drop_na()

dat_sum1 <- dat %>% group_by(bio1_x, bio1_y, bio12_x, bio12_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1a <- dat_sum1 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=bio12_x, ymax=bio12_y, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(a)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")),
                         colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("Available climate space") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(b)")
p1b <- dat_sum1 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=bio12_x, ymax=bio12_y, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(b)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")),colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("Protected climate space") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(c)")
p1c <- dat_sum1 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=bio12_x, ymax=bio12_y, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(c)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("% protected") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum2 <- dat %>% group_by(bio1_x, bio1_y, alt_x, alt_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(d)")
p2a <- dat_sum2 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=alt_x, ymax=alt_y, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(d)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(e)")
p2b <- dat_sum2 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=alt_x, ymax=alt_y, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(e)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(f)")
p2c <- dat_sum2 %>% ggplot() + geom_rect(aes(xmin=bio1_x, xmax=bio1_y, ymin=alt_x, ymax=alt_y, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(f)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())

dat_sum3 <- dat %>% group_by(bio12_x, bio12_y, alt_x, alt_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()
df <- data.frame(x = 0.04, y = 0.95, text = "(g)")

p3a <- dat_sum3 %>% ggplot() + geom_rect(aes(xmin=bio12_x, xmax=bio12_y, ymin=alt_x, ymax=alt_y, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(g)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(h)")
p3b <- dat_sum3 %>% ggplot() + geom_rect(aes(xmin=bio12_x, xmax=bio12_y, ymin=alt_x, ymax=alt_y, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(h)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() +theme(legend.position = "bottom", 
                      legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                      legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(i)")
p3c <- dat_sum3 %>% ggplot() + geom_rect(aes(xmin=bio12_x, xmax=bio12_y, ymin=alt_x, ymax=alt_y, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(i)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())

p <- p1a + p1b + p1c + p2a + p2b + p2c + p3a + p3b + p3c
ggsave(paste0("figures/ter_heatmap_all_bins.png"), width=12, height=15, dpi=1000)

########################################

# Plot marine heatmap of available climate space, protected climate space and proportion protected

# For the heatmap we need to summarise the data by prec and tmean together!!!

marspec_dat <- readRDS("data/summary_all_mar_bins.rds")
head(marspec_dat)
colnames(marspec_dat) <- c("bathy", "biogeo08", "biogeo13", "I-II", "III-IV", "Not-designated", "Total", "V-VI", "n")

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

dat <- marspec_dat %>% 
    left_join(data.frame(biogeo13_x=seq(-500, 3400, by=100), biogeo13_y=seq(-400, 3500, by=100), z=1:40), by=c("biogeo13"="z")) %>%
    left_join(data.frame(biogeo08_x=seq(-100, 4400, by=100), biogeo08_y=seq(0, 4500, by=100), z=1:46), by=c("biogeo08"="z")) %>% 
    left_join(data.frame(bathy_x=seq(-12000, -100, by=100), bathy_y=seq(-11900, 0, by=100), z=1:120), by=c("bathy"="z")) %>%
    drop_na()

dat_sum4 <- dat %>% group_by(biogeo13_x, biogeo13_y, biogeo08_x, biogeo08_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1a <- dat_sum4 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=biogeo08_x/100, ymax=biogeo08_y/100, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(a)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("Available climate space") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(b)")
p1b <- dat_sum4 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=biogeo08_x/100, ymax=biogeo08_y/100, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(b)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("Protected climate space") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(c)")
p1c <- dat_sum4 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=biogeo08_x/100, ymax=biogeo08_y/100, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(c)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("% protected") + 
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum5 <- dat %>% group_by(biogeo13_x, biogeo13_y, bathy_x, bathy_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(d)")
p2a <- dat_sum5 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(d)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(e)")
p2b <- dat_sum5 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(e)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(f)")
p2c <- dat_sum5 %>% ggplot() + geom_rect(aes(xmin=biogeo13_x/100, xmax=biogeo13_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(f)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())

dat_sum6 <- dat %>% group_by(biogeo08_x, biogeo08_y, bathy_x, bathy_y) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(g)")
p3a <- dat_sum6 %>% ggplot() + geom_rect(aes(xmin=biogeo08_x/100, xmax=biogeo08_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(g)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(h)")
p3b <- dat_sum6 %>% ggplot() + geom_rect(aes(xmin=biogeo08_x/100, xmax=biogeo08_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(h)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())
df <- data.frame(x = 0.04, y = 0.95, text = "(i)")
p3c <- dat_sum6 %>% ggplot() + geom_rect(aes(xmin=biogeo08_x/100, xmax=biogeo08_y/100, 
                                             ymin=bathy_x, ymax=bathy_y, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(i)") + 
    scale_fill_gradientn(name="%", colors=redblue2, limits=c(0,NA)) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank())

p <- p1a + p1b + p1c + p2a + p2b + p2c + p3a + p3b + p3c
ggsave(paste0("figures/mar_heatmap_all_bins.png"), width=12, height=15, dpi=1000)

########################################