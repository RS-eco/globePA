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
#workdir <- "C:/Users/admin/Documents/GitHub/globePA/"
#setwd(workdir)

# Specify colour scheme
redblue <- c("grey", scico(11, palette = 'roma'))
redblue2 <- scico(255, palette = 'roma')

########################################

# Plot terrestrial heatmap

# For the heatmap we need to summarise the data by prec and tmean together!!!
ter_dat <- readRDS("data/summary_all_ter_perc.rds")
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

m_ee <- readRDS("data/summary_wc_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- floor(m_ee[1,2])
m_ee[102,2] <- floor(m_ee[102,2])
m_ee[203,2] <- floor(m_ee[203,2])
m <- m_ee %>% group_split(var)

m2 <- readRDS("data/summary_earthenv_perc_optim.rds")

(no_cells <- ter_dat %>% ungroup() %>% summarise(no_cells=sum(n)) %>% as.numeric())

dat_sum1 <- ter_dat %>% group_by(bio01, bio12) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                         levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1 <- dat_sum1 %>% ggplot() + geom_raster(aes(x=bio01, y=bio12, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(a)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    scale_x_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[1]][c(2,26,50,75,99),2:3]), 1), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[4]][c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    coord_cartesian() + theme_bw() + theme(legend.position="none")

dat_sum2 <- ter_dat %>% group_by(bio01, elevation) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(b)")
p2 <- dat_sum2 %>% ggplot() + geom_raster(aes(x=bio01, y=elevation, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(b)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[1]][c(2,26,50,75,99),2:3]), 1), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m2[c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + theme(legend.position="none")

dat_sum3 <- ter_dat %>% group_by(bio12, elevation) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(c)")
p3 <- dat_sum3 %>% ggplot() + geom_raster(aes(x=bio12, y=elevation, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(c)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[4]][c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m2[c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme(legend.position="none")

area_sum1 <- dat_sum1 %>% ungroup() %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum2 <- dat_sum2 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum3 <- dat_sum3 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)

area_sum_all <- dplyr::bind_rows(list(a=area_sum1, b=area_sum2, c=area_sum3), .id="id")

total_area <- area_sum_all %>% filter(id == "a") %>% summarise(area=sum(area)) %>% unlist()
total_area[1]

#df <- data.frame(x = 0.04, y = 0.95, text = "(d)")
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

marspec_dat <- readRDS("data/summary_all_mar_perc.rds")
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

(no_cells <- marspec_dat %>% ungroup() %>% summarise(no_cells=sum(n)) %>% as.numeric())

m_ee <- readRDS("data/summary_marspec_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- m_ee[1,2]-1
m_ee[185,2] <- m_ee[185,2]-1
m_ee[335,2] <- m_ee[335,2]-1
m_ee[431,2] <- floor(m_ee[431,2])
m_ee[528,2] <- floor(m_ee[528,2])
m <- m_ee %>% group_split(var)

sapply(m, nrow)

dat_sum4 <- marspec_dat %>% group_by(biogeo13, biogeo08) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(e)")
p5 <- dat_sum4 %>% ggplot() + geom_raster(aes(x=biogeo13, y=biogeo08, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(e)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(breaks=c(2,25,48,72,95), labels=round(rowMeans(m[[5]][c(2,25,48,72,95),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,22,42,63,83), labels=round(rowMeans(m[[2]][c(2,22,42,63,83),2:3])/100, 1), 
                       limits=c(0,86), expand=c(0,0)) + 
    theme(legend.position="none")

dat_sum5 <- marspec_dat %>% group_by(biogeo13, bathy) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(f)")
p6 <- dat_sum5 %>% ggplot() + geom_raster(aes(x=biogeo13, y=bathy, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(f)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(breaks=c(2,25,48,72,95), labels=round(rowMeans(m[[5]][c(2,25,48,72,95),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[1]][c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme(legend.position="none")
p6

dat_sum5 %>% filter(perc2 == "50 - 100") %>% 
    left_join(m[[1]], by=c("bathy" = "z")) %>% arrange(desc(x))
dat_sum5 %>% filter(perc2 == "50 - 100") %>% 
    left_join(m[[1]], by=c("bathy" = "z"))
dat_sum5 %>% filter(perc2 == "50 - 100") %>% left_join(m[[5]], by=c("biogeo13" = "z")) %>%
    mutate(x=x/100, y=y/100)

dat_sum6 <- marspec_dat %>% group_by(biogeo08, bathy) %>% 
    summarise(perc=mean(Total), cells=sum(n)) %>% 
    #mutate(perc2 = as.character(cut(perc, breaks=c(0,1,2.5,7,16,25,40,100), include.lowest=T,
    #                                labels=c("0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                                         "25 - 40", "40 - 100")))) %>% drop_na() %>%
    #mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
    #                      levels=c("0", "0 - 1", "1 - 2.5", "2.5 - 7", "7 - 16", "16 - 25",
    #                               "25 - 40", "40 - 100")))
    mutate(perc2 = as.character(cut(perc, breaks=round(c(0,10^(seq(0,2,0.2))),0), include.lowest=T,
                                    labels=c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))) %>% drop_na() %>%
    mutate(perc2 = factor(if_else(perc == 0, "0", perc2),
                          levels=c("0", "0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 6", "6 - 10", "10 - 16", "16 - 25", "25 - 40", "40 - 63", "63 - 100")))

df <- data.frame(x = 0.04, y = 0.95, text = "(g)")
p7 <- dat_sum6 %>% ggplot() + geom_raster(aes(x=biogeo08, y=bathy, fill=perc2)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0615, npcy=0.934, label="(g)") + 
    scale_fill_manual(name="% protected", values=redblue) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + coord_cartesian() + theme_bw() + 
    scale_x_continuous(breaks=c(2,22,42,63,83), labels=round(rowMeans(m[[2]][c(2,22,42,63,83),2:3])/100, 1), 
                       limits=c(0,86), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(2,26,50,75,99), labels=round(rowMeans(m[[1]][c(2,26,50,75,99),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme(legend.position="none")
p7

dat_sum6 %>% filter(perc2 == "50 - 100") %>% left_join(m[[2]], by=c("biogeo08" = "z")) %>%
    mutate(x=x/100, y=y/100)
dat_sum6 %>% filter(perc2 == "50 - 100") %>% left_join(m[[2]], by=c("biogeo08" = "z")) %>%
    mutate(x=x/100, y=y/100) %>% filter(x >= 35)
dat_sum6 %>% filter(perc2 %in% c("30 - 50", "50 - 100")) %>% left_join(m[[2]], by=c("biogeo08" = "z")) %>%
    mutate(x=x/100, y=y/100) %>% filter(x >= 35.2, bathy <= 10) %>% summary()

area_sum4 <- dat_sum4 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum5 <- dat_sum5 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)
area_sum6 <- dat_sum6 %>% group_by(perc2) %>% summarise(area=sum(cells)*860050)

area_sum_all <- dplyr::bind_rows(list(e=area_sum4, f=area_sum5, g=area_sum6), .id="id")

total_area <- area_sum_all %>% filter(id == "e") %>% summarise(area=sum(area)) %>% unlist()
total_area[1]

#df <- data.frame(x = 0.04, y = 0.95, text = "(h)")
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

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(nrow=2, guides = 'collect')
ggsave(paste0("figures/Figure3_perc.png"), width=12, height=7, dpi=1000)
ggsave(paste0("figures/Figure3.pdf"), width=12, height=7, dpi=1000)

########################################

# Plot terrestrial heatmap of available climate space, protected climate space and proportion protected

# For the heatmap we need to summarise the data by prec and tmean together!!!
ter_dat <- readRDS("data/summary_all_ter_perc.rds")
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

m_ee <- readRDS("data/summary_wc_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- floor(m_ee[1,2])
m_ee[102,2] <- floor(m_ee[102,2])
m_ee[203,2] <- floor(m_ee[203,2])
m <- m_ee %>% group_split(var)

m2 <- readRDS("data/summary_earthenv_perc_optim.rds")

dat_sum1 <- ter_dat %>% group_by(bio01, bio12) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1a <- dat_sum1 %>% ggplot() + geom_raster(aes(x=bio01, y=bio12, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(a)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("Available climate space") + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(b)")
p1b <- dat_sum1 %>% ggplot() + geom_raster(aes(x=bio01, y=bio12, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(b)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("Protected climate space") + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(c)")
p1c <- dat_sum1 %>% ggplot() +  geom_raster(aes(x=bio01, y=bio12, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(c)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Annual precipitation (mm)") + 
    coord_cartesian() + ggtitle("% protected") + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum2 <- ter_dat %>% group_by(bio01, elevation) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6,
           prot_space2=perc*clim_space/100) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(d)")
p2a <- dat_sum2 %>% ggplot() + geom_raster(aes(x=bio01, y=elevation, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(d)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(e)")
p2b <- dat_sum2 %>% ggplot() + geom_raster(aes(x=bio01, y=elevation, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(e)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                                           limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(f)")
p2c <- dat_sum2 %>% ggplot() + geom_raster(aes(x=bio01, y=elevation, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(f)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Annual mean temp. (°C)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum3 <- ter_dat %>% group_by(bio12, elevation) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6,
           prot_space2=perc*clim_space/100) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(g)")
p3a <- dat_sum3 %>% ggplot() + geom_raster(aes(x=bio12, y=elevation, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(g)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(h)")
p3b <- dat_sum3 %>% ggplot() + geom_raster(aes(x=bio12, y=elevation, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(h)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(i)")
p3c <- dat_sum3 %>% ggplot() + geom_raster(aes(x=bio12, y=elevation, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.054, npcy=0.937, label="(i)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Annual precipitation (mm)", y="Elevation (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[4]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m2[c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.25, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), plot.title = element_text(hjust = 0.5, face="bold", size=12))

p <- p1a + p1b + p1c + p2a + p2b + p2c + p3a + p3b + p3c
ggsave(paste0("figures/ter_heatmap_all_perc.png"), width=12, height=15, dpi=1000)

########################################

# Plot marine heatmap of available climate space, protected climate space and proportion protected

# For the heatmap we need to summarise the data by prec and tmean together!!!

marspec_dat <- readRDS("data/summary_all_mar_perc.rds")
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

m_ee <- readRDS("data/summary_marspec_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- m_ee[1,2]-1
m_ee[185,2] <- m_ee[185,2]-1
m_ee[335,2] <- m_ee[335,2]-1
m_ee[431,2] <- floor(m_ee[431,2])
m_ee[528,2] <- floor(m_ee[528,2])
m <- m_ee %>% group_split(var)

sapply(m, nrow)

dat_sum4 <- marspec_dat %>% group_by(biogeo13, biogeo08) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6,
           prot_space2=perc*clim_space/100) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(a)")
p1a <- dat_sum4 %>% ggplot() + geom_raster(aes(x=biogeo13, y=biogeo08, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(a)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("Available climate space") + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(b)")
p1b <- dat_sum4 %>% ggplot() + geom_raster(aes(x=biogeo13, y=biogeo08, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(b)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("Protected climate space") + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(c)")
p1c <- dat_sum4 %>% ggplot() + geom_raster(aes(x=biogeo13, y=biogeo08, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(c)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Mean annual SSS (PSU)") + 
    coord_cartesian() + ggtitle("% protected") + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum5 <- marspec_dat %>% group_by(biogeo13, bathy) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6,
           prot_space2=perc*clim_space/100) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(d)")
p2a <- dat_sum5 %>% ggplot() + geom_raster(aes(x=biogeo13, y=bathy, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(d)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(e)")
p2b <- dat_sum5 %>% ggplot() + geom_raster(aes(x=biogeo13, y=bathy, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(e)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(f)")
p2c <- dat_sum5 %>% ggplot() + geom_raster(aes(x=biogeo13, y=bathy, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(f)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Mean annual SST (°C)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,24,48,72,94), labels=round(rowMeans(m[[5]][c(1,24,48,72,94),2:3])/100, 1), 
                       limits=c(0,97), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))

dat_sum6 <- marspec_dat %>% group_by(biogeo08, bathy) %>% 
    mutate(prot_cells=(Total/100)*n) %>% 
    summarise(cells=sum(n), prot_cells=sum(prot_cells)) %>% 
    mutate(perc = prot_cells/cells*100) %>%
    mutate(clim_space=cells*860050/1e+6,
           prot_space=prot_cells*860050/1e+6,
           prot_space2=perc*clim_space/100) %>% drop_na()

df <- data.frame(x = 0.04, y = 0.95, text = "(g)")
p3a <- dat_sum6 %>% ggplot() + geom_raster(aes(x=biogeo08, y=bathy, fill=clim_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(g)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(h)")
p3b <- dat_sum6 %>% ggplot() + geom_raster(aes(x=biogeo08, y=bathy, fill=prot_space/10000)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(h)") + 
    scale_fill_gradientn(name=expression(paste("Area \n(10000 km"^2,")")), colors=redblue2) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))
df <- data.frame(x = 0.04, y = 0.95, text = "(i)")
p3c <- dat_sum6 %>% ggplot() + geom_raster(aes(x=biogeo08, y=bathy, fill=perc)) + 
    geom_label_npc(data=df, aes(npcx=x, npcy=y, label=text), colour="white", alpha = 0.5) + 
    geom_text_npc(npcx=0.0545, npcy=0.9375, label="(i)") + 
    scale_fill_gradientn(name="%", colors=redblue2) + 
    labs(x="Mean annual SSS (PSU)", y="Bathymetry (m)") + 
    coord_cartesian() + 
    scale_x_continuous(breaks=c(1,21,42,63,82), labels=round(rowMeans(m[[2]][c(1,21,42,63,82),2:3])/100, 1), 
                       limits=c(0,85), expand=c(0,0)) + 
    scale_y_continuous(breaks=c(1,25,50,75,98), labels=round(rowMeans(m[[1]][c(1,25,50,75,98),2:3]), 0), 
                       limits=c(0,101), expand=c(0,0)) + 
    theme_bw() + theme(legend.position = "bottom", 
                       legend.key.width = unit(1.15, 'cm'), legend.title = element_text(vjust=0.85),
                       legend.background = element_blank(), 
                       plot.title = element_text(hjust = 0.5, face="bold", size=12))

p <- p1a + p1b + p1c + p2a + p2b + p2c + p3a + p3b + p3c
ggsave(paste0("figures/mar_heatmap_all_perc.png"), width=12, height=15, dpi=1000)

########################################