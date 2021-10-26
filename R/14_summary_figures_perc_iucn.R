#' ---
#' title: "Global Analysis of Protected Areas - Create environmental summary figures"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#Automatically install required packages, which are not yet installed
packages <- c("tidyverse", "patchwork", "ggpubr", "ggpmisc")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) instTotal.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

# Set working directory
#workdir <- "C:/Users/admin/Documents/GitHub/globePA/"
#setwd(workdir)

########################################

# Plot of temperature, precipitation/salinity and elevation/depth

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
  mutate(iucn_cat = factor(iucn_cat, levels=c("I-II", "III-IV", "V-VI", "Not-designated"),
                           labels=c("I-II", "III-IV", "V-VI", "Not-designated"))) %>% drop_na()

# Check number of cells!
unique(ter_dat$n)

ter_dat %>% filter(path=="bio12_perc", iucn_cat == "I-II") %>% 
  ungroup() %>% dplyr::select(perc) %>% summary()

# Summary
ter_dat %>% group_by(path, var) %>% summarise(sum=sum(perc)) %>% 
  summarise(max(sum))

# Number of cells
ter_dat %>% group_by(path) %>% summarise(total_cells=sum(n)/4)

# Plot frequeny plots of envdata
ter_dat %>% ggplot(aes(x = var, y=perc, fill=iucn_cat)) + 
  geom_area(stat="identity", position="stack") + facet_wrap(.~ path, scales="free") + 
  labs(x="", y="Area protected (%)") + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=expansion(mult=c(0,.1)))

ter_dat %>% group_by(path)

####################

#' ### Hyper-geometric distribution

iucn_n <- ter_dat %>% group_by(path, var) %>% summarise(n_iucn = n_distinct(iucn_cat))
unique(iucn_n$n_iucn)

# Total area & Total area protected
(tot_sum <- ter_dat %>% mutate(sum=n/4) %>% mutate(prot_cells=perc*sum) %>%
    group_by(path, iucn_cat) %>% summarise(sum=sum(sum), prot_cells=sum(prot_cells)) %>%
    mutate(prop_prot=prot_cells/sum))

# Global area climate
(clim_area <- ter_dat %>% group_by(path, var, iucn_cat) %>% summarise(area_clim=n/4) %>% 
    left_join(tot_sum))

# Expected proportion climate  
n_bin <- clim_area %>% group_by(path, iucn_cat) %>% summarise(n_bin = n())
(exp_val <- clim_area %>% left_join(n_bin) %>% mutate(prop_clim = area_clim/sum*n_bin) %>% 
    mutate(exp = (prop_clim*prop_prot),
           exp_aichi = 15*prop_clim,
           var_exp = (prop_clim*prop_prot*(1-prop_clim)*(1-prop_prot)/(sum-1))))

# Need to multiply by 100 to get perc value rather than proportion

# Need to multiply by bin size to get values up to 100 %
exp_val %>% group_by(path) %>% summarise(sum(prop_clim))
exp_val %>% group_by(path) %>% summarise(sum(exp))
exp_val %>% group_by(path) %>% summarise(sum(exp_aichi))

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

ter_dat %<>% filter(path %in% c("bio01_perc", "bio04_perc", "bio07_perc", "bio12_perc", "bio15_perc",  "elevation_perc")) %>% left_join(exp_val) %>% 
  left_join(as.data.frame(m_ee)) %>% 
  mutate(path = factor(path, levels =  c("bio01_perc", "bio04_perc", "bio07_perc", "bio12_perc", "bio15_perc",  "elevation_perc"),
                       labels = c("Annual mean temp. (°C)", "Temperature seasonality", "Temperature annual range", 
                                  "Annual precipitation (mm)", "Precipitation seasonality",  "Elevation (m)")))


###
# NOTE: If some summaries still have NAs after left_join, check if re-classification is correct
###

# goodness of fit test
test <- ter_dat %>% filter(path=="Annual mean temp. (°C)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

round(quantile(1:100, probs=c(0.01, 0.25, 0.5, 0.75, 0.99)))

p1 <- ter_dat %>% filter(path=="Annual mean temp. (°C)") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width= 1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  labs(y="Area protected (%)") + # Annual mean temperature is too long
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector(ter_dat %>% filter(path=="Annual mean temp. (°C)") %>% ungroup() %>% 
                                        select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title.x=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(),  strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p1

# goodness of fit test
test <- ter_dat %>% filter(path=="Temperature seasonality") %>% ungroup %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))
test$label <- paste0("X-squared = ", test$chisq,", \n df = ", test$df, ", \n p-value = ", test$pvalue)
#test$letter <- c("(b)", "(h)", "(h)", "(k)")

p2 <- ter_dat %>% filter(path=="Temperature seasonality") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width= 1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  labs(y="Area protected (%)") + # Annual mean temperature is too long
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=round(as.vector(ter_dat %>% filter(path=="Temperature seasonality") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)],0), 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p2

test <- ter_dat %>% filter(path=="Temperature annual range") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p3 <- ter_dat %>% filter(path=="Temperature annual range") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector(ter_dat %>% filter(path=="Temperature annual range") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none",  panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p3

# goodness of fit test
test <- ter_dat %>% filter(path=="Annual precipitation (mm)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p4 <- ter_dat %>% filter(path=="Annual precipitation (mm)") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=round(as.vector(ter_dat %>% filter(path=="Annual precipitation (mm)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)],0), 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none",  panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p4

# goodness of fit test
test <- ter_dat %>% filter(path=="Precipitation seasonality") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p5 <- ter_dat %>% filter(path=="Precipitation seasonality") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector(ter_dat %>% filter(path=="Precipitation seasonality") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")

# goodness of fit test
test <- ter_dat %>% filter(path=="Elevation (m)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p6 <- ter_dat %>% filter(path=="Elevation (m)") %>% ggplot() + 
  geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector( ter_dat %>% filter(path=="Elevation (m)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(name="IUCN", values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="bottom", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.x = element_text(size=10),
                     strip.text.y = element_text(size=12, face="bold"),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p6

#leg <- ggpubr::as_ggplot(ggpubr::get_legend(p6))

p <- p1 + p2 + p3 + p4 + p5 + {p6 + theme(legend.position="none")} + 
  #plot_spacer() + plot_spacer() + leg + plot_spacer() + plot_spacer() + 
  plot_layout(ncol=6) # , heights=c(14,1))
p
ggsave("figures/ter_sum_iucn.png", p, dpi=1000, width=15, height=8)

#########################

# Plot of environmental variables for marine areas

# Read and prepare data

marspec_dat <- readRDS("data/summary_ind_mar_perc.rds")
head(marspec_dat)
unique(marspec_dat$path)
marspec_dat %>% filter(path == "biogeo17_perc") %>% group_by(var) %>% group_keys() %>% unlist()

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

marspec_dat <- marspec_dat %>% dplyr::select(-c(Total, sum)) %>%
  tidyr::gather(iucn_cat, perc, -c(path, var, n)) %>%
  mutate(iucn_cat = factor(iucn_cat, levels=c("I-II", "III-IV", "V-VI", "Not-designated"),
                           labels=c("I-II", "III-IV", "V-VI", "Not-designated"))) %>% drop_na()

# Summary should be maximum of 100!!!
marspec_dat %>% group_by(path, var) %>% summarise(sum=sum(perc)) %>% 
  summarise(max(sum))

# Number of cells
# Need to divide area by 4, as we have 4 categories!!!
marspec_dat %>% group_by(path) %>% summarise(total_cells=sum(n)/4)

# Plot frequeny plots of envdata
marspec_dat %>% 
  ggplot(aes(x = var, y=perc, fill=iucn_cat)) + 
  geom_area(stat="identity", position="stack") + facet_wrap(.~ path, scales="free") + 
  labs(x="", y="Area protected (%)") + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=expansion(mult=c(0,.1)))

####################

#' ### Hyper-geometric distribution

iucn_n <- marspec_dat %>% group_by(path, var) %>% summarise(n_iucn = n_distinct(iucn_cat))
unique(iucn_n$n_iucn)

# Total area & Total area protected
(tot_sum <- marspec_dat %>% mutate(sum=n/4) %>% mutate(prot_cells=perc*sum) %>%
    group_by(path, iucn_cat) %>% summarise(sum=sum(sum), prot_cells=sum(prot_cells)) %>%
    mutate(prop_prot=prot_cells/sum))

# Global area climate
(clim_area <- marspec_dat %>% group_by(path, var, iucn_cat) %>% summarise(area_clim=n/4) %>% 
    left_join(tot_sum))

# Expected proportion climate  
n_bin <- clim_area %>% group_by(path, iucn_cat) %>% summarise(n_bin = n())
(exp_val <- clim_area %>% left_join(n_bin) %>% mutate(prop_clim = area_clim/sum*n_bin) %>% 
    mutate(exp = (prop_clim*prop_prot),
           exp_aichi = 15*prop_clim,
           var_exp = (prop_clim*prop_prot*(1-prop_clim)*(1-prop_prot)/(sum-1))))

# Need to multiply by 100 to get perc value rather than proportion

# Need to multiply by bin size to get values up to 100 %
exp_val %>% group_by(path) %>% summarise(sum(prop_clim))
exp_val %>% group_by(path) %>% summarise(sum(exp))
exp_val %>% group_by(path) %>% summarise(sum(exp_aichi))

####

m_ee <- readRDS("data/summary_marspec_perc_optim.rds")

# Round first and last value to include all ranges!
m_ee[1,2] <- m_ee[1,2]-1
m_ee[185,2] <- m_ee[185,2]-1
m_ee[335,2] <- m_ee[335,2]-1
m_ee[431,2] <- floor(m_ee[431,2])
m_ee[528,2] <- floor(m_ee[528,2])
colnames(m_ee) <- c("path", "x", "y", "var")
m_ee$path <- sub("30s", "perc", m_ee$path)
m_ee$var2 <- round(rowMeans(m_ee[, c("x", "y")])/100,1)

m <- readRDS("data/summary_earthenv_perc_optim.rds")
colnames(m) <- c("path", "x", "y", "var")
m$path <- "elevation_perc"
m$var2 <- round(rowMeans(m[, c("x", "y")]),1)
m_ee <- rbind(m_ee, m); rm(m)

marspec_dat %<>% filter(path %in% c("biogeo13_perc", "biogeo16_perc", "biogeo17_perc", "biogeo08_perc", "biogeo11_perc", "biogeo12_perc", "bathy_perc")) %>% left_join(exp_val) %>% 
  left_join(as.data.frame(m_ee)) %>% 
  mutate(path = factor(path, levels = c("biogeo13_perc", "biogeo16_perc", "biogeo17_perc", "biogeo08_perc", "biogeo11_perc", "biogeo12_perc", "bathy_perc"),
                       labels = c("Mean annual SST (°C)", "Annual range in SST", "Annual variance in SST",  "Mean annual SSS (psu)", 
                                  "Annual range in SSS", "Annual variance in SSS", "Bathymetry (m)")))

####################

# goodness of fit test
test <- marspec_dat %>% filter(path=="Mean annual SST (°C)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p1 <-  marspec_dat %>% filter(path=="Mean annual SST (°C)") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 48, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 48, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 48, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,25,48,72,95), 
                     labels=as.vector(marspec_dat %>% filter(path=="Mean annual SST (°C)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,25,48,72,95)], 
                     limits=c(0,97), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  labs(y="Area protected (%)") + 
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title.x=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p1

# goodness of fit test
test <-  marspec_dat %>% filter(path=="Annual range in SST") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p2 <- marspec_dat %>% filter(path=="Annual range in SST") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 49, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 49, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 49, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,25,49,73,96), 
                     labels=as.vector(marspec_dat %>% filter(path=="Annual range in SST") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,25,49,73,96)], 
                     limits=c(0,98), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + # , breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")

p2

# goodness of fit test
test <-  marspec_dat %>% filter(path=="Annual variance in SST") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p3 <- marspec_dat %>% filter(path=="Annual variance in SST") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,74,98), 
                     labels=as.vector(marspec_dat %>% filter(path=="Annual variance in SST") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,74,98)], 
                     limits=c(0,99), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + #, breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p3

# goodness of fit test
test <-  marspec_dat %>% filter(path=="Mean annual SSS (psu)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p4 <- marspec_dat %>% filter(path=="Mean annual SSS (psu)") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 42, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 42, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 42, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,22,42,63,83), 
                     labels=as.vector(marspec_dat %>% filter(path=="Mean annual SSS (psu)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,22,42,63,83)], 
                     limits=c(0,86), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + #, breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p4

# goodness of fit test
test <-  marspec_dat %>% filter(path=="Annual range in SSS") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p5 <- marspec_dat %>% filter(path=="Annual range in SSS") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 35, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 35, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 35, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,18,35,52,68), 
                     labels=as.vector(marspec_dat %>% filter(path=="Annual range in SSS") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,18,35,52,68)], 
                     limits=c(0,70), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + #, breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p5

# goodness of fit test
test <-  marspec_dat %>% filter(path=="Annual variance in SSS") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p6 <- marspec_dat %>% filter(path=="Annual variance in SSS") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 41, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 41, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 41, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,21,41,61,80), 
                     labels=as.vector(marspec_dat %>% filter(path=="Annual variance in SSS") %>% 
                                        ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,21,41,61,80)], 
                     limits=c(0,82), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + #, breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.y = element_blank(), strip.text.x = element_text(size=10),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p6

# goodness of fit test
test <- marspec_dat %>% filter(path=="Bathymetry (m)") %>% ungroup() %>% dplyr::select(path, iucn_cat, perc, exp) %>%
  group_by(path, iucn_cat) %>% summarise(perc = list(perc), exp=list(exp)) %>%
  group_by(path, iucn_cat) %>% 
  summarise(chisq = round(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$statistic, 2),
            df = chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$parameter,
            pvalue = signif(chisq.test(x=unlist(perc),p=unlist(exp), rescale.p=T)$p.value, 2))

p7 <- marspec_dat %>% filter(path=="Bathymetry (m)") %>%
  ggplot() + geom_bar(aes(x = var, y=perc, fill=iucn_cat), width=1, stat="identity", position="stack") +
  geom_line(aes(x=var, y=exp), colour="black") + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(chi^2)), " ==", chisq)), vjust = +1.5, parse=T) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste("df = ", df)), vjust = +4) + 
  geom_text(data=test, aes(x = 50, y = Inf, label=paste(expression(italic(p)), " ==", pvalue)), vjust = +4.75, parse=T) + 
  facet_grid(iucn_cat ~ path, switch="x", scales="free_y") + 
  scale_x_continuous(breaks=c(2,26,50,75,99), 
                     labels=as.vector(marspec_dat %>% filter(path=="Bathymetry (m)") %>% ungroup() %>% select("var2") %>% unlist(use.names=F))[c(2,26,50,75,99)], 
                     limits=c(0,101), expand=c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.01))) + #, breaks=c(0, 1, 5, 15, 30)) +
  scale_fill_manual(values=rev(c("#D43F3AFF", "#EEA236FF", "#46B8DAFF", "#5CB85CFF"))) + 
  theme_bw() + theme(legend.position="none", panel.grid.minor = element_blank(),
                     axis.title=element_blank(), strip.background = element_blank(),
                     strip.text.x = element_text(size=10),
                     strip.text.y = element_text(size=12, face="bold"),
                     strip.placement="outside") #+ coord_trans(y="sqrt")
p7

leg <- ggpubr::as_ggplot(ggpubr::get_legend(p7))

p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(ncol=7)
p
ggsave("figures/mar_sum_iucn.png", p, dpi=1000, width=17.5, height=8)
