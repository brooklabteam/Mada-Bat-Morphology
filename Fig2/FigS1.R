rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(ggnewscale)
library(gridExtra)
library(cowplot)
library(ggbeeswarm)
library(BSDA)
library(mgcv)
library(lme4)
library(sjPlot)


# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig2/"))

#load madagascar data  
dat1 <- read.csv(file = paste0(homewd,"morph_paper_dat_7_29_2021.csv"), header=T, stringsAsFactors = F)
head(dat1)

#get mean adult body mass and forearm length by species and sex
unique(dat1$bat_species)
unique(dat1$bat_sex)

#get just males and females of species of interest
sub.dat1 = subset(dat1, bat_sex=="male" | bat_sex=="female")
unique(sub.dat1$bat_age_class)

#and only take adults
sub.dat1 = subset(sub.dat1,  bat_age_class=="A" | bat_age_class=="L" | bat_age_class=="P" | bat_age_class=="NL")

#check as numeric
sub.dat1$bat_weight_g <- as.numeric(sub.dat1$bat_weight_g)
sub.dat1$bat_tibia_mm <- as.numeric(sub.dat1$bat_tibia_mm)
sub.dat1$bat_forearm_mm <- as.numeric(sub.dat1$bat_forearm_mm)
sub.dat1$ear_length_mm <- as.numeric(sub.dat1$ear_length_mm)

#subgrouped by genus, with shape by sex
#but for Mada bats, we will list spp. as genus
sub.dat1$Genus = NA
sub.dat1$Genus[sub.dat1$bat_species=="Pteropus rufus"] <- "Pteropus rufus"
sub.dat1$Genus[sub.dat1$bat_species=="Eidolon dupreanum"] <- "Eidolon dupreanum"
sub.dat1$Genus[sub.dat1$bat_species=="Rousettus madagascariensis"] <- "Rousettus madagascariensis"

sub.dat1$bat_sex[sub.dat1$bat_sex=="male"] <- "M"
sub.dat1$bat_sex[sub.dat1$bat_sex=="female"] <- "F"

unique(sub.dat1$roost_site)
sub.dat1$roost_site[sub.dat1$roost_site=="Ankarana_Canyon" |sub.dat1$roost_site=="Ankarana_Cathedral" | sub.dat1$roost_site=="Ankarana_Chauves_Souris"] <- "Ankarana"
sub.dat1$roost_site[sub.dat1$roost_site=="AngavoBe"  |sub.dat1$roost_site=="AngavoKely" |sub.dat1$roost_site=="Angavokely"] <- "Moramanga"
sub.dat1$roost_site[sub.dat1$roost_site=="Mangarivotra"| sub.dat1$roost_site=="Marovitsika"| sub.dat1$roost_site=="Maromizaha"] <- "Moramanga"
sub.dat1$roost_site[sub.dat1$roost_site=="Marotsipohy" |  sub.dat1$roost_site=="Ambakoana"| sub.dat1$roost_site=="Mahialambo"| sub.dat1$roost_site=="Lakato"] <- "Moramanga"

#now plot variation by morphology by site


sub.dat1$Genus = NA
sub.dat1$Genus[sub.dat1$bat_species=="Pteropus rufus"] <- "Pteropus rufus"
sub.dat1$Genus[sub.dat1$bat_species=="Eidolon dupreanum"] <- "Eidolon dupreanum"
sub.dat1$Genus[sub.dat1$bat_species=="Rousettus madagascariensis"] <- "Rousettus madagascariensis"

#and plot histogram

p0 <- ggplot(data=sub.dat1) +
      geom_histogram(aes(bat_tibia_mm, color=Genus)) + 
      facet_grid(bat_species~bat_sex)

p02 <- ggplot(data=sub.dat1) +
  geom_histogram(aes(bat_forearm_mm, color=Genus)) + 
  facet_grid(bat_species~bat_sex)

p03 <- ggplot(data=sub.dat1) +
  geom_histogram(aes(ear_length_mm, color=Genus)) + 
  facet_grid(bat_species~bat_sex)

#all normal so t.test appropriate -- pull sig data from Fi2 script
colz=c("Pteropus rufus" = "blue", "Eidolon dupreanum" = "lightgreen", "Rousettus madagascariensis" = "purple")

p1<- ggplot(data=sub.dat1) +
  geom_beeswarm(aes(x= roost_site, y= bat_tibia_mm, color=Genus))+
  geom_violin(aes(x=roost_site, y=bat_tibia_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  scale_color_manual(values = colz)+ facet_grid(~bat_species)+
  theme_bw()+theme(legend.position = "none")+
  theme(element_blank(), axis.title.x = element_blank(), 
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Tibia length (mm)") 

#print(p1)

p1<- ggplot(data=sub.dat1) +
  geom_beeswarm(aes(x= bat_sex, y= bat_tibia_mm, color=Genus))+
  geom_violin(aes(x=bat_sex, y=bat_tibia_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  scale_color_manual(values = colz)+ facet_grid(~bat_species)+
  theme_bw()+theme(legend.position = "none")+
  theme(element_blank(), axis.title.x = element_blank(), 
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Tibia length (mm)") 

#print(p1)

p1<- ggplot(data=sub.dat1) +
  geom_beeswarm(aes(x= bat_sex, y= bat_tibia_mm, color=Genus))+
  geom_violin(aes(x=bat_sex, y=bat_tibia_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  scale_color_manual(values = colz)+ facet_grid(~bat_species)+
  theme_bw()+theme(legend.position = "none")+
  theme(element_blank(), axis.title.x = element_blank(), 
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Tibia length (mm)") 



mod.dat <- cbind.data.frame(bat_species=rep(c("Pteropus rufus","Eidolon dupreanum", "Rousettus madagascariensis"),3))
mod.dat$value <- c(175,NA,175, 95,95,95, NA, NA,40)
mod.dat$metric <- rep(c("forearm", "tibia", "ear"), each=3)
mod.dat$label <- c("***", NA, "***", "***", "*", "***", NA, NA, "**")



#for all, plot boxplot by sex, species and trait
ptib <- ggplot(data=sub.dat1) + geom_boxplot(aes(x=bat_sex, y=bat_tibia_mm))+
        geom_label(data=subset(mod.dat, metric=="tibia"), aes(x=1.5, y=value, label=label), label.size = NA) +
        facet_grid(~bat_species)
pfor <- ggplot(data=sub.dat1) + geom_boxplot(aes(x=bat_sex, y=bat_forearm_mm))+
  facet_grid(~bat_species)
pear <- ggplot(data=sub.dat1) + geom_boxplot(aes(x=bat_sex, y=ear_length_mm))+
        facet_grid(~bat_species)



colz=c("Pteropus rufus" = "blue", "Eidolon dupreanum" = "lightgreen", "Rousettus madagascariensis" = "purple")



sub.dat1$bat_species = factor(sub.dat1$bat_species, levels=c("Pteropus rufus","Eidolon dupreanum", "Rousettus madagascariensis"))
mod.dat$bat_species = factor(mod.dat$bat_species, levels=c("Pteropus rufus","Eidolon dupreanum", "Rousettus madagascariensis"))

ptib <-  ggplot(data=sub.dat1) + geom_beeswarm(aes(x=bat_sex, y=bat_tibia_mm, color=Genus)) +
          geom_violin(aes(x=bat_sex, y=bat_tibia_mm),scale="width",fill=NA,draw_quantiles=c(0.025,0.5,0.975), width=.3) +
          facet_grid(~bat_species) +theme_bw()+theme(legend.position = "none")+
  geom_label(data=subset(mod.dat, metric=="tibia"), aes(x=1.5, y=value, label=label), label.size = NA, size=5) +
  scale_color_manual(values=colz) +
  theme(element_blank(), axis.title.x = element_blank(), 
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Tibia length (mm)") 
  
pfor <-  ggplot(data=sub.dat1) + geom_beeswarm(aes(x=bat_sex, y=bat_forearm_mm, color=Genus)) +
  geom_violin(aes(x=bat_sex, y=bat_forearm_mm),scale="width",fill=NA,draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  facet_grid(~bat_species) +theme_bw()+theme(legend.position = "none")+
  geom_label(data=subset(mod.dat, metric=="forearm"), aes(x=1.5, y=value, label=label), label.size = NA, size=5) +
  scale_color_manual(values=colz) +
  theme(element_blank(), axis.title.x = element_blank(), 
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Forearm length (mm)") 

pear <-  ggplot(data=sub.dat1) + geom_beeswarm(aes(x=bat_sex, y=ear_length_mm, color=Genus)) +
  geom_violin(aes(x=bat_sex, y=ear_length_mm),scale="width",fill=NA,draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  facet_grid(~bat_species) +theme_bw()+theme(legend.position = "none")+
  geom_label(data=subset(mod.dat, metric=="ear"), aes(x=1.5, y=value, label=label), label.size = NA, size=5) +
  scale_color_manual(values=colz) +
  theme(element_blank(), axis.title.x = element_blank(), strip.text = element_text(face = "italic"),
        plot.margin = unit(c(.3,.3,.3,.5), "lines"),
        strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Ear length (mm)") 


pall <- cowplot::plot_grid(pfor, ptib, pear, nrow=3,ncol=1)



ggsave(file = paste0(homewd, "/final-figures/FigS1.png"),
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)
