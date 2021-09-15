rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(ggnewscale)
library(gridExtra)
library(cowplot)
library(ggbeeswarm)
library(BSDA)
library(reshape2)

# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig2/"))

#load the data from the literature.
comp.dat <- read.csv(file = paste0(homewd, "fruit-bat-morphology-literature.csv"), header=T, stringsAsFactors = F)
head(comp.dat)

sub.comp = subset(comp.dat, Genus=="Eidolon" | Genus =="Rousettus" | Genus=="Pteropus")

head(sub.comp)
sub.comp = dplyr::select(sub.comp, Genus, Species, Binomial, Sex, AdultBodyMass_g, Adult_tibia_length_m,  Adult_ear_length_mm, Adult_forearm_length_mm)
sub.long <- melt(sub.comp, id.vars = c("Genus", "Species", "Binomial", "Sex"), measure.vars  = c("AdultBodyMass_g", "Adult_tibia_length_m",  "Adult_ear_length_mm", "Adult_forearm_length_mm"), variable.name = "metric")
head(sub.long)
sub.long$value <- as.numeric(sub.long$value)

p1 <- ggplot(data=subset(sub.long, metric!="AdultBodyMass_g")) + 
      geom_point(aes(x=Sex, y=value, color=Species, shape=Sex), size=3) +
      geom_point(data=subset(sub.long, metric!="AdultBodyMass_g" & Species=="rufus" | metric!="AdultBodyMass_g" & Species=="dupreanum" |metric!="AdultBodyMass_g" & Species=="madagascariensis"), aes(x=Sex, y=value, shape=Sex), color="black", size=3) +
      facet_grid(Genus~metric, scales = "free_y") + theme_bw()


ggsave(file = paste0(homewd, "/final-figures/testSister.png"),
       plot = p1,
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)
