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
library(lme4)

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

#now query -- are these island taxa?

island.dat <- read.csv(file = "island_bats.csv", header = T, stringsAsFactors = F)
head(island.dat)
head(sub.long)
island.dat$Binomial <- paste(island.dat$Genus, island.dat$Species, sep=" ")

island.dat <- dplyr::select(island.dat, Binomial, Island..Y.N)
head(island.dat)
names(island.dat) <- c("Binomial", "island")

#and join

all.long <- melt(comp.dat, id.vars = c("Genus", "Species", "Binomial", "Sex"), measure.vars  = c("AdultBodyMass_g", "Adult_tibia_length_m",  "Adult_ear_length_mm", "Adult_forearm_length_mm"), variable.name = "metric")
head(all.long)
all.long$value <- as.numeric(all.long$value)

all.merge <- merge(all.long, island.dat, by = "Binomial", all.x = T, sort = F)

head(all.merge)

#stats <- 
all.merge$island <- as.factor(all.merge$island)
all.merge$Sex <- as.factor(all.merge$Sex)
all.merge$Genus <- as.factor(all.merge$Genus)

all.merge$value <- as.numeric(all.merge$value)

p1 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_forearm_length_mm"))
summary(p1)

p2 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_tibia_length_m"))
summary(p2)

p3 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_ear_length_mm"))
summary(p3)
#this one sig. islands smaller significantly


#now try just our genera
p1 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_forearm_length_mm" & Genus=="Pteropus" | metric=="Adult_forearm_length_mm" & Genus=="Rousettus" |  metric=="Adult_forearm_length_mm" & Genus=="Eidolon"))
summary(p1)

p2 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_tibia_length_m"& Genus=="Pteropus" |metric=="Adult_tibia_length_m"& Genus=="Rousettus" | metric=="Adult_tibia_length_m"& Genus=="Eidolon"))
summary(p2)

p3 <- lmerTest::lmer(value~island + (1|Genus/Sex), data=subset(all.merge, metric=="Adult_ear_length_mm" & Genus=="Pteropus" | metric=="Adult_ear_length_mm" & Genus=="Eidolon" | metric=="Adult_ear_length_mm" & Genus=="Rousettus"))
summary(p3)
#t

#or sub by just the genera of interest
pPtertib <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_tibia_length_m"& Genus=="Pteropus" ))
summary(pPtertib)

pPterfor <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_forearm_length_mm"& Genus=="Pteropus" ))
summary(pPterfor)

pPterear <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_ear_length_mm"& Genus=="Pteropus" ))
summary(pPterear) #yes, islands significantly smaller



pRoutib <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_tibia_length_m"& Genus=="Rousettus" ))
summary(pRoutib) #yes, islands significantly smaller

pRoufor <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_forearm_length_mm"& Genus=="Rousettus" ))
summary(pRoufor) #yes, islands significantly smaller

pRouear <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_ear_length_mm"& Genus=="Rousettus" ))
summary(pRouear) #yes, islands significantly smaller





p1 <- ggplot(data=subset(all.merge, metric!="AdultBodyMass_g" & Genus=="Pteropus" |  metric!="AdultBodyMass_g" & Genus=="Rousettus" |  metric!="AdultBodyMass_g" & Genus=="Eidolon")) + 
   geom_point(aes(x=island, y=value, color=Species, shape=Sex), size=3, show.legend = F) +
   geom_point(data=subset(sub.merge, metric!="AdultBodyMass_g" & Species=="rufus" | metric!="AdultBodyMass_g" & Species=="dupreanum" |metric!="AdultBodyMass_g" & Species=="madagascariensis"), aes(x=island, y=value, shape=Sex), color="black", size=3) +
   facet_grid(Genus~metric, scales = "free_y") + theme_bw()

print(p1)

ggsave(file = paste0(homewd, "/final-figures/testSister_withIsland.png"),
       plot = p1,
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)


#all species

ptib <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_tibia_length_m"))
summary(ptib) #no

pfor <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_forearm_length_mm"))
summary(pfor) #islands significantly bigger

pear <- lmerTest::lmer(value~island + (1|Sex), data=subset(all.merge, metric=="Adult_ear_length_mm"))
summary(pear) #no



