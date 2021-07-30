rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(ggnewscale)
library(gridExtra)
library(cowplot)
library(ggbeeswarm)

# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig2/"))

#load the data from the literature.
comp.dat <- read.csv(file = paste0(homewd, "fruit-bat-morphology-literature.csv"), header=T, stringsAsFactors = F)
head(comp.dat)


#check as numeric
comp.dat$Adult_forearm_length_mm <- as.numeric(comp.dat$Adult_forearm_length_mm)
comp.dat$AdultBodyMass_g <- as.numeric(comp.dat$AdultBodyMass_g)

#slim to columns of interest
forearm.dat1 <- dplyr::select(comp.dat,Genus, Binomial, Sex,AdultBodyMass_g,Adult_forearm_length_mm, Adult_ear_length_mm, Adult_tibia_length_m)
#check only 2 sexes
unique(forearm.dat1$Sex)


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

unique(sub.dat1$bat_sex)

#summarise to just points of interest
mad.dat1 <- ddply(sub.dat1, .(Genus, bat_species, bat_sex, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm))
head(mad.dat1)
mad.dat1$origin = "Malagasy Bats"


#and summarize means of everything
mad.mean <- ddply(mad.dat1, .(Genus, bat_sex, bat_species), summarise, bat_weight_g = mean(bat_weight_g, na.rm = T), bat_forearm_mm = mean(bat_forearm_mm, na.rm = T), ear_length_mm = mean(ear_length_mm, na.rm = T), bat_tibia_mm=mean(bat_tibia_mm, na.rm = T))
mad.mean$origin <- "All Bats" 


#remove mada species from lit
lit.dat = subset(forearm.dat1, Binomial!="Pteropus rufus" & Binomial!="Eidolon dupreanum" & Binomial!= "Rousettus madagascariensis")
lit.dat$origin = "All Bats"

#now join all datasets together

head(forearm.dat1)
head(mad.dat1)
lit.dat <- dplyr::select(lit.dat, Genus, Binomial,Sex,  AdultBodyMass_g, Adult_forearm_length_mm, Adult_ear_length_mm, Adult_tibia_length_m,  origin)
mad.sub <- dplyr::select(mad.dat1, Genus, bat_species, bat_sex,bat_weight_g, bat_forearm_mm,ear_length_mm, bat_tibia_mm,  origin )
names(lit.dat) <- names(mad.sub)
names(mad.mean)

#and join

all.dat <- rbind(mad.sub, lit.dat, mad.mean)
all.dat$origin = factor(all.dat$origin, levels = c("Malagasy Bats", "All Bats"))

#make a color bar and specify madagascar bats
length(unique(all.dat$Genus))#30, with mada bats classed by specis
colz <- scales::hue_pal()(30) #makes a color bar the length of each species
names(colz) <- unique(all.dat$Genus)
colz #look at it. gives a color for each Genus
#now overwrite the mada bats
colz[names(colz)=="Pteropus rufus"] = "blue"
colz[names(colz)=="Eidolon dupreanum"] = "lightgreen"
colz[names(colz)=="Rousettus madagascariensis"] = "purple"

all.genera = sort(unique(all.dat$Genus))
all.genera = all.genera[all.genera!="Pteropus rufus" & all.genera!="Eidolon dupreanum" & all.genera!= "Rousettus madagascariensis"]

all.genera = c("Eidolon dupreanum", "Pteropus rufus", "Rousettus madagascariensis", all.genera)
#and factor genus
all.dat$Genus <- factor(all.dat$Genus, levels = c(all.genera))

#test plot
p.all <- ggplot(data=all.dat) + 
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats" | bat_species=="Rousettus madagascariensis" & origin=="All Bats"), 
             aes( x=bat_forearm_mm , y=bat_weight_g), color="black", size=5) +
  geom_point(aes( x=bat_forearm_mm , y=bat_weight_g, color=Genus), size=3)+ facet_grid(Sex~.)+
  scale_color_manual(values=colz) +
  facet_grid(bat_sex~origin)+theme_bw()+
  theme(strip.text.x = element_blank(), legend.position = "bottom")+
  theme(element_blank())+labs(x="Forearm Length (mm)", y="Body weight (g)")

print(p.all)

###################################################
################ fit model lmodel2 

library(lmodel2)

female.mod.mada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="F" & origin=="Malagasy Bats"), nperm = 99)
male.mod.mada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="M" & origin=="Malagasy Bats"), nperm = 99)


#and the non-mada
female.mod.nonmada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="F" & origin=="All Bats"), nperm = 99)
male.mod.nonmada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="M" & origin=="All Bats"), nperm = 99)

#save them as "m" and "b" as in y=mx+b
mfemMad <- female.mod.mada$regression.results[3,3]
bfemMad <- female.mod.mada$regression.results[3,2]
mmalMad <- male.mod.mada$regression.results[3,3]
bmalMad <- male.mod.mada$regression.results[3,2]

#and lci and uci
mfemMad_lci <- female.mod.mada$confidence.intervals[3,4]
bfemMad_lci <- female.mod.mada$confidence.intervals[3,2]
mmalMad_lci <- male.mod.mada$confidence.intervals[3,4]
bmalMad_lci <- male.mod.mada$confidence.intervals[3,2]

mfemNonMad_lci <- female.mod.nonmada$confidence.intervals[3,4]
bfemNonMad_lci <- female.mod.nonmada$confidence.intervals[3,2]
mmalNonMad_lci <- male.mod.nonmada$confidence.intervals[3,4]
bmalNonMad_lci <- male.mod.nonmada$confidence.intervals[3,2]

mfemMad_uci <- female.mod.mada$confidence.intervals[3,5]
bfemMad_uci <- female.mod.mada$confidence.intervals[3,3]
mmalMad_uci <- male.mod.mada$confidence.intervals[3,5]
bmalMad_uci <- male.mod.mada$confidence.intervals[3,3]

mfemNonMad_uci <- female.mod.nonmada$confidence.intervals[3,5]
bfemNonMad_uci <- female.mod.nonmada$confidence.intervals[3,3]
mmalNonMad_uci <- male.mod.nonmada$confidence.intervals[3,5]
bmalNonMad_uci <- male.mod.nonmada$confidence.intervals[3,3]



# Save R-squar for all of the model
RsqF<- female.mod.mada[["rsquare"]]
RsqM<-male.mod.mada[["rsquare"]]
RsqNF<-female.mod.nonmada[["rsquare"]]
RsqNM<-male.mod.nonmada[["rsquare"]]

rsq_all<-data.frame(RsqF,RsqM,RsqNM,RsqNF)
colnames(rsq_all)<-c("F Malagasy bats","M malagasy bats", "M non malagasy","F non malagay")

write.csv(rsq_all,"rsquare_bat_model.csv")



#write a function to predict y = mx+b
regress.func <- function(x,m,b){
  y = m*x+b
  return(y)
  
}


#and predict across your dataset
all.dat$predicted_mass <- NA
all.dat$predicted_mass[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"]), m=mfemMad, b=bfemMad))
all.dat$predicted_mass[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"]), m=mmalMad, b=bmalMad))
all.dat$predicted_mass[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"]), m=mfemMad, b=bfemMad))
all.dat$predicted_mass[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"]), m=mmalMad, b=bmalMad))


#do CIs later
all.dat$predicted_mass_lci <- NA
all.dat$predicted_mass_lci[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"]), m=mfemMad_lci, b=bfemMad_lci))
all.dat$predicted_mass_lci[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"]), m=mmalMad_lci, b=bmalMad_lci))
all.dat$predicted_mass_lci[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"]), m=mfemMad_lci, b=bfemMad_lci))
all.dat$predicted_mass_lci[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"]), m=mmalMad_lci, b=bmalMad_lci))

all.dat$predicted_mass_uci <- NA
all.dat$predicted_mass_uci[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats"]), m=mfemMad_uci, b=bfemMad_uci))
all.dat$predicted_mass_uci[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats"]), m=mmalMad_uci, b=bmalMad_uci))
all.dat$predicted_mass_uci[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"]), m=mfemMad_uci, b=bfemMad_uci))
all.dat$predicted_mass_uci[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"]), m=mmalMad_uci, b=bmalMad_uci))



#here plot with data:

pall_2 <- ggplot(data=all.dat) + 
  scale_color_manual(values=colz) +
  geom_line(aes(x=bat_forearm_mm , y=predicted_mass)) +
  geom_ribbon(aes(x=bat_forearm_mm , ymin=predicted_mass_lci, ymax=predicted_mass_uci),alpha=.3) +
  facet_grid(bat_sex~origin, switch= "x")+theme_bw()+
  geom_point(aes( x=bat_forearm_mm , y=bat_weight_g, color=Genus), size=3)+ 
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats" ),
             aes( x=bat_forearm_mm , y=bat_weight_g), color="black", size=5) +
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats" ),
             aes( x=bat_forearm_mm , y=bat_weight_g, color=Genus),  size=3) +
  theme( strip.background.y =  element_rect(fill="white"), 
         strip.background.x = element_blank(), strip.placement = "outside",
        legend.position = "right", legend.text = element_text(size=8),
        legend.key.height = unit(.5,"cm"))+
  theme( legend.text = element_text(face = "italic"),
         panel.grid = element_blank(),
        legend.title = element_text(size=10))+labs(x="Forearm Length (mm)", y="Body weight (g)")+
  labs(color="Malagasy spp. + All bat genera")

print(pall_2)


#now plot Ear and Tibia


#############################
##########Ear

p2T_6_1<- ggplot() +
  geom_violin(data= subset(all.dat, origin=="All Bats"), 
              aes(x=origin, y=bat_tibia_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  geom_beeswarm(dat=subset(all.dat, origin=="Malagasy Bats"),
                aes(x= origin, y= bat_tibia_mm, color=Genus))+
  scale_color_manual(values = colz)+
  geom_jitter(data=subset(all.dat, origin=="All Bats"),
              aes(x= origin, y= bat_tibia_mm, color=Genus),size=2,width=.1,height=0)+
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats"),
             aes(x= origin, y= bat_tibia_mm), color="black", size=4,) +
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats"),
             aes(x= origin, y= bat_tibia_mm, color=Genus), size=2) +
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank(), axis.title.x = element_blank(), strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Tibia length (mm)") 

print(p2T_6_1)

#########Ear

p2E_6_1<- ggplot() +
  geom_violin(data= subset(all.dat, origin=="All Bats"), 
              aes(x=origin, y=ear_length_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  geom_beeswarm(dat=subset(all.dat, origin=="Malagasy Bats"),
                aes(x= origin, y= ear_length_mm, color=Genus))+
  scale_color_manual(values = colz)+
  geom_jitter(data=subset(all.dat, origin=="All Bats"),
              aes(x= origin, y= ear_length_mm, color=Genus),size=2,width=.1,height=0)+
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats"),
             aes(x= origin, y= ear_length_mm), color="black", size=4,) +
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & origin=="All Bats" | bat_species=="Eidolon dupreanum" & origin=="All Bats"  | bat_species=="Rousettus madagascariensis" & origin=="All Bats"),
             aes(x= origin, y= ear_length_mm, color=Genus), size=2) +
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank(), axis.title.x = element_blank(), strip.background = element_rect(fill="white"))+scale_y_continuous(name = "Ear length (mm)") 

print(p2E_6_1)




p10<- plot_grid(p2T_6_1+ theme(legend.position = "none"),
                p2E_6_1 + theme(legend.position = "none"),
                ncol = 2, labels = "AUTO")


print(p10)

plot_grid(p10, pall_2 + theme(legend.position = "right"),
          ncol = 1,
          labels = c("","C"))




ggsave(file = paste0(homewd, "/final-figures/Fig2.png"),
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)

####ResultInterpret_Ear


quant_EarAllBAtsF<- quantile(all.dat$ear_length_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)
quant_EarAllBAtsF

quant_EarAllBAtsM<- quantile(all.dat$ear_length_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)


quant_EarAllBAtsM

############Result_2-Ear Malagasy bats#############

quant_EarPert_F<- quantile(all.dat$ear_length_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarPert_F

quant_EarPert_M<-quantile(all.dat$ear_length_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarPert_M
##

quant_EarEid_F<-quantile(all.dat$ear_length_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarEid_F

quant_EarEid_M<-quantile(all.dat$ear_length_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarEid_M
###
quant_EarRou_F<-quantile(all.dat$ear_length_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarRou_F

quant_EarRou_M<-quantile(all.dat$ear_length_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarRou_M



QuantBat_ear<- rbind.data.frame(quant_EarPert_F,quant_EarPert_M,quant_EarEid_F,quant_EarEid_M,
                          quant_EarRou_F,quant_EarRou_M, quant_EarAllBAtsF, quant_EarAllBAtsM)

names(QuantBat_ear) <- names(quant_EarEid_F)
head(QuantBat_ear)
QuantBat_ear$bat_species <- c(rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis", "All Bats"), each = 2))
QuantBat_ear$sex <- rep(c("F", "M"), 4)

QuantBat_ear$metric <- "ear"

##########
########### Tibia
####ResultInterpret_Ear




quant_tibiaAllBAtsF<- quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)
quant_tibiaAllBAtsF

quant_tibiaAllBAtsM<- quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)


quant_tibiaAllBAtsM

############Result_2-tibia Malagasy bats#############

quant_tibiaPert_F<- quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaPert_F

quant_tibiaPert_M<-quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaPert_M
##

quant_tibiaEid_F<-quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaEid_F

quant_tibiaEid_M<-quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaEid_M
###
quant_tibiaRou_F<-quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaRou_F

quant_tibiaRou_M<-quantile(all.dat$bat_tibia_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaRou_M



QuantBat_tibia<- rbind.data.frame(quant_tibiaPert_F,quant_tibiaPert_M,quant_tibiaEid_F,quant_tibiaEid_M,
                                quant_tibiaRou_F,quant_tibiaRou_M, quant_tibiaAllBAtsF, quant_tibiaAllBAtsM)

names(QuantBat_tibia) <- names(quant_tibiaEid_F)
head(QuantBat_tibia)
QuantBat_tibia$bat_species <- c(rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis", "All Bats"), each = 2))
QuantBat_tibia$sex <- rep(c("F", "M"), 4)

QuantBat_tibia$metric <- "tibia"

##########
########### FF
####ResultInterpret_Ear



quant_forearmAllBAtsF<- quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)
quant_forearmAllBAtsF

quant_forearmAllBAtsM<- quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="All Bats"],probs=c(0.025,0.5,0.975),na.rm = T)


quant_forearmAllBAtsM

############Result_2-forearm Malagasy bats#############

quant_forearmPert_F<- quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmPert_F

quant_forearmPert_M<-quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Pteropus rufus"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmPert_M
##

quant_forearmEid_F<-quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmEid_F

quant_forearmEid_M<-quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Eidolon dupreanum"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmEid_M
###
quant_forearmRou_F<-quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmRou_F

quant_forearmRou_M<-quantile(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$origin=="Malagasy Bats" & all.dat$bat_species=="Rousettus madagascariensis"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_forearmRou_M



QuantBat_forearm<- rbind.data.frame(quant_forearmPert_F,quant_forearmPert_M,quant_forearmEid_F,quant_forearmEid_M,
                                  quant_forearmRou_F,quant_forearmRou_M, quant_forearmAllBAtsF, quant_forearmAllBAtsM)

names(QuantBat_forearm) <- names(quant_forearmEid_F)
head(QuantBat_forearm)
QuantBat_forearm$bat_species <- c(rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis", "All Bats"), each = 2))
QuantBat_forearm$sex <- rep(c("F", "M"), 4)

QuantBat_forearm$metric <- "forearm"


all_quant = rbind(QuantBat_ear, QuantBat_tibia, QuantBat_forearm)

write.csv(all_quant, file = "QuantileDat.csv", row.names = F)
