rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(ggnewscale)
library(gridExtra)
library(cowplot)

# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig2/"))

#load the data from the literature
comp.dat <- read.csv(file = paste0(homewd, "fruit-bat-morphology-literature.csv"), header=T, stringsAsFactors = F)
head(comp.dat)


#check as numeric
comp.dat$Adult_forearm_length_mm <- as.numeric(comp.dat$Adult_forearm_length_mm)
comp.dat$AdultBodyMass_g <- as.numeric(comp.dat$AdultBodyMass_g)

#slim to columns of interest
forearm.dat1 <- dplyr::select(comp.dat,Genus, Binomial, Sex,AdultBodyMass_g,Adult_forearm_length_mm)
#check only 2 sexes
unique(forearm.dat1$Sex)

#and plot mass by forearm
unique(forearm.dat1$Binomial) #where is Rousettus???? need to add in later

colz <- c("F" = "deeppink", "M" = "cornflowerblue")
p1 <- ggplot(dat=forearm.dat1) + geom_point(aes( x=Adult_forearm_length_mm, y=AdultBodyMass_g, color=Binomial), size=3)+
      facet_grid(Sex~.)

print(p1)


#and try on log10 scale

p1log <- ggplot(dat=forearm.dat1) + geom_point(aes( x=Adult_forearm_length_mm, y=AdultBodyMass_g, color=Binomial), size=3)+
        facet_grid(Sex~.) + scale_y_log10() + scale_x_log10()

print(p1log)


#load madagascar data  
dat1 <- read.csv(file = paste0(homewd,"morph_paper_dat_7_23_2021.csv"), header=T, stringsAsFactors = F)
head(dat1)

#get mean adult body mass and forearm length by species and sex
unique(dat1$bat_species)
unique(dat1$bat_sex)

#get just males and females of species of interest
sub.dat1 = subset(dat1, bat_species== "Eidolon dupreanum" | bat_species== "Pteropus rufus" | bat_species== "Rousettus madagascariensis")
sub.dat1 = subset(sub.dat1, bat_sex=="male" | bat_sex=="female")
unique(sub.dat1$bat_age_class)

sub.dat1 = subset(sub.dat1,  bat_age_class=="A" | bat_age_class=="L" | bat_age_class=="P" | bat_age_class=="NL")

#check as numeric
sub.dat1$bat_weight_g <- as.numeric(sub.dat1$bat_weight_g)
sub.dat1$bat_tibia_mm <- as.numeric(sub.dat1$bat_tibia_mm)
sub.dat1$bat_forearm_mm <- as.numeric(sub.dat1$bat_forearm_mm)
sub.dat1$ear_length_mm <- as.numeric(sub.dat1$ear_length_mm)

#subgrouped by genus, with shape by sex
sub.dat1$Genus = NA
sub.dat1$Genus[sub.dat1$bat_species=="Pteropus rufus"] <- "Pteropus"
sub.dat1$Genus[sub.dat1$bat_species=="Eidolon dupreanum"] <- "Eidolon"
sub.dat1$Genus[sub.dat1$bat_species=="Rousettus madagascariensis"] <- "Rousettus"

sub.dat1$bat_sex[sub.dat1$bat_sex=="male"] <- "M"
sub.dat1$bat_sex[sub.dat1$bat_sex=="female"] <- "F"

unique(sub.dat1$bat_sex)

mad.dat1 <- ddply(sub.dat1, .(Genus, bat_species, bat_sex, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm))
head(mad.dat1)



#instead, try this:
head(forearm.dat1)
head(mad.dat1)
lit.dat <- dplyr::select(forearm.dat1, Binomial,Sex,  AdultBodyMass_g, Adult_forearm_length_mm)
mad.sub <- dplyr::select(mad.dat1, bat_species, bat_sex,bat_weight_g, bat_forearm_mm )
names(lit.dat) <- names(mad.sub)

#and the mad sum data
lit.dat = subset(lit.dat, bat_species!="Pteropus rufus" & bat_species!="Eidolon dupreanum")
mad.sub.sum = ddply(mad.sub, .(bat_species, bat_sex), summarize, bat_weight_g = mean(bat_weight_g, na.rm=T), bat_forearm_mm = mean(bat_forearm_mm, na.rm = T))


mad.sub$mada_bat <- "y"

lit.dat$mada_bat <- "n"
mad.sub.sum$mada_bat <- "n"

all.dat <- rbind(mad.sub, lit.dat, mad.sub.sum)

#make a color bar and specify madagascar bats
length(unique(all.dat$bat_species))#22
colz <- scales::hue_pal()(22) #makes a color bar the length of each species
names(colz) <- unique(all.dat$bat_species)
colz #look at it. gives a color for each species
#now overwrite the mada bats
colz[names(colz)=="Pteropus rufus"] = "blue"
colz[names(colz)=="Eidolon dupreanum"] = "lightgreen"
colz[names(colz)=="Rousettus madagascariensis"] = "purple"




p.all <- ggplot(data=all.dat) + 
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & mada_bat=="n" | bat_species=="Eidolon dupreanum" & mada_bat=="n" | bat_species=="Rousettus madagascariensis" & mada_bat=="n"), aes( x=bat_forearm_mm , y=bat_weight_g), color="black", size=5) +
  geom_point(aes( x=bat_forearm_mm , y=bat_weight_g, color=bat_species), size=3)+ facet_grid(Sex~.)+
  scale_color_manual(values=colz) +
  facet_grid(bat_sex~mada_bat)+theme_bw()+
  theme(strip.text.x = element_blank(), legend.position = "bottom")+theme(element_blank())+labs(x="Forearm Length (mm)", y="Body weight (g)")

print(p.all)

#now, to get the plots



###################################################
################ fit model lmodel2 Mada date

library(lmodel2)

female.mod.mada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="F" & mada_bat=="y"), nperm = 99)
male.mod.mada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="M" & mada_bat=="y"), nperm = 99)


#and the non-mada
female.mod.nonmada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="F" & mada_bat=="n"), nperm = 99)
male.mod.nonmada <- lmodel2(log10(bat_weight_g)~ log10(bat_forearm_mm), data=subset(all.dat, bat_sex=="M" & mada_bat=="n"), nperm = 99)

#save them as "m" and "b" as in y=mx+b
mfemMad <- female.mod.mada$regression.results[3,3]
bfemMad <- female.mod.mada$regression.results[3,2]
mmalMad <- male.mod.mada$regression.results[3,3]
bmalMad <- male.mod.mada$regression.results[3,2]

mfemNonMad <- female.mod.nonmada$regression.results[3,3]
bfemNonMad <- female.mod.nonmada$regression.results[3,2]
mmalNonMad <- male.mod.nonmada$regression.results[3,3]
bmalNonMad <- male.mod.nonmada$regression.results[3,2]



# Save R-squar for all of the model
RsqF<- female.mod.mada[["rsquare"]]
RsqM<-male.mod.mada[["rsquare"]]
RsqNF<-female.mod.nonmada[["rsquare"]]
RsqNM<-male.mod.nonmada[["rsquare"]]

rsq_all<-data.frame(RsqF,RsqM,RsqNM,RsqNF)
colnames(rsq_all)<-c("F Malagasy bats","M malagasy bats", "M non malagasy","F non malagay")

write.csv(rsq_all,"rsquare_bat_model.csv")

#mBody = modBodyMass$regression.results[3,3]
#bBody = modBodyMass$regression.results[3,2]

#and the confidence intervals: - do later



#write a function to predict y = mx+b
regress.func <- function(x,m,b){
  y = m*x+b
  return(y)
  
}


#and predict across your dataset
all.dat$predicted_mass <- NA
all.dat$predicted_mass[all.dat$bat_sex=="F" & all.dat$mada_bat=="y"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$mada_bat=="y"]), m=mfemMad, b=bfemMad))
all.dat$predicted_mass[all.dat$bat_sex=="M" & all.dat$mada_bat=="y"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$mada_bat=="y"]), m=mmalMad, b=bmalMad))
all.dat$predicted_mass[all.dat$bat_sex=="F" & all.dat$mada_bat=="n"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="F" & all.dat$mada_bat=="n"]), m=mfemMad, b=bfemMad))
all.dat$predicted_mass[all.dat$bat_sex=="M" & all.dat$mada_bat=="n"] <- 10^(regress.func(x=log10(all.dat$bat_forearm_mm[all.dat$bat_sex=="M" & all.dat$mada_bat=="n"]), m=mmalMad, b=bmalMad))


#do CIs later


#here plot with data:

pall_2 <- ggplot(data=all.dat) + 
  geom_point(data=subset(all.dat, bat_species=="Pteropus rufus" & mada_bat=="n" | bat_species=="Eidolon dupreanum" & mada_bat=="n" | bat_species=="Rousettus madagascariensis" & mada_bat=="n"),
             aes( x=bat_forearm_mm , y=bat_weight_g), color="black", size=5) +
  geom_point(aes( x=bat_forearm_mm , y=bat_weight_g, color=bat_species), size=3)+ 
  facet_grid(Sex~.)+
  scale_color_manual(values=colz) +
  geom_line(aes(x=bat_forearm_mm , y=predicted_mass)) +
  facet_grid(bat_sex~mada_bat)+theme_bw()+
  theme(strip.text.x = element_blank(), legend.position = "right")+theme(element_blank(), legend.text = element_text(face = "italic"))+labs(x="Forearm Length (mm)", y="Body weight (g)")+
  labs(color="Bat species")

print(pall_2)

View(all.dat)

all.dat1= subset(all.dat, bat_species!="Eidolon dupreanum"& bat_species!="Pteropus rufus" & bat_species!="Rousettus madagascariensis")


#############################
##########Ear
#################################################
########################

comp.dat <- read.csv(file = "Fruit-Bat-Morphology.csv", header=T, stringsAsFactors = F)
na.omit(comp.dat)
head(comp.dat)



#summarize
comp.dat$Adult_tibia_length_m <- as.numeric(comp.dat$Adult_tibia_length_m)
comp.dat$Adult_ear_length_mm <- as.numeric(comp.dat$Adult_ear_length_mm)

Eartib.dat <- dplyr::select(comp.dat,Genus, Binomial, Sex,Adult_tibia_length_m,Adult_ear_length_mm)

head(Eartib.dat)

na.omit(Eartib.dat)

Eartib.dat1= subset(Eartib.dat, Sex=="Female" | Sex=="Male")

unique(Eartib.dat1$Sex)

Eartib.dat1$Sex[Eartib.dat1$Sex=="Male"] <- "M"
Eartib.dat1$Sex[Eartib.dat1$Sex=="Female"] <- "F"
unique(Eartib.dat1$Sex)


### try without outliers
#look at the forearm lengths
#sort(unique(forearm.dat1$Adult_forearm_length_mm))
#forearm.dat1=subset(forearm.dat1, Adult_forearm_length_mm>80)

#also, check out tyhe outliers in the plot!
#and plot mass by forearm
library(ggbeeswarm)

unique(Eartib.dat1$Binomial) #where is Rousettus???? need to add in later

colzE <- c("F" = "deeppink", "M" = "cornflowerblue")
p1E <- ggplot(dat=Eartib.dat1) + geom_quasirandom(aes( x=Binomial, y=Adult_ear_length_mm, color=Binomial), size=3)+
  
  facet_grid(Sex~.)


print(p1E)

#that one female forearm is definitely an outlier! overwrite--it was probably entered wrong
#Santino, this was your entry-- can you go look it up?
#Dobsonia chapmani. you list forearm of 1282 mm from Pangutalan_et_al_2004
#I'm going to guess you are missing a decimal and it should be 128.2

Eartib.dat1$Adult_ear_length_mm[Eartib.dat1$Adult_ear_length_mm>50 & !is.na(Eartib.dat1$Adult_ear_length_mm)] <- 37.5

#replot 
p1E_1 <- ggplot(dat=Eartib.dat1) + geom_quasirandom(aes( x= Sex,  y=Adult_ear_length_mm,color=Binomial), size=3)


print(p1E_1)


#load mada data  
dat1 <- read.csv(file = "catch_dat_5_18_2021.csv", header=T, stringsAsFactors = F)
head(dat1)


#get mean adult body mass and forearm length by species and sex

unique(dat1$bat_species)
unique(dat1$bat_sex)
sub.dat1 = subset(dat1, bat_species== "Eidolon dupreanum" | bat_species== "Pteropus rufus" | bat_species== "Rousettus madagascariensis")

sub.dat1 = subset(sub.dat1, bat_sex=="male" | bat_sex=="female")
unique(sub.dat1$bat_age_class)

sub.dat1 = subset(sub.dat1,  bat_age_class=="A" | bat_age_class=="L" | bat_age_class=="P" | bat_age_class=="NL")


sub.dat1$bat_weight_g <- as.numeric(sub.dat1$bat_weight_g)

sub.dat1$bat_tibia_mm <- as.numeric(sub.dat1$bat_tibia_mm)

sub.dat1$bat_forearm_mm <- as.numeric(sub.dat1$bat_forearm_mm)

sub.dat1$ear_length_mm <- as.numeric(sub.dat1$ear_length_mm)



#subgrouped by genus, with shape by sex
sub.dat1$Genus = NA
sub.dat1$Genus[sub.dat1$bat_species=="Pteropus rufus"] <- "Pteropus"

sub.dat1$Genus[sub.dat1$bat_species=="Eidolon dupreanum"] <- "Eidolon"
sub.dat1$Genus[sub.dat1$bat_species=="Rousettus madagascariensis"] <- "Rousettus"

#names(sub.dat1)[19] <- "Sex"

sub.dat1$bat_sex[sub.dat1$bat_sex=="male"] <- "M"

sub.dat1$bat_sex[sub.dat1$bat_sex=="female"] <- "F"

unique(sub.dat1$bat_sex)

mad.dat1 <- ddply(sub.dat1, .(Genus, bat_species, bat_sex, ear_length_mm,bat_tibia_mm))


head(mad.dat1)

#first plot to see if there are outliers


library(ggnewscale)

color.vect <- c('Pteropus rufus'='blue','Eidolon dupreanum'='lightgreen','Rousettus madagascariensis'='purple')

p2E <- ggplot(dat=Eartib.dat1) +geom_quasirandom(aes(x= origin, y= Adult_ear_length_mm, color= Binomial, size=3))+ facet_grid(Sex~.)+
  
  new_scale("color")+
  
  geom_quasirandom(data=mad.dat1, aes(x= origin, y=ear_length_mm , color=bat_species))+scale_color_manual(values = color.vect)+
  
  facet_grid(bat_sex~.)+ theme_bw()+theme(legend.position = "none")+
  theme(element_blank())

print(p2E)


Eartib.dat1$origin <- "Non_Malagasy_bats" 

mad.dat1$origin <- "Malagasy_bats"


#look at the outliers in the dataset
mad.dat1[mad.dat1$bat_species=="Rousettus madagascariensis" & mad.dat1$ear_length_mm>30,]


library(ggnewscale)

#plot again
p2E_1 <- ggplot(dat=mad.dat1) +geom_beeswarm(aes(x= origin, y= ear_length_mm, color=bat_species))+scale_color_manual(values = color.vect)+
  new_scale("color")+
  geom_beeswarm(data=Eartib.dat1, aes(x= origin, y= Adult_ear_length_mm, color=Binomial))+
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank())+labs(x="Regions", y="Ear length (mm)")

print(p2E_1)



###########              Tibia     #################
###########                          #################

p2T <- ggplot(dat=Eartib.dat1) +geom_beeswarm(aes(x= origin, y= Adult_tibia_length_m, color= Binomial))+ facet_grid(Sex~.)+
  
  new_scale("color")+
  
  geom_beeswarm(data=mad.dat1, aes(x= origin, y=bat_tibia_mm , color=bat_species))+scale_color_manual(values = color.vect)+
  
  facet_grid(bat_sex~.)+ theme_bw()+theme(legend.position = "none")+
  theme(element_blank())

print(p2T)


Eartib.dat1$origin <- "Non_Malagasy_bats" 

mad.dat1$origin <- "Malagasy_bats"

#look at the outliers in the dataset
mad.dat1[mad.dat1$bat_species=="Rousettus madagascariensis" & mad.dat1$ear_length_mm>30,]


library(ggnewscale)

#plot again
p2T_1 <- ggplot(dat=mad.dat1) +geom_beeswarm(aes(x= origin, y= bat_tibia_mm, color=bat_species))+scale_color_manual(values = color.vect)+
  new_scale("color")+
  geom_beeswarm(data=Eartib.dat1, aes(x= origin, y= Adult_tibia_length_m, color=Binomial))+
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank())+labs(x="Regions", y="Tibia length (mm)")

print(p2T_1)


###############
###############
p2T_6<- ggplot() +
  geom_violin(data= Eartib.dat1, aes(x=origin, y=Adult_tibia_length_m),scale="width",fill="grey70",draw_quantiles=c(0.025,0.5,0.975)) +
  geom_violin(dat=mad.dat1,aes(x= origin, y= bat_tibia_mm),scale="width",fill="grey70",draw_quantiles=c(0.025,0.5,0.975))+
  geom_jitter(dat=mad.dat1,aes(x= origin, y= bat_tibia_mm, color=bat_species))+
  scale_color_manual(values = color.vect)+
  new_scale("color")+
  geom_jitter(data=Eartib.dat1, aes(x= origin, y= Adult_tibia_length_m, color=Binomial),size=2,width=.1,height=0)+
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank())+ 
  scale_x_discrete(name="Regions",labels=c("Malagasy_bats"="Malagasy bats",
                                           "Non_Malagasy_bats"="Non-malagasy bats"))+ scale_y_continuous(name = "Tibia length (mm)")

print(p2T_6)

####test###2

p2T_6_1<- ggplot() +
  geom_violin(data= Eartib.dat1, aes(x=origin, y=Adult_tibia_length_m),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  geom_beeswarm(dat=mad.dat1,aes(x= origin, y= bat_tibia_mm, color=bat_species))+
  scale_color_manual(values = color.vect)+
  new_scale("color")+
  geom_jitter(data=Eartib.dat1, aes(x= origin, y= Adult_tibia_length_m, color=Binomial),size=2,width=.1,height=0)+
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank())+ 
  scale_x_discrete(name="Regions",labels=c("Malagasy_bats"="Malagasy bats",
                                           "Non_Malagasy_bats"="Non-malagasy bats"))+ scale_y_continuous(name = "Tibia length (mm)")

print(p2T_6_1)

#########Ear

p2E_6_1<- ggplot() +
  geom_violin(data= Eartib.dat1, aes(x=origin, y=Adult_ear_length_mm),scale="width",fill="white",draw_quantiles=c(0.025,0.5,0.975), width=.3) +
  geom_beeswarm(dat=mad.dat1,aes(x= origin, y= ear_length_mm, color=bat_species))+
  scale_color_manual(values = color.vect)+
  new_scale("color")+
  geom_jitter(data=Eartib.dat1, aes(x= origin, y= Adult_ear_length_mm, color=Binomial),size=2,width=.1,height=0)+
  facet_grid(bat_sex~.)+theme_bw()+theme(legend.position = "none")+
  theme(element_blank())+ 
  scale_x_discrete(name="Regions",labels=c("Malagasy_bats"="Malagasy bats",
                                           "Non_Malagasy_bats"="Non-malagasy bats"))+ scale_y_continuous(name = "Ear length (mm)")

print(p2E_6_1)


library(cowplot)


p10<- plot_grid(p2T_6_1+ theme(legend.position = "none"),
                p2E_6_1 + theme(legend.position = "none"),
                ncol = 2, labels = "AUTO")


print(p10)

plot_grid(p10, pall_2 + theme(legend.position = "right"),
          ncol = 1,
          labels = c("","C"))




ggsave(file = "Result2_final.png",
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)

####ResultInterpret_Ear


View(all.dat1)
class(all.dat1$bat_forearm_mm)

summary(Eartib.dat1)

quant_EarAllBAtsF<- quantile(Eartib.dat1$Adult_ear_length_mm[Eartib.dat1$Sex=="F"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarAllBAtsF

quant_EarAllBAtsM<- quantile(Eartib.dat1$bat_forearm_mm[Eartib.dat1$Sex=="M"],probs=c(0.025,0.5,0.975),na.rm = T)




############Result_2-Ear Malagasy bats#############

quant_EarPert_F<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarPert_F

quant_EarPert_M<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarPert_M
##

quant_EarEid_F<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarEid_F

quant_EarEid_M<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarEid_M
###
quant_EarRou_F<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarRou_F

quant_EarRou_M<-quantile(mad.dat1$ear_length_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_EarRou_M



QuantBat_ear<- data.frame(quant_EarPert_F,quant_EarPert_M,quant_EarEid_F,quant_EarEid_M,
                          quant_EarRou_F,quant_EarRou_M, quant_EarAllBAtsF, quant_EarAllBAtsM)

View(QuantBat_ear)

write.csv(QuantBat_ear, file = "QuantileDatEar.csv")

##########
########### Tibia
####ResultInterpret_Ear



summary(Eartib.dat1)

quant_tibiAllBAtsF<- quantile(Eartib.dat1$Adult_tibia_length_m[Eartib.dat1$Sex=="F"],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibiaAllBAtsM<- quantile(Eartib.dat1$Adult_tibia_length_m[Eartib.dat1$Sex=="M"],probs=c(0.025,0.5,0.975),na.rm = T)




############Result_2-Tibia Malagasy bats#############

quant_tibPert_F<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibPert_F

quant_tibPert_M<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibPert_M
##

quant_tibEid_F<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibEid_F

quant_tibEid_M<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibEid_M
###
quant_tibRou_F<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibRou_F

quant_tibRou_M<-quantile(mad.dat1$bat_tibia_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_tibRou_M



QuantBat_tib<- data.frame(quant_tibPert_F,quant_tibPert_M,quant_tibEid_F,quant_tibEid_M,
                          quant_tibRou_F,quant_tibRou_M, quant_tibiAllBAtsF, quant_tibiaAllBAtsM)

View(QuantBat_tib)

write.csv(QuantBat_tib, file = "QuantileDat_tib.csv")


##########
########### FF
####ResultInterpret_Ear





quant_FFAllBAtsF<- quantile(all.dat1$bat_forearm_mm[all.dat1$bat_sex=="F"],probs=c(0.025,0.5,0.975),na.rm = T)


quant_FFAllBAtsM<- quantile(all.dat1$bat_forearm_mm[all.dat1$bat_sex=="M"],probs=c(0.025,0.5,0.975),na.rm = T)




############Result_2-Tibia Malagasy bats#############

quant_FFPert_F<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFPert_F

quant_FFPert_M<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Pteropus rufus"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFPert_M
##

quant_FFEid_F<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFEid_F

quant_FFEid_M<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Eidolon dupreanum"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFEid_M
###
quant_FFRou_F<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="F" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFRou_F

quant_FFRou_M<-quantile(mad.dat1$bat_forearm_mm[mad.dat1$bat_species=="Rousettus madagascariensis"&mad.dat1$bat_sex=="M" ],probs=c(0.025,0.5,0.975),na.rm = T)

quant_FFRou_M



QuantBat_FF<- data.frame(quant_FFPert_F,quant_FFPert_M,quant_FFEid_F,quant_FFEid_M,
                         quant_FFRou_F,quant_FFRou_M, quant_FFAllBAtsF, quant_FFAllBAtsM)

View(QuantBat_FF)

write.csv(QuantBat_FF, file = "QuantileDat_FF_real.csv")

summary(mad.dat1)


