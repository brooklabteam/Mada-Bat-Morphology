###############ANALYSE TAONA#########################
rm(list=ls())

library(tidyverse)
library(readr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(ggplot2)
library(lubridate)
library(cowplot)
library(gratia)

# first, set the working directory to this Fig4 subfolder
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/"
# above should be the parent directory for the entire project
# for me, this is "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/"

setwd(paste0(homewd, "Fig4/"))


bat<- read.csv(file = paste0(homewd, "morph_paper_dat_7_23_2021.csv"), header=T, stringsAsFactors = F)
head(bat)

bat$bat_forearm_mm[bat$bat_forearm_mm==165.3 & bat$sampleid=="KEL246"] <- 105.3 #KEL246
bat = subset(bat, sampleid!="TSI058")
bat <- subset(bat, sampleid!="MIZ544")


###FIGURE_4
#' Ity manaraka ity ny tondrozotra (Guide lines)

#'Can you identify the earliest capture of a new juvenile for each of our three species? 
#'Then, set that as day 1 for each species respectively 
#'and for Figure 4, plot day since day 1 on the x axis with: 
#'(a) forearm length, 
#'(b) tibia, and 
#'(c) ear for all three species, divided "by sex" and "species"

####_Select the data to use

data1 <- dplyr::select(bat,bat_species,sampleid,bat_sex,roost_site,collection_date,
                       bat_age_class,bat_weight_g,body_length_cm,bat_forearm_mm,bat_tibia_mm,
                       ear_length_mm,bat_age_class)


#check for NAs in dataset
nrow(data1[is.na(data1$collection_date),]) #0
nrow(data1[is.na(data1$bat_forearm_mm),]) #10
nrow(data1[is.na(data1$bat_weight_g),]) #19
nrow(data1[is.na(data1$bat_tibia_mm),]) #9
nrow(data1[is.na(data1$ear_length_mm),]) #10

# Make numeric
data1$ bat_weight_g<-as.numeric(data1$ bat_weight_g)
data1$ body_length_cm<-as.numeric(data1$ body_length_cm)
data1$ ear_length_mm<-as.numeric(data1$ ear_length_mm)
data1$bat_forearm_mm<-as.numeric(data1$bat_forearm_mm)
data1$bat_tibia_mm<-as.numeric(data1$bat_tibia_mm)
head(data1)
###############################################################################################################################
################################# PTEROPUS RUFUS ##############################################################################
###############################################################################################################################

# 
pter_dat<-subset(data1, bat_species=="Pteropus rufus")
head(pter_dat)
#' the birth day may change in the differents region e.g. Pteropus from moramanga != Mahabo
#' so we selected the bat from the same region
#' here Moramanga because this is the only region where we have a large samples

unique(pter_dat$roost_site) #all are moramabga sites except Makira and Mahabo
pter_dat=subset(pter_dat,roost_site!="Makira"&roost_site!="Mahabo")
pter_dat=subset(pter_dat,bat_sex!="unknown")
unique(pter_dat$bat_sex)

unique(pter_dat$bat_sex)
unique(pter_dat$bat_age_class)

#' we only need the data about the juveniles for this figure
pter_dat_juv<-subset(pter_dat, bat_age_class=="J")
unique(pter_dat_juv$bat_age_class)


#get day of year using 'lubridate' package
class(pter_dat_juv$collection_date)
pter_dat_juv$collection_date<-as.Date(pter_dat_juv$collection_date, format = "%m/%d/%y")
pter_dat_juv$yday<-yday(pter_dat_juv$collection_date)
head(pter_dat_juv)

#plot(pter_dat_juv$bat_forearm_mm,pter_dat_juv$new_yday)
pter_dat_juv$month <- month(pter_dat_juv$collection_date)
#when is the earliest day of year that a juvenile is observed new? 
#(find the earliest day of year for juveniles that are clearly still in the first year of life)
pter_dat_juv[pter_dat_juv$month==8,] #these bats are big, almost a year old
pter_dat_juv[pter_dat_juv$month==9,] #some of these are small. September 29

new_first_yday<-yday(dmy("29-09-2019")) # earliest date when we observed a new born
new_last_yday<-new_first_yday-1         # the day before the observation becomes the new last day of the year
diff_new_yday<-365-new_last_yday        # this give the difference between the normal last day (from january 1st) and the new last day of the year



jour<-c() # I create an empty vector for the new value of the day from the birth
for (i in 1:length(pter_dat_juv$yday)){
  if(pter_dat_juv$yday[i]<=new_last_yday){
    jour<-append(jour,pter_dat_juv$yday[i]+diff_new_yday)
  }else{
    jour<-append(jour,pter_dat_juv$yday[i]-new_last_yday)
  }
  print(as.data.frame(jour))
}


#check they are the same length
length(pter_dat_juv$bat_species)==length(jour) 
pter_dat_juv$new_yday<-jour


# "TSI039"& "TSI040" are exactly one year old, so we remove them
pter_dat_juv=subset(pter_dat_juv,sampleid!="TSI039"&sampleid!="TSI040")


### ANALYSE Forarm BATS
mod_FF <- gam(bat_forearm_mm~s(new_yday,k=7, bs="tp"), data=pter_dat_juv)
summary(mod_FF)
plot(mod_FF)

pter_dat_juv$predicted_mod_FF<- predict(mod_FF, type="response", se.fit = T)$fit
pter_dat_juv$predicted_FF_SE <- predict(mod_FF, type="response", se.fit = T)$se.fit
pter_dat_juv$predicted_FF_lci <- pter_dat_juv$predicted_mod_FF -1.96*pter_dat_juv$predicted_FF_SE
pter_dat_juv$predicted_FF_uci <- pter_dat_juv$predicted_mod_FF +1.96*pter_dat_juv$predicted_FF_SE
pter_dat_juv$predicted_FF_lci[pter_dat_juv$predicted_FF_lci<0] <- 0



unique(pter_dat_juv$bat_sex)


#now, get the "slope" of the fitted gam at
#day 30, 60, and 90, using the "derivatives" function of 
#the gratia package

#function
get.gam.deriv <- function(orig_dat, orig_gam, days_to_calc, bat_spp, measurement){
  
  newDF <- with(orig_dat, data.frame(new_yday = days_to_calc))
  
  out = derivatives(orig_gam,
                    newdata = newDF,
                    order=1,
                    type="central",
                    eps = 1e-7,
                    interval = "confidence")
  
  out_df <- cbind.data.frame(bat_species=bat_spp, 
                             measurement=measurement, 
                             day_of_year=days_to_calc, 
                             slope= out$derivative,
                             lci = out$lower,
                             uci=out$upper)
  return(out_df)
  
}


out.FF <- get.gam.deriv(orig_dat = pter_dat_juv, 
                         orig_gam = mod_FF, 
                         days_to_calc = c(1:100),
                         bat_spp = "Pteropus_rufus", 
                         measurement ="forearm")

formula_text = paste0("slope at day 60=\n", signif(out.FF$slope[out.FF$day_of_year==60], 3), " [", signif(out.FF$lci[out.FF$day_of_year==60], 3), "-", signif(out.FF$uci[out.FF$day_of_year==60], 3), "]")


sexforma<-c("male"=16,"female"=17)


FFP<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_forearm_mm,shape=bat_sex),color="grey",size=3,data=pter_dat_juv)+
  ylab("") +
  ggtitle("Pteropus rufus")+
  xlab("")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="italic"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=300, y = 60, label = formula_text,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  geom_line(aes(x=new_yday, y=predicted_mod_FF), color="blue", size=1,data = pter_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_FF_lci, ymax=predicted_FF_uci), fill="blue", size=1, alpha=.3,data = pter_dat_juv)
FFP

##################################################################################"
### Ilay resaka TIBIA indray izao

### ANALYSE TIBIA BATS

# GAM ny Femelle

mod_TF <- gam(bat_tibia_mm~s(new_yday,k=7, bs="tp"), data=pter_dat_juv)
summary(mod_TF)
plot(mod_TF)

pter_dat_juv$predicted_mod_TF<- predict(mod_TF, type="response", se.fit = T)$fit
pter_dat_juv$predicted_TF_SE <- predict(mod_TF, type="response", se.fit = T)$se.fit
pter_dat_juv$predicted_TF_lci <- pter_dat_juv$predicted_mod_TF -1.96*pter_dat_juv$predicted_TF_SE
pter_dat_juv$predicted_TF_uci <- pter_dat_juv$predicted_mod_TF +1.96*pter_dat_juv$predicted_TF_SE
pter_dat_juv$predicted_TF_lci[pter_dat_juv$predicted_TF_lci<0] <- 0


out.TFP <- get.gam.deriv(orig_dat = pter_dat_juv, 
                           orig_gam = mod_TF, 
                           days_to_calc = c(1:100),
                           bat_spp = "Pteropus_rufus", 
                           measurement ="tibia")

formula_text = paste0("slope at day 60=\n", signif(out.TFP$slope[out.TFP$day_of_year==60], 3), " [", signif(out.TFP$lci[out.TFP$day_of_year==60], 3), "-", signif(out.TFP$uci[out.TFP$day_of_year==60], 3), "]")

TFP<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_tibia_mm,shape=bat_sex),color="grey",size=3,data=pter_dat_juv) +
  ylab("") +
  ggtitle("Pteropus rufus")+
  xlab("")+
  labs(title="")+
  theme_bw()+ 
  theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="italic"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  annotate("text" ,x=300, y = 40, label = formula_text,size=3)+
  geom_line(aes(x=new_yday, y=predicted_mod_TF), color="blue", size=1,data = pter_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_TF_lci, ymax=predicted_TF_uci), fill="blue", size=1, alpha=.3,data = pter_dat_juv)

TFP



###########################################"
#'    (c) ear 

# GAM ny Femelle
mod_EF <- gam(ear_length_mm~s(new_yday,k=7, bs="tp"), data=pter_dat_juv)
summary(mod_EF)

plot(mod_EF)
pter_dat_juv$predicted_mod_EF<- predict(mod_EF, type="response", se.fit = T)$fit
pter_dat_juv$predicted_EF_SE <- predict(mod_EF, type="response", se.fit = T)$se.fit
pter_dat_juv$predicted_EF_lci <- pter_dat_juv$predicted_mod_EF -1.96*pter_dat_juv$predicted_EF_SE
pter_dat_juv$predicted_EF_uci <- pter_dat_juv$predicted_mod_EF +1.96*pter_dat_juv$predicted_EF_SE
pter_dat_juv$predicted_EF_lci[pter_dat_juv$predicted_EF_lci<0] <- 0


out.EF <- get.gam.deriv(orig_dat = pter_dat_juv, 
                         orig_gam = mod_EF, 
                         days_to_calc = c(1:100),
                         bat_spp = "Pteropus_rufus", 
                         measurement ="ear_length")

formula_text = paste0("slope at day 60=\n", signif(out.EF$slope[out.EF$day_of_year==60], 3), " [", signif(out.EF$lci[out.EF$day_of_year==60], 3), "-", signif(out.EF$uci[out.EF$day_of_year==60], 3), "]")


EFP <- ggplot() +
  geom_point(aes(x=new_yday, y=ear_length_mm,shape=bat_sex),color="grey",size=3,data=pter_dat_juv) +
  ylab("") +
  xlab("Day of year")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=300, y = 20, label = formula_text ,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("29 sep","05 jan", "06 apr","05 jul"))+
  geom_line(aes(x=new_yday, y=predicted_mod_EF), color="blue", size=1,data = pter_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_EF_lci, ymax=predicted_EF_uci),fill="blue", size=1, alpha=.3,data = pter_dat_juv)
EFP


A <-plot_grid(NULL,FFP+ theme(legend.position = "none"),
               NULL,TFP+ theme(legend.position = "none"),
               NULL,EFP+  theme(legend.position = "none", legend.title = element_blank(),
                                legend.background = element_blank()),
               rel_heights = c(0.1,1,-0.09,1,-0.02,1),
               ncol = 1, label_y = 1.07,
               labels = c("","B","","E","","H"),
               label_size = 10,
               align = "v")
print(A)



#and save the data
out.Pter <- rbind(out.FF,out.TFP,out.EF)
########################################################################################################################
############################### ROUSETTUS MADAGASCARIENSIS ######################################################################
#####################################################################################################
#
rou_dat<-subset(data1, bat_species=="Rousettus madagascariensis")
head(rou_dat)


unique(rou_dat$roost_site)
rou_dat<-subset(rou_dat,roost_site=="Maromizaha")
rou_dat<-subset(rou_dat,bat_sex!="unknown")

unique(rou_dat$bat_sex)
unique(rou_dat$bat_age_class)

#' ary satria "Juvenil no reaka eto dia izayihany no data alaina
rou_dat_juv<-subset(rou_dat, bat_age_class=="J")
unique(rou_dat_juv$bat_age_class)

#'Avadiaka day of the  ny daty normal mahazatra (day 1= January 1st)
#'(ilay package lubridate no nampiasaina nanovana ilay daty)

class(rou_dat_juv$collection_date)
rou_dat_juv$collection_date<-as.Date(rou_dat_juv$collection_date, format = "%m/%d/%y")
rou_dat_juv$yday<-yday(rou_dat_juv$collection_date)

#plot(rou_dat_juv$bat_forearm_mm,rou_dat_juv$new_yday)

rou_dat_juv$month <- month(rou_dat_juv$collection_date)
rou_dat_juv[rou_dat_juv$month==11,]
rou_dat_juv[rou_dat_juv$month==12,]

new_first_yday<-yday(dmy("12-12-2014")) # earliest date when we observed a new born
new_last_yday<-new_first_yday-1         # the day befor the observation become the new last 
dif_new_yday<-365-new_last_yday         # this give the difference between the normal last day (from january 1st) and the new last 


jour<-c() # I creat an empty vector for the new value of the day from the birth
for (i in 1:length(rou_dat_juv$yday)){
  if(rou_dat_juv$yday[i]<=new_last_yday){
    jour<-append(jour,rou_dat_juv$yday[i]+dif_new_yday)
  }else{
    jour<-append(jour,rou_dat_juv$yday[i]-new_last_yday)
  }
  print(as.data.frame(jour))
}



length(rou_dat_juv$bat_species)==length(jour) 
rou_dat_juv$new_yday<-jour




### ANALYSE FoREARME BATS


# GAM ny Femelle
mod_FF <- gam(bat_forearm_mm~s(new_yday,k=7, bs="tp"), data=rou_dat_juv)
summary(mod_FF)
plot(mod_FF)


rou_dat_juv$predicted_mod_FF<- predict(mod_FF, type="response", se.fit = T)$fit
rou_dat_juv$predicted_zaza_SE <- predict(mod_FF, type="response", se.fit = T)$se.fit
rou_dat_juv$predicted_zaza_lci <- rou_dat_juv$predicted_mod_FF -1.96*rou_dat_juv$predicted_zaza_SE
rou_dat_juv$predicted_zaza_uci <- rou_dat_juv$predicted_mod_FF +1.96*rou_dat_juv$predicted_zaza_SE
rou_dat_juv$predicted_zaza_lci[rou_dat_juv$predicted_zaza_lci<0] <- 0



out.FF <- get.gam.deriv(orig_dat = rou_dat_juv, 
                        orig_gam = mod_FF, 
                        days_to_calc = c(1:100),
                        bat_spp = "Rousettus_madagascariensis", 
                        measurement ="forearm")

formula_text = paste0("slope at day 60=\n", signif(out.FF$slope[out.FF$day_of_year==60], 3), " [", signif(out.FF$lci[out.FF$day_of_year==60], 3), "-", signif(out.FF$uci[out.FF$day_of_year==60], 3), "]")



## PLOTS An'izy roa Mitambatra

sexloko<-c("male"="lightblue","female"="orange")

FFR<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_forearm_mm,shape=bat_sex),color="grey",size=3,data=rou_dat_juv)+
  scale_color_manual(values = sexloko,aesthetics = "colour")+
  ylab("") +
  ggtitle("Rousettus madagascariensis")+
  xlab("")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="italic"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=250, y = 30, label = formula_text,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  geom_line(aes(x=new_yday, y=predicted_mod_FF), color="purple", size=1,data = rou_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_zaza_lci, ymax=predicted_zaza_uci), fill="purple", size=1, alpha=.3,data = rou_dat_juv)
FFR


##################################################################################"
### Ilay resaka TIBIA indray izao

### ANALYSE TIBIA BATS

# GAM ny Femelle

mod_TF <- gam(bat_tibia_mm~s(new_yday,k=7, bs="tp"), data=rou_dat_juv)
summary(mod_TF)
plot(mod_TF)

rou_dat_juv$predicted_mod_TF<- predict(mod_TF, type="response", se.fit = T)$fit
rou_dat_juv$predicted_TF_SE <- predict(mod_TF, type="response", se.fit = T)$se.fit
rou_dat_juv$predicted_TF_lci <- rou_dat_juv$predicted_mod_TF -1.96*rou_dat_juv$predicted_TF_SE
rou_dat_juv$predicted_TF_uci <- rou_dat_juv$predicted_mod_TF +1.96*rou_dat_juv$predicted_TF_SE
rou_dat_juv$predicted_TF_lci[rou_dat_juv$predicted_TF_lci<0] <- 0




out.TFP <- get.gam.deriv(orig_dat = rou_dat_juv, 
                         orig_gam = mod_TF, 
                         days_to_calc = c(1:100),
                         bat_spp = "Rousettus_madagascariensis", 
                         measurement ="tibia")

formula_text = paste0("slope at day 60=\n", signif(out.TFP$slope[out.TFP$day_of_year==60], 3), " [", signif(out.TFP$lci[out.TFP$day_of_year==60], 3), "-", signif(out.TFP$uci[out.TFP$day_of_year==60], 3), "]")

########PLOT
TFR<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_tibia_mm,shape=bat_sex),color="grey",size=3,data=rou_dat_juv) +
  scale_color_manual(values = sexloko,aesthetics = "colour")+
  ylab("")+
  xlab("")+
  labs(title="")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  annotate("text" ,x=250, y = 20, label = formula_text,size=3)+
  geom_line(aes(x=new_yday, y=predicted_mod_TF), color="purple", size=1,data = rou_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_TF_lci, ymax=predicted_TF_uci), fill="purple", size=1, alpha=.3,data = rou_dat_juv)
TFR


###########################################"
#'    (c) ear 

# GAM ny Femelle
mod_EF <- gam(ear_length_mm~s(new_yday,k=7, bs="tp"), data=rou_dat_juv)
summary(mod_EF)

plot(mod_EF)
rou_dat_juv$predicted_mod_EF<- predict(mod_EF, type="response", se.fit = T)$fit
rou_dat_juv$predicted_EF_SE <- predict(mod_EF, type="response", se.fit = T)$se.fit
rou_dat_juv$predicted_EF_lci <- rou_dat_juv$predicted_mod_EF -1.96*rou_dat_juv$predicted_EF_SE
rou_dat_juv$predicted_EF_uci <- rou_dat_juv$predicted_mod_EF +1.96*rou_dat_juv$predicted_EF_SE
rou_dat_juv$predicted_EF_lci[rou_dat_juv$predicted_EF_lci<0] <- 0


out.EF <- get.gam.deriv(orig_dat = rou_dat_juv, 
                        orig_gam = mod_EF, 
                        days_to_calc = c(1:100),
                        bat_spp = "Rousettus_madagascariensis", 
                        measurement ="ear_length")

formula_text = paste0("slope at day 60=\n", signif(out.EF$slope[out.EF$day_of_year==60], 3), " [", signif(out.EF$lci[out.EF$day_of_year==60], 3), "-", signif(out.EF$uci[out.EF$day_of_year==60], 3), "]")





EFR <- ggplot() +
  geom_point(aes(x=new_yday, y=ear_length_mm,shape=bat_sex),color="grey",size=3,data=rou_dat_juv) +
  scale_color_manual(values = sexloko,aesthetics = "colour")+
  ylab("") +
  xlab("Day of year")+
  ggtitle("") +
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    
                    legend.background = element_rect(color="black",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=160, y = 5, label = formula_text,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("12 dec","25 mar", "03 jun","10 sep"))+
  geom_line(aes(x=new_yday, y=predicted_mod_EF), color="purple", size=1,data = rou_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_EF_lci, ymax=predicted_EF_uci),fill="purple", size=1, alpha=.3,data = rou_dat_juv)
EFR



B<-plot_grid(NULL,FFR+ theme(legend.position = "none"),NULL,
               TFR+ theme(legend.position = "none"),NULL,
               EFR + theme(legend.position = c(.8,.17), 
                           legend.title = element_blank(), legend.text = element_text(size=5)),
               rel_heights = c(0.1,1,-0.09,1,-0.09,1),
               ncol = 1, label_y = 1.07,
               labels = c("","C","","F","","I"),
               label_size = 10,
               align = "v")
print(B)


out.Rou<- rbind(out.FF,out.TFP,out.EF)
###############################################################################################################################################
##########################  EIDOLON DUPREANUM  ################################################################################################
###############################################################################################################################################

eid_dat<-subset(data1, bat_species=="Eidolon dupreanum")
head(eid_dat)
#' satria tsy mety tsy mitovy ny fahaterahan'ireo biby ireo amin'ny toerana samihafa
#' dia izay toerana mifanakaiky sy mitovy no atao
#' Eto izao Moramanga

unique(eid_dat$roost_site)
eid_dat<-subset(eid_dat,roost_site!="Ankarana_Chauves_Souris"&roost_site!="Mahabo")
eid_dat<-subset(eid_dat,roost_site!="Ankarana_Cathedral"&roost_site!="Ankarana_Canyon")
eid_dat<-subset(eid_dat,bat_sex!="unknown")

unique(eid_dat$bat_sex)
unique(eid_dat$bat_age_class)

#' ary satria "Juvenil no reaka eto dia izayihany no data alaina
eid_dat_juv<-subset(eid_dat, bat_age_class=="J")
unique(eid_dat_juv$bat_age_class)



#'Avadiaka day of the day of the year ny daty normal mahazatra (day 1= January 1st)
#'(ilay package lubridate no nampiasaina nanovana ilay daty)

class(eid_dat_juv$collection_date)

eid_dat_juv$collection_date<-as.Date(eid_dat_juv$collection_date, format="%m/%d/%y")
eid_dat_juv$yday<-yday(eid_dat_juv$collection_date)

eid_dat_juv$month <- month(eid_dat_juv$collection_date)

eid_dat_juv[eid_dat_juv$month==9,] #big bats
eid_dat_juv[eid_dat_juv$month==10,] #big bats
eid_dat_juv[eid_dat_juv$month==11,] #some very small (<100 g bats)

#plot(eid_dat_juv$bat_forearm_mm,eid_dat_juv$new_yday)

#' Nojerevana tao anatin'ny Data base hoe oviana no fotaoana aloha indrindra nahitana bats terabao
#' eto dia 29-09-2018 == 272th day of the year (from january)
#' Ka havadiaka io indray no day 1
#' raha izany dia izao: 
new_first_yday<-yday(dmy("16-11-2019")) # earliest date when we observed a new born (Nov 16)
new_last_yday<-new_first_yday-1         # the day befor the observation become the new last day of the year
dif_new_yday<-365-new_last_yday         # this give the difference between the normal last day (from january 1st) and the new last day of the year

# Eto izaho manao kajy ka izay ao alohan'ny 272 dia ampiana ny "dif_new_yday" 
# ary izay manomboka eo amin'ny 272 indray dia analana ny valeur ny "new_last_day"

jour<-c() # I creat an empty vector for the new value of the day from the birth
for (i in 1:length(eid_dat_juv$yday)){
  if(eid_dat_juv$yday[i]<=new_last_yday){
    jour<-append(jour,eid_dat_juv$yday[i]+dif_new_yday)
  }else{
    jour<-append(jour,eid_dat_juv$yday[i]-new_last_yday)
  }
  print(as.data.frame(jour))
}

#' Ny valiny azo io ambony io dia ilay taona vaovao 
#' AMpidiritsika ao anatin'ilay donn?e "eid_dat_juv"

length(eid_dat_juv$bat_species)==length(jour) # mampitovy ny halavvan'izy roa
eid_dat_juv$new_yday<-jour

# There are some 'Young Last Year" that are 1 year+ in age -- remove them
eid_dat_juv[eid_dat_juv$month==11,]
eid_dat_juv[eid_dat_juv$month==3,]

#eid_dat_juv[eid_dat_juv$bat_forearm_mm>140,]
#eid_dat_juv[eid_dat_juv$bat_forearm_mm<40,]
#eid_dat_juv=subset(eid_dat_juv,bat_forearm_mm<140&bat_forearm_mm>40)

mod_FF <- gam(bat_forearm_mm~s(new_yday,k=7, bs="tp")+bat_sex, data=eid_dat_juv)

summary(mod_FF)

eid_dat_juv$predicted_mod_FF<- predict(mod_FF, type="response", se.fit = T)$fit
eid_dat_juv$predicted_FF_SE <- predict(mod_FF, type="response", se.fit = T)$se.fit
eid_dat_juv$predicted_FF_lci <- eid_dat_juv$predicted_mod_FF -1.96*eid_dat_juv$predicted_FF_SE
eid_dat_juv$predicted_FF_uci <- eid_dat_juv$predicted_mod_FF +1.96*eid_dat_juv$predicted_FF_SE
eid_dat_juv$predicted_FF_lci[eid_dat_juv$predicted_TF_lci<0] <- 0
### ANALYSE FoREARME BATS

unique(eid_dat_juv$bat_sex)


out.FF <- get.gam.deriv(orig_dat = eid_dat_juv, 
                        orig_gam = mod_FF, 
                        days_to_calc = c(1:100),
                        bat_spp = "Eidolon_dupreanum", 
                        measurement ="forearm")

formula_text = paste0("slope at day 60=\n", signif(out.FF$slope[out.FF$day_of_year==60], 3), " [", signif(out.FF$lci[out.FF$day_of_year==60], 3), "-", signif(out.FF$uci[out.FF$day_of_year==60], 3), "]")



FFE<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_forearm_mm,shape=bat_sex),
             color="grey",size=3,data=eid_dat_juv)+
  ylab("Forearm length (mm)") +
  ggtitle("Eidolon dupreanum")+
  xlab("")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="italic"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=260, y = 80, label = formula_text,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  geom_line(aes(x=new_yday, y=predicted_mod_FF), color="light green", size=1,data = eid_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_FF_lci, ymax=predicted_FF_uci), fill="light green", size=1, alpha=.3,data = eid_dat_juv)
FFE

##################################################################################"
### Ilay resaka TIBIA indray izao

### ANALYSE TIBIA BATS

# GAM ny Femelle

mod_TF <- gam(bat_tibia_mm~s(new_yday,k=7, bs="cr")+bat_sex, data=eid_dat_juv)
summary(mod_TF)
plot(mod_TF)

eid_dat_juv$predicted_mod_TF<- predict(mod_TF, type="response", se.fit = T)$fit
eid_dat_juv$predicted_TF_SE <- predict(mod_TF, type="response", se.fit = T)$se.fit
eid_dat_juv$predicted_TF_lci <- eid_dat_juv$predicted_mod_TF -1.96*eid_dat_juv$predicted_TF_SE
eid_dat_juv$predicted_TF_uci <- eid_dat_juv$predicted_mod_TF +1.96*eid_dat_juv$predicted_TF_SE
eid_dat_juv$predicted_TF_lci[eid_dat_juv$predicted_TF_lci<0] <- 0



########PLOT
#linear model with tibia Female
out.TFP <- get.gam.deriv(orig_dat = eid_dat_juv, 
                         orig_gam = mod_TF, 
                         days_to_calc = c(1:100),
                         bat_spp = "Eidolon_dupreanum", 
                         measurement ="tibia")

formula_text = paste0("slope at day 60=\n", signif(out.TFP$slope[out.TFP$day_of_year==60], 3), " [", signif(out.TFP$lci[out.TFP$day_of_year==60], 3), "-", signif(out.TFP$uci[out.TFP$day_of_year==60], 3), "]")




TFE<-ggplot() +
  geom_point(aes(x=new_yday, y=bat_tibia_mm,shape=bat_sex),color="grey",size=3,data=eid_dat_juv) +
  scale_color_manual(values = sexloko,aesthetics = "colour")+
  ylab("Tibia length (mm)") +
  ggtitle("Eidolon dupreanum")+
  xlab("")+
  labs(title="")+
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                                    
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("","", "",""))+
  annotate("text" ,x=250, y = 35, label = formula_text,size=3)+
  geom_line(aes(x=new_yday, y=predicted_mod_TF), color="light green", size=1,data = eid_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_TF_lci, ymax=predicted_TF_uci), fill="light green", size=1, alpha=.3,data = eid_dat_juv)
TFE

###########################################"
#'    (c) ear 

# GAM ny Femelle
mod_EF <- gam(ear_length_mm~s(new_yday,k=5, bs="cr"), data=eid_dat_juv)
summary(mod_EF)

plot(mod_EF)
eid_dat_juv$predicted_mod_EF<- predict(mod_EF, type="response", se.fit = T)$fit
eid_dat_juv$predicted_EF_SE <- predict(mod_EF, type="response", se.fit = T)$se.fit
eid_dat_juv$predicted_EF_lci <- eid_dat_juv$predicted_mod_EF -1.96*eid_dat_juv$predicted_EF_SE
eid_dat_juv$predicted_EF_uci <- eid_dat_juv$predicted_mod_EF +1.96*eid_dat_juv$predicted_EF_SE
eid_dat_juv$predicted_EF_lci[eid_dat_juv$predicted_EF_lci<0] <- 0



out.EF <- get.gam.deriv(orig_dat = eid_dat_juv, 
                        orig_gam = mod_EF, 
                        days_to_calc = c(1:100),
                        bat_spp = "Eidolon_dupreanum", 
                        measurement ="ear_length")

formula_text = paste0("slope at day 60=\n", signif(out.EF$slope[out.EF$day_of_year==60], 3), " [", signif(out.EF$lci[out.EF$day_of_year==60], 3), "-", signif(out.EF$uci[out.EF$day_of_year==60], 3), "]")


EFE <- ggplot() +
  geom_point(aes(x=new_yday, y=ear_length_mm,shape=bat_sex),color="grey",size=3,data=eid_dat_juv) +
  scale_color_manual(values = sexloko,aesthetics = "colour")+
  ylab("Ear length(mm)") +
  ggtitle("Eidolon dupreanum")+
  xlab("Day of year")+
  ggtitle("") +
  theme_bw()+ theme(panel.grid = element_blank(),
                    plot.title = element_text(color="black", size=12, face="bold"),
                    axis.title.x = element_text(color="black", size=12),
                    axis.title.y = element_text(color="black", size=12),
                   
                    legend.background = element_rect(color="gray",size = .1),
                    legend.text = element_text(size = 12))+
  annotate("text" ,x=250, y = 17, label = formula_text,size=3)+
  scale_x_continuous(breaks = seq(0,361,100),
                     labels = c("16 nov","24 fev", "03 jun","11 sep"))+
  geom_line(aes(x=new_yday, y=predicted_mod_EF), color="light green", size=1,data = eid_dat_juv)+
  geom_ribbon(aes(x=new_yday, ymin=predicted_EF_lci, ymax=predicted_EF_uci),fill="light green", size=1, alpha=.3,data = eid_dat_juv)
EFE

out.Eid <- rbind(out.FF,out.TFP,out.EF)

C<-plot_grid(NULL,FFE+ theme(legend.position = "none"),NULL,
          TFE+ theme(legend.position = "none"),NULL,
          EFE + theme(legend.position = "none", legend.title = element_blank(),
                      legend.background = element_blank()) ,
          rel_heights = c(0.1,1,-0.09,1,-0.09,1),
          ncol = 1,
          labels = c("","A","","D","","G"),
          label_size = 10,
          align = "v")


print(C)



p<- plot_grid(C,A,B,
              rel_widths = c(2,2),
              nrow = 1)



print(p)


ggsave(file = paste0(homewd, "final-figures/Fig4_juvenile_growth_rates.png"),
       units="mm",  
       width=120, 
       height=90, 
       scale=2.5, 
       dpi=300)


          
out.all <- rbind(out.Pter, out.Rou, out.Eid)

#now save the derivative output here to this folder
write.csv(out.all, file = "Fig4_GAM_derivatives.csv", row.names = F)