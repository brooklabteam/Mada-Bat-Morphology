rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(lmodel2)

# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig3/"))


#load the catching data
dat <- read.csv(file = paste0(homewd, "morph_paper_dat_7_23_2021.csv"), header=T, stringsAsFactors = F)
head(dat)


data1 <- dplyr::select(dat,sampleid, roost_site, collection_date,
                       bat_species, bat_sex, bat_age_class, bat_weight_g,
                       body_length_cm, bat_forearm_mm, bat_tibia_mm,
                       ear_length_mm)




# check that there are not any NAs in the date category:

nrow(data1[is.na(data1$collection_date),])

###
### 
data1$collection_date <- as.Date(data1$collection_date, format = "%m/%d/%y")
month(data1$collection_date)

#add month and day of year
data1$month= month(data1$collection_date)
data1$day= yday(data1$collection_date)

head(data1)


### sub-select to just our 3 spp. of interest
Allspp <- filter(select(data1, sampleid,bat_species, roost_site, collection_date,month,day, bat_sex, 
                        bat_age_class, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm, body_length_cm), 
                 bat_species %in% c("Eidolon dupreanum","Pteropus rufus","Rousettus madagascariensis")) 

#sub-select to only the Adults (no "J" and no "NA")
unique(Allspp$bat_age_class)
AllsppAdults1 <- filter(select(Allspp, sampleid,bat_species, roost_site, collection_date,month,day, bat_sex,                              
                               bat_age_class, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm, body_length_cm), 
                        bat_age_class %in% c("A","NL","L", "P")) 

#sub-select only the Moramanga sites
unique(AllsppAdults1$roost_site)
AllsppAdult_Mora <- filter(select(AllsppAdults1, sampleid,bat_species, roost_site,day, collection_date,month, bat_sex, 
                                  bat_age_class, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm, body_length_cm), 
                           roost_site %in% c("AngavoKely","Angavokely", "AngavoBe","Ambakoana","Maromizaha","Marotsipohy", "Mahialambo", "Lakato", "Marovitsika", "Mangarivotra")) 

unique(AllsppAdult_Mora$roost_site)

#remove NA rows at the bottom
AllsppAdult_Mora[is.na(AllsppAdult_Mora),]
sum(is.na(AllsppAdult_Mora))
#remove NAs
AllsppAdult_Mora <- na.omit(AllsppAdult_Mora)



#######
AllsppAdult_Mora$bat_weight_g= as.numeric(AllsppAdult_Mora$bat_weight_g)

AllsppAdult_Mora$bat_forearm_mm= as.numeric(AllsppAdult_Mora$bat_forearm_mm)




#first, plot it to visualize
p1 <- ggplot(data = AllsppAdult_Mora) + geom_point(aes(x=bat_forearm_mm,y=bat_weight_g, color=bat_species))

print(p1) #this is all species together. 

#look with log10
p2 <- ggplot(data = AllsppAdult_Mora) + geom_point(aes(x=bat_forearm_mm,y=bat_weight_g)) +
      scale_y_log10() + scale_x_log10()
print(p2)

#now divide up by species
Eid.df = subset(AllsppAdult_Mora, bat_species=="Eidolon dupreanum")
Eid.df = arrange(Eid.df, desc(bat_forearm_mm))
head(Eid.df)
tail(Eid.df)


#and Pter
Pter.df = subset(AllsppAdult_Mora, bat_species=="Pteropus rufus")
Pter.df = arrange(Pter.df, desc(bat_forearm_mm))
head(Pter.df) #116 is too low
tail(Pter.df) #116 is too low


Rou.df = subset(AllsppAdult_Mora, bat_species=="Rousettus madagascariensis")
Rou.df = arrange(Rou.df, desc(bat_forearm_mm))
head(Rou.df) 
tail(Rou.df)


AllsppAdult_Mora <- rbind(Rou.df, Pter.df, Eid.df)



#now fit model by species and sex:
Pter.dat.adultF = subset(AllsppAdult_Mora, bat_species=="Pteropus rufus" &  bat_sex=="female")
Pter.dat.adultM = subset(AllsppAdult_Mora, bat_species=="Pteropus rufus"&  bat_sex=="male")
Eid.dat.adultF = subset(AllsppAdult_Mora, bat_species=="Eidolon dupreanum" &  bat_sex=="female")
Eid.dat.adultM = subset(AllsppAdult_Mora, bat_species=="Eidolon dupreanum" &  bat_sex=="male")
Rou.dat.adultF = subset(AllsppAdult_Mora, bat_species=="Rousettus madagascariensis" &  bat_sex=="female")
Rou.dat.adultM = subset(AllsppAdult_Mora, bat_species=="Rousettus madagascariensis" &  bat_sex=="male")


modPterF <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Pter.dat.adultF,nperm= 99)
modPterM <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Pter.dat.adultM,nperm= 99)

modEidF <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Eid.dat.adultF,nperm= 99)
modEidM <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Eid.dat.adultM,nperm= 99)

modRouF <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Rou.dat.adultF,nperm= 99)
modRouM <- lmodel2(log10(bat_weight_g)~log10(bat_forearm_mm), data=Rou.dat.adultM,nperm= 99)

#save function y=mx+b

mPterF = modPterF$regression.results[3,3]
bPterF = modPterF$regression.results[3,2]
mPterM = modPterM$regression.results[3,3]
bPterM = modPterM$regression.results[3,2]


mEidF = modEidF$regression.results[3,3]
bEidF = modEidF$regression.results[3,2]
mEidM = modEidM$regression.results[3,3]
bEidM = modEidM$regression.results[3,2]

mRouF = modRouF$regression.results[3,3]
bRouF = modRouF$regression.results[3,2]
mRouM = modRouM$regression.results[3,3]
bRouM = modRouM$regression.results[3,2]

#and do the same for confidence intervals too:

#lci
mPterF_lci = modPterF$confidence.intervals[3,4]
bPterF_lci = modPterF$confidence.intervals[3,2]
mPterM_lci = modPterM$confidence.intervals[3,4]
bPterM_lci = modPterM$confidence.intervals[3,2]


mEidF_lci = modEidF$confidence.intervals[3,4]
bEidF_lci = modEidF$confidence.intervals[3,2]
mEidM_lci = modEidM$confidence.intervals[3,4]
bEidM_lci = modEidM$confidence.intervals[3,2]

mRouF_lci = modRouF$confidence.intervals[3,4]
bRouF_lci = modRouF$confidence.intervals[3,2]
mRouM_lci = modRouM$confidence.intervals[3,4]
bRouM_lci = modRouM$confidence.intervals[3,2]

#uci
mPterF_uci = modPterF$confidence.intervals[3,5]
bPterF_uci = modPterF$confidence.intervals[3,3]
mPterM_uci = modPterM$confidence.intervals[3,5]
bPterM_uci = modPterM$confidence.intervals[3,3]


mEidF_uci = modEidF$confidence.intervals[3,5]
bEidF_uci = modEidF$confidence.intervals[3,3]
mEidM_uci = modEidM$confidence.intervals[3,5]
bEidM_uci = modEidM$confidence.intervals[3,3]

mRouF_uci = modRouF$confidence.intervals[3,5]
bRouF_uci = modRouF$confidence.intervals[3,3]
mRouM_uci = modRouM$confidence.intervals[3,5]
bRouM_uci = modRouM$confidence.intervals[3,3]

# predict function y = mx+b
#but rewrite for log
regress.func <- function(x,m,b){
  log10y = m*log10(x)+b
  y=10^(log10y)
  return(y)
  
}


#and now predict across your dataset

Pter.dat.adultF$prediction <- regress.func(x= Pter.dat.adultF$bat_forearm_mm, m=mPterF, b=bPterF)
Pter.dat.adultM$prediction <- regress.func(x= Pter.dat.adultM$bat_forearm_mm, m=mPterM, b=bPterM)

Eid.dat.adultF$prediction <- regress.func(x= Eid.dat.adultF$bat_forearm_mm, m=mEidF, b=bEidF)
Eid.dat.adultM$prediction <- regress.func(x= Eid.dat.adultM$bat_forearm_mm, m=mEidM, b=bEidM)

Rou.dat.adultF$prediction <- regress.func(x= Rou.dat.adultF$bat_forearm_mm, m=mRouF, b=bRouF)
Rou.dat.adultM$prediction <- regress.func(x= Rou.dat.adultM$bat_forearm_mm, m=mRouM, b=bRouM)


#and the lower and upper CIs 
Pter.dat.adultF$prediction_lci <- regress.func(x= Pter.dat.adultF$bat_forearm_mm, m=mPterF_lci, b=bPterF_lci)
Pter.dat.adultM$prediction_lci <- regress.func(x= Pter.dat.adultM$bat_forearm_mm, m=mPterM_lci, b=bPterM_lci)

Eid.dat.adultF$prediction_lci <- regress.func(x= Eid.dat.adultF$bat_forearm_mm, m=mEidF_lci, b=bEidF_lci)
Eid.dat.adultM$prediction_lci <- regress.func(x= Eid.dat.adultM$bat_forearm_mm, m=mEidM_lci, b=bEidM_lci)

Rou.dat.adultF$prediction_lci <- regress.func(x= Rou.dat.adultF$bat_forearm_mm, m=mRouF_lci, b=bRouF_lci)
Rou.dat.adultM$prediction_lci <- regress.func(x= Rou.dat.adultM$bat_forearm_mm, m=mRouM_lci, b=bRouM_lci)


Pter.dat.adultF$prediction_uci <- regress.func(x= Pter.dat.adultF$bat_forearm_mm, m=mPterF_uci, b=bPterF_uci)
Pter.dat.adultM$prediction_uci <- regress.func(x= Pter.dat.adultM$bat_forearm_mm, m=mPterM_uci, b=bPterM_uci)

Eid.dat.adultF$prediction_uci <- regress.func(x= Eid.dat.adultF$bat_forearm_mm, m=mEidF_uci, b=bEidF_uci)
Eid.dat.adultM$prediction_uci <- regress.func(x= Eid.dat.adultM$bat_forearm_mm, m=mEidM_uci, b=bEidM_uci)

Rou.dat.adultF$prediction_uci <- regress.func(x= Rou.dat.adultF$bat_forearm_mm, m=mRouF_uci, b=bRouF_uci)
Rou.dat.adultM$prediction_uci <- regress.func(x= Rou.dat.adultM$bat_forearm_mm, m=mRouM_uci, b=bRouM_uci)


#join back
AllsppAdult_Mora <- rbind(Pter.dat.adultF, Eid.dat.adultF, Rou.dat.adultF,
                          Pter.dat.adultM, Eid.dat.adultM, Rou.dat.adultM)
head(AllsppAdult_Mora )

p3b <- ggplot(data = AllsppAdult_Mora) + 
  geom_point(aes(x=bat_forearm_mm,y=bat_weight_g, color=bat_age_class)) +
  geom_line(aes(x=bat_forearm_mm, y= prediction), color="black", size=1) +
  geom_ribbon(aes(x=bat_forearm_mm, ymin= prediction_lci, ymax=prediction_uci), fill="black", alpha=.3) +
  facet_grid(bat_species~bat_sex) +
  coord_cartesian(xlim=c(50,200), ylim=c(0,1000))

print(p3b) #good!

# Now can get residuals for all bat spp by sites
AllsppAdult_Mora$resid <- AllsppAdult_Mora$bat_weight_g - AllsppAdult_Mora$prediction



#Now repeat with individual GAMs by species to model the residuals

unique(AllsppAdult_Mora$bat_species)
unique(AllsppAdult_Mora$bat_age_class)
Pter.dat.adult = subset(AllsppAdult_Mora, bat_species=="Pteropus rufus")
Eid.dat.adult = subset(AllsppAdult_Mora, bat_species=="Eidolon dupreanum")
Rou.dat.adult = subset(AllsppAdult_Mora, bat_species=="Rousettus madagascariensis")

sum(is.na(Pter.dat.adult$bat_sex))
sum(is.na(Pter.dat.adult$resid)) 
Pter.dat.adult= Pter.dat.adult[!is.na(Pter.dat.adult$resid),]

sum(is.na(Eid.dat.adult$bat_sex))
sum(is.na(Eid.dat.adult$resid)) 
Eid.dat.adult= Eid.dat.adult[!is.na(Eid.dat.adult$resid),]


sum(is.na(Rou.dat.adult$bat_sex))
sum(is.na(Rou.dat.adult$resid))
Rou.dat.adult= Rou.dat.adult[!is.na(Rou.dat.adult$resid),]


library(mgcv)
#k = 7 as suggested by package author Simon Wood
gamPterplot <- gam(resid~ 
                     s(day, by=as.numeric(bat_sex=="male"),  k=7, bs = "cc") +
                     s(day, by=as.numeric(bat_sex=="female"), k=7, bs = "cc"), data = Pter.dat.adult)

#save output
sink("gam_Pruf.txt")
summary(gamPterplot) #males significant
sink()

gamEidplot <- gam(resid~ 
                    s(day, by=as.numeric(bat_sex=="male"), k=7, bs = "cc") +
                    s(day, by=as.numeric(bat_sex=="female"), k=7, bs = "cc"),
                  data = Eid.dat.adult)


sink("gam_Edup.txt")
summary(gamEidplot) #males significant
sink()


gamRouplot <- gam(resid~ 
                    s(day, by=as.numeric(bat_sex=="male"),    k=7, bs = "cc") +
                    s(day, by=as.numeric(bat_sex=="female"),   k=7, bs = "cc"),
                  data = Rou.dat.adult)
sink("gam_Rmad.txt")
summary(gamRouplot) #female slightly sig but mostly neither sex is sig.
sink()

#now add the predictions to each dataframe

Pter.dat.adult$prediction_resid_plot = predict.gam(gamPterplot, type="response", se.fit=T)$fit
Pter.dat.adult$prediction_resid_plot_lci = predict.gam(gamPterplot, type="response", se.fit=T)$fit -1.96*(predict.gam(gamPterplot, type="response", se.fit=T)$se)
Pter.dat.adult$prediction_resid_plot_uci = predict.gam(gamPterplot, type="response", se.fit=T)$fit +1.96*(predict.gam(gamPterplot, type="response", se.fit=T)$se)

Eid.dat.adult$prediction_resid_plot = predict.gam(gamEidplot, type="response", se.fit=T)$fit
Eid.dat.adult$prediction_resid_plot_lci = predict.gam(gamEidplot, type="response", se.fit=T)$fit -1.96*(predict.gam(gamEidplot, type="response", se.fit=T)$se)
Eid.dat.adult$prediction_resid_plot_uci = predict.gam(gamEidplot, type="response", se.fit=T)$fit +1.96*(predict.gam(gamEidplot, type="response", se.fit=T)$se)


Rou.dat.adult$prediction_resid_plot = predict.gam(gamRouplot, type="response", se.fit=T)$fit
Rou.dat.adult$prediction_resid_plot_lci = predict.gam(gamRouplot, type="response", se.fit=T)$fit -1.96*(predict.gam(gamRouplot, type="response", se.fit=T)$se)
Rou.dat.adult$prediction_resid_plot_uci = predict.gam(gamRouplot, type="response", se.fit=T)$fit +1.96*(predict.gam(gamRouplot, type="response", se.fit=T)$se)



new.All.dat <- rbind(Pter.dat.adult, Eid.dat.adult, Rou.dat.adult)
unique(new.All.dat$roost_site)



#and plot

ColM<- c("Pteropus rufus"="blue", "Eidolon dupreanum"="light green","Rousettus madagascariensis"="purple")

class(new.All.dat$day)

#visualize all together
p4 <- ggplot(data = new.All.dat) + 
  geom_point(aes(x= as.numeric(day), y= resid, color=bat_species), alpha=.3)+ scale_color_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0)) +
  xlab ("Days of year")+ 
  ylab("Mass residuals")+
  geom_line(aes(x=day, y= prediction_resid_plot), col="red", size=1)+
  geom_ribbon(aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci),
              fill="red",size=1, alpha=.3 ) +
  facet_grid(bat_species~bat_sex, scales = "free_y")+theme_bw() + theme(element_blank()) 
  

#now, save just the males as the main plot
new.All.dat$xlab = paste0(new.All.dat$bat_sex, " bats")

p4_main <- ggplot(data = subset(new.All.dat, bat_sex=="male")) + 
  geom_point(aes(x= as.numeric(day), y= resid, color=bat_species), alpha=.3, show.legend = F)+ 
  scale_color_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0)) +
  xlab ("Days of year")+ 
  ylab("Mass residuals")+
  geom_line(aes(x=day, y= prediction_resid_plot), col="red", size=1)+
  geom_ribbon(aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci),
              fill="red",size=1, alpha=.3 ) +
  facet_grid(bat_species~xlab, scales = "free_y")+theme_bw() + 
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(0,91,182, 274, 365), 
                     labels = c("Jan-1", "Apr-1", "Jul-1", "Oct-1", "Dec-31"))

p4_main


ggsave(file = paste0(homewd, "final-figures/Fig4_male_seasonal_mass_residuals.png"),
       plot = p4_main,
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)


#and the supplementary figure with the females

p4_supp <- ggplot(data = subset(new.All.dat, bat_sex=="female")) + 
  geom_point(aes(x= as.numeric(day), y= resid, color=bat_species), alpha=.3, show.legend = F)+ 
  scale_color_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0)) +
  xlab ("Days of year")+ 
  ylab("Mass residuals")+
  geom_line(aes(x=day, y= prediction_resid_plot), col="red", size=1)+
  geom_ribbon(aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci),
              fill="red",size=1, alpha=.3 ) +
  facet_grid(bat_species~xlab, scales = "free_y")+theme_bw() + 
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(0,91,182, 274, 365), 
                     labels = c("Jan-1", "Apr-1", "Jul-1", "Oct-1", "Dec-31"))

p4_supp


ggsave(file = paste0(homewd, "final-figures/FigS1_female_seasonal_mass_residuals.png"),
       plot = p4_supp,
       units="mm",  
       width=90, 
       height=60, 
       scale=3, 
       dpi=300)
