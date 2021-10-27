rm(list = ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lmodel2)
library(lmer)
library(lmerTest)

# Set wd to data on this computer. Also ID homewd, assuming that 
# Mada-GIS is cloned to the same series of sub-folders
homewd = "/Users/caraebrook/Documents/R/R_repositories/Mada-Bat-Morphology/" #should be wherever "Mada-Bat-Morphology" is stored on your home computer
setwd(paste0(homewd, "/", "Fig3/"))


#load the catching data
dat <- read.csv(file = paste0(homewd, "morph_paper_dat_7_29_2021.csv"), header=T, stringsAsFactors = F)
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


#sub-select to only the Adults (no "J" and no "NA")
unique(data1$bat_age_class)
AllsppAdults1 <- filter(select(data1, sampleid,bat_species, roost_site, collection_date,month,day, bat_sex,                              
                               bat_age_class, bat_weight_g, bat_forearm_mm, bat_tibia_mm, ear_length_mm, body_length_cm), 
                        bat_age_class %in% c("A","NL","L", "P")) 

AllsppAdults1$bat_weight_g= as.numeric(AllsppAdults1$bat_weight_g)

AllsppAdults1$bat_forearm_mm= as.numeric(AllsppAdults1$bat_forearm_mm)

                                     
AllsppAdults1$bat_species <- factor(AllsppAdults1$bat_species)

AllsppAdults1 <- subset(AllsppAdults1, !is.na(bat_sex) &!is.na(bat_weight_g) & !is.na(bat_forearm_mm))
modAllF <- lm(log10(bat_weight_g)~log10(bat_forearm_mm) +(bat_species), data=subset(AllsppAdults1, bat_sex=="female"))
modAllM <- lm(log10(bat_weight_g)~log10(bat_forearm_mm)+(bat_species), data=subset(AllsppAdults1, bat_sex=="male"))
modAllFmixed <- lmer(log10(bat_weight_g)~log10(bat_forearm_mm) +(1|bat_species), data=subset(AllsppAdults1, bat_sex=="female"))
modAllMmixed <- lmer(log10(bat_weight_g)~log10(bat_forearm_mm)+(1|bat_species), data=subset(AllsppAdults1, bat_sex=="male"))

AIC(modAllF, modAllFmixed)
AIC(modAllM, modAllMmixed)
summary(modAllFmixed)
AllsppAdults1$prediction <- NA
AllsppAdults1$prediction[AllsppAdults1$bat_sex=="female"] <- 10^predict(modAllF)
AllsppAdults1$prediction[AllsppAdults1$bat_sex=="male"] <- 10^predict(modAllM)

#plot for reality check
p1 <- ggplot(data=AllsppAdults1) +
      geom_line(aes(x=bat_forearm_mm, y=prediction, color=bat_species)) +
      geom_point(aes(x=bat_forearm_mm, y=bat_weight_g, color=bat_species)) +
      facet_grid(~bat_sex) +theme_bw()
p1

#sub-select only the Moramanga sites
unique(AllsppAdults1$roost_site)
AllsppAdult_Mora <- filter(select(AllsppAdults1, sampleid,bat_species, roost_site,day, collection_date,month, bat_sex, prediction,
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


#and predict

p3b <- ggplot(data = AllsppAdult_Mora) + 
  geom_point(aes(x=bat_forearm_mm,y=bat_weight_g, color=bat_age_class)) +
  geom_line(aes(x=bat_forearm_mm, y= prediction), color="black", size=1) +
  #geom_ribbon(aes(x=bat_forearm_mm, ymin= prediction_lci, ymax=prediction_uci), fill="black", alpha=.3) +
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

gamPterF <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Pter.dat.adult, bat_sex=="female"))
gamPterM <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Pter.dat.adult, bat_sex=="male"))
summary(gamPterF)
summary(gamPterM)


#save output
sink("gam_Pruf_F.txt")
summary(gamPterF) #sig
sink()

#save output
sink("gam_Pruf_M.txt")
summary(gamPterM) #sig
sink()

#gamEidplot <- gam(resid~ 
#                    s(day, by=as.numeric(bat_sex=="male"), k=7, bs = "cc") +
#                    s(day, by=as.numeric(bat_sex=="female"), k=7, bs = "cc"),
#                  data = Eid.dat.adult)


gamEidF <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Eid.dat.adult, bat_sex=="female"))
gamEidM <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Eid.dat.adult, bat_sex=="male"))
summary(gamEidF)
summary(gamEidM)


sink("gam_Edup_F.txt")
summary(gamEidF) #sig
sink()


sink("gam_Edup_M.txt")
summary(gamEidM) #sig
sink()


#gamRouplot <- gam(resid~ 
#                    s(day, by=as.numeric(bat_sex=="male"),    k=7, bs = "cc") +
#                    s(day, by=as.numeric(bat_sex=="female"),   k=7, bs = "cc"),
#                  data = Rou.dat.adult)


gamRouF <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Rou.dat.adult, bat_sex=="female"))
gamRouM <- gam(resid~ s(day, k=7, bs = "cc"), data = subset(Rou.dat.adult, bat_sex=="male"))
summary(gamRouF)
summary(gamRouM)


sink("gam_Rmad_F.txt")
summary(gamRouF) #weak sig
sink()


sink("gam_Rmad_M.txt")
summary(gamRouM) #only females sig
sink()

#now add the predictions to each dataframe
Pter.dat.adult$prediction_resid_plot <- NA
Pter.dat.adult$prediction_resid_plot_lci <- NA
Pter.dat.adult$prediction_resid_plot_uci <- NA
Pter.dat.adult$prediction_resid_plot[Pter.dat.adult$bat_sex=="female"] = predict.gam(gamPterF, type="response", se.fit=T)$fit
Pter.dat.adult$prediction_resid_plot_lci[Pter.dat.adult$bat_sex=="female"] = predict.gam(gamPterF, type="response", se.fit=T)$fit -1.96*(predict.gam(gamPterF, type="response", se.fit=T)$se)
Pter.dat.adult$prediction_resid_plot_uci[Pter.dat.adult$bat_sex=="female"] = predict.gam(gamPterF, type="response", se.fit=T)$fit +1.96*(predict.gam(gamPterF, type="response", se.fit=T)$se)
Pter.dat.adult$prediction_resid_plot[Pter.dat.adult$bat_sex=="male"] = predict.gam(gamPterM, type="response", se.fit=T)$fit
Pter.dat.adult$prediction_resid_plot_lci[Pter.dat.adult$bat_sex=="male"] = predict.gam(gamPterM, type="response", se.fit=T)$fit -1.96*(predict.gam(gamPterM, type="response", se.fit=T)$se)
Pter.dat.adult$prediction_resid_plot_uci[Pter.dat.adult$bat_sex=="male"] = predict.gam(gamPterM, type="response", se.fit=T)$fit +1.96*(predict.gam(gamPterM, type="response", se.fit=T)$se)


Eid.dat.adult$prediction_resid_plot <- NA
Eid.dat.adult$prediction_resid_plot_lci <- NA
Eid.dat.adult$prediction_resid_plot_uci <- NA
Eid.dat.adult$prediction_resid_plot[Eid.dat.adult$bat_sex=="female"] = predict.gam(gamEidF, type="response", se.fit=T)$fit
Eid.dat.adult$prediction_resid_plot_lci[Eid.dat.adult$bat_sex=="female"] = predict.gam(gamEidF, type="response", se.fit=T)$fit -1.96*(predict.gam(gamEidF, type="response", se.fit=T)$se)
Eid.dat.adult$prediction_resid_plot_uci[Eid.dat.adult$bat_sex=="female"] = predict.gam(gamEidF, type="response", se.fit=T)$fit +1.96*(predict.gam(gamEidF, type="response", se.fit=T)$se)
Eid.dat.adult$prediction_resid_plot[Eid.dat.adult$bat_sex=="male"] = predict.gam(gamEidM, type="response", se.fit=T)$fit
Eid.dat.adult$prediction_resid_plot_lci[Eid.dat.adult$bat_sex=="male"] = predict.gam(gamEidM, type="response", se.fit=T)$fit -1.96*(predict.gam(gamEidM, type="response", se.fit=T)$se)
Eid.dat.adult$prediction_resid_plot_uci[Eid.dat.adult$bat_sex=="male"] = predict.gam(gamEidM, type="response", se.fit=T)$fit +1.96*(predict.gam(gamEidM, type="response", se.fit=T)$se)


Rou.dat.adult$prediction_resid_plot <- NA
Rou.dat.adult$prediction_resid_plot_lci <- NA
Rou.dat.adult$prediction_resid_plot_uci <- NA
Rou.dat.adult$prediction_resid_plot[Rou.dat.adult$bat_sex=="female"] = predict.gam(gamRouF, type="response", se.fit=T)$fit
Rou.dat.adult$prediction_resid_plot_lci[Rou.dat.adult$bat_sex=="female"] = predict.gam(gamRouF, type="response", se.fit=T)$fit -1.96*(predict.gam(gamRouF, type="response", se.fit=T)$se)
Rou.dat.adult$prediction_resid_plot_uci[Rou.dat.adult$bat_sex=="female"] = predict.gam(gamRouF, type="response", se.fit=T)$fit +1.96*(predict.gam(gamRouF, type="response", se.fit=T)$se)
Rou.dat.adult$prediction_resid_plot[Rou.dat.adult$bat_sex=="male"] = predict.gam(gamRouM, type="response", se.fit=T)$fit
Rou.dat.adult$prediction_resid_plot_lci[Rou.dat.adult$bat_sex=="male"] = predict.gam(gamRouM, type="response", se.fit=T)$fit -1.96*(predict.gam(gamRouM, type="response", se.fit=T)$se)
Rou.dat.adult$prediction_resid_plot_uci[Rou.dat.adult$bat_sex=="male"] = predict.gam(gamRouM, type="response", se.fit=T)$fit +1.96*(predict.gam(gamRouM, type="response", se.fit=T)$se)




new.All.dat <- rbind(Pter.dat.adult, Eid.dat.adult, Rou.dat.adult)
unique(new.All.dat$roost_site)



#and plot

ColM<- c("Pteropus rufus"="blue", "Eidolon dupreanum"="light green","Rousettus madagascariensis"="purple")

class(new.All.dat$day)

#visualize all together
p4 <- ggplot(data = new.All.dat) + 
  geom_rect(aes(xmin=111, xmax=304, ymin=-Inf, ymax=Inf),fill="#FEEEAA", alpha=0.5)+
  geom_rect(aes(xmin=0, xmax=111, ymin=-Inf, ymax=Inf),fill="gray90", alpha=0.5)+
  geom_rect(aes(xmin=304, xmax=365, ymin=-Inf, ymax=Inf),fill="gray90", alpha=0.5)+
  geom_point(aes(x= as.numeric(day), y= resid, color=bat_species), alpha=.3)+ scale_color_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0)) +
  xlab ("Days of year")+ 
  ylab("Mass residuals")+
  geom_line(aes(x=day, y= prediction_resid_plot), col="red", size=1)+
  geom_ribbon(aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci),
              fill="red",size=1, alpha=.3 ) +
  facet_grid(bat_species~bat_sex, scales = "free_y")+theme_bw() + theme(element_blank()) 
  

#now, save just the males as the main plot
#new.All.dat$xlab = paste0(new.All.dat$bat_sex, " bats")
new.All.dat$xlab = new.All.dat$bat_sex
new.All.dat$xlab[new.All.dat$xlab=="male"] <- "M"
new.All.dat$xlab[new.All.dat$xlab=="female"] <- "F"

#arrange plots by size:
new.All.dat$bat_species <- factor(new.All.dat$bat_species, levels = c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"))

#winter for the males
seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))

#observed gestation for the females
library(lubridate)
preg.dat <- cbind.data.frame(x = c(yday("2015-07-10"),yday("2019-09-29"),
                                   yday("2014-08-03"), yday("2019-11-16"),
                                   yday("2018-09-11"), yday("2014-12-12")), bat_species= rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"), each=2))
preg.dat$xlab = "F"

preg.dat$bat_species <- factor(preg.dat$bat_species, levels=c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"))

p4_main <- ggplot(data = new.All.dat) + 
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="cornflowerblue", alpha=0.3)+
  geom_ribbon(data = preg.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="hotpink3", alpha=0.3)+
  geom_point(aes(x= as.numeric(day), y= resid, color=bat_species), alpha=.3, show.legend = F)+ 
  scale_color_manual(values=ColM)+ 
  scale_fill_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Days of year")+ 
  ylab("Mass residuals")+
  geom_ribbon(aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci), fill="black",
              size=1, alpha=.3 ) +
  geom_line(aes(x=day, y= prediction_resid_plot, color=bat_species), size=1, show.legend = F)+
  facet_grid(bat_species~xlab, scales = "free_y")+theme_bw() + 
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(0,91,182, 274, 365), 
                     labels = c("Jan-1", "Apr-1", "Jul-1", "Oct-1", "Dec-31"))

p4_main


ggsave(file = paste0(homewd, "final-figures/Fig3new.png"),
       plot = p4_main,
       units="mm",  
       width=80, 
       height=60, 
       scale=3, 
       dpi=300)


#and for supplement, try GAM of just body mass with or without random effect of forearm

#or try just the mass GAM - with site 
AllsppAdults1$roost_site[AllsppAdults1$roost_site=="Lakato" |AllsppAdults1$roost_site=="Marovitsika" |AllsppAdults1$roost_site=="Ambakoana" |AllsppAdults1$roost_site=="AngavoBe" | AllsppAdults1$roost_site=="AngavoKely" | AllsppAdults1$roost_site=="Maromizaha" |AllsppAdults1$roost_site=="Mangarivotra" | AllsppAdults1$roost_site=="Mahialambo" |AllsppAdults1$roost_site=="Marotsipohy" |AllsppAdults1$roost_site=="Angavokely" ] <- "Moramanga"
AllsppAdults1$roost_site[AllsppAdults1$roost_site=="Ankarana_Canyon" |AllsppAdults1$roost_site=="Ankarana_Cathedral" |AllsppAdults1$roost_site=="Ankarana_Chauves_Souris" ] <- "Ankarana"
AllsppAdults1$roost_site <- as.factor(AllsppAdults1$roost_site)
MoramangAdults = subset(AllsppAdults1, roost_site=="Moramanga")
MoramangAdults <- arrange(MoramangAdults, day)
Pter.dat.adult = subset(MoramangAdults , bat_species=="Pteropus rufus")
Eid.dat.adult = subset(MoramangAdults , bat_species=="Eidolon dupreanum")
Rou.dat.adult = subset(MoramangAdults, bat_species=="Rousettus madagascariensis")
gamPterF <- gam(bat_weight_g~ s(day, k=7, bs = "cc") + 
                              s(bat_forearm_mm, bs="re"), data = subset(Pter.dat.adult, bat_sex=="female"))
gamPterM <- gam(bat_weight_g~ s(day, k=7, bs = "cc")+
                              s(bat_forearm_mm, bs="re"), data = subset(Pter.dat.adult, bat_sex=="male"))
summary(gamPterF) #day + forearm
summary(gamPterM) #day + forearm



gamEidF <- gam(bat_weight_g~ s(day, k=7, bs = "cc") + 
                 s(bat_forearm_mm, bs="re"), data = subset(Eid.dat.adult, bat_sex=="female"))
gamEidM <- gam(bat_weight_g~ s(day, k=7, bs = "cc") + 
                 s(bat_forearm_mm, bs="re"), data = subset(Eid.dat.adult, bat_sex=="male"))
summary(gamEidF)#day + forearm
summary(gamEidM) #day + forarm


gamRouF <- gam(bat_weight_g~ s(day, k=7, bs = "cc") + 
                 s(bat_forearm_mm, bs="re"), data = subset(Rou.dat.adult, bat_sex=="female"))
gamRouM <- gam(bat_weight_g~ s(day, k=7, bs = "cc") + 
                 s(bat_forearm_mm, bs="re"), data = subset(Rou.dat.adult, bat_sex=="male"))
summary(gamRouF)#day + forearm
summary(gamRouM) #forearm only


#and no forearm for plotting
gamPterF <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Pter.dat.adult, bat_sex=="female"))
gamPterM <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Pter.dat.adult, bat_sex=="male"))

gamEidF <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Eid.dat.adult, bat_sex=="female"))
gamEidM <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Eid.dat.adult, bat_sex=="male"))


gamRouF <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Rou.dat.adult, bat_sex=="female"))
gamRouM <- gam(bat_weight_g~ s(day, k=7, bs = "cc"), data = subset(Rou.dat.adult, bat_sex=="male"))


#and predict by day and species
predict.dat <- cbind.data.frame(bat_species=rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"), each = 365*2), bat_sex= rep(rep(c("female", "male"), each=365), 3), day = rep((1:365), 3*2))


#now add the predictions to each dataframe
predict.dat$prediction_resid_plot <- NA
predict.dat$prediction_resid_plot_lci <- NA
predict.dat$prediction_resid_plot_uci <- NA
predict.dat$prediction_resid_plot[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamPterF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamPterF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Pteropus rufus",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamPterM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Pteropus rufus"] = predict.gam(gamPterM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamPterM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Pteropus rufus",], type="response", se.fit=T)$se)


predict.dat$prediction_resid_plot[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamEidF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamEidF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Eidolon dupreanum",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamEidM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Eidolon dupreanum"] = predict.gam(gamEidM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamEidM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Eidolon dupreanum",], type="response", se.fit=T)$se)


predict.dat$prediction_resid_plot[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamRouF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="female"& predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamRouF, newdata = predict.dat[predict.dat$bat_sex=="female" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Rousettus madagascariensis",],  type="response", se.fit=T)$fit
predict.dat$prediction_resid_plot_lci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$fit -1.96*(predict.gam(gamRouM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$se)
predict.dat$prediction_resid_plot_uci[predict.dat$bat_sex=="male"& predict.dat$bat_species=="Rousettus madagascariensis"] = predict.gam(gamRouM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$fit +1.96*(predict.gam(gamRouM, newdata = predict.dat[predict.dat$bat_sex=="male" & predict.dat$bat_species=="Rousettus madagascariensis",], type="response", se.fit=T)$se)


predict.dat$xlab <- "F"
predict.dat$xlab[predict.dat$bat_sex=="male"] <- "M"

new.All.dat <- rbind(Pter.dat.adult, Eid.dat.adult, Rou.dat.adult)
seas.dat = cbind.data.frame(x=c(111, 304), xlab=rep("M", 2))

#observed gestation for the females
library(lubridate)
preg.dat <- cbind.data.frame(x = c(yday("2015-07-10"),yday("2019-09-29"),
                                   yday("2014-08-03"), yday("2019-11-16"),
                                   yday("2018-09-11"), yday("2014-12-12")), bat_species= rep(c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"), each=2))
preg.dat$xlab = "F"

preg.dat$bat_species <- factor(preg.dat$bat_species, levels=c("Pteropus rufus", "Eidolon dupreanum", "Rousettus madagascariensis"))

ColM<- c("Pteropus rufus"="blue", "Eidolon dupreanum"="light green","Rousettus madagascariensis"="purple")

predict.dat <- arrange(predict.dat, bat_species, bat_sex, day)
FigS2 <- ggplot(data = new.All.dat) + 
  geom_ribbon(data = seas.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="cornflowerblue", alpha=0.3)+
  geom_ribbon(data = preg.dat, aes(x=x, ymin=-Inf, ymax=Inf),fill="hotpink3", alpha=0.3)+
  geom_point(aes(x= as.numeric(day), y= bat_weight_g, color=bat_species), alpha=.3, show.legend = F)+ 
  scale_color_manual(values=ColM)+ 
  scale_fill_manual(values=ColM)+ 
  geom_hline(aes(yintercept=0), color="gray50") +
  xlab ("Days of year")+ 
  ylab("Mass (g)")+
  geom_ribbon(data = predict.dat, aes(x= day, ymin=prediction_resid_plot_lci, ymax=prediction_resid_plot_uci, 
                                      group=bat_species), fill="black",alpha=.3 ) +
  geom_line(data = predict.dat, aes(x=day, y= prediction_resid_plot, color=bat_species), size=1, show.legend = F)+
  facet_grid(bat_species~xlab, scales = "free_y")+theme_bw() + 
  theme(strip.background= element_rect(fill="white"), 
        strip.text.y = element_text(face="italic"),
        panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks=c(0,91,182, 274, 365), 
                     labels = c("Jan-1", "Apr-1", "Jul-1", "Oct-1", "Dec-31"))

FigS2


ggsave(file = paste0(homewd, "final-figures/FigS2.png"),
       plot = p4_main,
       units="mm",  
       width=80, 
       height=60, 
       scale=3, 
       dpi=300)