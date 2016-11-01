# Script for figuring out the medoids in all zones so they can be used as distance references
# 6/1/2016 - Edited script for determining medoids of each estuary so they can be used as distance references
#
setwd("~/Desktop/Github Repo/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData")
library(haven) #for loading SAS data
library(cluster) #for finding medoid
library(geosphere) # for calculating great circle distances between each medoid
library(ggplot2) #for plotting
library(nlstools) #for fitting nls models

################################
# SELECT DATA
# same way as in Delta Method for Producing Nominal Indices.R
########################################
ap = subset(read_sas("ap_yoy_bay_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) #gear 20
apr = subset(read_sas("ap_yoy_riv_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) #gear 23

ck = subset(read_sas("ck_yoy_bay_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11)) #gear 20
ckr = subset(read_sas("ck_yoy_riv_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11) & Zone == "F") #gear 22, 23

ch = subset(read_sas("ch_yoy_bay_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(19,20)) #for some reason gear hadnt been selected so I needed to subset here
chr = subset(read_sas("ch_yoy_riv_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(22,23)) #again same problem as above

tb = subset(read_sas("tb_yoy_bay_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(19,20), select=c(year, bay, Gear, number, bio_reference, Longitude, Latitude))
tbr = subset(read_sas("tb_yoy_riv_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(22,23), select=c(year, bay, Gear,number, bio_reference, Longitude, Latitude))
nametb <- names(tb)
nametbr <- names(tbr)
# columns are misordered so they wont join. I selected columns specifically. 


ir = subset(read_sas("ir_yoy_riv_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11) & Gear %in% c(22,23))
irb = subset(read_sas("ir_yoy_bay_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11) & Gear %in% c(19,20))

jx = subset(read_sas("jx_yoy_riv_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11))
#no bay data for jax so no jxb

#combine the data sets because some of the rivers were missing data and some gear/zones were partitioning weird
ck_all = rbind(ck,ckr)
ch_all = rbind(ch, chr)
ap_all = rbind(ap,apr)
ir_all <- rbind(ir, irb)
tb_all = rbind(tb, tbr)
jx_all <- jx

########################################
# MEDOID CALCULATION
#########################################


### Figuring out whether to use the mediod or the mean. The mediod is an actual point in the data set whereas the mean is a number not included. 
AP_LL <- subset(ap_all, select=c("Longitude", "Latitude")) 
  APMed <- pam(AP_LL,1)$medoids
    #Lat <- APmed[,2]
    #Long <- TBAmed[,1]
  #plot(TBA_LL$Longitude ~ TBA_LL$Latitude)
  #  points(Lat, Long, pch=16, col="red")

#   meanLL <- data.frame(colMeans(LatLong))
#   test <- cbind(meanLL[1,], meanLL[2,])

#   plot(TB_BAY_AUn$Longitude ~ TB_BAY_AUn$Latitude)
#   cords <- xy.coords(-82.63272,27.9578)
#   points(test, pch=16, col="red")
CH_LL <- subset(ch_all, select=c("Longitude", "Latitude")) 
  CHMed <- pam(CH_LL,1)$medoids

CK_LL <- subset(ck_all, select=c("Longitude", "Latitude")) 
  CKMed <- pam(CK_LL,1)$medoids

IR_LL <-subset(ir_all, select=c("Longitude", "Latitude"))
  IRMed <- pam(IR_LL,1)$medoids

TB_LL <-na.omit(subset(tb_all, select=c("Longitude", "Latitude")))
  TBMed <- pam(TB_LL,1)$medoids

JX_LL <-subset(jx_all, select=c("Longitude", "Latitude"))
  JXMed <- pam(JX_LL,1)$medoids

########################################################
## CALCULATING GREAT CIRCLE DISTANCES
#########################################################

AP_compars = rbind(distGeo(APMed, CHMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, CKMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, IRMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, TBMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, JXMed, a=6378137, f=1/298.257223563))

CH_compars = rbind(distGeo(CHMed, CKMed, a=6378137, f=1/298.257223563),
                   distGeo(CHMed, IRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHMed, TBMed, a=6378137, f=1/298.257223563),
                   distGeo(CHMed, JXMed, a=6378137, f=1/298.257223563))


CK_compars = rbind(distGeo(CKMed, IRMed, a=6378137, f=1/298.257223563),
                   distGeo(CKMed, TBMed, a=6378137, f=1/298.257223563),
                   distGeo(CKMed, JXMed, a=6378137, f=1/298.257223563))


IR_compars = rbind(distGeo(IRMed, TBMed, a=6378137, f=1/298.257223563),
                    distGeo(IRMed, JXMed, a=6378137, f=1/298.257223563))

TB_compars = distGeo(TBMed, JXMed, a=6378137, f=1/298.257223563)



#join distances together. make sure they are in the correct order so that they can bind together with the csv imported below. 
distances <- rbind(AP_compars, CH_compars, CK_compars, IR_compars, TB_compars)
rownames(distances) <- c("AP_CH", "AP_CK", "AP_IR", "AP_TB", "AP_JX", 
                         "CH_CK", "CH_IR", "CH_TB", "CH_JX",
                         "CK_IR", "CK_TB", "CK_JX",
                         "IR_TB", "IR_JX",
                         "TB_JX")

#bring in the rho_P_vector and combinind with the distances
rho_P_vector <- read.csv('~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes/rho_P_vector.csv')

#combine rho and distances vectors and then turn distance into kilometers, name x and y for plotting convention below
t<-cbind(rho_P_vector, distances) %>% mutate(distancesKM=distances/1000) %>% arrange(distancesKM) %>% rename(y=rho, x=distancesKM)
write.csv(t,'~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes/rho_vs_distance.csv' )

############################################
#ESTIMATE SPATIAL DECAY WITH NLS MODEL
#1. Fix p0= 1, just exclude it from the equation
#2. Estimate p0 
#Pyper et al. 2001
#Peterman et al. 1998
#P(d)= p0e(-d/v)- constrain p0 to 1
#P(d)=p0e(-d/v) - estimate P0
#v (e-folding scale) where e-folding scale tells distance 
#############################################

#must find acceptable starting values (can play around with these in excel-rho_vs_distance_param_start_estimation.csv)
p0 = 1
v = 125
#p0 constrained to 1, weighted by the total sample number which is shared between each comparison
m1 = nls(y ~exp(-(x/v)), start=list(v=v), data=t, weights=(N))
summary(m1)
#estimated v 173.90(SE 38.23)

# p0 estimated
p0=0.9
v=125
m2 = nls(y ~p0*exp(-(x/v)), start=list(p0=p0,v=v), data=t, weights=(N))
summary(m2)
yfitted <- predict(m2)
boot <- nlsBoot(m2, niter=2000) #bootstrapping 
bootCI <- boot$bootCI #gives confidence intervals and median values for p0 and v (e-folding scale) where e-folding scale tells distance 

#evalute fit with anova and F test. if they are significantly different than F test will indicated significant p value
anova(m1, m2)
# Interpreting F table: https://sakai.duke.edu/access/content/group/25e08a3d-9fc4-41b0-a7e9-815732c1c4ba/New%20folder/Stat%20Topic%20Files/Non-Linear%20Regression/FTestTutorial.pdf
# If p-value is significant, then it indicates the more complex model fits the data significantly better than the simpler
# In our case the p value is not significant so we don't need to estimate p0- we can constrain it to 1

#################################
#PLOT CORRELATION BY DISTANCE
#################################
#Plot correlation by distance and add the fitted curve to the plot(here p0 is estimated. see below for explanation)
#http://stackoverflow.com/questions/25030653/fitting-with-ggplot2-geom-smooth-and-nls
# NOTE: within geom_smooth  it does not recognize variable names; they must be named x and y; weight must be in an aesthetic in ggplot

rhobydis <- ggplot(data=t, aes(x=x, y=y))+geom_point()+ 
  geom_smooth(method="nls",formula=y ~exp(-(x/v)), method.args=list(start=c(v=150)), aes(weight=N), se=FALSE, color="black", size=0.5)+                                           
  ylab("Correlation") +
  xlab("Distance (km)")+ 
  geom_vline(xintercept = 174, linetype="dotted")+
  scale_x_continuous(limits=c(75,500))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),  
        panel.background=element_rect(fill='white', colour='black'),                                                    
        axis.text.x=element_text(colour="black"), #changing  colour of x axis
        axis.text.y=element_text(colour="black"), #changing colour of y axis
        plot.title=element_text(size=14), # changing size of plot title)+
        legend.text=element_text(size=10))



#check to make sure that ggplot is producing correct plot
t_fitted <- cbind(t, yfitted)
plot(y ~x, data=t)

