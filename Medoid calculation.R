# Script for figuring out the medoids in all zones so they can be used as distance references
# 6/1/2016 - Edited script for determining medoids of each estuary so they can be used as distance references
setwd("~/Desktop/Github Repo/Seatrout/Data/FIM SAS Data")
library(haven) #for loading SAS data
library(cluster) #for finding medoid
library(geosphere) # for calculating great circle distances between each medoid
library(ggplot2)

APBay <- read_sas("ap_yoy_bay_cn_c.sas7bdat")
CHBay <- read_sas ("ch_yoy_bay_cn_c.sas7bdat")
CHRiv <- read_sas ("ch_yoy_riv_cn_c.sas7bdat")
CKBay <- read_sas ("ck_yoy_bay_cn_c.sas7bdat")
IRRiv <- read_sas ("ir_yoy_riv_cn_c.sas7bdat")
JXRiv <- read_sas ("jx_yoy_riv_cn_c.sas7bdat")
TBBay <- read_sas ("tb_yoy_bay_cn_c.sas7bdat")
TBRiv <- read_sas ("tb_yoy_riv_cn_c.sas7bdat")




### Figuring out whether to use the mediod or the mean. The mediod is an actual point in the data set whereas the mean is a number not included. 
APB_LL <- subset(APBay, select=c("Longitude", "Latitude")) 
  APMed <- pam(APB_LL,1)$medoids
    #Lat <- APmed[,2]
    #Long <- TBAmed[,1]
  #plot(TBA_LL$Longitude ~ TBA_LL$Latitude)
  #  points(Lat, Long, pch=16, col="red")

#   meanLL <- data.frame(colMeans(LatLong))
#   test <- cbind(meanLL[1,], meanLL[2,])

#   plot(TB_BAY_AUn$Longitude ~ TB_BAY_AUn$Latitude)
#   cords <- xy.coords(-82.63272,27.9578)
#   points(test, pch=16, col="red")
CHB_LL <- subset(CHBay, select=c("Longitude", "Latitude")) 
  CHBMed <- pam(CHB_LL,1)$medoids

CHR_LL <- subset(CHRiv, select=c("Longitude", "Latitude")) 
  CHRMed <- pam(CHR_LL,1)$medoids

CKB_LL <- subset(CKBay, select=c("Longitude", "Latitude")) 
  CKBMed <- pam(CKB_LL,1)$medoids

IRR_LL <-subset(IRRiv, select=c("Longitude", "Latitude"))
  IRRMed <- pam(IRR_LL,1)$medoids

TBB_LL <-na.omit(subset(TBBay, select=c("Longitude", "Latitude")))
  TBBMed <- pam(TBB_LL,1)$medoids

TBR_LL <-na.omit(subset(TBRiv, select=c("Longitude", "Latitude")))
  TBRMed <- pam(TBR_LL,1)$medoids

JXR_LL <-subset(JXRiv, select=c("Longitude", "Latitude"))
  JXRMed <- pam(JXR_LL,1)$medoids

## Calculating great-circle distances between medoids #
AP_compars = rbind(distGeo(APMed, CHBMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, CHRMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, CKBMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, IRRMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, TBBMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, TBRMed, a=6378137, f=1/298.257223563),
                        distGeo(APMed, JXRMed, a=6378137, f=1/298.257223563))

CHB_compars = rbind(distGeo(CHBMed, CHRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHBMed, CKBMed, a=6378137, f=1/298.257223563),
                   distGeo(CHBMed, IRRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHBMed, TBBMed, a=6378137, f=1/298.257223563),
                   distGeo(CHBMed, TBRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHBMed, JXRMed, a=6378137, f=1/298.257223563))

CHR_compars = rbind(distGeo(CHRMed, CKBMed, a=6378137, f=1/298.257223563),
                   distGeo(CHRMed, IRRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHRMed, TBBMed, a=6378137, f=1/298.257223563),
                   distGeo(CHRMed, TBRMed, a=6378137, f=1/298.257223563),
                   distGeo(CHRMed, JXRMed, a=6378137, f=1/298.257223563))

CK_compars = rbind(distGeo(CKBMed, IRRMed, a=6378137, f=1/298.257223563),
                    distGeo(CKBMed, TBBMed, a=6378137, f=1/298.257223563),
                    distGeo(CKBMed, TBRMed, a=6378137, f=1/298.257223563),
                    distGeo(CKBMed, JXRMed, a=6378137, f=1/298.257223563))

IR_compars = rbind(distGeo(IRRMed, TBBMed, a=6378137, f=1/298.257223563),
                    distGeo(IRRMed, TBRMed, a=6378137, f=1/298.257223563),
                    distGeo(IRRMed, JXRMed, a=6378137, f=1/298.257223563))

TB_compars = rbind(distGeo(TBBMed, TBRMed, a=6378137, f=1/298.257223563),
                   distGeo(TBBMed, JXRMed, a=6378137, f=1/298.257223563))

JX_compars = rbind(distGeo(JXRMed, TBRMed, a=6378137, f=1/298.257223563))
                  


#join distances together. make sure they are in the correct order so that they can bind together with the csv imported below. 
distances <- rbind(AP_compars, CHB_compars, CHR_compars, CK_compars, IR_compars, TB_compars, JX_compars)
library(dplyr)
rho_P_vector <- read.csv('~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes/rho_P_vector.csv')
t<-cbind(rho_P_vector, distances) %>% mutate(distancesKM=distances/1000) %>% arrange(distancesKM) %>% rename(y=rho, x=distancesKM)#combine rho and distances vectors and then turn distance into kilometers


############################
#Plot correlation by distance and add the fitted curve to the plot(here p0 is estimated. see below for explanation)
#http://stackoverflow.com/questions/25030653/fitting-with-ggplot2-geom-smooth-and-nls
# NOTE: within geom_smooth  it does not recognize variable names; they must be named x and y
rhobydis <- ggplot(data=t, aes(x=x, y=y))+geom_point()+ 
            geom_smooth(method="nls",formula=y ~p0* exp(-(x/v)), method.args=list(start=c(p0=1, v=5)), se=FALSE)+                                             
            theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),	
                  panel.background=element_rect(fill='white', colour='black'),                                                    
                  axis.text.x=element_text(colour="black"), #changing  colour of x axis
                  axis.text.y=element_text(colour="black"), #changing colour of y acis
                  plot.title=element_text(size=14), # changing size of plot title)+
                  legend.text=element_text(size=10))

###############################
#Estimate spatial decay 
#1. Fix p0= 1, just exclude it from the equation
#2. Estimate p0 
p0 = 1
v = 5

library(nlstools)
fit = nls(rho ~p0*exp(-(distancesKM/v)), start=list( p0=p0, v=v), data=t)
summary(fit)
yfitted <- predict(fit)










