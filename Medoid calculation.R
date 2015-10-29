# Script for figuring out the medoids in all zones so they can be used as distance references

setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")

TB_A <- read.csv("TB_A_coordinates.csv")
TB_B <- read.csv("TB_B_coordinates.csv")
TB_C <- read.csv("TB_C_coordinates.csv")
TB_D <- read.csv("TB_D_coordinates.csv")
TB_E <- read.csv("TB_E_coordinates.csv")
TB_K <- read.csv("TB_K_coordinates.csv")
TB_L <- read.csv("TB_L_coordinates.csv")
TB_M <- read.csv("TB_M_coordinates.csv")
TB_N <- read.csv("TB_N_coordinates.csv")

JX_A <- read.csv("JX_A_coordinates.csv")
JX_B <- read.csv("JX_B_coordinates.csv")
JX_C <- read.csv("JX_C_coordinates.csv")
JX_D <- read.csv("JX_D_coordinates.csv")
JX_E <- read.csv("JX_E_coordinates.csv")
JX_F <- read.csv("JX_F_coordinates.csv")

IR_A <- read.csv("IR_A_coordinates.csv")
IR_B <- read.csv("IR_B_coordinates.csv")
IR_C <- read.csv("IR_C_coordinates.csv")
IR_D <- read.csv("IR_D_coordinates.csv")
IR_E <- read.csv("IR_E_coordinates.csv")
IR_F <- read.csv("IR_F_coordinates.csv")
IR_H <- read.csv("IR_H_coordinates.csv")

CK_B <- read.csv("CK_B_coordinates.csv")
CK_C <- read.csv("CK_C_coordinates.csv")
CK_F <- read.csv("CK_F_coordinates.csv")

CH_A <- read.csv("CH_A_coordinates.csv")
CH_B <- read.csv("CH_B_coordinates.csv")
CH_C <- read.csv("CH_C_coordinates.csv")
CH_D <- read.csv("CH_D_coordinates.csv")
CH_M <- read.csv("CH_M_coordinates.csv")
CH_P <- read.csv("CH_P_coordinates.csv")

AP_A <- read.csv("AP_A_coordinates.csv")
AP_B <- read.csv("AP_B_coordinates.csv")
AP_C <- read.csv("AP_C_coordinates.csv")

### Figuring out whether to use the mediod or the mean. The mediod is an actual point in the data set whereas the mean is a number not included. 
TBA_LL <- subset(TB_A, select=c("Longitude", "Latitude")) 
  library(cluster)
  TBAmed <- pam(TBA_LL,1)$medoids
    Lat <- TBAmed[,2]
    Long <- TBAmed[,1]
  plot(TBA_LL$Longitude ~ TBA_LL$Latitude)
    points(Lat, Long, pch=16, col="red")

#   meanLL <- data.frame(colMeans(LatLong))
#   test <- cbind(meanLL[1,], meanLL[2,])

#   plot(TB_BAY_AUn$Longitude ~ TB_BAY_AUn$Latitude)
#   cords <- xy.coords(-82.63272,27.9578)
#   points(test, pch=16, col="red")
TBA_LL <-subset(TB_A, select=c("Longitude", "Latitude"))
  TBAmed <- pam(TBA_LL,1)$medoids
    write.csv(TBAmed, "TB_A_medoid.csv")
TBB_LL <-subset(TB_B, select=c("Longitude", "Latitude"))
  TBBmed <- pam(TBB_LL,1)$medoids
    write.csv(TBBmed, "TB_B_medoid.csv")
TBC_LL <-subset(TB_C, select=c("Longitude", "Latitude"))
  TBCmed <- pam(TBC_LL,1)$medoids
    write.csv(TBCmed, "TB_C_medoid.csv")
TBD_LL <-subset(TB_D, select=c("Longitude", "Latitude"))
  TBDmed <- pam(TBD_LL,1)$medoids
    write.csv(TBDmed, "TB_D_medoid.csv")
TBE_LL <-subset(TB_E, select=c("Longitude", "Latitude"))
  TBEmed <- pam(TBE_LL,1)$medoids
    write.csv(TBEmed, "TB_E_medoid.csv")
TBK_LL <-subset(TB_K, select=c("Longitude", "Latitude"))
  TBKmed <- pam(TBK_LL,1)$medoids
    write.csv(TBKmed, "TB_K_medoid.csv")
TBL_LL <-subset(TB_L, select=c("Longitude", "Latitude"))
  TBLmed <- pam(TBL_LL,1)$medoids
    write.csv(TBLmed, "TB_L_medoid.csv")
TBM_LL <-subset(TB_M, select=c("Longitude", "Latitude"))
  TBMmed <- pam(TBM_LL,1)$medoids
    write.csv(TBMmed, "TB_M_medoid.csv")
TBN_LL <-subset(TB_N, select=c("Longitude", "Latitude"))
  TBNmed <- pam(TBN_LL,1)$medoids
    write.csv(TBNmed, "TB_N_medoid.csv")


JXA_LL <-subset(JX_A, select=c("Longitude", "Latitude"))
  JXAmed <- pam(JXA_LL,1)$medoids
    write.csv(JXAmed, "JX_A_medoid.csv")
JXB_LL <-subset(JX_B, select=c("Longitude", "Latitude"))
  JXBmed <- pam(JXB_LL,1)$medoids
    write.csv(JXBmed, "JX_B_medoid.csv")
JXC_LL <-subset(JX_C, select=c("Longitude", "Latitude"))
  JXCmed <- pam(JXC_LL,1)$medoids
    write.csv(JXCmed, "JX_C_medoid.csv")
JXD_LL <-subset(JX_D, select=c("Longitude", "Latitude"))
  JXDmed <- pam(JXD_LL,1)$medoids
    write.csv(JXDmed, "JX_D_medoid.csv")
JXE_LL <-subset(JX_E, select=c("Longitude", "Latitude"))
  JXEmed <- pam(JXE_LL,1)$medoids
    write.csv(JXEmed, "JX_E_medoid.csv")
JXF_LL <-subset(JX_F, select=c("Longitude", "Latitude"))
  JXFmed <- pam(JXF_LL,1)$medoids
    write.csv(JXFmed, "JX_F_medoid.csv")

IRA_LL <-subset(IR_A, select=c("Longitude", "Latitude"))
  IRAmed <- pam(IRA_LL,1)$medoids
    write.csv(IRAmed, "IR_A_medoid.csv")
IRB_LL <-subset(IR_B, select=c("Longitude", "Latitude"))
  IRBmed <- pam(IRB_LL,1)$medoids
    write.csv(IRBmed, "IR_B_medoid.csv")
IRC_LL <-subset(IR_C, select=c("Longitude", "Latitude"))
  IRCmed <- pam(IRC_LL,1)$medoids
    write.csv(IRCmed, "IR_C_medoid.csv")
IRD_LL <-subset(IR_D, select=c("Longitude", "Latitude"))
  IRDmed <- pam(IRD_LL,1)$medoids
    write.csv(IRDmed, "IR_D_medoid.csv")
IRE_LL <-subset(IR_E, select=c("Longitude", "Latitude"))
  IREmed <- pam(IRE_LL,1)$medoids
    write.csv(IREmed, "IR_E_medoid.csv")
IRF_LL <-subset(IR_F, select=c("Longitude", "Latitude"))
  IRFmed <- pam(IRF_LL,1)$medoids
    write.csv(IRFmed, "IR_F_medoid.csv")
IRH_LL <-subset(IR_H, select=c("Longitude", "Latitude"))
  IRHmed <- pam(IRH_LL,1)$medoids
    write.csv(IRHmed, "IR_H_medoid.csv")

CKB_LL <-subset(CK_B, select=c("Longitude", "Latitude"))
  CKBmed <- pam(CKB_LL,1)$medoids
    write.csv(CKBmed, "CK_B_medoid.csv")
CKC_LL <-subset(CK_C, select=c("Longitude", "Latitude"))
  CKCmed <- pam(CKC_LL,1)$medoids
    write.csv(CKCmed, "CK_C_medoid.csv")
CKF_LL <-subset(CK_F, select=c("Longitude", "Latitude"))
  CKFmed <- pam(CKF_LL,1)$medoids
    write.csv(CKFmed, "CK_F_medoid.csv")

CHA_LL <-subset(CH_A, select=c("Longitude", "Latitude"))
  CHAmed <- pam(CHA_LL,1)$medoids
    write.csv(CHAmed, "CH_A_medoid.csv")
CHB_LL <-subset(CH_B, select=c("Longitude", "Latitude"))
  CHBmed <- pam(CHB_LL,1)$medoids
    write.csv(CHBmed, "CH_B_medoid.csv")
CHC_LL <-subset(CH_C, select=c("Longitude", "Latitude"))
  CHCmed <- pam(CHC_LL,1)$medoids
    write.csv(CHCmed, "CH_C_medoid.csv")
CHD_LL <-subset(CH_D, select=c("Longitude", "Latitude"))
  CHDmed <- pam(CHD_LL,1)$medoids
    write.csv(CHDmed, "CH_D_medoid.csv")
CHM_LL <-subset(CH_M, select=c("Longitude", "Latitude"))
  CHMmed <- pam(CHM_LL,1)$medoids
    write.csv(CHMmed, "CH_M_medoid.csv")
CHP_LL <-subset(CH_P, select=c("Longitude", "Latitude"))
  CHPmed <- pam(CHP_LL,1)$medoids
    write.csv(CHPmed, "CH_P_medoid.csv")

APA_LL <-subset(AP_A, select=c("Longitude", "Latitude"))
  APAmed <- pam(APA_LL,1)$medoids
    write.csv(APAmed, "AP_A_medoid.csv")
APB_LL <-subset(AP_B, select=c("Longitude", "Latitude"))
  APBmed <- pam(APB_LL,1)$medoids
    write.csv(APBmed, "AP_B_medoid.csv")
APC_LL <-subset(AP_C, select=c("Longitude", "Latitude"))
  APCmed <- pam(APC_LL,1)$medoids
    write.csv(APCmed, "AP_C_medoid.csv")


## Calculating great-circle distances between medoids ##

# https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
# distCosine - law of cosines great circle distance 
# distGeo - distance on an ellipsoid (the geodesic)

library(geosphere)
# Tampa Bay #
 # A-B
distGeo(TBAmed, TBBmed, a=6378137, f=1/298.257223563) #16490 m
 #A-C
distGeo(TBAmed, TBCmed, a=6378137, f=1/298.257223563) #24449.63 m
 #A-D
distGeo(TBAmed, TBDmed, a=6378137, f=1/298.257223563) #31474.76 m
 #A-E
distGeo(TBAmed, TBEmed, a=6378137, f=1/298.257223563) #24449.63 m
 #A-K
distGeo(TBAmed, TBKmed, a=6378137, f=1/298.257223563) #29594.98 m
 #A-L
distGeo(TBAmed, TBLmed, a=6378137, f=1/298.257223563) #35010.35 m
 #A-M
distGeo(TBAmed, TBMmed, a=6378137, f=1/298.257223563) #51399.39 m
 #A-N
distGeo(TBAmed, TBNmed, a=6378137, f=1/298.257223563) #55208.98 m
 #B-C
distGeo(TBBmed, TBCmed, a=6378137, f=1/298.257223563) #14887.12 m
 #B-D
distGeo(TBBmed, TBDmed, a=6378137, f=1/298.257223563) #18792.31m
 #B-E
distGeo(TBBmed, TBEmed, a=6378137, f=1/298.257223563) #28622.34 m
 #B-K
distGeo(TBBmed, TBKmed, a=6378137, f=1/298.257223563) #22417.49 m
 #B-L
distGeo(TBBmed, TBLmed, a=6378137, f=1/298.257223563) #19595.52 m
 #B-M
distGeo(TBBmed, TBMmed, a=6378137, f=1/298.257223563) #34950.52 m
 #B-N
distGeo(TBBmed, TBNmed, a=6378137, f=1/298.257223563) #38754.65 m
 #C-D
distGeo(TBCmed, TBDmed, a=6378137, f=1/298.257223563) #30760.65 m
 #C-E
distGeo(TBCmed, TBEmed, a=6378137, f=1/298.257223563) #33741.94 m
 #C-K
distGeo(TBCmed, TBKmed, a=6378137, f=1/298.257223563) #7658.382 m
 #C-L
distGeo(TBCmed, TBLmed, a=6378137, f=1/298.257223563) #15080.27 m
 #C-M
distGeo(TBCmed, TBMmed, a=6378137, f=1/298.257223563) #36195.12 m
 #C-N
distGeo(TBCmed, TBNmed, a=6378137, f=1/298.257223563) #39595.34 m
 #D-E
distGeo(TBDmed, TBEmed, a=6378137, f=1/298.257223563) #17068.33 m
 #D-K
distGeo(TBDmed, TBKmed, a=6378137, f=1/298.257223563) #38228.97 m
 #D-L
distGeo(TBDmed, TBLmed, a=6378137, f=1/298.257223563) #25192.25 m
 #D-M
distGeo(TBDmed, TBMmed, a=6378137, f=1/298.257223563) #26680.88 m
 #D-N
distGeo(TBDmed, TBNmed, a=6378137, f=1/298.257223563) #30103.71 m
 #E-K
distGeo(TBEmed, TBKmed, a=6378137, f=1/298.257223563) #39568.18 m
 #E-L
distGeo(TBEmed, TBLmed, a=6378137, f=1/298.257223563) #20928.78 m
 #E-M
distGeo(TBEmed, TBMmed, a=6378137, f=1/298.257223563) #9942.947 m
 #E-N
distGeo(TBEmed, TBNmed, a=6378137, f=1/298.257223563) #13105.81 m
 #K-L
distGeo(TBKmed, TBLmed, a=6378137, f=1/298.257223563) #19227.97 m
 #K-M
distGeo(TBKmed, TBMmed, a=6378137, f=1/298.257223563) #40531.64 m
 #K-N
distGeo(TBKmed, TBNmed, a=6378137, f=1/298.257223563) #43611.04 m
 #L-M
distGeo(TBLmed, TBMmed, a=6378137, f=1/298.257223563) #21390.12 m
 #L-N
distGeo(TBLmed, TBNmed, a=6378137, f=1/298.257223563) #24638.74 m
 #M-N
distGeo(TBMmed, TBNmed, a=6378137, f=1/298.257223563) #24638.74 m

# JAX #

 #A-B
distGeo(JXAmed, JXBmed, a=6378137, f=1/298.257223563) #16788.34 m
 #A-C
distGeo(JXAmed, JXCmed, a=6378137, f=1/298.257223563) #31452.12 m
 #A-D
distGeo(JXAmed, JXDmed, a=6378137, f=1/298.257223563) #52309.28 m
 #A-E
distGeo(JXAmed, JXEmed, a=6378137, f=1/298.257223563) #78780.8 m
 #A-F
distGeo(JXAmed, JXFmed, a=6378137, f=1/298.257223563) #105224.2 m
 #B-C
distGeo(JXBmed, JXCmed, a=6378137, f=1/298.257223563) #14834.72 m
 #B-D
distGeo(JXBmed, JXDmed, a=6378137, f=1/298.257223563) #36561.24 m
 #B-E
distGeo(JXBmed, JXEmed, a=6378137, f=1/298.257223563) #62255.74 m
 #B-F
distGeo(JXBmed, JXFmed, a=6378137, f=1/298.257223563) #88474.35 m
 #C-D
distGeo(JXCmed, JXDmed, a=6378137, f=1/298.257223563) #22309.26 m
 #C-E
distGeo(JXCmed, JXEmed, a=6378137, f=1/298.257223563) #47421.76 m
 #C-F
distGeo(JXCmed, JXFmed, a=6378137, f=1/298.257223563) #88474.35 m
 #D-E
distGeo(JXDmed, JXEmed, a=6378137, f=1/298.257223563) #27868.56 m
 #D-F
distGeo(JXDmed, JXFmed, a=6378137, f=1/298.257223563) #55613.71 m

# INDIAN RIVER #

 #A-B
distGeo(IRAmed, IRBmed, a=6378137, f=1/298.257223563) #21417.441 m
 #A-C
distGeo(IRAmed, IRCmed, a=6378137, f=1/298.257223563) #58804.78 m
 #A-D
distGeo(IRAmed, IRDmed, a=6378137, f=1/298.257223563) #25679.88 m
 #A-E
distGeo(IRAmed, IREmed, a=6378137, f=1/298.257223563) #41226.73 m 
 #A-H
distGeo(IRAmed, IRHmed, a=6378137, f=1/298.257223563) #98171.38 m
  #A-F
distGeo(IRAmed, IRFmed, a=6378137, f=1/298.257223563) #97743.54 m
 #B-C
distGeo(IRBmed, IRCmed, a=6378137, f=1/298.257223563) 
 #B-D
distGeo(IRBmed, IRDmed, a=6378137, f=1/298.257223563) 
 # B-E
distGeo(IRBmed, IREmed, a=6378137, f=1/298.257223563) 
 # B-H
distGeo(IRBmed, IRHmed, a=6378137, f=1/298.257223563)
 # B-F
distGeo(IRBmed, IRFmed, a=6378137, f=1/298.257223563) 
# C-D
distGeo(IRCmed, IRDmed, a=6378137, f=1/298.257223563) 
# C-E
distGeo(IRCmed, IREmed, a=6378137, f=1/298.257223563) 
# C-H
distGeo(IRCmed, IRHmed, a=6378137, f=1/298.257223563) 
# C-F
distGeo(IRCmed, IRFmed, a=6378137, f=1/298.257223563) 
# D-E
distGeo(IRDmed, IREmed, a=6378137, f=1/298.257223563) 
# D-H
distGeo(IRDmed, IRHmed, a=6378137, f=1/298.257223563) 
# D-F
distGeo(IRDmed, IRFmed, a=6378137, f=1/298.257223563) 
# E-H
distGeo(IREmed, IRHmed, a=6378137, f=1/298.257223563) 
# E-F
distGeo(IREmed, IRFmed, a=6378137, f=1/298.257223563) 
#H-F
distGeo(IRHmed, IRFmed, a=6378137, f=1/298.257223563) 

# CEDAR KEY #
# B-C
distGeo(CKBmed, CKCmed, a=6378137, f=1/298.257223563) 
# B-F
distGeo(CKBmed, CKFmed, a=6378137, f=1/298.257223563) 
# C-F
distGeo(CKCmed, CKFmed, a=6378137, f=1/298.257223563) 





























