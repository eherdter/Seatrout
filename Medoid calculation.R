# Script for figuring out the medoids in all zones so they can be used as distance references
# 6/1/2016 - Edited script for determining medoids of each estuary so they can be used as distance references
setwd("~/Desktop/Github Repo/Seatrout/Data/FIM SAS Data")
library(haven)

APBay <- read_sas("ap_yoy_bay_cn_c.sas7bdat")
CHBay <- read_sas ("ch_yoy_bay_cn_c.sas7bdat")
CKBay <- read_sas ("ck_yoy_bay_cn_c.sas7bdat")
CKRiv <- read_sas ("ck_yoy_riv_cn_c.sas7bdat")
IRRiv <- read_sas ("ir_yoy_riv_cn_c.sas7bdat")
JXBay <- read_sas ("jx_yoy_bay_cn_c.sas7bdat")
JXRiv <- read_sas ("jx_yoy_riv_cn_c.sas7bdat")
TBBay <- read_sas ("tb_yoy_bay_cn_c.sas7bdat")
TBRiv <- read_sas ("tb_yoy_riv_cn_c.sas7bdat")












JX_A <- read.csv("JX_A.csv")
JX_B <- read.csv("JX_B.csv")
JX_C <- read.csv("JX_C.csv")
JX_D <- read.csv("JX_D.csv")
JX_E <- read.csv("JX_E.csv")
JX_F <- read.csv("JX_F.csv")

IR_A <- read.csv("IR_A.csv")
IR_B <- read.csv("IR_B.csv")
IR_C <- read.csv("IR_C.csv")
IR_D <- read.csv("IR_D.csv")
IR_E <- read.csv("IR_E.csv")
IR_F <- read.csv("IR_F.csv")
IR_H <- read.csv("IR_H.csv")

CK_B <- read.csv("CK_B.csv")
CK_C <- read.csv("CK_C.csv")
CK_F <- read.csv("CK_F.csv")

CH_A <- read.csv("CH_A.csv")
CH_B <- read.csv("CH_B.csv")
CH_C <- read.csv("CH_C.csv")
CH_D <- read.csv("CH_D.csv")
CH_M <- read.csv("CH_M.csv")
CH_P <- read.csv("CH_P.csv")

AP_A <- read.csv("AP_A.csv")
AP_B <- read.csv("AP_B.csv")
AP_C <- read.csv("AP_C.csv")

TB_A_OV <- read.csv("TB_A_OV.csv")
TB_B_OV <- read.csv("TB_B_OV.csv")
TB_C_OV <- read.csv("TB_C_OV.csv")
TB_D_OV <- read.csv("TB_D_OV.csv")
TB_E_OV <- read.csv("TB_E_OV.csv")
TB_A_ONV <- read.csv("TB_A_ONV.csv")
TB_B_ONV <- read.csv("TB_B_ONV.csv")
TB_C_ONV <- read.csv("TB_C_ONV.csv")
TB_D_ONV <- read.csv("TB_D_ONV.csv")
TB_E_ONV <- read.csv("TB_E_ONV.csv")
TB_A_S <- read.csv("TB_A_S.csv")
TB_B_S <- read.csv("TB_B_S.csv")
TB_C_S <- read.csv("TB_C_S.csv")
TB_D_S <- read.csv("TB_D_S.csv")
TB_E_S <- read.csv("TB_E_S.csv")


AP_A_OV <- read.csv("AP_A_OV.csv")
AP_B_OV <- read.csv("AP_B_OV.csv")
AP_A_ONV <- read.csv("AP_A_ONV.csv")
AP_B_ONV <- read.csv("AP_B_ONV.csv")
AP_A_S <- read.csv("AP_A_S.csv")
AP_B_S <- read.csv("AP_B_S.csv")

CH_A_OV <- read.csv("CH_A_OV.csv")
CH_B_OV <- read.csv("CH_B_OV.csv")
CH_C_OV <- read.csv("CH_C_OV.csv")
CH_D_OV <- read.csv("CH_D_OV.csv")
CH_A_ONV <- read.csv("CH_A_ONV.csv")
CH_B_ONV <- read.csv("CH_B_ONV.csv")
CH_C_ONV <- read.csv("CH_C_ONV.csv")
CH_D_ONV <- read.csv("CH_D_ONV.csv")
CH_A_S <- read.csv("CH_A_S.csv")
CH_B_S <- read.csv("CH_B_S.csv")
CH_C_S <- read.csv("CH_C_S.csv")
CH_D_S <- read.csv("CH_D_S.csv")

CK_B_OV <- read.csv("CK_B_OV.csv")
CK_C_OV <- read.csv("CK_C_OV.csv")
CK_B_ONV <- read.csv("CK_B_ONV.csv")
CK_C_ONV <- read.csv("CK_C_ONV.csv")
CK_B_S <- read.csv("CK_B_S.csv")
CK_C_S <- read.csv("CK_C_S.csv")

IR_A_OV <- read.csv("IR_A_OV.csv")
IR_B_OV <- read.csv("IR_B_OV.csv")
IR_C_OV <- read.csv("IR_C_OV.csv")
IR_D_OV <- read.csv("IR_D_OV.csv")
IR_E_OV <- read.csv("IR_E_OV.csv")
IR_H_OV <- read.csv("IR_H_OV.csv")
IR_A_ONV <- read.csv("IR_A_ONV.csv")
IR_B_ONV <- read.csv("IR_B_ONV.csv")
IR_C_ONV <- read.csv("IR_C_ONV.csv")
IR_D_ONV <- read.csv("IR_D_ONV.csv")
IR_E_ONV <- read.csv("IR_E_ONV.csv")
IR_H_ONV <- read.csv("IR_H_ONV.csv")
IR_A_S <- read.csv("IR_A_S.csv")
IR_B_S <- read.csv("IR_B_S.csv")
IR_C_S <- read.csv("IR_C_S.csv")
IR_D_S <- read.csv("IR_D_S.csv")
IR_E_S <- read.csv("IR_E_S.csv")
IR_H_S <- read.csv("IR_H_S.csv")

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
    #write.csv(TBAmed, "TB_A_medoid.csv")
TBB_LL <-subset(TB_B, select=c("Longitude", "Latitude"))
  TBBmed <- pam(TBB_LL,1)$medoids
    #write.csv(TBBmed, "TB_B_medoid.csv")
TBC_LL <-subset(TB_C, select=c("Longitude", "Latitude"))
  TBCmed <- pam(TBC_LL,1)$medoids
    #write.csv(TBCmed, "TB_C_medoid.csv")
TBD_LL <-subset(TB_D, select=c("Longitude", "Latitude"))
  TBDmed <- pam(TBD_LL,1)$medoids
    #write.csv(TBDmed, "TB_D_medoid.csv")
TBE_LL <-subset(TB_E, select=c("Longitude", "Latitude"))
  TBEmed <- pam(TBE_LL,1)$medoids
    #write.csv(TBEmed, "TB_E_medoid.csv")
TBK_LL <-subset(TB_K, select=c("Longitude", "Latitude"))
  TBKmed <- pam(TBK_LL,1)$medoids
    #write.csv(TBKmed, "TB_K_medoid.csv")
TBL_LL <-subset(TB_L, select=c("Longitude", "Latitude"))
  TBLmed <- pam(TBL_LL,1)$medoids
    #write.csv(TBLmed, "TB_L_medoid.csv")
TBM_LL <-subset(TB_M, select=c("Longitude", "Latitude"))
  TBMmed <- pam(TBM_LL,1)$medoids
    #write.csv(TBMmed, "TB_M_medoid.csv")
TBN_LL <-subset(TB_N, select=c("Longitude", "Latitude"))
  TBNmed <- pam(TBN_LL,1)$medoids
    #write.csv(TBNmed, "TB_N_medoid.csv")


JXA_LL <-subset(JX_A, select=c("Longitude", "Latitude"))
  JXAmed <- pam(JXA_LL,1)$medoids
    #write.csv(JXAmed, "JX_A_medoid.csv")
JXB_LL <-subset(JX_B, select=c("Longitude", "Latitude"))
  JXBmed <- pam(JXB_LL,1)$medoids
    #write.csv(JXBmed, "JX_B_medoid.csv")
JXC_LL <-subset(JX_C, select=c("Longitude", "Latitude"))
  JXCmed <- pam(JXC_LL,1)$medoids
    #write.csv(JXCmed, "JX_C_medoid.csv")
JXD_LL <-subset(JX_D, select=c("Longitude", "Latitude"))
  JXDmed <- pam(JXD_LL,1)$medoids
    #write.csv(JXDmed, "JX_D_medoid.csv")
JXE_LL <-subset(JX_E, select=c("Longitude", "Latitude"))
  JXEmed <- pam(JXE_LL,1)$medoids
    #write.csv(JXEmed, "JX_E_medoid.csv")
JXF_LL <-subset(JX_F, select=c("Longitude", "Latitude"))
  JXFmed <- pam(JXF_LL,1)$medoids
    #write.csv(JXFmed, "JX_F_medoid.csv")

IRA_LL <-subset(IR_A, select=c("Longitude", "Latitude"))
  IRAmed <- pam(IRA_LL,1)$medoids
    #write.csv(IRAmed, "IR_A_medoid.csv")
IRB_LL <-subset(IR_B, select=c("Longitude", "Latitude"))
  IRBmed <- pam(IRB_LL,1)$medoids
    #write.csv(IRBmed, "IR_B_medoid.csv")
IRC_LL <-subset(IR_C, select=c("Longitude", "Latitude"))
  IRCmed <- pam(IRC_LL,1)$medoids
    #write.csv(IRCmed, "IR_C_medoid.csv")
IRD_LL <-subset(IR_D, select=c("Longitude", "Latitude"))
  IRDmed <- pam(IRD_LL,1)$medoids
    #write.csv(IRDmed, "IR_D_medoid.csv")
IRE_LL <-subset(IR_E, select=c("Longitude", "Latitude"))
  IREmed <- pam(IRE_LL,1)$medoids
    #write.csv(IREmed, "IR_E_medoid.csv")
IRF_LL <-subset(IR_F, select=c("Longitude", "Latitude"))
  IRFmed <- pam(IRF_LL,1)$medoids
    #write.csv(IRFmed, "IR_F_medoid.csv")
IRH_LL <-subset(IR_H, select=c("Longitude", "Latitude"))
  IRHmed <- pam(IRH_LL,1)$medoids
    #write.csv(IRHmed, "IR_H_medoid.csv")

CKB_LL <-subset(CK_B, select=c("Longitude", "Latitude"))
  CKBmed <- pam(CKB_LL,1)$medoids
    #write.csv(CKBmed, "CK_B_medoid.csv")
CKC_LL <-subset(CK_C, select=c("Longitude", "Latitude"))
  CKCmed <- pam(CKC_LL,1)$medoids
    #write.csv(CKCmed, "CK_C_medoid.csv")
CKF_LL <-subset(CK_F, select=c("Longitude", "Latitude"))
  CKFmed <- pam(CKF_LL,1)$medoids
    #write.csv(CKFmed, "CK_F_medoid.csv")

CHA_LL <-subset(CH_A, select=c("Longitude", "Latitude"))
  CHAmed <- pam(CHA_LL,1)$medoids
    #write.csv(CHAmed, "CH_A_medoid.csv")
CHB_LL <-subset(CH_B, select=c("Longitude", "Latitude"))
  CHBmed <- pam(CHB_LL,1)$medoids
    #write.csv(CHBmed, "CH_B_medoid.csv")
CHC_LL <-subset(CH_C, select=c("Longitude", "Latitude"))
  CHCmed <- pam(CHC_LL,1)$medoids
    #write.csv(CHCmed, "CH_C_medoid.csv")
CHD_LL <-subset(CH_D, select=c("Longitude", "Latitude"))
  CHDmed <- pam(CHD_LL,1)$medoids
    #write.csv(CHDmed, "CH_D_medoid.csv")
CHM_LL <-subset(CH_M, select=c("Longitude", "Latitude"))
  CHMmed <- pam(CHM_LL,1)$medoids
    #write.csv(CHMmed, "CH_M_medoid.csv")
CHP_LL <-subset(CH_P, select=c("Longitude", "Latitude"))
  CHPmed <- pam(CHP_LL,1)$medoids
    #write.csv(CHPmed, "CH_P_medoid.csv")

APA_LL <-subset(AP_A, select=c("Longitude", "Latitude"))
  APAmed <- pam(APA_LL,1)$medoids
    #write.csv(APAmed, "AP_A_medoid.csv")
APB_LL <-subset(AP_B, select=c("Longitude", "Latitude"))
  APBmed <- pam(APB_LL,1)$medoids
    #write.csv(APBmed, "AP_B_medoid.csv")
APC_LL <-subset(AP_C, select=c("Longitude", "Latitude"))
  APCmed <- pam(APC_LL,1)$medoids
    #write.csv(APCmed, "AP_C_medoid.csv")

#Use Low_frequency_Stratum_hmisc_adjusted_Pvals_edited.csv to determine which comparisons need to be made
library(cluster)
TBA_ONVLL <- subset(TB_A_ONV, select=c("Longitude", "Latitude"))
  TBA_ONVmed <- pam(TBA_ONVLL,1)$medoids

TBB_ONVLL <- subset(TB_B_ONV, select=c("Longitude", "Latitude"))
  TBB_ONVmed <- pam(TBB_ONVLL,1)$medoids

TBB_OVLL <- subset(TB_B_OV, select=c("Longitude", "Latitude"))
TBB_OVmed <- pam(TBB_OVLL,1)$medoids

TBB_SLL <- subset(TB_B_S, select=c("Longitude", "Latitude"))
TBB_Smed <- pam(TBB_SLL,1)$medoids

TBC_ONVLL <- subset(TB_C_ONV, select=c("Longitude", "Latitude"))
TBC_ONVmed <- pam(TBC_ONVLL,1)$medoids

TBD_ONVLL <- subset(TB_D_ONV, select=c("Longitude", "Latitude"))
  TBD_ONVmed <- pam(TBD_ONVLL,1)$medoids

TBD_OVLL <- subset(TB_D_OV, select=c("Longitude", "Latitude"))
TBD_OVmed <- pam(TBD_OVLL,1)$medoids

TBE_ONVLL <- subset(TB_E_ONV, select=c("Longitude", "Latitude"))
TBE_ONVmed <- pam(TBE_ONVLL,1)$medoids

TBE_OVLL <- subset(TB_E_OV, select=c("Longitude", "Latitude"))
TBE_OVmed <- pam(TBE_OVLL,1)$medoids

TBE_SLL <- subset(TB_E_S, select=c("Longitude", "Latitude"))
  TBE_Smed <- pam(TBE_SLL,1)$medoids

CKC_ONVLL <- subset(CK_C_ONV, select=c("Longitude", "Latitude"))
CKC_ONVmed <- pam(CKC_ONVLL,1)$medoids

CKB_ONVLL <- subset(CK_B_ONV, select=c("Longitude", "Latitude"))
CKB_ONVmed <- pam(CKB_ONVLL,1)$medoids

CKB_OVLL <- subset(CK_B_OV, select=c("Longitude", "Latitude"))
CKB_OVmed <- pam(CKB_OVLL,1)$medoids

CKB_SLL <- subset(CK_B_S, select=c("Longitude", "Latitude"))
CKB_Smed <- pam(CKB_SLL,1)$medoids



CKC_OVLL <- subset(CK_C_OV, select=c("Longitude", "Latitude"))
CKC_OVmed <- pam(CKC_OVLL,1)$medoids

CKC_SLL <- subset(CK_C_S, select=c("Longitude", "Latitude"))
CKC_Smed <- pam(CKC_SLL,1)$medoids



CHB_OVLL <- subset(CH_B_OV, select=c("Longitude", "Latitude"))
CHB_OVmed <- pam(CHB_OVLL,1)$medoids

CHA_ONVLL <- subset(CH_A_ONV, select=c("Longitude", "Latitude"))
CHA_ONVmed <- pam(CHA_ONVLL,1)$medoids

CHA_OVLL <- subset(CH_A_OV, select=c("Longitude", "Latitude"))
CHA_OVmed <- pam(CHA_OVLL,1)$medoids

CHB_ONVLL <- subset(CH_B_ONV, select=c("Longitude", "Latitude"))
CHB_ONVmed <- pam(CHB_ONVLL,1)$medoids

CHB_SLL <- subset(CH_B_S, select=c("Longitude", "Latitude"))
CHB_Smed <- pam(CHB_SLL,1)$medoids

CHC_ONVLL <- subset(CH_C_ONV, select=c("Longitude", "Latitude"))
CHC_ONVmed <- pam(CHC_ONVLL,1)$medoids

CHC_OVLL <- subset(CH_C_OV, select=c("Longitude", "Latitude"))
CHC_OVmed <- pam(CHC_OVLL,1)$medoids

CHC_SLL <- subset(CH_C_S, select=c("Longitude", "Latitude"))
CHC_Smed <- pam(CHC_SLL,1)$medoids

CHD_OVLL <- subset(CH_D_OV, select=c("Longitude", "Latitude"))
CHD_OVmed <- pam(CHD_OVLL,1)$medoids



APB_ONVLL <- subset(AP_B_ONV, select=c("Longitude", "Latitude"))
APB_ONVmed <- pam(APB_ONVLL,1)$medoids

APA_ONVLL <- subset(AP_A_ONV, select=c("Longitude", "Latitude"))
APA_ONVmed <- pam(APA_ONVLL,1)$medoids

APA_SLL <- subset(AP_A_S, select=c("Longitude", "Latitude"))
APA_Smed <- pam(APA_SLL,1)$medoids

APB_OVLL <- subset(AP_B_OV, select=c("Longitude", "Latitude"))
APB_OVmed <- pam(APB_OVLL,1)$medoids

TBA_OVLL <- subset(TB_A_OV, select=c("Longitude", "Latitude"))
TBA_OVmed <- pam(TBA_OVLL,1)$medoids

IRD_OVLL <- subset(IR_D_OV, select=c("Longitude", "Latitude"))
IRD_OVmed <- pam(IRD_OVLL,1)$medoids

IRD_SLL <- subset(IR_D_S, select=c("Longitude", "Latitude"))
IRD_Smed <- pam(IRD_SLL,1)$medoids

IRH_SLL <- subset(IR_H_S, select=c("Longitude", "Latitude"))
IRH_Smed <- pam(IRH_SLL,1)$medoids

IRH_ONVLL <- subset(IR_H_ONV, select=c("Longitude", "Latitude"))
IRH_ONVmed <- pam(IRH_ONVLL,1)$medoids


IRC_OVLL <- subset(IR_C_OV, select=c("Longitude", "Latitude"))
IRC_OVmed <- pam(IRC_OVLL,1)$medoids

IRC_SLL <- subset(IR_C_S, select=c("Longitude", "Latitude"))
IRC_Smed <- pam(IRC_SLL,1)$medoids


## Calculating great-circle distances between medoids- STRATUMS #
library(geosphere)
TBA_ONV_compars = rbind(distGeo(TBA_ONVmed, TBB_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, TBD_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, TBE_Smed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, CKC_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, CHB_OVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, CHC_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_ONVmed, APB_ONVmed, a=6378137, f=1/298.257223563))

TBA_OV_compars <- rbind(distGeo(TBA_OVmed, TBD_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBA_OVmed, CKC_ONVmed, a=6378137, f=1/298.257223563))

TBB_ONV_compars <- rbind(distGeo(TBB_ONVmed, CKC_ONVmed, a=6378137, f=1/298.257223563),
                         distGeo(TBB_ONVmed, CHB_OVmed, a=6378137, f=1/298.257223563),
                         distGeo(TBB_ONVmed, CHC_OVmed, a=6378137, f=1/298.257223563),
                         distGeo(TBB_ONVmed, IRD_OVmed, a=6378137, f=1/298.257223563))

TBB_OV_compars <- rbind(distGeo(TBB_OVmed, TBD_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBB_OVmed, TBE_ONVmed, a=6378137, f=1/298.257223563))

distGeo(TBB_Smed, TBE_Smed, a=6378137, f=1/298.257223563)

TBC_ONV_compars <- rbind(distGeo(TBC_ONVmed, TBD_OVmed, a=6378137, f=1/298.257223563),
                          distGeo(TBC_ONVmed, CHA_ONVmed, a=6378137, f=1/298.257223563),
                          distGeo(TBC_ONVmed, CHB_ONVmed, a=6378137, f=1/298.257223563),
                          distGeo(TBC_ONVmed, CHB_OVmed, a=6378137, f=1/298.257223563))

distGeo(TBD_ONVmed, CKC_ONVmed, a=6378137, f=1/298.257223563)

TBD_OV_compars <- rbind(distGeo(TBD_OVmed, CHA_ONVmed, a=6378137, f=1/298.257223563),
                        distGeo(TBD_OVmed, CHB_Smed, a=6378137, f=1/298.257223563),
                        distGeo(TBD_OVmed, IRD_OVmed, a=6378137, f=1/298.257223563))

distGeo(TBE_OVmed, IRC_OVmed, a=6378137, f=1/298.257223563)

distGeo(TBE_Smed, APB_ONVmed, a=6378137, f=1/298.257223563)

distGeo(CKC_OVmed, CHA_OVmed, a=6378137, f=1/298.257223563)

distGeo(CKC_Smed, IRD_Smed, a=6378137, f=1/298.257223563)

distGeo(CKB_ONVmed, APB_OVmed, a=6378137, f=1/298.257223563)

distGeo(CKB_OVmed, CHB_ONVmed, a=6378137, f=1/298.257223563)

distGeo(CKB_Smed, CHB_OVmed, a=6378137, f=1/298.257223563)
distGeo(CKB_Smed, CHD_OVmed, a=6378137, f=1/298.257223563)

distGeo(CHA_ONVmed, CHB_ONVmed, a=6378137, f=1/298.257223563)

distGeo(CHB_ONVmed, CHB_OVmed, a=6378137, f=1/298.257223563)
distGeo(CHB_ONVmed, CHC_OVmed, a=6378137, f=1/298.257223563)

distGeo(CHB_OVmed, CHC_ONVmed, a=6378137, f=1/298.257223563)
distGeo(CHB_OVmed, CHC_OVmed, a=6378137, f=1/298.257223563)

distGeo(CHC_ONVmed, APB_ONVmed, a=6378137, f=1/298.257223563)
distGeo(CHC_ONVmed, IRH_Smed, a=6378137, f=1/298.257223563)

distGeo(CHC_OVmed, IRH_Smed, a=6378137, f=1/298.257223563)

distGeo(CHC_Smed, CHD_OVmed, a=6378137, f=1/298.257223563)

distGeo(APA_ONVmed, APA_Smed, a=6378137, f=1/298.257223563)

distGeo(IRC_Smed, IRH_ONVmed, a=6378137, f=1/298.257223563)




































## Calculating great-circle distances between medoids- ZONES ##

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

        # Tampa Bay A to Jax #
TBA_JX <- rbind( distGeo(TBAmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBAmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay A  to Indian River #
TBA_IR <- rbind(distGeo(TBAmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IRFmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, IRHmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay A to Cedar Key
TBA_CK <- rbind(distGeo(TBAmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay A to Charlotte Harbor
TBA_CH <- rbind(distGeo(TBAmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBAmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay A to Appalachicola
TBA_AP <- rbind(distGeo(TBAmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBAmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay B to Jax #
TBB_JX <- rbind( distGeo(TBBmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBBmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay B to Indian River
TBB_IR <- rbind(distGeo(TBBmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay B to Cedar Key
TBB_CK <- rbind(distGeo(TBBmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay B to Charlotte Harbor
TBB_CH <- rbind(distGeo(TBBmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBBmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay B to Appalachicola
TBB_AP <- rbind(distGeo(TBBmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBBmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay C to Jax #
TBC_JX <- rbind( distGeo(TBCmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBCmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay C to Indian River
TBC_IR <- rbind(distGeo(TBCmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay C to Cedar Key
TBC_CK <- rbind(distGeo(TBCmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay C to Charlotte Harbor
TBC_CH <- rbind(distGeo(TBCmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBCmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay C to Appalachicola
TBC_AP <- rbind(distGeo(TBCmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBCmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay D to Jax #
TBD_JX <- rbind( distGeo(TBDmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBDmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay D to Indian River
TBD_IR <- rbind(distGeo(TBDmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay D to Cedar Key
TBD_CK <- rbind(distGeo(TBDmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay D to Charlotte Harbor
TBD_CH <- rbind(distGeo(TBDmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBDmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay D to Appalachicola
TBD_AP <- rbind(distGeo(TBDmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBDmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay E to Jax #
TBE_JX <- rbind( distGeo(TBEmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBEmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay E to Indian River
TBE_IR <- rbind(distGeo(TBEmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay E to Cedar Key
TBE_CK <- rbind(distGeo(TBEmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay E to Charlotte Harbor
TBE_CH <- rbind(distGeo(TBEmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBEmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay E to Appalachicola
TBE_AP <- rbind(distGeo(TBEmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBEmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay K to Jax #
TBK_JX <- rbind( distGeo(TBKmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBKmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay K to Indian River
TBK_IR <- rbind(distGeo(TBKmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay K to Cedar Key
TBK_CK <- rbind(distGeo(TBKmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay K to Charlotte Harbor
TBK_CH <- rbind(distGeo(TBKmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBKmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay K to Appalachicola
TBK_AP <- rbind(distGeo(TBKmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBKmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay L to Jax #
TBL_JX <- rbind( distGeo(TBLmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBLmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay L to Indian River
TBL_IR <- rbind(distGeo(TBLmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay L to Cedar Key
TBL_CK <- rbind(distGeo(TBLmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay L to Charlotte Harbor
TBL_CH <- rbind(distGeo(TBLmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBLmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay L to Appalachicola
TBL_AP <- rbind(distGeo(TBLmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBLmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay M to Jax #
TBM_JX <- rbind( distGeo(TBMmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBMmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay M to Indian River
TBM_IR <- rbind(distGeo(TBMmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay M to Cedar Key
TBM_CK <- rbind(distGeo(TBMmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay M to Charlotte Harbor
TBM_CH <- rbind(distGeo(TBMmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBMmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay M to Appalachicola
TBM_AP <- rbind(distGeo(TBMmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBMmed, APCmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay N to Jax #
TBN_JX <- rbind( distGeo(TBNmed, JXAmed, a=6378137, f=1/298.257223563), distGeo(TBNmed, JXBmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, JXCmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, JXDmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, JXEmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, JXFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay N to Indian River
TBN_IR <- rbind(distGeo(TBNmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IRBmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IRCmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, IRFmed, a=6378137, f=1/298.257223563)) 
        # Tampa Bay N to Cedar Key
TBN_CK <- rbind(distGeo(TBNmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Tampa Bay N to Charlotte Harbor
TBN_CH <- rbind(distGeo(TBNmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(TBNmed, CHPmed, a=6378137, f=1/298.257223563))
        # Tampa Bay N to Appalachicola
TBN_AP <- rbind(distGeo(TBNmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(TBNmed, APCmed, a=6378137, f=1/298.257223563) )













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
 #E-F
distGeo(JXEmed, JXFmed, a=6378137, f=1/298.257223563) #55613.71 m

        # Jax A to Indian River #
JXA_IR <- rbind(distGeo(JXAmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXAmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax A to Cedar Key #
JXA_CK <- rbind(distGeo(JXAmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax A to Charlotte Harbor #
JXA_CH <- rbind(distGeo(JXAmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXAmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax A to Appalachicola
JXA_AP <- rbind(distGeo(JXAmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXAmed, APCmed, a=6378137, f=1/298.257223563) )
        # Jax B to Indian River #
JXB_IR <- rbind(distGeo(JXBmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXBmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax B to Cedar Key #
JXB_CK <- rbind(distGeo(JXBmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax B to Charlotte Harbor #
JXB_CH <- rbind(distGeo(JXBmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXBmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax B to Appalachicola
JXB_AP <- rbind(distGeo(JXBmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXBmed, APCmed, a=6378137, f=1/298.257223563) )
        # Jax C to Indian River #
JXC_IR <- rbind(distGeo(JXCmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXCmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax C to Cedar Key #
JXC_CK <- rbind(distGeo(JXCmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax C to Charlotte Harbor #
JXC_CH <- rbind(distGeo(JXCmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXCmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax C to Appalachicola
JXC_AP <- rbind(distGeo(JXCmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXCmed, APCmed, a=6378137, f=1/298.257223563) )
        # Jax D to Indian River #
JXD_IR <- rbind(distGeo(JXDmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXDmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax D to Cedar Key #
JXD_CK <- rbind(distGeo(JXDmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax D to Charlotte Harbor #
JXD_CH <- rbind(distGeo(JXDmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXDmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax D to Appalachicola
JXD_AP <- rbind(distGeo(JXDmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXDmed, APCmed, a=6378137, f=1/298.257223563) )

        # Jax E to Indian River #
JXE_IR <- rbind(distGeo(JXEmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXEmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax E to Cedar Key #
JXE_CK <- rbind(distGeo(JXEmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax E to Charlotte Harbor #
JXE_CH <- rbind(distGeo(JXEmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXEmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax E to Appalachicola
JXE_AP <- rbind(distGeo(JXEmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXEmed, APCmed, a=6378137, f=1/298.257223563) )

        # Jax F to Indian River #
JXF_IR <- rbind(distGeo(JXFmed, IRAmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, IRBmed, a=6378137, f=1/298.257223563) , distGeo(JXFmed, IRCmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, IRDmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, IREmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, IRHmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, IRFmed, a=6378137, f=1/298.257223563)  )
        # Jax F to Cedar Key #
JXF_CK <- rbind(distGeo(JXFmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CKFmed, a=6378137, f=1/298.257223563) )
        # Jax F to Charlotte Harbor #
JXF_CH <- rbind(distGeo(JXFmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(JXFmed, CHPmed, a=6378137, f=1/298.257223563))
        # Jax F to Appalachicola
JXF_AP <- rbind(distGeo(JXFmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(JXFmed, APCmed, a=6378137, f=1/298.257223563) )



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

        #Indian River A to Cedar Key#
IRA_CK <- rbind(distGeo(IRAmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CKFmed, a=6378137, f=1/298.257223563) )
        #Indian River A to Charlotte Harbor #
IRA_CH <- rbind(distGeo(IRAmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRAmed, CHPmed, a=6378137, f=1/298.257223563))
        #Indian River A to Appalachicola
IRA_AP <- rbind(distGeo(IRAmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRAmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRAmed, APCmed, a=6378137, f=1/298.257223563) )

        #Indian River B to Cedar Key#
IRB_CK <- rbind(distGeo(IRBmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CKFmed, a=6378137, f=1/298.257223563) )
        #Indian River B to Charlotte Harbor #
IRB_CH <- rbind(distGeo(IRBmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRBmed, CHPmed, a=6378137, f=1/298.257223563))
        #Indian River B to Appalachicola
IRB_AP <- rbind(distGeo(IRBmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRBmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRBmed, APCmed, a=6378137, f=1/298.257223563) )

        #Indian River C to Cedar Key#
IRC_CK <- rbind(distGeo(IRCmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CKFmed, a=6378137, f=1/298.257223563) )
        #Indian River C to Charlotte Harbor #
IRC_CH <- rbind(distGeo(IRCmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRCmed, CHPmed, a=6378137, f=1/298.257223563))
        #Indian River C to Appalachicola
IRC_AP <- rbind(distGeo(IRCmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRCmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRCmed, APCmed, a=6378137, f=1/298.257223563) )

        #Indian River D to Cedar Key#
IRD_CK <- rbind(distGeo(IRDmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CKFmed, a=6378137, f=1/298.257223563) )
        #Indian River D to Charlotte Harbor #
IRD_CH <- rbind(distGeo(IRDmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRDmed, CHPmed, a=6378137, f=1/298.257223563))
        #Indian River D to Appalachicola
IRD_AP <- rbind(distGeo(IRDmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRDmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRDmed, APCmed, a=6378137, f=1/298.257223563) )

        #Indian River E to Cedar Key#
IRE_CK <- rbind(distGeo(IREmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CKFmed, a=6378137, f=1/298.257223563) )
        #Indian River E to Charlotte Harbor #
IRE_CH <- rbind(distGeo(IREmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IREmed, CHPmed, a=6378137, f=1/298.257223563))
        #Indian River E to Appalachicola
IRE_AP <- rbind(distGeo(IREmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IREmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IREmed, APCmed, a=6378137, f=1/298.257223563) )

#Indian River F to Cedar Key#
IRF_CK <- rbind(distGeo(IRFmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CKFmed, a=6378137, f=1/298.257223563) )
#Indian River F to Charlotte Harbor #
IRF_CH <- rbind(distGeo(IRFmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRFmed, CHPmed, a=6378137, f=1/298.257223563))
#Indian River F to Appalachicola
IRF_AP <- rbind(distGeo(IRFmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRFmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRFmed, APCmed, a=6378137, f=1/298.257223563) )

IRF_CK <-rbind(IRF_CK, IRF_CH, IRF_AP)

#Indian River H to Cedar Key#
IRH_CK <- rbind(distGeo(IRHmed, CKBmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CKCmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CKFmed, a=6378137, f=1/298.257223563) )
#Indian River H to Charlotte Harbor #
IRH_CH <- rbind(distGeo(IRHmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(IRHmed, CHPmed, a=6378137, f=1/298.257223563))
#Indian River H to Appalachicola
IRH_AP <- rbind(distGeo(IRHmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(IRHmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(IRHmed, APCmed, a=6378137, f=1/298.257223563) )

IRH_CK <-rbind(IRH_CK, IRH_CH, IRH_AP)



# CEDAR KEY #
# B-C
distGeo(CKBmed, CKCmed, a=6378137, f=1/298.257223563) 
# B-F
distGeo(CKBmed, CKFmed, a=6378137, f=1/298.257223563) 
# C-F
distGeo(CKCmed, CKFmed, a=6378137, f=1/298.257223563) 


#Cedar Key B to Charlotte Harbor #
CKB_CH <- rbind(distGeo(CKBmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(CKBmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(CKBmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(CKBmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(CKBmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(CKBmed, CHPmed, a=6378137, f=1/298.257223563))
#Cedar Key B to Appalachicola
CKB_AP <- rbind(distGeo(CKBmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CKBmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CKBmed, APCmed, a=6378137, f=1/298.257223563) )

CKB_CH <-rbind(CKB_CH, CKB_AP)

#Cedar Key C to Charlotte Harbor #
CKC_CH <- rbind(distGeo(CKCmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(CKCmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(CKCmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(CKCmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(CKCmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(CKCmed, CHPmed, a=6378137, f=1/298.257223563))
#Cedar Key C to Appalachicola
CKC_AP <- rbind(distGeo(CKCmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CKCmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CKCmed, APCmed, a=6378137, f=1/298.257223563) )

CKC_CH <-rbind(CKC_CH, CKC_AP)

#Cedar Key F to Charlotte Harbor #
CKF_CH <- rbind(distGeo(CKFmed, CHAmed, a=6378137, f=1/298.257223563),distGeo(CKFmed, CHBmed, a=6378137, f=1/298.257223563),distGeo(CKFmed, CHCmed, a=6378137, f=1/298.257223563),distGeo(CKFmed, CHDmed, a=6378137, f=1/298.257223563),distGeo(CKFmed, CHMmed, a=6378137, f=1/298.257223563),distGeo(CKFmed, CHPmed, a=6378137, f=1/298.257223563))
#Cedar Key F to Appalachicola
CKF_AP <- rbind(distGeo(CKFmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CKFmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CKFmed, APCmed, a=6378137, f=1/298.257223563) )

CKF_CH <-rbind(CKF_CH, CKF_AP)

# CHARLOTTE HARBOR #

# A-B
distGeo(CHAmed, CHBmed, a=6378137, f=1/298.257223563) 
# A-C
distGeo(CHAmed, CHCmed, a=6378137, f=1/298.257223563) 
# A-D
distGeo(CHAmed, CHDmed, a=6378137, f=1/298.257223563) 
# A-M
distGeo(CHAmed, CHMmed, a=6378137, f=1/298.257223563) 
# A-P
distGeo(CHAmed, CHPmed, a=6378137, f=1/298.257223563) 
# B-C
distGeo(CHBmed, CHCmed, a=6378137, f=1/298.257223563) 
# B-D
distGeo(CHBmed, CHDmed, a=6378137, f=1/298.257223563) 
# B-M
distGeo(CHBmed, CHMmed, a=6378137, f=1/298.257223563) 
# B-P
distGeo(CHBmed, CHPmed, a=6378137, f=1/298.257223563) 
# C-D
distGeo(CHCmed, CHDmed, a=6378137, f=1/298.257223563) 
# C-M
distGeo(CHCmed, CHMmed, a=6378137, f=1/298.257223563) 
# C-P
distGeo(CHCmed, CHPmed, a=6378137, f=1/298.257223563) 
# D-M
distGeo(CHDmed, CHMmed, a=6378137, f=1/298.257223563) 
# D-P
distGeo(CHDmed, CHPmed, a=6378137, f=1/298.257223563) 
# M-P
distGeo(CHMmed, CHPmed, a=6378137, f=1/298.257223563) 


#CHBrlotte Harbor A to Appalachicola
CHA_AP <- rbind(distGeo(CHAmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHAmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHAmed, APCmed, a=6378137, f=1/298.257223563) )

#Charlotte Harbor B to Appalachicola
CHB_AP <- rbind(distGeo(CHBmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHBmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHBmed, APCmed, a=6378137, f=1/298.257223563) )

#Charlotte Harbor C to Appalachicola
CHC_AP <- rbind(distGeo(CHCmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHCmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHCmed, APCmed, a=6378137, f=1/298.257223563) )

#Charlotte Harbor D to Appalachicola
CHD_AP <- rbind(distGeo(CHDmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHDmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHDmed, APCmed, a=6378137, f=1/298.257223563) )

#Charlotte Harbor M to Appalachicola
CHM_AP <- rbind(distGeo(CHMmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHMmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHMmed, APCmed, a=6378137, f=1/298.257223563) )

#Charlotte Harbor P to Appalachicola
CHP_AP <- rbind(distGeo(CHPmed, APAmed, a=6378137, f=1/298.257223563) ,distGeo(CHPmed, APBmed, a=6378137, f=1/298.257223563) ,distGeo(CHPmed, APCmed, a=6378137, f=1/298.257223563) )







# Appalachicola ##

# A-B
distGeo(APAmed, APBmed, a=6378137, f=1/298.257223563) 
# A-C
distGeo(APAmed, APCmed, a=6378137, f=1/298.257223563) 
# B-C
distGeo(APBmed, APCmed, a=6378137, f=1/298.257223563) 































