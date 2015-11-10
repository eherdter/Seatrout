setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")
# can use these data frames because they came from Seatrout Raw.R
# Example of Origin: 
#TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
#write.csv(TB_RIV_MUn, "TB_M.csv")
#TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
#write.csv(TB_RIV_NUn, "TB_N.csv")

TB_A <- read.csv("TB_A.csv")
TB_B <- read.csv("TB_B.csv")
TB_C <- read.csv("TB_C.csv")
TB_D <- read.csv("TB_D.csv")
TB_E <- read.csv("TB_E.csv")
TB_K <- read.csv("TB_K.csv")
TB_L <- read.csv("TB_L.csv")
TB_M <- read.csv("TB_M.csv")
TB_N <- read.csv("TB_N.csv")

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




##########################################################
# Use the Stratum variable in the _physical data sheets. Per the procedure manual, S is for shorelines,
# A and B represent offshore seines with A being on vegetated sites and B on non vegetated sites.
# The shoreline is stratified by the presence of over-hanging vegetation
# Rivers are NOT stratified so All of Jax is absent

# 1. Subset by Offshore veg and non-veg and shore
#       > OV= offshore Veg, ONV= offshore non Veg, S= shoreline
#         > summarise by year and month


AP_A_OV <- subset(AP_BAY_AUn, Stratum =="A")
library(plyr)
APA_OV_sum <- ddply(AP_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_OV_sum <- APA_OV_sum$TotalNumberofAnimalsCollectedinHaul/APA_OV_sum$NumberofHauls
AP_B_OV <- subset(AP_BAY_BUn, Stratum=="A")
APB_OV_sum <- ddply(AP_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_OV_sum <- APB_OV_sum$TotalNumberofAnimalsCollectedinHaul/APB_OV_sum$NumberofHauls
AP_A_ONV <- subset(AP_BAY_AUn, Stratum=="B")
APA_ONV_sum <- ddply(AP_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_ONV_sum <- APA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/AP_A_ONV_sum$NumberofHauls
AP_B_ONV <- subset(AP_BAY_BUn, Stratum=="B")
APB_ONV_sum <- ddply(AP_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_ONV_sum <- APB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/APB_ONV_sum$NumberofHauls
AP_A_S <- subset(AP_BAY_AUn, Stratum=="S")
APA_S_sum <- ddply(AP_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_S_sum <- AP_A_S_sum$TotalNumberofAnimalsCollectedinHaul/APA_S_sum$NumberofHauls
AP_B_S <- subset(AP_BAY_BUn, Stratum=="S")
APB_S_sum <- ddply(AP_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_S_sum <- APB_S_sum$TotalNumberofAnimalsCollectedinHaul/APB_S_sum$NumberofHauls

CH_A_OV <- subset(CH_BAY_AUn, Stratum="A")
CHA_OV_sum <- ddply(CHA_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_OV_sum <- CHA_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHA_OV_sum$NumberofHauls
CH_B_OV <- subset(CH_BAY_BUn, Stratum="A")
CHB_OV_sum <- ddply(CH_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_OV_sum <- CHB_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHB_OV_sum$NumberofHauls
CH_C_OV <- subset(CH_BAY_CUn, Stratum="A")
CH_C_OV_sum <- ddply(CH_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_OV_sum <- CHC_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHC_OV_sum$NumberofHauls
CH_D_OV <- subset(CH_BAY_DUn, Stratum="A")
CHD_OV_sum <- ddply(CH_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_OV_sum <- CHD_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHD_OV_sum$NumberofHauls
CH_A_ONV <- subset(CH_BAY_AUn, Stratum="B")
CHA_ONV_sum <- ddply(CH_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_ONV_sum <- CHA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHA_ONV_sum$NumberofHauls
CH_B_ONV <- subset(CH_BAY_BUn, Stratum="B")
CHB_ONV_sum <- ddply(CH_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_ONV_sum <- CHB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHB_ONV_sum$NumberofHauls
CH_C_ONV <- subset(CH_BAY_CUn, Stratum="B")
CHC_ONV_sum <- ddply(CH_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_ONV_sum <- CHC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHC_ONV_sum$NumberofHauls
CH_D_ONV <- subset(CH_BAY_DUn, Stratum="B")
CHD_ONV_sum <- ddply(CH_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_ONV_sum <- CHD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHD_ONV_sum$NumberofHauls
CH_A_S <- subset(CH_BAY_AUn, Stratum="S")
CHA_S_sum <- ddply(CH_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_S_sum <- CHA_S_sum$TotalNumberofAnimalsCollectedinHaul/CHA_S_sum$NumberofHauls
CH_B_S <- subset(CH_BAY_BUn, Stratum="S")
CHB_S_sum <- ddply(CH_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_S_sum <- CHB_S_sum$TotalNumberofAnimalsCollectedinHaul/CHB_S_sum$NumberofHauls
CH_C_S <- subset(CH_BAY_CUn, Stratum="S")
CHC_S_sum <- ddply(CH_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_S_sum <- CHC_S_sum$TotalNumberofAnimalsCollectedinHaul/CHC_S_sum$NumberofHauls
CH_D_S <- subset(CH_BAY_DUn, Stratum="S")
CHD_S_sum <- ddply(CH_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_S_sum <- CHD_S_sum$TotalNumberofAnimalsCollectedinHaul/CHD_S_sum$NumberofHauls


CK_B_OV <- subset(CK_BAY_BUn, Stratum="A")
CKB_OV_sum <- ddply(CK_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_OV_sum <- CKB_OV_sum$TotalNumberofAnimalsCollectedinHaul/CKB_OV_sum$NumberofHauls
CK_C_OV <- subset(CK_BAY_CUn, Stratum="A")
CKC_OV_sum <- ddply(CK_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_OV_sum <- CKC_OV_sum$TotalNumberofAnimalsCollectedinHaul/CKC_OV_sum$NumberofHauls
CK_B_ONV <- subset(CK_BAY_BUn, Stratum="B")
CKB_ONV_sum <- ddply(CK_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_ONV_sum <- CKB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CKB_ONV_sum$NumberofHauls
CK_C_ONV <- subset(CK_BAY_CUn, Stratum="B")
CKC_ONV_sum <- ddply(CK_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_ONV_sum <- CKC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CKC_ONV_sum$NumberofHauls
CK_B_S <- subset(CK_BAY_BUn, Stratum="S")
CKB_S_sum <- ddply(CK_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_S_sum <- CKB_S_sum$TotalNumberofAnimalsCollectedinHaul/CKB_S_sum$NumberofHauls
CK_C_S <- subset(CK_BAY_CUn, Stratum="S")
CKC_S_sum <- ddply(CK_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_S_sum <- CKC_S_sum$TotalNumberofAnimalsCollectedinHaul/CKC_S_sum$NumberofHauls


IR_A_OV <- subset(IR_BAY_AUn, Stratum="A")
IRA_OV_sum <- ddply(IR_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_OV_sum <- IRA_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRA_OV_sum$NumberofHauls
IR_B_OV <- subset(IR_BAY_BUn, Stratum="A")
IRB_OV_sum <- ddply(IR_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_OV_sum <- IRB_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRB_OV_sum$NumberofHauls
IR_C_OV <- subset(IR_BAY_CUn, Stratum="A")
IRC_OV_sum <- ddply(IR_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_OV_sum <- IRC_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRC_OV_sum$NumberofHauls
IR_D_OV <- subset(IR_BAY_DUn, Stratum="A")
IRD_OV_sum <- ddply(IR_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_OV_sum <- IRD_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRD_OV_sum$NumberofHauls
IR_E_OV <- subset(IR_BAY_EUn, Stratum="A")
IRE_OV_sum <- ddply(IR_E_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_OV_sum <- IRE_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRE_OV_sum$NumberofHauls
IR_H_OV <- subset(IR_BAY_HUn, Stratum="A")
IRH_OV_sum <- ddply(IR_H_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_OV_sum <- IRH_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRH_OV_sum$NumberofHauls
IR_A_ONV <- subset(IR_BAY_AUn, Stratum="B")
IRA_ONV_sum <- ddply(IR_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_ONV_sum <- IRA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRA_ONV_sum$NumberofHauls
IR_B_ONV <- subset(IR_BAY_BUn, Stratum="B")
IRB_ONV_sum <- ddply(IR_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_ONV_sum <- IRB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRB_ONV_sum$NumberofHauls
IR_C_ONV <- subset(IR_BAY_CUn, Stratum="B")
IRC_ONV_sum <- ddply(IR_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_ONV_sum <- IRC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRC_ONV_sum$NumberofHauls
IR_D_ONV <- subset(IR_BAY_DUn, Stratum="B")
IRD_ONV_sum <- ddply(IR_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_ONV_sum <- IRD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRD_ONV_sum$NumberofHauls
IR_E_ONV <- subset(IR_BAY_EUn, Stratum="B")
IRE_ONV_sum <- ddply(IR_E_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_ONV_sum <- IRE_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRE_ONV_sum$NumberofHauls
IR_H_ONV <- subset(IR_BAY_HUn, Stratum="B")
IRH_ONV_sum <- ddply(IR_H_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_ONV_sum <- IRH_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRH_ONV_sum$NumberofHauls
IR_A_S <- subset(IR_BAY_AUn, Stratum="S")
IRA_S_sum <- ddply(IR_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_S_sum <- IRA_S_sum$TotalNumberofAnimalsCollectedinHaul/IRA_S_sum$NumberofHauls
IR_B_S <- subset(IR_BAY_BUn, Stratum="S")
IRB_S_sum <- ddply(IR_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_S_sum <- IRB_S_sum$TotalNumberofAnimalsCollectedinHaul/IRB_S_sum$NumberofHauls
IR_C_S <- subset(IR_BAY_CUn, Stratum="S")
IRC_S_sum <- ddply(IR_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_S_sum <- IRC_S_sum$TotalNumberofAnimalsCollectedinHaul/IRC_S_sum$NumberofHauls
IR_D_S <- subset(IR_BAY_DUn, Stratum="S")
IRD_S_sum <- ddply(IR_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_S_sum <- IRD_S_sum$TotalNumberofAnimalsCollectedinHaul/IRD_S_sum$NumberofHauls
IR_E_S <- subset(IR_BAY_EUn, Stratum="S")
IRE_S_sum <- ddply(IR_E_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_S_sum <- IRE_S_sum$TotalNumberofAnimalsCollectedinHaul/IRE_S_sum$NumberofHauls
IR_H_S <- subset(IR_BAY_HUn, Stratum="S")
IRH_S_sum <- ddply(IR_H_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_S_sum <- IRH_S_sum$TotalNumberofAnimalsCollectedinHaul/IRH_S_sum$NumberofHauls

TB_A_OV <- subset(TB_BAY_AUn, Stratum=="A")
TBA_OV_sum <- ddply(TB_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_OV_sum <- TBA_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_OV_sum$NumberofHauls
TB_B_OV <- subset(TB_BAY_BUn, Stratum=="A")
TBB_OV_sum <- ddply(TB_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_OV_sum <- TBB_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBB_OV_sum$NumberofHauls
TB_C_OV <- subset(TB_BAY_CUn, Stratum=="A")
TBC_OV_sum <- ddply(TB_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_OV_sum <- TBC_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_OV_sum$NumberofHauls
TB_D_OV <- subset(TB_BAY_DUn, Stratum=="A")
TBD_OV_sum <- ddply(TB_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_OV_sum <- TBD_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBD_OV_sum$NumberofHauls
TB_E_OV <- subset(TB_BAY_EUn, Stratum=="A")
TBE_OV_sum <- ddply(TB_E_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_OV_sum <- TBE_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBE_OV_sum$NumberofHauls
TB_A_ONV <- subset(TB_BAY_AUn, Stratum=="B")
TBA_ONV_sum <- ddply(TB_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_ONV_sum <- TBA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_ONV_sum$NumberofHauls
TB_B_ONV <- subset(TB_BAY_BUn, Stratum=="B")
TBB_ONV_sum <- ddply(TB_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_ONV_sum <- TBB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBB_ONV_sum$NumberofHauls
TB_C_ONV <- subset(TB_BAY_CUn, Stratum=="B")
TBC_ONV_sum <- ddply(TB_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_ONV_sum <- TBC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBC_ONV_sum$NumberofHauls
TB_D_ONV <- subset(TB_BAY_DUn, Stratum=="B")
TBD_ONV_sum <- ddply(TB_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_ONV_sum <- TBD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBD_ONV_sum$NumberofHauls
TB_E_ONV <- subset(TB_BAY_EUn, Stratum=="B")
TBE_ONV_sum <- ddply(TB_E_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_ONV_sum <- TBE_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBE_ONV_sum$NumberofHauls
TB_A_S <- subset(TB_BAY_AUn, Stratum=="S")
TBA_S_sum <- ddply(TB_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_S_sum <- TBA_S_sum$TotalNumberofAnimalsCollectedinHaul/TBA_S_sum$NumberofHauls
TB_B_S <- subset(TB_BAY_BUn, Stratum=="S")
TBB_S_sum <- ddply(TB_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_S_sum <- TBB_S_sum$TotalNumberofAnimalsCollectedinHaul/TBB_S_sum$NumberofHauls
TB_C_S <- subset(TB_BAY_CUn, Stratum=="S")
TBC_S_sum <- ddply(TB_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_S_sum <- TBC_S_sum$TotalNumberofAnimalsCollectedinHaul/TBC_S_sum$NumberofHauls
TB_D_S <- subset(TB_BAY_DUn, Stratum=="S")
TBD_S_sum <- ddply(TB_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_S_sum <- TBD_S_sum$TotalNumberofAnimalsCollectedinHaul/TBD_S_sum$NumberofHauls
TB_E_S <- subset(TB_BAY_EUn, Stratum=="S")
TBE_S_sum <- ddply(TB_E_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_S_sum <- TBE_S_sum$TotalNumberofAnimalsCollectedinHaul/TBE_S_sum$NumberofHauls

# >>>>>>>>> SUM recruitment over season ###
TBE_S_sumrec <- ddply(TBE_S_sum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls) )
TBE_S_sumrec$CPUE <- TBE_S_sumrec$TotalCollected/TBE_S_sumrec$TotalNumberofHauls


