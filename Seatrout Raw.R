# This script loads Spotted Seatrout raw survey data. They are stored as .sas7bdat at FWRI but I will be using
# haven to convert them and work with them in R
setwd("~/Desktop/Github Repo/Seatrout/Data/Raw Survey Data/Seatrout FIM Data")

library(haven)
#Appalachicola
AP_C  <- data.frame(read_sas("apm_cn_c.sas7bdat"))
AP_Hab <- read_sas("apm_cn_hab.sas7bdat")
AP_Hyd <- read_sas("apm_cn_hyd.sas7bdat")
AP_L <- read_sas("apm_cn_l.sas7bdat")

#Charlotte Harbor
CH_C <- read_sas("chm_cn_c.sas7bdat")
CH_Hab <- read_sas("chm_cn_hab.sas7bdat")
CH_Hyd <- read_sas("chm_cn_hyd.sas7bdat")
CH_L <- read_sas("chm_cn_l.sas7bdat")

#Cedar Key
CK_C <- read_sas("ckm_cn_c.sas7bdat")
CK_Hab <- read_sas("ckm_cn_hab.sas7bdat")
CK_Hyd <- read_sas("ckm_cn_hyd.sas7bdat")
CK_L <- read_sas("ckm_cn_l.sas7bdat")

#Indian River (Northern Indian River Lagoon???)
IR_C <- read_sas("irm_cn_c.sas7bdat")
IR_Hab <- read_sas("irm_cn_hab.sas7bdat")
IR_Hyd <- read_sas("irm_cn_hyd.sas7bdat")
IR_L <- read_sas("irm_cn_l.sas7bdat")

#JAX
JX_C <- read_sas("jxm_cn_c.sas7bdat")
JX_Hab <- read_sas("jxm_cn_hab.sas7bdat")
JX_Hyd <- read_sas("jxm_cn_hyd.sas7bdat")
JX_L <- read_sas("jxm_cn_l.sas7bdat")

#Tampa Bay
TB_C <- read_sas("tbm_cn_c.sas7bdat")
TB_Hab <- read_sas("tbm_cn_hab.sas7bdat")
TB_Hyd <- read_sas("tbm_cn_hyd.sas7bdat")
TB_L <- read_sas("tbm_cn_l.sas7bdat")

#Tequesta (Southern Indian River Lagoon??- index no calculated becuase 21.3m seines were not included in sampling)
TQM_C <- read_sas("tqm_cn_c.sas7bdat")
TQM_Hab <- read_sas("tqm_cn_hab.sas7bdat")
TQM_Hyd <- read_sas("tqm_cn_hyd.sas7bdat")
TQM_L <- read_sas("tqm_cn_l.sas7bdat")


#########################################################
# Explore aspects of each data set
###########
 
head()
melt()
unique()


#########################################################
# Constraints to Data sets
# _L$sl => between 0-100 mm (YOY animals in this range)
# _C$gear => 20 & 23 (bay seines and river seines that target YOY)
# _C$month => depends on recruitment window in each estuary
#               => Jax 5<=x<=11
#               => nor. IRL 5<=x<=11
#               => CK  5<=x<=11
#               => TB  4<=x<=10
#               => CH  4<=x<=10
#               => AP  6<=x<=10
# _C$Zone => depends on each estuary (for a more in depth description of monthly sampling within bay or riverine see Annual Report)
#               => TB BAY(A-E), RIVERINE (K-N)
#               => CH BAY(A-D), RIVERINE (M-P)
#               => nor.IRL BAY(A-E, H), RIVERINE (F)
#               => AP BAY(A-B), RIVERINE (C)
#               => Jax RIVERINE (A-F)
#               => CK BAY(B-C), RIVERINE (F)


########################################################
# Apply constraints, Project 1= AM (long term monitoring)
########################################################
#apl <- subset(AP_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
# I don't think I need to merge the length because selecting the 
#gear is an automatic filter for any fish larger than 100m, I believe. 
 
apc_bay <- subset(AP_C, ( gr ==20 | gr==19) & month >=6 & month <= 10 & (Zone == "A" | Zone == "B"))
#must include the gear constraints or else large seines might be included
apc_riv <- subset(AP_C, (gr==23   ) & month >=6 & month <= 10 & (Zone == "C"))                     
                     

chc_bay <- subset(CH_C, ( gr==20 | gr==19) & month >= 4 & month <= 10 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D"))
chc_riv <- subset(CH_C, (gr==23 ) & month >= 4 & month <= 10 & (Zone == "M" | Zone== "P"))                     

ckc_bay <- subset(CK_C, ( gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "B" | Zone == "C"))
ckc_riv <- subset(CK_C, (gr==23 ) & month >= 5 & month <= 11 & Zone == "F")                     

irc_bay <- subset(IR_C, ( gr==20 | gr ==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H"))
irc_riv <- subset(IR_C, (gr==23 ) & month >= 5 & month <= 11 & Zone == "F")                     


jxc_riv <- subset(JX_C, (gr==23 ) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone== "C" | Zone=="D" | Zone=="E"  | Zone == "F") )                     

tbc_bay <- subset(TB_C, ( gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E") )
tbc_riv <- subset(TB_C, (gr==23 ) & month >= 5 & month <= 11 & (Zone == "K" | Zone == "L" | Zone == "M" | Zone== "N") )                     
# apparantly gear==19 was also included as a bay gear


###################################################################
# Seperate by Zone within Estuary
##########################################
AP_BAY_A <- subset(apc_bay, Zone=="A")
AP_BAY_B <- subset(apc_bay, Zone=="B")

AP_RIV_C <- subset(apc_riv, Zone=="C")

CH_BAY_A <- subset(chc_bay, Zone=="A")
CH_BAY_B <- subset(chc_bay, Zone=="B")
CH_BAY_C <- subset(chc_bay, Zone=="C")
CH_BAY_D <- subset(chc_bay, Zone=="D")

CH_RIV_M <- subset(chc_riv, Zone=="M")
CH_RIV_P <- subset(chc_riv, Zone=="P")

CK_BAY_B <- subset(ckc_bay, Zone=="B")
CK_BAY_C <- subset(ckc_bay, Zone=="C")

CK_RIV_F <- subset(ckc_riv, Zone=="F")


IR_BAY_A <- subset(irc_bay, Zone=="A")
IR_BAY_B <- subset(irc_bay, Zone=="B")
IR_BAY_C <- subset(irc_bay, Zone=="C")
IR_BAY_D <- subset(irc_bay, Zone=="D")
IR_BAY_E <- subset(irc_bay, Zone=="E")
IR_BAY_H <- subset(irc_bay, Zone=="H")

IR_RIV_F <- subset(irc_riv, Zone=="F")

JX_RIV_A <- subset(jxc_riv, Zone=="A")
JX_RIV_B <- subset(jxc_riv, Zone=="B")
JX_RIV_C <- subset(jxc_riv, Zone=="C")
JX_RIV_D <- subset(jxc_riv, Zone=="D")
JX_RIV_E <- subset(jxc_riv, Zone=="E")
JX_RIV_F <- subset(jxc_riv, Zone=="F")

TB_BAY_A <- subset(tbc_bay, Zone=="A")
TB_BAY_B <- subset(tbc_bay, Zone=="B")
TB_BAY_C <- subset(tbc_bay, Zone=="C")
TB_BAY_D <- subset(tbc_bay, Zone=="D")
TB_BAY_E <- subset(tbc_bay, Zone=="E")

TB_RIV_K <- subset(tbc_riv, Zone=="K")
TB_RIV_L <- subset(tbc_riv, Zone=="L")
TB_RIV_M <- subset(tbc_riv, Zone=="M")
TB_RIV_N <- subset(tbc_riv, Zone=="N")


########## 
# Make sure total numbers from all months are the same as reported in the FWRI data report
##########
# all numbers are verified except for Indian River 10/06/2015
library(plyr)
# Tampa Bay#
TBUn <- subset(TB_C, !duplicated(Reference))
tbc_bayFWRItest <- subset(TBUn, (gr==20 | gr==19)) # equivalent to Gear==20gear can also be reported as gr==19 so I need to include 19 in the bay seines ,  select=c(Reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )  #& ( Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E"), select=c(Reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
TBBtest <- ddply(tbc_bayFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

tbc_rivFWRItest <- subset(TBUn, Gear==23)
TBRtest <- ddply(tbc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

# Jax #
JXUn <- subset(JX_C, !duplicated(Reference))
jxc_rivFWRItest <- subset(JXUn, Gear==23)
JXRtest <- ddply(jxc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

# Indian River #
IRUn <- subset(IR_C, !duplicated(Reference))
year14 <- subset(IRUn, year==2014, select=c(year, number, n, Reference, Gear, gr))
irc_rivFWRItest <- subset(IRUn, Gear==23)
IRRtest <- ddply(irc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

irc_bayFWRItest <- subset(IRUn,  gear)
IRBtest <- ddply(irc_bayFWRItest, c("year"), summarise, NumberofUniqueBioReferences=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

# Cedar Key ##
CKUn <- subset(CK_C, !duplicated(Reference))
ckc_bayFWRItest <- subset(CKUn, Gear==20 )
CKBtest <- ddply(ckc_bayFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

ckc_rivFWRItest <- subset(CK_C, Gear==23)
CKRtest <- ddply(ckc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

# Appalachicola ## 
APUn <-subset(AP_C, !duplicated(Reference))
apc_bayFWRItest <- subset(APUn, Gear==20)
APBtest <- ddply(apc_bayFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

apc_rivFWRItest <- subset(APUn, Gear==23)
APRtest <- ddply(apc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

# Charlotte Harbor #
CHUn <- subset(CH_C, !duplicated(Reference))
chc_bayFWRItest <- subset(CHUn, Gear==20)
CHBtest <- ddply(chc_bayFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

chc_rivFWRItest <- subset(CHUn, Gear ==23)
CHRtest <- ddply(chc_rivFWRItest, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))



##################################################
## Evaluting zones not covered in the 2014 annual report ####
##################################################

apzone <- subset(AP_C, Zone== "D")
unique(apzone$year)
irzone <- subset(IR_C, Zone=="O" | Zone == "G")
unique(irzone$year)

chzone <- subset(CH_C, Zone=="G" | Zone== "E" | Zone == "H")
unique(chzone$year)




########################################################################################################
# To plot absolute numbers caught in each month I must determine the total number of species collected in each haul 
#. Each haul is represented as a unique (Reference). The total number of particular species collected in each
# haul is "n" or "number". Thusly,  each unique Reference has an associated ("n", "number"). 
# Therefore I will capture unique (Un) Reference and then use the N or Number to determine numbers. 
########################################################################################################

AP_BAY_AUn <- subset(AP_BAY_A, !duplicated(Reference))
  write.csv(AP_BAY_AUn, "AP_A_coordinates.csv")
AP_BAY_BUn <- subset(AP_BAY_B, !duplicated(Reference))
  write.csv(AP_BAY_BUn, "AP_B_coordinates.csv")
AP_RIV_CUn <- subset(AP_RIV_C, !duplicated(Reference))
  write.csv(AP_BAY_CUn, "AP_C_coordinates.csv")

CH_BAY_AUn <- subset(CH_BAY_A, !duplicated(Reference))
  write.csv(CH_BAY_AUn, "CH_A_coordinates.csv")
CH_BAY_BUn <- subset(CH_BAY_B, !duplicated(Reference))
  write.csv(CH_BAY_BUn, "CH_B_coordinates.csv")
CH_BAY_CUn <- subset(CH_BAY_C, !duplicated(Reference))
  write.csv(CH_BAY_CUn, "CH_C_coordinates.csv")
CH_BAY_DUn <- subset(CH_BAY_D, !duplicated(Reference))
  write.csv(CH_BAY_DUn, "CH_D_coordinates.csv")
CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(Reference))
  write.csv(CH_RIV_MUn, "CH_M_coordinates.csv")
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(Reference))
  write.csv(CH_RIV_PUn, "CH_P_coordinates.csv")

CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(Reference))
  write.csv(CK_BAY_BUn, "CK_B_coordinates.csv")
CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(Reference))
  write.csv(CK_BAY_CUn, "CK_C_coordinates.csv")
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(Reference))
  write.csv(CK_RIV_FUn, "CK_F_coordinates.csv")

IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(Reference))
  write.csv(IR_BAY_AUn, "IR_A_coordinates.csv")
IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(Reference))
  write.csv(IR_BAY_BUn, "IR_B_coordinates.csv")
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(Reference))
  write.csv(IR_BAY_CUn, "IR_C_coordinates.csv")
IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(Reference))
  write.csv(IR_BAY_DUn, "IR_D_coordinates.csv")
IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(Reference))
  write.csv(IR_BAY_EUn, "IR_E_coordinates.csv")
IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(Reference))
  write.csv(IR_BAY_HUn, "IR_H_coordinates.csv")
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(Reference))
  write.csv(IR_RIV_FUn, "IR_F_coordinates.csv")

JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(Reference))
  write.csv(JX_RIV_AUn, "JX_A_coordinates.csv")
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(Reference))
  write.csv(JX_RIV_BUn, "JX_B_coordinates.csv")
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(Reference))
  write.csv(JX_RIV_CUn, "JX_C_coordinates.csv")
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(Reference))
  write.csv(JX_RIV_DUn, "JX_D_coordinates.csv")
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(Reference))
  write.csv(JX_RIV_EUn, "JX_E_coordinates.csv")
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(Reference))
  write.csv(JX_RIV_FUn, "JX_F_coordinates.csv")

TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(Reference))
  write.csv(TB_BAY_AUn, "TB_A_coordinates.csv")
TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(Reference))
  write.csv(TB_BAY_BUn, "TB_B_coordinates.csv")
TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(Reference))
  write.csv(TB_BAY_CUn, "TB_C_coordinates.csv")
TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(Reference))
  write.csv(TB_BAY_DUn, "TB_D_coordinates.csv")
TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(Reference))
  write.csv(TB_BAY_EUn, "TB_E_coordinates.csv")
TB_RIV_KUn <- subset(TB_RIV_K, !duplicated(Reference))
  write.csv(TB_RIV_KUn, "TB_K_coordinates.csv")
TB_RIV_LUn <- subset(TB_RIV_L, !duplicated(Reference))
  write.csv(TB_RIV_LUn, "TB_L_coordinates.csv")
TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
  write.csv(TB_RIV_MUn, "TB_M_coordinates.csv")
TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
  write.csv(TB_RIV_NUn, "TB_N_coordinates.csv")

#############################################################
# Summarise the data. 
# I've chosed to summarize the data by year and month. Number of References is the number of different References (aka Hauls)being summarized by month of each year. 
# TotalNumberofAnimalsCollectedinHauls in the summation of the total number of animals collected in the hauls for month by year. 
# MedianNumberofAnimas, MeanNumberofAnimals
# APB_Asum = Appachicola Bay _ zone A (summarised)
# APR_Csum = Appalachicol (River) _ zone C (summarised)
##############################################################################################################
library(plyr)
APB_Asum <- ddply(AP_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_Bsum <- ddply(AP_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APR_Csum <- ddply(AP_RIV_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

CHB_Asum <- ddply(CH_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Bsum <- ddply(CH_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Csum <- ddply(CH_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Dsum <- ddply(CH_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Msum <- ddply(CH_RIV_MUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Psum <- ddply(CH_RIV_PUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

CKB_Bsum <- ddply(CK_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_Csum <- ddply(CK_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKR_Fsum <- ddply(CK_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

IRB_Asum <- ddply(IR_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Bsum <- ddply(IR_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Csum <- ddply(IR_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Dsum <- ddply(IR_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Esum <- ddply(IR_BAY_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Hsum <- ddply(IR_BAY_HUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRR_Fsum <- ddply(IR_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

JXR_Asum <- ddply(JX_RIV_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Bsum <- ddply(JX_RIV_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Csum <- ddply(JX_RIV_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Dsum <- ddply(JX_RIV_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Esum <- ddply(JX_RIV_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Fsum <- ddply(JX_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TBB_Asum <- ddply(TB_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Bsum <- ddply(TB_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Csum <- ddply(TB_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Dsum <- ddply(TB_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Esum <- ddply(TB_BAY_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TBR_Ksum <- ddply(TB_RIV_KUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Lsum <- ddply(TB_RIV_LUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Msum <- ddply(TB_RIV_MUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Nsum <- ddply(TB_RIV_NUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))





################################################################
# Use raw catch data to make a time series of raw abundance index. 
# First, by Zone.
#   - > sum abundance over all months of recruitment season
#   - > chose month with most recruitment (Not sure about this step)
# Then, by shore and offshore (veg and non veg). ###  SEE BELOW ###
#   - > sum abundance over all months of recruitment season
#   - > chose month with most recruitment


## Tampa Bay ##
  # Bay #
TBB_A_sumrec <- ddply(TBB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls) )
TBB_A_sumrec$CPUE <- TBB_A_sumrec$TotalCollected/TBB_A_sumrec$TotalNumberofHauls
library(ggplot2)
  plot_TBB_A_sumrec <- ggplot(TBB_A_sumrec, aes(x=year, y=CPUE))+ geom_line() + geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone A")


write.csv(TBB_A_sumrec, "TampaBay_Bay_A_sumrec.csv")
  
TBB_B_sumrec <- ddply(TBB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_B_sumrec$CPUE <- TBB_B_sumrec$TotalCollected/TBB_B_sumrec$TotalNumberofHauls
  plot_TBB_B_sumrec <- ggplot(TBB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(TBB_B_sumrec, "TampaBay_Bay_B_sumrec.csv")

TBB_C_sumrec <- ddply(TBB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_C_sumrec$CPUE <- TBB_C_sumrec$TotalCollected/TBB_C_sumrec$TotalNumberofHauls
  plot_TBB_C_sumrec <- ggplot(TBB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone C")
write.csv(TBB_C_sumrec, "TampaBay_Bay_C_sumrec.csv")

TBB_D_sumrec <- ddply(TBB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_D_sumrec$CPUE <- TBB_D_sumrec$TotalCollected/TBB_D_sumrec$TotalNumberofHauls
  plot_TBB_D_sumrec <- ggplot(TBB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone D")
write.csv(TBB_D_sumrec, "TampaBay_Bay_D_sumrec.csv")

TBB_E_sumrec <- ddply(TBB_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_E_sumrec$CPUE <- TBB_E_sumrec$TotalCollected/TBB_E_sumrec$TotalNumberofHauls
plot_TBB_E_sumrec <- ggplot(TBB_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone E")
write.csv(TBB_E_sumrec, "TampaBay_Bay_E_sumrec.csv")
          ###### Add in the Multiplot Function ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


          ## Plot ##
TBB_ZONE_sumrec_multiplot <- multiplot(plot_TBB_A_sumrec, plot_TBB_B_sumrec, plot_TBB_C_sumrec, plot_TBB_D_sumrec, plot_TBB_E_sumrec,cols=2)


  # River #
TBR_K_sumrec <- ddply(TBR_Ksum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_K_sumrec$CPUE <- TBR_K_sumrec$TotalCollected/TBR_K_sumrec$TotalNumberofHauls
  plot_TBR_K_sumrec <- ggplot(TBR_K_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone K")
write.csv(TBR_K_sumrec, "TampaBay_Riv_K_sumrec.csv")

TBR_L_sumrec <- ddply(TBR_Lsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_L_sumrec$CPUE <- TBR_L_sumrec$TotalCollected/TBR_L_sumrec$TotalNumberofHauls
  plot_TBR_L_sumrec <- ggplot(TBR_L_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone L")

write.csv(TBR_L_sumrec, "TampaBay_Riv_L_sumrec.csv")



TBR_M_sumrec <- ddply(TBR_Msum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_M_sumrec$CPUE <- TBR_M_sumrec$TotalCollected/TBR_M_sumrec$TotalNumberofHauls
  plot_TBR_M_sumrec <- ggplot(TBR_M_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone M")

write.csv(TBR_M_sumrec, "TampaBay_Riv_M_sumrec.csv")

TBR_N_sumrec <- ddply(TBR_Nsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_N_sumrec$CPUE <- TBR_N_sumrec$TotalCollected/TBR_N_sumrec$TotalNumberofHauls
  plot_TBR_N_sumrec <- ggplot(TBR_N_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone N")
write.csv(TBR_N_sumrec, "TampaBay_Riv_N_sumrec.csv")


TBR_ZONE_sumrec_multiplot <- multiplot(plot_TBR_K_sumrec, plot_TBR_L_sumrec,plot_TBR_M_sumrec,plot_TBR_N_sumrec, cols=2)

## JAX ##
JXR_A_sumrec <- ddply(JXR_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_A_sumrec$CPUE <- JXR_A_sumrec$TotalCollected/JXR_A_sumrec$TotalNumberofHauls
  plot_JXR_A_sumrec <- ggplot(JXR_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone A")
write.csv(JXR_A_sumrec, "Jax_Riv_A_sumrec.csv")


JXR_B_sumrec <- ddply(JXR_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_B_sumrec$CPUE <- JXR_B_sumrec$TotalCollected/JXR_B_sumrec$TotalNumberofHauls
  plot_JXR_B_sumrec <- ggplot(JXR_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(JXR_B_sumrec, "Jax_Riv_B_sumrec.csv")


JXR_C_sumrec <- ddply(JXR_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_C_sumrec$CPUE <- JXR_C_sumrec$TotalCollected/JXR_C_sumrec$TotalNumberofHauls
  plot_JXR_C_sumrec <- ggplot(JXR_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone C")

write.csv(JXR_C_sumrec, "Jax_Riv_C_sumrec.csv")


JXR_D_sumrec <- ddply(JXR_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_D_sumrec$CPUE <- JXR_D_sumrec$TotalCollected/JXR_D_sumrec$TotalNumberofHauls
  plot_JXR_D_sumrec <- ggplot(JXR_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone D")
write.csv(JXR_D_sumrec, "Jax_Riv_D_sumrec.csv")



JXR_E_sumrec <- ddply(JXR_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_E_sumrec$CPUE <- JXR_E_sumrec$TotalCollected/JXR_E_sumrec$TotalNumberofHauls
  plot_JXR_E_sumrec <- ggplot(JXR_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone E")
write.csv(JXR_E_sumrec, "Jax_Riv_E_sumrec.csv")



JXR_F_sumrec <- ddply(JXR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_F_sumrec$CPUE <- JXR_F_sumrec$TotalCollected/JXR_F_sumrec$TotalNumberofHauls
  plot_JXR_F_sumrec <- ggplot(JXR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone F")
write.csv(JXR_F_sumrec, "Jax_Riv_F_sumrec.csv")


multiplot(plot_JXR_A_sumrec,plot_JXR_B_sumrec,plot_JXR_C_sumrec,plot_JXR_D_sumrec,plot_JXR_E_sumrec,plot_JXR_F_sumrec, cols=2) 


## INDIAN RIVER ##
# - Stations A, B, and E were onl sampled only in October and November
IRB_A_sumrec <- ddply(IRB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_A_sumrec$CPUE <- IRB_A_sumrec$TotalCollected/IRB_A_sumrec$TotalNumberofHauls
  plot_IRB_A_sumrec <- ggplot(IRB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone A")
write.csv(IRB_A_sumrec, "IndianRiver_Bay_A_sumrec.csv")


IRB_B_sumrec <- ddply(IRB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_B_sumrec$CPUE <- IRB_B_sumrec$TotalCollected/IRB_B_sumrec$TotalNumberofHauls
  plot_IRB_B_sumrec <- ggplot(IRB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(IRB_B_sumrec, "IndianRiver_Bay_B_sumrec.csv")

IRB_C_sumrec <- ddply(IRB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_C_sumrec$CPUE <- IRB_C_sumrec$TotalCollected/IRB_C_sumrec$TotalNumberofHauls
  plot_IRB_C_sumrec <- ggplot(IRB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone C")
write.csv(IRB_C_sumrec, "IndianRiver_Bay_C_sumrec.csv")

IRB_D_sumrec <- ddply(IRB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_D_sumrec$CPUE <- IRB_D_sumrec$TotalCollected/IRB_D_sumrec$TotalNumberofHauls
  plot_IRB_D_sumrec <- ggplot(IRB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone D")
write.csv(IRB_D_sumrec, "IndianRiver_Bay_D_sumrec.csv")

IRB_E_sumrec <- ddply(IRB_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_E_sumrec$CPUE <- IRB_E_sumrec$TotalCollected/IRB_E_sumrec$TotalNumberofHauls
  plot_IRB_E_sumrec <- ggplot(IRB_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone E")
write.csv(IRB_E_sumrec, "IndianRiver_Bay_E_sumrec.csv")


IRB_H_sumrec <- ddply(IRB_Hsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_H_sumrec$CPUE <- IRB_H_sumrec$TotalCollected/IRB_H_sumrec$TotalNumberofHauls
  plot_IRB_H_sumrec <- ggplot(IRB_H_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone H")
write.csv(IRB_H_sumrec, "IndianRiver_Bay_H_sumrec.csv")


IRR_F_sumrec <- ddply(IRR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRR_F_sumrec$CPUE <- IRR_F_sumrec$TotalCollected/IRR_F_sumrec$TotalNumberofHauls
  plot_IRR_F_sumrec <- ggplot(IRR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone F")
write.csv(IRR_F_sumrec, "IndianRiver_Riv_F_sumrec.csv")


multiplot(plot_IRB_A_sumrec,plot_IRB_B_sumrec,plot_IRB_C_sumrec,plot_IRB_D_sumrec,plot_IRB_E_sumrec,plot_IRB_H_sumrec,plot_IRR_F_sumrec, cols=3) 


## Cedar Key ##

CKB_B_sumrec <- ddply(CKB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKB_B_sumrec$CPUE <- CKB_B_sumrec$TotalCollected/CKB_B_sumrec$TotalNumberofHauls
  plot_CKB_B_sumrec <- ggplot(CKB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(CKB_B_sumrec, "CedarKey_Bay_B_sumrec.csv")


CKB_C_sumrec <- ddply(CKB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKB_C_sumrec$CPUE <- CKB_C_sumrec$TotalCollected/CKB_C_sumrec$TotalNumberofHauls
  plot_CKB_C_sumrec <- ggplot(CKB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone C")
write.csv(CKB_C_sumrec, "CedarKey_Bay_C_sumrec.csv")


CKR_F_sumrec <- ddply(CKR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKR_F_sumrec$CPUE <- CKR_F_sumrec$TotalCollected/CKR_F_sumrec$TotalNumberofHauls
  plot_CKR_F_sumrec <- ggplot(CKR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone F")
write.csv(CKR_F_sumrec, "CedarKey_Riv_F_sumrec.csv")


multiplot(plot_CKB_B_sumrec,plot_CKB_C_sumrec,plot_CKR_F_sumrec, cols=1)


## Appilachicola ##

APB_A_sumrec <- ddply(APB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APB_A_sumrec$CPUE <- APB_A_sumrec$TotalCollected/APB_A_sumrec$TotalNumberofHauls
  plot_APB_A_sumrec <- ggplot(APB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone A")
write.csv(APB_A_sumrec, "Appalachicola_Bay_A_sumrec.csv")


APB_B_sumrec <- ddply(APB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APB_B_sumrec$CPUE <- APB_B_sumrec$TotalCollected/APB_B_sumrec$TotalNumberofHauls
  plot_APB_B_sumrec <- ggplot(APB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(APB_B_sumrec, "Appalachicola_Bay_B_sumrec.csv")


APR_C_sumrec <- ddply(APR_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APR_C_sumrec$CPUE <- APR_C_sumrec$TotalCollected/APR_C_sumrec$TotalNumberofHauls
plot_APR_C_sumrec <- ggplot(APR_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone C")
write.csv(APR_C_sumrec, "Appalachicola_Riv_C_sumrec.csv")


multiplot(plot_APB_A_sumrec,plot_APB_B_sumrec,plot_APR_C_sumrec, cols=1)

## Charlotte Harbor ##

CHB_A_sumrec <- ddply(CHB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_A_sumrec$CPUE <- CHB_A_sumrec$TotalCollected/CHB_A_sumrec$TotalNumberofHauls
  plot_CHB_A_sumrec <- ggplot(CHB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone A")
write.csv(CHB_A_sumrec, "CharlotteHarbor_Bay_A_sumrec.csv")


CHB_B_sumrec <- ddply(CHB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_B_sumrec$CPUE <- CHB_B_sumrec$TotalCollected/CHB_B_sumrec$TotalNumberofHauls
  plot_CHB_B_sumrec <- ggplot(CHB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone B")
write.csv(CHB_B_sumrec, "CharlotteHarbor_Bay_B_sumrec.csv")


CHB_C_sumrec <- ddply(CHB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_C_sumrec$CPUE <- CHB_C_sumrec$TotalCollected/CHB_C_sumrec$TotalNumberofHauls
  plot_CHB_C_sumrec <- ggplot(CHB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone C")
write.csv(CHB_C_sumrec, "CharlotteHarbor_Bay_C_sumrec.csv")


CHB_D_sumrec <- ddply(CHB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_D_sumrec$CPUE <- CHB_D_sumrec$TotalCollected/CHB_D_sumrec$TotalNumberofHauls
  plot_CHB_D_sumrec <- ggplot(CHB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone D")
write.csv(CHB_D_sumrec, "CharlotteHarbor_Bay_D_sumrec.csv")


CHR_M_sumrec <- ddply(CHR_Msum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHR_M_sumrec$CPUE <- CHR_M_sumrec$TotalCollected/CHR_M_sumrec$TotalNumberofHauls
  plot_CHR_M_sumrec <- ggplot(CHR_M_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                      xlab("Year")+ ylab("CPUE C.neb")+
                      scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                      panel.background=element_rect(fill='white', colour='black'))+
                      ggtitle( "Zone M")
write.csv(CHR_M_sumrec, "CharlotteHarbor_Riv_M_sumrec.csv")



CHR_P_sumrec <- ddply(CHR_Psum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHR_P_sumrec$CPUE <- CHR_P_sumrec$TotalCollected/CHR_P_sumrec$TotalNumberofHauls
  plot_CHR_P_sumrec <- ggplot(CHR_P_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
                    xlab("Year")+ ylab("CPUE C.neb")+
                    scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
                    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                    panel.background=element_rect(fill='white', colour='black'))+
                    ggtitle( "Zone P")
write.csv(CHR_P_sumrec, "CharlotteHarbor_Riv_P_sumrec.csv")


multiplot(plot_CHB_A_sumrec,plot_CHB_B_sumrec,plot_CHB_C_sumrec,plot_CHB_D_sumrec,plot_CHR_M_sumrec,plot_CHR_P_sumrec, cols=2)










###########################################################
## Partition the data further by shore and offshore. Can use previously defined dataframes.  
# By each Estuary
#
#  -> Bay Zones
#    -> Zone
#       -> Shore
#       -> Offshore
#         -> Veg
#         -> Unveg
#  -> River Zones
#    -> Zone
#       -> Over
#       -> Nonover
##########################################################
### THESE VARIABLES NEED TO BE CHECKED. I DONT THINK IM
# SUBSETTING BY THE CORRECT VARIABLES.

## Perhaps is this because they havent sampled in zone A in a while...??
# Check on unique values for bveg
# Sum by zone and compare to the annual report to figure out correct variables. 



##  Tampa Bay ### 

tbc_bay_un_2014 <- subset(TB_C, (gr==20 | gr==19) & !duplicated(Reference) & year==2014)
  tbc_shore <- subset(tbc_bay_un_2014, ShoreDistance <=5 | ShoreDistance == 99)
  tbc_shore_sum <- ddply(tbc_shore, c("year"),summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number)) 
    

# Bay #
TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(Reference))
  TBBA_Shore <- subset(TB_BAY_AUn, ShoreDistance <= 5)
  TBBA_Off_Veg <-subset(TB_BAY_AUn, ShoreDistance > 5 & (bveg = "SAV" | bveg = " Algea" | bveg = "Other"))
  TBBA_Off_NonVeg <- subset(TB_BAY_AUn, ShoreDistance > 5 & (bveg=="None")) 

          # Summarised #
      TBBA_Shore_sum <- ddply(TBBA_Shore, c("year"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBA_Off_Veg_sum <- ddply(TBBA_Off_Veg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBA_Off_NonVeg_sum <- ddply(TBBA_Off_NonVeg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(Reference))
  TBBB_Shore <- subset(TB_BAY_BUn, ShoreDistance <= 5)
  TBBB_Off_Veg <-subset(TB_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBB_Off_NonVeg <- subset(TB_BAY_BUn, ShoreDistance >5 & (bveg =="None"))

          # Summarised #
      TBBB_Shore_sum <- ddply(TBBB_Shore, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBB_Off_Veg_sum <- ddply(TBBB_Off_Veg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBB_Off_NonVeg_sum <- ddply(TBBB_Off_NonVeg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(Reference))
  TBBC_Shore <- subset(TB_BAY_CUn, ShoreDistance <= 5)
  TBBC_Off_Veg <-subset(TB_BAY_CUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBC_Off_NonVeg <- subset(TB_BAY_CUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

          # Summarised #
      TBBC_Shore_sum <- ddply(TBBC_Shore, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBC_Off_Veg_sum <- ddply(TBBC_Off_Veg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBC_Off_NonVeg_sum <- ddply(TBBC_Off_NonVeg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(Reference))
  TBBD_Shore <- subset(TB_BAY_DUn, ShoreDistance <=5)
  TBBD_Off_Veg <-subset(TB_BAY_DUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBD_Off_NonVeg <- subset(TB_BAY_DUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

          # Summarised #
      TBBD_Shore_sum <- ddply(TBBD_Shore, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBD_Off_Veg_sum <- ddply(TBBD_Off_Veg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBD_Off_NonVeg_sum <- ddply(TBBD_Off_NonVeg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(Reference))
  TBBE_Shore <- subset(TB_BAY_EUn, ShoreDistance <=5)
  TBBE_Off_Veg <-subset(TB_BAY_EUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBE_Off_NonVeg <- subset(TB_BAY_EUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

          # Summarised #
      TBBE_Shore_sum <- ddply(TBBE_Shore, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBE_Off_Veg_sum <- ddply(TBBE_Off_Veg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
      TBBE_Off_NonVeg_sum <- ddply(TBBE_Off_NonVeg, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))


    # River # 
TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
TB_RIV_OUn <- subset(TB_RIV_O, !duplicated(Reference))
TB_RIV_PUn <- subset(TB_RIV_P, !duplicated(Reference))


## Appalachicola ##
    # Bay #
AP_BAY_AUn <- subset(AP_BAY_A, !duplicated(Reference))
  APBA_Shore <- subset(AP_BAY_AUn, ShoreDistance <=5)
  APBA_Off_Veg <-subset(AP_BAY_AUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  APBA_Off_NonVeg <- subset(AP_BAY_AUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

AP_BAY_BUn <- subset(AP_BAY_B, !duplicated(Reference))
  APBB_Shore <- subset(AP_BAY_BUn, ShoreDistance <=5)
  APBB_Off_Veg <-subset(AP_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  APBB_Off_NonVeg <- subset(AP_BAY_BUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

    # River #
AP_RIV_CUn <- subset(AP_RIV_C, !duplicated(Reference))


## Charlotte Harbor ##
    # Bay #
CH_BAY_AUn <- subset(CH_BAY_A, !duplicated(Reference))
  CHBA_Shore <- subset(CH_BAY_AUn, ShoreDistance <=5)
  CHBA_Off_Veg <-subset(CH_BAY_AUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CHBA_Off_NonVeg <- subset(CH_BAY_AUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

CH_BAY_BUn <- subset(CH_BAY_B, !duplicated(Reference))
  CHBB_Shore <- subset(CH_BAY_BUn, ShoreDistance <=5)
  CHBB_Off_Veg <-subset(CH_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CHBB_Off_NonVeg <- subset(CH_BAY_BUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

CH_BAY_CUn <- subset(CH_BAY_C, !duplicated(Reference))
  CHBC_Shore <- subset(CH_BAY_CUn, ShoreDistance <=5)
  CHBC_Off_Veg <-subset(CH_BAY_CUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CHBC_Off_NonVeg <- subset(CH_BAY_CUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

CH_BAY_DUn <- subset(CH_BAY_D, !duplicated(Reference))
  CHBD_Shore <- subset(CH_BAY_DUn, ShoreDistance <=5)
  CHBD_Off_Veg <-subset(CH_BAY_DUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CHBD_Off_NonVeg <- subset(CH_BAY_DUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

  # River #
CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(Reference))
CH_RIV_NUn <- subset(CH_RIV_N, !duplicated(Reference))
CH_RIV_OUn <- subset(CH_RIV_O, !duplicated(Reference))
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(Reference))


## Cedar Key ##
  # Bay #
CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(Reference))
  CKBB_Shore <- subset(CK_BAY_BUn, ShoreDistance <=5)
  CKBB_Off_Veg <-subset(CK_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CKBB_Off_NonVeg <- subset(CK_BAY_BUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(Reference))
  CKBC_Shore <- subset(CK_BAY_CUn, ShoreDistance <=5)
  CKBC_Off_Veg <-subset(CK_BAY_CUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  CKBC_Off_NonVeg <- subset(CK_BAY_CUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

  # River #
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(Reference))


## Northern Indian River ##
  # Bay #
IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(Reference))
  IRBA_Shore <- subset(IR_BAY_AUn, ShoreDistance <=5)
  IRBA_Off_Veg <-subset(IR_BAY_AUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBA_Off_NonVeg <- subset(IR_BAY_AUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(Reference))
  IRBB_Shore <- subset(IR_BAY_BUn, ShoreDistance <=5)
  IRBB_Off_Veg <-subset(IR_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBB_Off_NonVeg <- subset(IR_BAY_BUn, ShoreDistance >5 & (bveg =="None" | bveg = .))
  
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(Reference))
  IRBC_Shore <- subset(IR_BAY_CUn, ShoreDistance <=5)
  IRBC_Off_Veg <-subset(IR_BAY_CUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBC_Off_NonVeg <- subset(IR_BAY_CUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(Reference))
  IRBD_Shore <- subset(IR_BAY_DUn, ShoreDistance <=5)
  IRBD_Off_Veg <-subset(IR_BAY_DUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBD_Off_NonVeg <- subset(IR_BAY_DUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(Reference))
  IRBE_Shore <- subset(IR_BAY_EUn, ShoreDistance <=5)
  IRBE_Off_Veg <-subset(IR_BAY_EUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBE_Off_NonVeg <- subset(IR_BAY_EUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(Reference))
  IRBH_Shore <- subset(IR_BAY_HUn, ShoreDistance <=5)
  IRBH_Off_Veg <-subset(IR_BAY_HUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  IRBH_Off_NonVeg <- subset(IR_BAY_HUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

  # River #
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(Reference))

## Northeast Florida JAX ##
  # River #
JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(Reference))
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(Reference))
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(Reference))
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(Reference))
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(Reference))
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(Reference))






