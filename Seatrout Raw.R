# This script loads Spotted Seatrout raw survey data. They are stored as .sas7bdat at FWRI but I will be using
# haven to convert them and work with them in R
setwd("~/Desktop/Github Repo/Seatrout/Data/Raw Survey Data/Seatrout FIM Data")

library(haven)
#Appalachicola
AP_C  <- data.frame(read_sas("apm_cn_c.sas7bdat")) #catch data for C.neb
AP_Hab <- read_sas("apm_cn_hab.sas7bdat")         # habitat data for C.neb
AP_Hyd <- read_sas("apm_cn_hyd.sas7bdat")         # hydrolab data for C.neb
AP_L <- read_sas("apm_cn_l.sas7bdat")             # length data from the rep for C.neb
AP_P <- read_sas("apm_physical.sas7bdat")         # overall physical data 
  AP_C <-  merge(AP_C, subset(AP_P, select=c("Reference", "Stratum")), by="Reference")      # turn AP_C into the merged data set of AP_C and AP_P

#Charlotte Harbor
CH_C <- read_sas("chm_cn_c.sas7bdat")
CH_Hab <- read_sas("chm_cn_hab.sas7bdat")
CH_Hyd <- read_sas("chm_cn_hyd.sas7bdat")
CH_L <- read_sas("chm_cn_l.sas7bdat")
CH_P <- read_sas("chm_physical.sas7bdat") 
  CH_C <- merge(CH_C, subset(CH_P, select=c("Reference", "Stratum")), by="Reference")

#Cedar Key
CK_C <- read_sas("ckm_cn_c.sas7bdat")
CK_Hab <- read_sas("ckm_cn_hab.sas7bdat")
CK_Hyd <- read_sas("ckm_cn_hyd.sas7bdat")
CK_L <- read_sas("ckm_cn_l.sas7bdat")
CK_P <- read_sas("ckm_physical.sas7bdat")         
  CK_C <- merge(CK_C, subset(CK_P, select=c("Reference", "Stratum")), by="Reference")

#Indian River (Northern Indian River Lagoon???)
IR_C <- read_sas("irm_cn_c.sas7bdat")
IR_Hab <- read_sas("irm_cn_hab.sas7bdat")
IR_Hyd <- read_sas("irm_cn_hyd.sas7bdat")
IR_L <- read_sas("irm_cn_l.sas7bdat")
IR_P  <-read_sas("irm_physical.sas7bdat")        
  IR_C <- merge(IR_C, subset(IR_P, select=c("Reference", "Stratum")), by ="Reference")

#JAX
JX_C <- read_sas("jxm_cn_c.sas7bdat")
JX_Hab <- read_sas("jxm_cn_hab.sas7bdat")
JX_Hyd <- read_sas("jxm_cn_hyd.sas7bdat")
JX_L <- read_sas("jxm_cn_l.sas7bdat")
JX_P <- read_sas("jxm_physical.sas7bdat")
  JX_C <- merge(JX_C, subset(JX_P, select=c("Reference", "Stratum")), by="Reference")

#Tampa Bay
TB_C <- read_sas("tbm_cn_c.sas7bdat")
TB_Hab <- read_sas("tbm_cn_hab.sas7bdat")
TB_Hyd <- read_sas("tbm_cn_hyd.sas7bdat")
TB_L <- read_sas("tbm_cn_l.sas7bdat")
TB_P <- read_sas("tbm_physical.sas7bdat")
  TB_C <- merge(TB_C, subset(TB_P,select=c("Reference", "Stratum")), by="Reference")


#Tequesta (Southern Indian River Lagoon??- index no calculated becuase 21.3m seines were not included in sampling)
TQM_C <- read_sas("tqm_cn_c.sas7bdat")
TQM_Hab <- read_sas("tqm_cn_hab.sas7bdat")
TQM_Hyd <- read_sas("tqm_cn_hyd.sas7bdat")
TQM_L <- read_sas("tqm_cn_l.sas7bdat")
TQM_P <- read_sas("tqm_physical.sas7bdat")         



#########################################################
# Explore aspects of each data set
###########
 
head()
melt()
unique()


#########################################################
# Constraints to Data sets
# _L$sl => between 0-100 mm (YOY animals in this range)
# _C$Gear => 20 & 23 (bay seines and river seines that target YOY)
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
 
apc_bay <- subset(AP_C, Gear==20 & month >=6 & month <= 10 & (Zone == "A" | Zone == "B"))
#must include the gear constraints or else large seines might be included
apc_riv <- subset(AP_C, Gear==23 & month >=6 & month <= 10 & (Zone == "C"))                     
                     

chc_bay <- subset(CH_C, Gear==20 & month >= 4 & month <= 10 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D"))
chc_riv <- subset(CH_C, Gear==23 & month >= 4 & month <= 10 & (Zone == "M" | Zone== "P"))                     

ckc_bay <- subset(CK_C, Gear==20 & month >= 5 & month <= 11 & (Zone == "B" | Zone == "C"))
ckc_riv <- subset(CK_C, Gear==23 & month >= 5 & month <= 11 & Zone == "F")                     

irc_bay <- subset(IR_C, Gear==20 & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H"))
irc_riv <- subset(IR_C, Gear==23 & month >= 5 & month <= 11 & Zone == "F")                     


jxc_riv <- subset(JX_C, Gear==23 & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone== "C" | Zone=="D" | Zone=="E"  | Zone == "F") )                     

tbc_bay <- subset(TB_C, Gear==20 & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E") )
tbc_riv <- subset(TB_C, Gear==23 & month >= 5 & month <= 11 & (Zone == "K" | Zone == "L" | Zone == "M" | Zone== "N") )                     
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
  write.csv(AP_BAY_AUn, "AP_A.csv")
AP_BAY_BUn <- subset(AP_BAY_B, !duplicated(Reference))
  write.csv(AP_BAY_BUn, "AP_B.csv")
AP_RIV_CUn <- subset(AP_RIV_C, !duplicated(Reference))
  write.csv(AP_RIV_CUn, "AP_C.csv")

CH_BAY_AUn <- subset(CH_BAY_A, !duplicated(Reference))
  write.csv(CH_BAY_AUn, "CH_A.csv")
CH_BAY_BUn <- subset(CH_BAY_B, !duplicated(Reference))
  write.csv(CH_BAY_BUn, "CH_B.csv")
CH_BAY_CUn <- subset(CH_BAY_C, !duplicated(Reference))
  write.csv(CH_BAY_CUn, "CH_C.csv")
CH_BAY_DUn <- subset(CH_BAY_D, !duplicated(Reference))
  write.csv(CH_BAY_DUn, "CH_D.csv")
CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(Reference))
  write.csv(CH_RIV_MUn, "CH_M.csv")
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(Reference))
  write.csv(CH_RIV_PUn, "CH_P.csv")

CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(Reference))
  write.csv(CK_BAY_BUn, "CK_B.csv")
CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(Reference))
  write.csv(CK_BAY_CUn, "CK_C.csv")
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(Reference))
  write.csv(CK_RIV_FUn, "CK_F.csv")

IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(Reference))
  write.csv(IR_BAY_AUn, "IR_A.csv")
IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(Reference))
  write.csv(IR_BAY_BUn, "IR_B.csv")
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(Reference))
  write.csv(IR_BAY_CUn, "IR_C.csv")
IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(Reference))
  write.csv(IR_BAY_DUn, "IR_D.csv")
IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(Reference))
  write.csv(IR_BAY_EUn, "IR_E.csv")
IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(Reference))
  write.csv(IR_BAY_HUn, "IR_H.csv")
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(Reference))
  write.csv(IR_RIV_FUn, "IR_F.csv")

JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(Reference))
  write.csv(JX_RIV_AUn, "JX_A.csv")
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(Reference))
  write.csv(JX_RIV_BUn, "JX_B.csv")
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(Reference))
  write.csv(JX_RIV_CUn, "JX_C.csv")
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(Reference))
  write.csv(JX_RIV_DUn, "JX_D.csv")
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(Reference))
  write.csv(JX_RIV_EUn, "JX_E.csv")
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(Reference))
  write.csv(JX_RIV_FUn, "JX_F.csv")

TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(Reference))
  write.csv(TB_BAY_AUn, "TB_A.csv")
TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(Reference))
  write.csv(TB_BAY_BUn, "TB_B.csv")
TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(Reference))
  write.csv(TB_BAY_CUn, "TB_C.csv")
TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(Reference))
  write.csv(TB_BAY_DUn, "TB_D.csv")
TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(Reference))
  write.csv(TB_BAY_EUn, "TB_E.csv")
TB_RIV_KUn <- subset(TB_RIV_K, !duplicated(Reference))
  write.csv(TB_RIV_KUn, "TB_K.csv")
TB_RIV_LUn <- subset(TB_RIV_L, !duplicated(Reference))
  write.csv(TB_RIV_LUn, "TB_L.csv")
TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
  write.csv(TB_RIV_MUn, "TB_M.csv")
TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
  write.csv(TB_RIV_NUn, "TB_N.csv")








