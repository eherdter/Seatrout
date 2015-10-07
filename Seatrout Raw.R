# This script loads Spotted Seatrout raw survy data. They are stored as .sas7bdat at FWRI but I will be using
# haven to convert them and work with them in R
setwd("~/Desktop/Github Repo/Seatrout/Data/Raw Survey Data/Seatrout FIM Data")

library(haven)
#Appalachicola
AP_C  <- data.frame(read_sas("apm_cn_c.sas7bdat"))
unique(AP_C$bveg)
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
# _C$month => depends on recruitment window in each eastuary
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
apl <- subset(AP_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
apc_bay <- subset(AP_C, (gr == 23 | gr ==20 | gr==19) & month >=6 & month <= 10 & (Zone == "A" | Zone == "B"))
#must include the gear constraints or else large seines might be included
apc_riv <- subset(AP_C, (gr==23   | gr ==20 | gr==19) & month >=6 & month <= 10 & (Zone == "C"))                     
                     

chl <- subset(CH_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
chc_bay <- subset(CH_C, (gr==23 | gr==20 | gr==19) & month >= 4 & month <= 10 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D"))
chc_riv <- subset(CH_C, (gr==23 | gr==20 | gr==19) & month >= 4 & month <= 10 & (Zone == "M" | Zone == "N" | Zone == "O" | Zone== "P"))                     


ckl <- subset(CK_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
ckc_bay <- subset(CK_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "B" | Zone == "C"))
ckc_riv <- subset(CK_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & Zone == "F")                     

irl <- subset(IR_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
irc_bay <- subset(IR_C, (gr==23 | gr==20) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H"))
irc_riv <- subset(IR_C, (gr==23 | gr==20) & month >= 5 & month <= 11 & Zone == "F")                     

jxl <- subset(JX_L, sl <= 100, select =c(Reference, sl, COUNT , nl))
jxc_riv <- subset(JX_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone== "C" | Zone=="D" | Zone=="E"  | Zone == "F") )                     

tbl <- subset(TB_L, sl <= 100, select =c(Reference, sl, COUNT, nl))
tbc_bay <- subset(TB_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E") )
tbc_riv <- subset(TB_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "K" | Zone == "L" | Zone == "M" | Zone== "N") )                     
# apparantly gear==19 was also included as a bay gear



############################################################
# Merge values from c_bay and c_riv into ##l so all values from the c_bay and c_riv can be retained 
# but only at the Reference numbers in the length variables. If I wanted to retain all values -> all=TRUE
############################################################

AP_BAY <- merge(apc_bay, apl, by="Reference")
unique(AP_BAY$Zone)
AP_RIV <- merge(apc_riv, apl, by="Reference")

CH_BAY <- merge(chc_bay, chl, by="Reference")
CH_RIV <- merge(chc_riv, chl, by="Reference")

CK_BAY <- merge(ckc_bay, ckl, by="Reference")
CK_RIV <- merge(ckc_riv, ckl, by="Reference")

IR_BAY <- merge(irc_bay, irl, by="Reference")
IR_RIV <- merge(irc_riv, irl, by="Reference")

JX_RIV <- merge(jxc_riv, jxl, by="Reference")

TB_BAY <- merge(tbc_bay, tbl, by="Reference")
TB_RIV <- merge(tbc_riv, tbl, by="Reference")

######### SHORT EXPLORATION INTO BIO_REFERENCE CODES###############
# NOTE: catch file and length file have 2185 bio_references in common and 16276 in difference. 
# Therefore when merged they create a df "new" that has 2185 unique bio_reference numbers. 

library(dplyr)
data.frame(tbc_bay)
data.frame(tbl)
same <- semi_join(tbc_bay, tbl, by= 'bio_reference') #2185
diff <- anti_join(tbc_bay, tbl, by= 'bio_reference') #16276

untbl <- unique(tbl$bio_reference) #2290 unique bio_reference in tbl, length of tbl is 9224
untbc <- unique(tbc_bay$bio_reference) #18461 unique bio_reference in tbc_bay, length is 18461
unnew <- unique(new$bio_reference) # 2185 unique bio_reference in new, length is 8794

###################################################################
# Seperate by Zone within Estuary
##########################################
AP_BAY_A <- subset(AP_BAY, Zone=="A")
AP_BAY_B <- subset(AP_BAY, Zone=="B")

AP_RIV_C <- subset(AP_RIV, Zone=="C")

CH_BAY_A <- subset(CH_BAY, Zone=="A")
CH_BAY_B <- subset(CH_BAY, Zone=="B")
CH_BAY_C <- subset(CH_BAY, Zone=="C")
CH_BAY_D <- subset(CH_BAY, Zone=="D")

CH_RIV_M <- subset(CH_RIV, Zone=="M")
CH_RIV_N <- subset(CH_RIV, Zone=="N")
CH_RIV_O <- subset(CH_RIV, Zone=="O")
CH_RIV_P <- subset(CH_RIV, Zone=="P")

CK_BAY_B <- subset(CK_BAY, Zone=="B")
CK_BAY_C <- subset(CK_BAY, Zone=="C")

CK_RIV_F <- subset(CK_BAY, Zone=="F")


IR_BAY_A <- subset(IR_BAY, Zone=="A")
IR_BAY_B <- subset(IR_BAY, Zone=="B")
IR_BAY_C <- subset(IR_BAY, Zone=="C")
IR_BAY_D <- subset(IR_BAY, Zone=="D")
IR_BAY_E <- subset(IR_BAY, Zone=="E")
IR_BAY_H <- subset(IR_BAY, Zone=="H")

IR_RIV_F <- subset(IR_RIV, Zone=="F")

JX_RIV_A <- subset(JX_RIV, Zone=="A")
JX_RIV_B <- subset(JX_RIV, Zone=="B")
JX_RIV_C <- subset(JX_RIV, Zone=="C")
JX_RIV_D <- subset(JX_RIV, Zone=="D")
JX_RIV_E <- subset(JX_RIV, Zone=="E")
JX_RIV_F <- subset(JX_RIV, Zone=="F")

TB_BAY_A <- subset(TB_BAY, Zone=="A")
TB_BAY_B <- subset(TB_BAY, Zone=="B")
TB_BAY_C <- subset(TB_BAY, Zone=="C")
TB_BAY_D <- subset(TB_BAY, Zone=="D")
TB_BAY_E <- subset(TB_BAY, Zone=="E")

TB_RIV_M <- subset(TB_RIV, Zone=="M")
TB_RIV_N <- subset(TB_RIV, Zone=="N")
TB_RIV_O <- subset(TB_RIV, Zone=="O")
TB_RIV_P <- subset(TB_RIV, Zone=="P")


########################################################################################################
# To plot absolute numbers caught in each month I must determine the total number of species collected in each haul 
#. Each haul is represented as a unique (Reference). The total number of particular species collected in each
# haul is "n" or "number". Thusly,  each unique Reference has an associated ("n", "number"). 
# Therefore I will capture unique (Un) Reference and then use the N or Number to determine numbers. 
# 
# All References have already been filtered by length (see above constraint steps) so I know that I will be capturing YOY fish (<= 100 mm)
########################################################################################################

AP_BAY_AUn <- subset(AP_BAY_A, !duplicated(Reference))
AP_BAY_BUn <- subset(AP_BAY_B, !duplicated(Reference))
AP_RIV_CUn <- subset(AP_RIV_C, !duplicated(Reference))

CH_BAY_AUn <- subset(CH_BAY_A, !duplicated(Reference))
CH_BAY_BUn <- subset(CH_BAY_B, !duplicated(Reference))
CH_BAY_CUn <- subset(CH_BAY_C, !duplicated(Reference))
CH_BAY_DUn <- subset(CH_BAY_D, !duplicated(Reference))
CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(Reference))
CH_RIV_NUn <- subset(CH_RIV_N, !duplicated(Reference))
CH_RIV_OUn <- subset(CH_RIV_O, !duplicated(Reference))
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(Reference))

CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(Reference))
CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(Reference))
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(Reference))

IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(Reference))
IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(Reference))
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(Reference))
IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(Reference))
IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(Reference))
IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(Reference))
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(Reference))

JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(Reference))
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(Reference))
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(Reference))
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(Reference))
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(Reference))
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(Reference))

TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(Reference))
TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(Reference))
TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(Reference))
TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(Reference))
TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(Reference))
TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
TB_RIV_OUn <- subset(TB_RIV_O, !duplicated(Reference))
TB_RIV_PUn <- subset(TB_RIV_P, !duplicated(Reference))

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
CHR_Nsum <- ddply(CH_RIV_NUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Osum <- ddply(CH_RIV_OUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
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

##  Tampa Bay ###
    # Bay #
TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(Reference))
  TBBA_Shore <- subset(TB_BAY_AUn, ShoreDistance <= 5)
  TBBA_Off_Veg <-subset(TB_BAY_Aun, ShoreDistance > 5 & (bveg = "SAV" | bveg = " Algea" | bveg = "Other"))
  TBBA_Off_NonVeg <- subset(TB_BAY_Aun, ShoreDistance > 5 & (bveg=="None" | bveg= .)) # is this going to work for a missing value?? 

TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(Reference))
  TBBB_Shore <- subset(TB_BAY_BUn, ShoreDistance <= 5)
  TBBB_Off_Veg <-subset(TB_BAY_BUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBB_Off_NonVeg <- subset(TB_BAY_BUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(Reference))
  TBBC_Shore <- subset(TB_BAY_CUn, ShoreDistance <= 5)
  TBBC_Off_Veg <-subset(TB_BAY_CUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBC_Off_NonVeg <- subset(TB_BAY_CUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(Reference))
  TBBD_Shore <- subset(TB_BAY_DUn, ShoreDistance <=5)
  TBBD_Off_Veg <-subset(TB_BAY_DUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBD_Off_NonVeg <- subset(TB_BAY_DUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(Reference))
  TBBE_Shore <- subset(TB_BAY_EUn, ShoreDistance <=5)
  TBBE_Off_Veg <-subset(TB_BAY_EUn, ShoreDistance > 5 & (bveg="SAV" | bveg= "Algea" | bveg ="Other"))
  TBBE_Off_NonVeg <- subset(TB_BAY_EUn, ShoreDistance >5 & (bveg =="None" | bveg = .))

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


CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(Reference))
CH_RIV_NUn <- subset(CH_RIV_N, !duplicated(Reference))
CH_RIV_OUn <- subset(CH_RIV_O, !duplicated(Reference))
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(Reference))


## Cedar Key ##
CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(Reference))
CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(Reference))
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(Reference))

IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(Reference))
IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(Reference))
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(Reference))
IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(Reference))
IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(Reference))
IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(Reference))
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(Reference))

JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(Reference))
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(Reference))
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(Reference))
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(Reference))
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(Reference))
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(Reference))






