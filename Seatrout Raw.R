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
# Apply constraints
########################################################
apl <- subset(AP_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
apc_bay <- subset(AP_C, (gr == 23 | gr ==20 | gr==19) & month >=6 & month <= 10 & (Zone == "A" | Zone == "B") , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
#must include the gear constraints or else large seines might be included
apc_riv <- subset(AP_C, (gr==23   | gr ==20 | gr==19) & month >=6 & month <= 10 & (Zone == "C"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     
                     

chl <- subset(CH_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
chc_bay <- subset(CH_C, (gr==23 | gr==20 | gr==19) & month >= 4 & month <= 10 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
chc_riv <- subset(CH_C, (gr==23 | gr==20 | gr==19) & month >= 4 & month <= 10 & (Zone == "M" | Zone == "N" | Zone == "O" | Zone== "P"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     


ckl <- subset(CK_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
ckc_bay <- subset(CK_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "B" | Zone == "C") , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
ckc_riv <- subset(CK_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & Zone == "F" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

irl <- subset(IR_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
irc_bay <- subset(IR_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
irc_riv <- subset(IR_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & Zone == "F" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

jxl <- subset(JX_L, sl <= 100, select =c(bio_reference, sl, COUNT , nl))
jxc_riv <- subset(JX_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone== "C" | Zone=="D" | Zone=="E"  | Zone == "F"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

tbl <- subset(TB_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
tbc_bay <- subset(TB_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
tbc_riv <- subset(TB_C, (gr==23 | gr==20 | gr==19) & month >= 5 & month <= 11 & (Zone == "K" | Zone == "L" | Zone == "M" | Zone== "N") , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     




############################################################
# Merge values from c_bay and c_riv into ##l so all values from the c_bay and c_riv can be retained 
# but only at the bio_reference numbers in the length variables. If I wanted to retain all values -> all=TRUE
############################################################

AP_BAY <- merge(apc_bay, apl, by="bio_reference")
unique(AP_BAY$Zone)
AP_RIV <- merge(apc_riv, apl, by="bio_reference")

CH_BAY <- merge(chc_bay, chl, by="bio_reference")
CH_RIV <- merge(chc_riv, chl, by="bio_reference")

CK_BAY <- merge(ckc_bay, ckl, by="bio_reference")
CK_RIV <- merge(ckc_riv, ckl, by="bio_reference")

IR_BAY <- merge(irc_bay, irl, by="bio_reference")
IR_RIV <- merge(irc_riv, irl, by="bio_reference")

JX_RIV <- merge(jxc_riv, jxl, by="bio_reference")

TB_BAY <- merge(tbc_bay, tbl, by="bio_reference")
TB_RIV <- merge(tbc_riv, tbl, by="bio_reference")

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
AP_BAY_A <- subset(AP_BAY, Zone=="A", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
AP_BAY_B <- subset(AP_BAY, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

AP_RIV_C <- subset(AP_RIV, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

CH_BAY_A <- subset(CH_BAY, Zone=="A", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_BAY_B <- subset(CH_BAY, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_BAY_C <- subset(CH_BAY, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_BAY_D <- subset(CH_BAY, Zone=="D", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

CH_RIV_M <- subset(CH_RIV, Zone=="M", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_RIV_N <- subset(CH_RIV, Zone=="N", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_RIV_O <- subset(CH_RIV, Zone=="O", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CH_RIV_P <- subset(CH_RIV, Zone=="P", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

CK_BAY_B <- subset(CK_BAY, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
CK_BAY_C <- subset(CK_BAY, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

CK_RIV_F <- subset(CK_BAY, Zone=="F", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))


IR_BAY_A <- subset(IR_BAY, Zone=="A", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
IR_BAY_B <- subset(IR_BAY, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
IR_BAY_C <- subset(IR_BAY, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
IR_BAY_D <- subset(IR_BAY, Zone=="D", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
IR_BAY_E <- subset(IR_BAY, Zone=="E", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
IR_BAY_H <- subset(IR_BAY, Zone=="H", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

IR_RIV_F <- subset(IR_RIV, Zone=="F", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

JX_RIV_A <- subset(JX_RIV, Zone=="A", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
JX_RIV_B <- subset(JX_RIV, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
JX_RIV_C <- subset(JX_RIV, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
JX_RIV_D <- subset(JX_RIV, Zone=="D", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
JX_RIV_E <- subset(JX_RIV, Zone=="E", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
JX_RIV_F <- subset(JX_RIV, Zone=="F", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

TB_BAY_A <- subset(TB_BAY, Zone=="A", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_BAY_B <- subset(TB_BAY, Zone=="B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_BAY_C <- subset(TB_BAY, Zone=="C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_BAY_D <- subset(TB_BAY, Zone=="D", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_BAY_E <- subset(TB_BAY, Zone=="E", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))

TB_RIV_M <- subset(TB_RIV, Zone=="M", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_RIV_N <- subset(TB_RIV, Zone=="N", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_RIV_O <- subset(TB_RIV, Zone=="O", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))
TB_RIV_P <- subset(TB_RIV, Zone=="P", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number, sl, COUNT, nl))


########################################################################################################
# To plot absolute numbers caught in each month I must determine the total number of species collected in each haul 
#. Each haul is represented as a unique (bio_reference). The total number of particular species collected in each
# haul is "n" or "number". Thusly,  each unique bio_reference has an associated ("n", "number"). 
# Therefore I will capture unique (Un) bio_reference and then use the N or Number to determine numbers. 
# 
# All bio_references have already been filtered by length (see above constraint steps) so I know that I will be capturing YOY fish (<= 100 mm)
########################################################################################################

AP_BAY_AUn <- subset(AP_BAY_A, !duplicated(bio_reference))
AP_BAY_BUn <- subset(AP_BAY_B, !duplicated(bio_reference))
AP_RIV_CUn <- subset(AP_RIV_C, !duplicated(bio_reference))

CH_BAY_AUn <- subset(CH_BAY_A, !duplicated(bio_reference))
CH_BAY_BUn <- subset(CH_BAY_B, !duplicated(bio_reference))
CH_BAY_CUn <- subset(CH_BAY_C, !duplicated(bio_reference))
CH_BAY_DUn <- subset(CH_BAY_D, !duplicated(bio_reference))
CH_RIV_MUn <- subset(CH_RIV_M, !duplicated(bio_reference))
CH_RIV_NUn <- subset(CH_RIV_N, !duplicated(bio_reference))
CH_RIV_OUn <- subset(CH_RIV_O, !duplicated(bio_reference))
CH_RIV_PUn <- subset(CH_RIV_P, !duplicated(bio_reference))

CK_BAY_BUn <- subset(CK_BAY_B, !duplicated(bio_reference))
CK_BAY_CUn <- subset(CK_BAY_C, !duplicated(bio_reference))
CK_RIV_FUn <- subset(CK_RIV_F, !duplicated(bio_reference))

IR_BAY_AUn <- subset(IR_BAY_A, !duplicated(bio_reference))
IR_BAY_BUn <- subset(IR_BAY_B, !duplicated(bio_reference))
IR_BAY_CUn <- subset(IR_BAY_C, !duplicated(bio_reference))
IR_BAY_DUn <- subset(IR_BAY_D, !duplicated(bio_reference))
IR_BAY_EUn <- subset(IR_BAY_E, !duplicated(bio_reference))
IR_BAY_HUn <- subset(IR_BAY_H, !duplicated(bio_reference))
IR_RIV_FUn <- subset(IR_RIV_F, !duplicated(bio_reference))

JX_RIV_AUn <- subset(JX_RIV_A, !duplicated(bio_reference))
JX_RIV_BUn <- subset(JX_RIV_B, !duplicated(bio_reference))
JX_RIV_CUn <- subset(JX_RIV_C, !duplicated(bio_reference))
JX_RIV_DUn <- subset(JX_RIV_D, !duplicated(bio_reference))
JX_RIV_EUn <- subset(JX_RIV_E, !duplicated(bio_reference))
JX_RIV_FUn <- subset(JX_RIV_F, !duplicated(bio_reference))

TB_BAY_AUn <- subset(TB_BAY_A, !duplicated(bio_reference))
TB_BAY_BUn <- subset(TB_BAY_B, !duplicated(bio_reference))
TB_BAY_CUn <- subset(TB_BAY_C, !duplicated(bio_reference))
TB_BAY_DUn <- subset(TB_BAY_D, !duplicated(bio_reference))
TB_BAY_EUn <- subset(TB_BAY_E, !duplicated(bio_reference))
TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(bio_reference))
TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(bio_reference))
TB_RIV_OUn <- subset(TB_RIV_O, !duplicated(bio_reference))
TB_RIV_PUn <- subset(TB_RIV_P, !duplicated(bio_reference))

#############################################################
# Summarise the data. 
# I've chosed to summarize the data by year and month. Number of BioReferences is the number of different bio_references being summarized by month of each year. 
# TotalNumberofAnimalsCollectedinHauls in the summation of the total number of animals collected in the hauls for month by year. 
# MedianNumberofAnimas, MeanNumberofAnimals
# APB_Asum = Appachicola Bay _ zone A (summarised)
# APR_Csum = Appalachicol (River) _ zone C (summarised)
##############################################################################################################
library(plyr)
APB_Asum <- ddply(AP_BAY_AUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_Bsum <- ddply(AP_BAY_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APR_Csum <- ddply(AP_RIV_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

CHB_Asum <- ddply(CH_BAY_AUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Bsum <- ddply(CH_BAY_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Csum <- ddply(CH_BAY_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Dsum <- ddply(CH_BAY_DUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Msum <- ddply(CH_RIV_MUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Nsum <- ddply(CH_RIV_NUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Osum <- ddply(CH_RIV_OUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Psum <- ddply(CH_RIV_PUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

CKB_Bsum <- ddply(CK_BAY_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_Csum <- ddply(CK_BAY_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKR_Fsum <- ddply(CK_RIV_FUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

IRB_Asum <- ddply(IR_BAY_AUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Bsum <- ddply(IR_BAY_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Csum <- ddply(IR_BAY_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Dsum <- ddply(IR_BAY_DUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Esum <- ddply(IR_BAY_EUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Hsum <- ddply(IR_BAY_HUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRR_Fsum <- ddply(IR_RIV_FUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

JXR_Asum <- ddply(JX_RIV_AUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Bsum <- ddply(JX_RIV_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Csum <- ddply(JX_RIV_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Dsum <- ddply(JX_RIV_DUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Esum <- ddply(JX_RIV_EUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Fsum <- ddply(JX_RIV_FUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

TBB_Asum <- ddply(TB_BAY_AUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Bsum <- ddply(TB_BAY_BUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Csum <- ddply(TB_BAY_CUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Dsum <- ddply(TB_BAY_DUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Esum <- ddply(TB_BAY_EUn, c("year", "month"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))


########## 
# Make sure total numbers from all months are the same as reported in the FWRI data report
##########
tbc_bayFWRItest <- subset(TB_C, (gr==20 | gr==19)) # gear can also be reported as 19 so I need to include 19 in the bay seines ,  select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )  #& ( Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E"), select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
##### gear is also 19 ####
TBBtest <- ddply(tbc_bayFWRItest, c("year"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))

tbc_rivFWRItest <- subset(TB_C, gr==23)
TBRtest <- 








test1 <- merge(tbc_bayFWRItest, TB_L, by="bio_reference")
testUn <- subset(test, !duplicated(bio_reference))


testsum <- ddply(testUn, c("year"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
testsum2 <- ddply(tbc_bayFWRItest, c("year"), summarise, NumberofBioReferences=length(bio_reference) , TotalNumberofAnimalsCollectedinHauls=sum(n), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))





















