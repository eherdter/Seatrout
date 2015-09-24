# This script loads Spotted Seatrout raw survy data. They are stored as .sas7bdat at FWRI but I will be using
# haven to convert them and work with them in R
setwd("~/Desktop/Github Repo/Seatrout/Data/Raw Survey Data/Seatrout FIM Data")

library(haven)
#Appalachicola
AP_C  <- read_sas("apm_cn_c.sas7bdat")
unique(AP_C$Zone) #Why Zones here not present on sampling map "A" "B" "D" "C"
AP_Hab <- read_sas("apm_cn_hab.sas7bdat")
AP_Hyd <- read_sas("apm_cn_hyd.sas7bdat")
AP_L <- read_sas("apm_cn_l.sas7bdat")

#Charlotte Harbor
CH_C <- read_sas("chm_cn_c.sas7bdat")
unique(CH_C$Zone)  # Why are there Zones here not present on sampling map "A" "C" "B" "D" "P" "M" "G" "E" "H" ??
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
# _L$sl => between 0-100 (YOY animals in this range)
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
#########################
apl <- subset(AP_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
apc_bay <- subset(AP_C, gr==23 | gr==20 & month >= 6 & month <= 10 & Zone == "A" | Zone== "B", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
apc_riv <- subset(AP_C, gr==23 | gr==20 & month >= 6 & month <= 10 & Zone == "C", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     
                     

chl <- subset(CH_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
chc_bay <- subset(CH_C, gr==23 | gr==20 & month >= 4 & month <= 10 & Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
chc_riv <- subset(CH_C, gr==23 | gr==20 & month >= 4 & month <= 10 & Zone == "M" | Zone == "N" | Zone == "O" | Zone== "P", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     


ckl <- subset(CK_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
ckc_bay <- subset(CK_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "B" | Zone == "C" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
ckc_riv <- subset(CK_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "F" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

irl <- subset(IR_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
irc_bay <- subset(IR_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
irc_riv <- subset(IR_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "F" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

jxl <- subset(JX_L, sl <= 100, select =c(bio_reference, sl, COUNT , nl))
jxc_riv <- subset(JX_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "A" | Zone == "B" | Zone== "C" | Zone=="D" | Zone=="E"  | Zone == "F", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

tbl <- subset(TB_L, sl <= 100, select =c(bio_reference, sl, COUNT, nl))
tbc_bay <- subset(TB_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "A" | Zone == "B" | Zone == "C" | Zone== "D" | Zone=="E" | Zone=="H", select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )
tbc_riv <- subset(TB_C, gr==23 | gr==20 & month >= 5 & month <= 11 & Zone == "F" , select=c(bio_reference, Longitude, Latitude, Zone, Grid, month, year, gr, bottom, n, number) )                     

############################################################
# Merge values from c_bay and c_riv into ##l so all values from the c_bay and c_riv can be retained 
# but only at the bio_reference numbers in the length variables. If I wanted to retain all values -> all=TRUE
############################################################

AP_BAY <- merge(apc_bay, apl, by="bio_reference")
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


