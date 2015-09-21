# This script loads Spotted Seatrout raw survy data. They are stored as .sas7bdat at FWRI but I will be using
# haven to convert them and work with them in R
setwd("~/Desktop/Github Repo/Seatrout/Data/Raw Survey Data/Seatrout FIM Data")

library(haven)
#Appalachicola
AP_C  <- read_sas("apm_cn_c.sas7bdat")
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
IRM_C <- read_sas("irm_cn_c.sas7bdat")
IRM_Hab <- read_sas("irm_cn_hab.sas7bdat")
IRM_Hyd <- read_sas("irm_cn_hyd.sas7bdat")
IRM_L <- read_sas("irm_cn_l.sas7bdat")

#JAX
JXM_C <- read_sas("jxm_cn_c.sas7bdat")
JXM_Hab <- read_sas("jxm_cn_hab.sas7bdat")
JXM_Hyd <- read_sas("jxm_cn_hyd.sas7bdat")
JXM_L <- read_sas("jxm_cn_l.sas7bdat")

#Tampa Bay
TBM_C <- read_sas("tbm_cn_c.sas7bdat")
TBM_Hab <- read_sas("tbm_cn_hab.sas7bdat")
TBM_Hyd <- read_sas("tbm_cn_hyd.sas7bdat")
TBM_L <- read_sas("tbm_cn_l.sas7bdat")

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
#               => AB  6<=x<=10
# _C$Zone => depends on each estuary (for a more in depth description of monthly sampling within bay or riverine see Annual Report)
#               => TB BAY(A-E), RIVERINE (K-N)
#               => CH BAY(A-D), RIVERINE (M-P)
#               => nor.IRL BAY(A-E, H), RIVERINE (F)
#               => AP BAY(A-B), RIVERINE (C)
#               => Jax RIVERINE (A-F)
#               => CK BAY(B-C), RIVERINE (F)







