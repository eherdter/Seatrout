# This script loads raw survey data. They are stored as .sas7bdat at FWRI but I will be using
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

#Indian River (Northern and Southern Indian River Lagoon???)
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

#Tequesta (Southern Indian River Lagoon??)
TQM_C <- read_sas("tqm_cn_c.sas7bdat")
TQM_Hab <- read_sas("tqm_cn_hab.sas7bdat")
TQM_Hyd <- read_sas("tqm_cn_hyd.sas7bdat")
TQM_L <- read_sas("tqm_cn_l.sas7bdat")



