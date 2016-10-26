# 10/26/2016

# This method uses the Delta method for determining the nominal catch rate of Spotted Seatrout in the FIM catch
# Uses methods described in Chyan-huei et al 1992

# positive haul= hauls with Seatrout
# zero haul= hauls without any Seatrout 

# Positive Data = total numbers of Seatrout in a year/ total numbers of positive hauls in year
# Proportion Positive = total number of positive hauls / total numbers of all hauls (positive and zero) 

# Adjusted Index = Positive Data * Proportion Positive

###########################
# Set working directory
###########################
setwd("~/Desktop/Github Repo/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData")

###########################
# Import Data Sets
###########################
# These data sets were produced using the spp_comb_5_13_EG_2bays_yoy_2015.sas program which is stored in my scratch folder
# Bay and river stations were denoted by the gear sampling  code. There is a hard copy of this script in my green folder. 


# select the important recruitment months for each zone


ap = read_sas("ap_yoy_bay_cn_c.sas7bdat") 
apr = read_sas("ap_yoy_riv_cn_c.sas7bdat") 
ck = read_sas("ck_yoy_bay_cn_c.sas7bdat") 
ckr = read_sas("ck_yoy_riv_cn_c.sas7bdat")
ch = read_sas("ch_yoy_bay_cn_c.sas7bdat") 
chr = read_sas("ch_yoy_riv_cn_c.sas7bdat") 
tb = read_sas("tb_yoy_bay_cn_c.sas7bdat") 
tbr = read_sas("tb_yoy_riv_cn_c.sas7bdat") 
ir = read_sas("ir_yoy_riv_cn_c.sas7bdat")
irb = read_sas("ir_yoy_bay_cn_c.sas7bdat") 
jx = read_sas("jx_yoy_riv_cn_c.sas7bdat") 
jxb = read_sas("jx_yoy_bay_cn_c.sas7bdat") 
tq = read_sas("tq_yoy_bay_cn_c.sas7bdat") 
tqr = read_sas("tq_yoy_riv_cn_c.sas7bdat") 