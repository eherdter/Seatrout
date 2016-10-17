# 10/17/2016 This script imports recreational (MRFSS, MRIP) length data 
# Main Objectives of this script: 
# 1. Imports mrfss and mrip length data.  
# 2. Determens length frequencies
# 3. Statistical comparison of bay-bay length frequencies. 
###############################################################
library(FSA)
library(magrittr)
library(dplyr)
library(plotrix)
library(haven)
library(Matching)

# set working directory
setwd("~/Desktop/Github Repo/Seatrout/Data")

# **************Length frequency distributions of MRFSS and MRIP

#import the sas data **************************
mrfss <- read_sas("mrfss_lens_8115.sas7bdat")
mrip <- read_sas("mrip_lens_20042015.sas7bdat")

# subset each estuary using the Country of Encounter codes
# From the definitions defined by FWRI
# County_Bay_assignments.xlsx in GitHub>Seatrout>Data

#load data
#select the counties that most closely align with the bays in question
# convert fork length to tl using the equation TLmm = 1.00467 * FL + 0.04850 from the SAS mrfss leng_freq
mrfss_AP <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(5,33,37,45,65,77,91,113,123,129,131,133)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)
mrfss_TB <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(53,101,57,103,81)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)
mrfss_CH <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(27,115,7,15,71,21,87,51)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)
mrfss_CK <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(75,29,17,1)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)
mrfss_JX <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(19,31,35,89,107,109)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)
mrfss_IR <- subset(read_sas("mrfss_lens_8115.sas7bdat"), CNTY %in% c(9,11,25,61,85,99,111,127)) %>% mutate(tl = (1.00467*flmm+0.04850)/10)

#########################################                                                                  
#AMONG GROUP STATISTICAL COMPARISONS
#########################################

#First with K-S Test
#bootstrapped version of the K-S test that is insensitive to ties with noncontinuous data is implemented in ks.boot
ks<- c(ks.boot(mrfss_TB$tl, mrfss_CK$tl, nboots=5000)$p.value, ks.boot(mrfss_TB$tl, mrfss_CH$tl, nboots=5000)$p.value)

#if comparing more than two groups then each pair must be compared separately and the p value from each comparison must
# be adjusted for an increasing experimentwise error rate due to multiple comparisons. 
ks_ALL <-c(ks.boot(mrfss_TB$tl, mrfss_CK$tl, nboots=5000)$ks.boot.pvalue, 
  ks.boot(mrfss_TB$tl, mrfss_CH$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_TB$tl, mrfss_AP$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_TB$tl, mrfss_JX$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_TB$tl, mrfss_IR$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CK$tl, mrfss_CH$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CK$tl, mrfss_AP$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CK$tl, mrfss_JX$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CK$tl, mrfss_IR$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CH$tl, mrfss_AP$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CH$tl, mrfss_JX$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_CH$tl, mrfss_IR$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_AP$tl, mrfss_JX$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_AP$tl, mrfss_IR$tl, nboots=5000)$ks.boot.pvalue,
  ks.boot(mrfss_JX$tl, mrfss_IR$tl, nboots=5000)$ks.boot.pvalue)

  
p.adjust(ks_ALL)  
  