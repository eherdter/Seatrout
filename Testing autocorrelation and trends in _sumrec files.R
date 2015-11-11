# This script tests creates lagged scatterplots, tests for autocorrelation and tests for linear trends in the 
# output files of Summarized by Zone.R
setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")

# 1. plot each time series (Done in Seatrout Raw.R)
# 2. test autocorrelation for each 
#   a. graphically summarize autocorrelation- lagged scatterplot (Done below)
#   b. acf/acf2 and correlogram (aka autocorrelation plot) (continue with all time series)  
#     b1. how to evaluate the autocorrelation .. http://stats.stackexchange.com/questions/78281/confidence-band-in-correlogram (which assumes normal assumption)
#     b2. what is the distribution of the output from a general linear model??... 

#   c. test for a trend- using time series analysis pdf-Colorado lecture; Mann-Kendall test?

# Useful Resources:
#  http://wwwuser.gwdg.de/~cscherb1/content/Statistics%20Course%20files/A%20short%20introduction%20to%20time%20series%20analysis%20in%20R.pdf
#  http://www.stat.pitt.edu/stoffer/tsa2/Examples.htm#lag.plot1
#  http://www.ltrr.arizona.edu/~dmeko/notes_3.pdf
#  http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture16_TS3.pdf


TB_A <- read.csv("TampaBay_Bay_A_sumrec.csv")
TB_B <- read.csv("TampaBay_Bay_B_sumrec.csv")
TB_C <- read.csv("TampaBay_Bay_C_sumrec.csv")
TB_D <- read.csv("TampaBay_Bay_D_sumrec.csv")
TB_E <- read.csv("TampaBay_Bay_E_sumrec.csv")
TB_K <- read.csv("TampaBay_Riv_K_sumrec.csv")
TB_L <- read.csv("TampaBay_Riv_L_sumrec.csv")
TB_M <- read.csv("TampaBay_Riv_M_sumrec.csv")
TB_N <- read.csv("TampaBay_Riv_N_sumrec.csv")

AP_A <- read.csv("Appalachicola_Bay_A_sumrec.csv")
AP_B <- read.csv("Appalachicola_Bay_B_sumrec.csv")
AP_C <- read.csv("Appalachicola_Riv_C_sumrec.csv")

CK_B <- read.csv("CedarKey_Bay_B_sumrec.csv")
CK_C <- read.csv("CedarKey_Bay_C_sumrec.csv")
CK_F <- read.csv("CedarKey_Riv_F_sumrec.csv")

CH_A <- read.csv("CharlotteHarbor_Bay_A_sumrec.csv")
CH_B <- read.csv("CharlotteHarbor_Bay_B_sumrec.csv")
CH_C <- read.csv("CharlotteHarbor_Bay_C_sumrec.csv")
CH_D <- read.csv("CharlotteHarbor_Bay_D_sumrec.csv")
CH_M <- read.csv("CharlotteHarbor_Riv_M_sumrec.csv")
CH_P <- read.csv("CharlotteHarbor_Riv_P_sumrec.csv")

IR_A <- read.csv("IndianRiver_Bay_A_sumrec.csv")
IR_B <- read.csv("IndianRiver_Bay_B_sumrec.csv")
IR_C <- read.csv("IndianRiver_Bay_C_sumrec.csv")
IR_D <- read.csv("IndianRiver_Bay_D_sumrec.csv")
IR_E <- read.csv("IndianRiver_Bay_E_sumrec.csv")
IR_H <- read.csv("IndianRiver_Bay_H_sumrec.csv")
IR_F <- read.csv("IndianRiver_Riv_F_sumrec.csv")

JX_A <-read.csv("Jax_Riv_A_sumrec.csv")
JX_B <-read.csv("Jax_Riv_B_sumrec.csv")
JX_C <-read.csv("Jax_Riv_C_sumrec.csv")
JX_D <-read.csv("Jax_Riv_D_sumrec.csv")
JX_E <-read.csv("Jax_Riv_E_sumrec.csv")
JX_F <-read.csv("Jax_Riv_F_sumrec.csv")

#A. LAGGED SCATTERPLOTS of each time series
# From Notes_3, GEOS 585A, Spring 2015 handout printed from arizona (website above)
# " An attribute of the lagged scatterplot is that it can display autocorrelation regardless of the form of the
# dependence on the past values. An assumption of linear dependence is not necessary."
# Such nonlinear dependence might not be effectively summarized by other methods (such as the acf function)

library(astsa) # from the stat pitt site (accompanying book downloaded Time Series Analysis and Its Applications with R Examples)
lag1.plot(TB_A$CPUE, 4, corr=TRUE, smooth=TRUE)
# Critical level of correlation for 95% significance (alpha = 0.5) r= 0+- 2/sqrt(N)
# r= 0+- 2/sqrt(19) (GEOS 585A)
r_sig <- 2/sqrt(19) # for all it is 0.4588315
lag1.plot(TB_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_C$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_D$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_E$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_K$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_L$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_M$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TB_N$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(AP_A$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(AP_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(AP_C$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CK_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CK_C$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CK_F$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CH_A$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CH_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CH_C$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CH_D$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CH_M$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CH_P$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IR_A$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_C$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_D$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_E$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_H$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IR_F$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(JX_A$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(JX_B$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(JX_C$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(JX_D$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(JX_E$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(JX_F$CPUE, 4, corr=TRUE, smooth=TRUE)

# B. ACF and Correlograms
# perform autocorrelation function and make correlogram
#below, the correlogram is an automatic output of the acf2 function

acf2(JX_A$CPUE)
acf2(JX_B$CPUE)
acf2(JX_C$CPUE)
acf2(JX_D$CPUE)
acf2(JX_E$CPUE)
acf2(JX_F$CPUE)

acf2(TB_A$CPUE)
acf2(TB_B$CPUE)
acf2(TB_C$CPUE)
acf2(TB_D$CPUE)
acf2(TB_E$CPUE)
acf2(TB_K$CPUE)
acf2(TB_L$CPUE)
acf2(TB_M$CPUE)
acf2(TB_N$CPUE)

acf2(AP_A$CPUE)
acf2(AP_B$CPUE)
acf2(AP_C$CPUE)

acf2(CK_B$CPUE)
acf2(CK_C$CPUE)
acf2(CK_F$CPUE)

acf2(CH_A$CPUE)
acf2(CH_B$CPUE)
acf2(CH_C$CPUE)
acf2(CH_D$CPUE)
acf2(CH_M$CPUE)
acf2(CH_P$CPUE)

acf2(IR_A$CPUE)
acf2(IR_B$CPUE)
acf2(IR_C$CPUE)
acf2(IR_D$CPUE)
acf2(IR_E$CPUE)
acf2(IR_H$CPUE)
acf2(IR_F$CPUE)

# C. Test for a trend using Mann Kendall test (since no autocorrelation present I do not need to use a modified test)
library(Kendall)
MannKendall(IR_A$CPUE)
MannKendall(IR_B$CPUE)
MannKendall(IR_C$CPUE)
MannKendall(IR_D$CPUE)
MannKendall(IR_E$CPUE)
MannKendall(IR_H$CPUE)
MannKendall(IR_F$CPUE)

MannKendall(JX_A$CPUE)
MannKendall(JX_B$CPUE)
MannKendall(JX_C$CPUE)
MannKendall(JX_D$CPUE)
MannKendall(JX_E$CPUE)
MannKendall(JX_F$CPUE)

MannKendall(TB_A$CPUE)
MannKendall(TB_B$CPUE)
MannKendall(TB_C$CPUE)
MannKendall(TB_D$CPUE)
MannKendall(TB_E$CPUE)
MannKendall(TB_K$CPUE)
MannKendall(TB_L$CPUE)
MannKendall(TB_M$CPUE)
MannKendall(TB_N$CPUE)

MannKendall(AP_A$CPUE)
MannKendall(AP_B$CPUE)
MannKendall(AP_C$CPUE)

MannKendall(CK_B$CPUE)
MannKendall(CK_C$CPUE)
MannKendall(CK_F$CPUE)

MannKendall(CH_A$CPUE)
MannKendall(CH_B$CPUE)
MannKendall(CH_C$CPUE)
MannKendall(CH_D$CPUE)
MannKendall(CH_M$CPUE)
MannKendall(CH_P$CPUE)

 
##### TEST STRATUMS WITHIN ZONES ######

APA_ONV <- read.csv("APA_ONV_sumrec.csv")
APA_OV <- read.csv("APA_OV_sumrec.csv")
APA_S <- read.csv("APA_S_sumrec.csv")

APB_ONV<- read.csv("APB_ONV_sumrec.csv")
APB_OV <- read.csv("APB_OV_sumrec.csv")
APB_S <- read.csv("APB_S_sumrec.csv")

CHA_ONV <- read.csv("CHA_ONV_sumrec.csv")
CHA_OV <- read.csv("CHA_OV_sumrec.csv")
CHA_S <- read.csv("CHA_S_sumrec.csv")

CHB_ONV <- read.csv("CHB_ONV_sumrec.csv")
CHB_OV <- read.csv("CHB_OV_sumrec.csv")
CHB_S <- read.csv("CHB_S_sumrec.csv")

CHC_ONV <- read.csv("CHC_ONV_sumrec.csv")
CHC_OV <- read.csv("CHC_OV_sumrec.csv")
CHC_S <- read.csv("CHC_S_sumrec.csv")

CHD_ONV <- read.csv("CHD_ONV_sumrec.csv")
CHD_OV<- read.csv("CHD_OV_sumrec.csv")
CHD_S <- read.csv("CHD_S_sumrec.csv")

CKB_ONV <- read.csv("CKB_ONV_sumrec.csv")
CKB_OV <- read.csv("CKB_OV_sumrec.csv")
CKB_S <- read.csv("CKB_S_sumrec.csv")

CKC_ONV<- read.csv("CKC_ONV_sumrec.csv")
CKC_OV <- read.csv("CKC_OV_sumrec.csv")
CKC_S<- read.csv("CKC_S_sumrec.csv")

IRA_ONV <- read.csv("IRA_ONV_sumrec.csv")
IRA_OV <- read.csv("IRA_OV_sumrec.csv")
IRA_S <- read.csv("IRA_S_sumrec.csv")

IRB_ONV <- read.csv("IRB_ONV_sumrec.csv")
IRB_OV <- read.csv("IRB_OV_sumrec.csv")
IRB_S <- read.csv("IRB_S_sumrec.csv")

IRC_ONV <- read.csv("IRC_ONV_sumrec.csv")
IRC_OV <- read.csv("IRC_OV_sumrec.csv")
IRC_S <- read.csv("IRC_S_sumrec.csv")

IRD_ONV <- read.csv("IRD_ONV_sumrec.csv")
IRD_OV <- read.csv("IRD_OV_sumrec.csv")
IRD_S <- read.csv("IRD_S_sumrec.csv")

IRE_ONV <- read.csv("IRE_ONV_sumrec.csv")
IRE_OV <- read.csv("IRE_OV_sumrec.csv")
IRE_S <- read.csv("IRE_S_sumrec.csv")

IRH_ONV <- read.csv("IRH_ONV_sumrec.csv")
IRH_OV <- read.csv("IRH_OV_sumrec.csv")
IRH_S <- read.csv("IRH_S_sumrec.csv")

TBA_ONV <- read.csv("TBA_ONV_sumrec.csv")
TBA_OV <- read.csv("TBA_OV_sumrec.csv")
TBA_S <- read.csv("TBA_S_sumrec.csv")

TBB_ONV <- read.csv("TBB_ONV_sumrec.csv")
TBB_OV <- read.csv("TBB_OV_sumrec.csv")
TBB_S <- read.csv("TBB_S_sumrec.csv")

TBC_ONV <- read.csv("TBC_ONV_sumrec.csv")
TBC_OV <- read.csv("TBC_OV_sumrec.csv")
TBC_S <- read.csv("TBC_S_sumrec.csv")

TBD_ONV <- read.csv("TBD_ONV_sumrec.csv")
TBD_OV <- read.csv("TBD_OV_sumrec.csv")
TBD_S <- read.csv("TBD_S_sumrec.csv")

TBE_ONV <- read.csv("TBE_ONV_sumrec.csv")
TBE_OV <- read.csv("TBE_OV_sumrec.csv")
TBE_S <- read.csv("TBE_S_sumrec.csv")

###########################
#LAGGED SCATTERPLOTS ###############
library(astsa)
# Critical level of correlation for 95% significance (alpha = 0.5) r= 0+- 2/sqrt(N)
# r= 0+- 2/sqrt(17) (GEOS 585A)= .485
lag1.plot(APA_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(APA_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(APA_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(APB_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(APB_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(APB_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CHA_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHA_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHA_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CHB_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHB_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHB_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CHC_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHC_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHC_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CHD_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHD_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CHD_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CKB_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CKB_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CKB_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(CKC_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CKC_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(CKC_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRA_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRA_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRA_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRB_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRB_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRB_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRC_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRC_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRC_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRD_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRD_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRD_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRE_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRE_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRE_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(IRH_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRH_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(IRH_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(TBA_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBA_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBA_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(TBB_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBB_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBB_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(TBC_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBC_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBC_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(TBD_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBD_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBD_S$CPUE, 4, corr=TRUE, smooth=TRUE)

lag1.plot(TBE_ONV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBE_OV$CPUE, 4, corr=TRUE, smooth=TRUE)
lag1.plot(TBE_S$CPUE, 4, corr=TRUE, smooth=TRUE)

#### MANN KENDALL TREND TESTING ######
library(Kendall)

MannKendall(APA_ONV$CPUE)
MannKendall(APA_OV$CPUE)
MannKendall(APA_S$CPUE)

MannKendall(APB_ONV$CPUE)
MannKendall(APB_OV$CPUE)
MannKendall(APB_S$CPUE)

MannKendall(CHA_ONV$CPUE)
MannKendall(CHA_OV$CPUE)
MannKendall(CHA_S$CPUE)

MannKendall(CHB_ONV$CPUE)
MannKendall(CHB_OV$CPUE)
MannKendall(CHB_S$CPUE)

MannKendall(CHC_ONV$CPUE)
MannKendall(CHC_OV$CPUE)
MannKendall(CHC_S$CPUE)

MannKendall(CHD_ONV$CPUE)
MannKendall(CHD_OV$CPUE)
MannKendall(CHD_S$CPUE)

MannKendall(CKB_ONV$CPUE)
MannKendall(CKB_OV$CPUE)
MannKendall(CKB_S$CPUE)

MannKendall(CKC_ONV$CPUE)
MannKendall(CKC_OV$CPUE)
MannKendall(CKC_S$CPUE)

MannKendall(IRA_ONV$CPUE)
MannKendall(IRA_OV$CPUE)
MannKendall(IRA_S$CPUE)

MannKendall(IRB_ONV$CPUE)
MannKendall(IRB_OV$CPUE)
MannKendall(IRB_S$CPUE)

MannKendall(IRC_ONV$CPUE)
MannKendall(IRC_OV$CPUE)
MannKendall(IRC_S$CPUE)

MannKendall(IRD_ONV$CPUE)
MannKendall(IRD_OV$CPUE)
MannKendall(IRD_S$CPUE)

MannKendall(IRE_ONV$CPUE)
MannKendall(IRE_OV$CPUE)
MannKendall(IRE_S$CPUE)

MannKendall(IRH_ONV$CPUE)
MannKendall(IRH_OV$CPUE)
MannKendall(IRH_S$CPUE)

MannKendall(TBA_ONV$CPUE)
MannKendall(TBA_OV$CPUE)
MannKendall(TBA_S$CPUE)

MannKendall(TBB_ONV$CPUE)
MannKendall(TBB_OV$CPUE)
MannKendall(TBB_S$CPUE)

MannKendall(TBC_ONV$CPUE)
MannKendall(TBC_OV$CPUE)
MannKendall(TBC_S$CPUE)

MannKendall(TBD_ONV$CPUE)
MannKendall(TBD_OV$CPUE)
MannKendall(TBD_S$CPUE)

MannKendall(TBE_ONV$CPUE)
MannKendall(TBE_OV$CPUE)
MannKendall(TBE_S$CPUE)


