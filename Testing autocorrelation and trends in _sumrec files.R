# This script tests creates lagged scatterplots, tests for autocorrelation and tests for linear trends in the 
# output files of Seatrout Raw.R
setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")


# to do next: (COPIED FROM Seatrout IOA.R)
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

 



