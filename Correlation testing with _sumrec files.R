

setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")
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
###################################
#1.  Testing distribution and normal, randomness of residuals to prepare for correlation testing. 
# data to be analyzed should have normally-distributed residuals that are uncorrelated with the independent variable. 

############################


# A. Test residuals for homscedasticity 
TBA_model <- lm(log(CPUE) ~ year, TB_A)
TBA_resid<- resid(TBA_model)
plot(TB_A$year, TBA_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBA_resid)

TBB_model <- lm(log(CPUE) ~ year, TB_B)
TBB_resid<- resid(TBB_model)
plot(TB_B$year, TBB_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBB_resid)

TBC_model <- lm(log(CPUE) ~ year, TB_C)
TBC_resid<- resid(TBC_model)
plot(TB_C$year, TBC_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBC_resid)

TBD_model <- lm(log(CPUE) ~ year, TB_D)
TBD_resid<- resid(TBD_model)
plot(TB_D$year, TBD_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBD_resid)

TBE_model <- lm(log(CPUE) ~ year, TB_E)
TBE_resid<- resid(TBE_model)
plot(TB_E$year, TBE_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBE_resid)

TBK_model <- lm(log(CPUE) ~ year, TB_K)
TBK_resid<- resid(TBK_model)
plot(TB_K$year, TBK_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBK_resid)

TBL_model <- lm(log(CPUE) ~ year, TB_L)
TBL_resid<- resid(TBL_model)
plot(TB_L$year, TBL_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBL_resid)

TBM_model <- lm(log(CPUE) ~ year, TB_M)
TBM_resid<- resid(TBM_model)
plot(TB_M$year, TBM_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBM_resid)

TBN_model <- lm(log(CPUE) ~ year, TB_N)
TBN_resid<- resid(TBN_model)
plot(TB_N$year, TBN_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBN_resid)

  #B. test residuals for correlation 

library(car)
durbinWatsonTest(TBA_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBB_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBC_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBD_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBE_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBK_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBL_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBM_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")
durbinWatsonTest(TBN_model, max.lag=1, simulate=TRUE, reps=1000, method="resample")

# So far it seems with the log transform the data are homoskedastic and the 
# residuals are random. 

###############################################
#2. Calcualate geographic distance between sites along with correlation between CPUE from each Zone. 
#     -> I think I will have to do some GIS plotting to determine the distribution of sites used within each Zone. 




