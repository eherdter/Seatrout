

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
#2. Add log transformed CPUE to each data frame

TB_A$logCPUE <- log(TB_A$CPUE)
TB_B$logCPUE <- log(TB_B$CPUE)
TB_C$logCPUE <- log(TB_C$CPUE)
TB_D$logCPUE <- log(TB_D$CPUE)
TB_E$logCPUE <- log(TB_E$CPUE)
TB_K$logCPUE <- log(TB_K$CPUE)
TB_L$logCPUE <- log(TB_L$CPUE)
TB_M$logCPUE <- log(TB_M$CPUE)
TB_N$logCPUE <- log(TB_N$CPUE)

AP_A$logCPUE <- log(AP_A$CPUE)
AP_B$logCPUE <- log(AP_B$CPUE)
AP_C$logCPUE <- log(AP_C$CPUE)

CK_B$logCPUE <- log(CK_B$CPUE)
CK_C$logCPUE <- log(CK_C$CPUE)
CK_F$logCPUE <- log(CK_F$CPUE)


CH_A$logCPUE <- log(CH_A$CPUE)
CH_B$logCPUE <- log(CH_B$CPUE)
CH_C$logCPUE <- log(CH_C$CPUE)
CH_D$logCPUE <- log(CH_D$CPUE)
CH_M$logCPUE <- log(CH_M$CPUE)
CH_P$logCPUE <- log(CH_P$CPUE)

IR_A$logCPUE <- log(IR_A$CPUE)
IR_B$logCPUE <- log(IR_B$CPUE)
IR_C$logCPUE <- log(IR_C$CPUE)
IR_D$logCPUE <- log(IR_D$CPUE)
IR_E$logCPUE <- log(IR_E$CPUE)
IR_H$logCPUE <- log(IR_H$CPUE)
IR_F$logCPUE <- log(IR_F$CPUE)

JX_A$logCPUE <- log(JX_A$CPUE)
JX_B$logCPUE <- log(JX_B$CPUE)
JX_C$logCPUE <- log(JX_C$CPUE)
JX_D$logCPUE <- log(JX_D$CPUE)
JX_E$logCPUE <- log(JX_E$CPUE)
JX_F$logCPUE <- log(JX_F$CPUE)


###############################################
#3. Correlation between the ecological phenomenon of interest  using all years for which data from both sites are available
#  > All Tampa Bay Zones 1996-2014
#  > Appalachicola A,B 1998-2014 C 2000-2014
#  > Cedar Key B,C 1996-2014 F 2001-2014
#  > Indian River D,C 1996-2014, H 1998-2014, F 2001-2014
#  > Jax A,B,C,D,E 2001-2014, F 2005-2014
#  > Charlotte Harbor A, B,C, M, P 1996-2014  D 2004-2014

# # Can correlate log(CPUE)
#   > 1996-2014: Tampa Bay (ALL), Cedar Key  (B,C), Indian River(D,C), Charlotte Harbor (A,B,C,M,P)
#   > 1998-2014: Tampa Bay truncated (All), Cedar Key (B,C- truncated), Appalachicola (A,B), Indian River (D,C- truncated) and H, 
#   > 2001-2014: All except Jax F and E, CHarlotte Harbor D


#1996-2014 
ninetysix <- cbind(TB_A$logCPUE, TB_B$logCPUE, TB_C$logCPUE, TB_D$logCPUE, TB_E$logCPUE, TB_K$logCPUE, TB_L$logCPUE, TB_M$logCPUE, TB_N$logCPUE, CK_B$logCPUE, CK_C$logCPUE, IR_D$logCPUE, IR_C$logCPUE, CH_A$logCPUE, CH_B$logCPUE, CH_C$logCPUE, CH_M$logCPUE, CH_P$logCPUE)
colnames(ninetysix) <- c("TB_A", "TB_B", "TB_C", "TB_D", "TB_E", "TB_K", "TB_L", "TB_M", "TB_N", "CK_B", "CK_C", "IR_D", "IR_C", "CH_A", "CH_B", "CH_C", "CH_M", "CH_N")

library(ltm)
cors_ninetysix <- rcor.test(ninetysix, p.adjust=TRUE, p.adjust.method="bonferroni", method="pearson")
    cors_ninetysix_mat <- cors_ninetysix$cor.mat
    cors_ninetysix_pval <- data.frame(cors_ninetysix$p.values)
    cors_ninetysix_sig <- subset(cors_ninetysix_pval, pvals <0.05)

#1998-2014
ninetyeight <- cbind(subset(TB_A, year > 1997, select=c(logCPUE)),
                      subset(TB_B, year > 1997, select=c(logCPUE)),
                      subset(TB_C, year > 1997, select=c(logCPUE)),
                      subset(TB_D, year > 1997, select=c(logCPUE)),
                      subset(TB_E, year > 1997, select=c(logCPUE)),
                      subset(TB_K, year > 1997, select=c(logCPUE)),
                      subset(TB_L, year > 1997, select=c(logCPUE)),
                      subset(TB_M, year > 1997, select=c(logCPUE)),
                      subset(TB_N, year > 1997, select=c(logCPUE)),
                      subset(CK_B, year > 1997, select=c(logCPUE)),
                      subset(CK_C, year > 1997, select=c(logCPUE)),
                      subset(IR_C, year > 1997, select=c(logCPUE)),
                      subset(IR_D, year > 1997, select=c(logCPUE)),
                       subset(CH_A, year > 1997, select=c(logCPUE)),
                      subset(CH_B, year > 1997, select=c(logCPUE)),
                      subset(CH_C, year > 1997, select=c(logCPUE)),
                      subset(CH_M, year > 1997, select=c(logCPUE)),
                      subset(CH_P, year > 1997, select=c(logCPUE)),
                      AP_A$logCPUE,AP_B$logCPUE,IR_H$logCPUE)
colnames(ninetyeight) <- c("TB_A", "TB_B", "TB_C", "TB_D", "TB_E", "TB_K", "TB_L", "TB_M", "TB_N", "CK_B", "CK_C", "IR_C", "IR_D", "AP_A", "AP_B" , "IR_H")
cors_ninetyeight <- rcor.test(ninetyeight, p.adjust=TRUE, p.adjust.method="bonferroni", method="pearson")
    cors_ninetyeight_mat <- cors_ninetyeight$cor.mat
    cors_ninetyeight_pval <- data.frame(cors_ninetyeight$p.values)
    cors_ninetyeight_sig <- subset(cors_ninetyeight_pval, pvals < 0.05)

#2001-2014
oh_one <- cbind(subset(TB_A, year > 2000, select=c(logCPUE)),
                subset(TB_B, year > 2000, select=c(logCPUE)),
                subset(TB_C, year > 2000, select=c(logCPUE)),
                subset(TB_D, year > 2000, select=c(logCPUE)),
                subset(TB_E, year > 2000, select=c(logCPUE)),
                subset(TB_K, year > 2000, select=c(logCPUE)),
                subset(TB_L, year > 2000, select=c(logCPUE)),
                subset(TB_M, year > 2000, select=c(logCPUE)),
                subset(TB_N, year > 2000, select=c(logCPUE)),
                subset(CK_B, year > 2000, select=c(logCPUE)),
                subset(CK_C, year > 2000, select=c(logCPUE)),
                subset(CK_F, year > 2000, select=c(logCPUE)),
                subset(IR_C, year > 2000, select=c(logCPUE)), 
                subset(IR_D, year > 2000, select=c(logCPUE)),
                subset(IR_H, year > 2000, select=c(logCPUE)),
                subset(IR_F, year > 2000, select=c(logCPUE)),
                subset(CH_A, year > 2000, select=c(logCPUE)),
                subset(CH_B, year > 2000, select=c(logCPUE)),
                subset(CH_C, year > 2000, select=c(logCPUE)),
                subset(CH_M, year > 2000, select=c(logCPUE)),
                subset(CH_P, year > 2000, select=c(logCPUE)),
                subset(AP_A, year > 2000, select=c(logCPUE)),
                subset(AP_B, year > 2000, select=c(logCPUE)),
                subset(AP_C, year > 2000, select=c(logCPUE)),
                subset(JX_A, year >= 2001, select=c(logCPUE)),
                subset(JX_B, year >= 2001, select=c(logCPUE)),
                subset(JX_C, year >= 2001, select=c(logCPUE)),
                subset(JX_D, year >= 2001, select=c(logCPUE)))
 



colnames(oh_one) <- c("TB_A", "TB_B", "TB_C", "TB_D", "TB_E", "TB_K", "TB_L", "TB_M", "TB_N", "CK_B", "CK_C", "CK_F", "IR_C", "IR_D", "IR_H", "IR_F", "CH_A", "CH_B", "CH_C", "CH_M", "CH_P", "AP_A", "AP_B" ,"AP_C", "JX_A", "JX_B", "JX_C", "JX_D") 
cors_oh_one <- rcor.test(oh_one, p.adjust=TRUE, p.adjust.method="bonferroni", method="pearson")
cors_oh_one_mat <- cors_oh_one$cor.mat
cors_oh_one_pval <- data.frame(cors_oh_one$p.values)
cors_oh_one_sig <- subset(cors_oh_one_pval, pvals < 0.05)


                     
