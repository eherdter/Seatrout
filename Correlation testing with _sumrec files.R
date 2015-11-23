

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

########################################################
##### TEST STRATUMS WITHIN ZONES ######
########################################################

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

###################################
#1.  Testing distribution and normal, randomness of residuals to prepare for correlation testing. 
# data to be analyzed should have normally-distributed residuals that are uncorrelated with the independent variable. 

############################

# A. Test residuals for homscedasticity 
#add 0.01 to CPUE so that zeros can still be transformed
TBA_ONV$CPUEe <- TBA_ONV$CPUE + 0.01
TBA_ONV_model <- lm(log(CPUEe) ~ year, TBA_ONV)
TBA_ONV_resid<- resid(TBA_ONV_model)
plot(TBA_ONV$year, TBA_ONV_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBA_ONV_resid)

TBB_ONV$CPUEe <- TBB_ONV$CPUE + 0.01
TBB_ONV_model <- lm(log(CPUEe) ~ year, TBB_ONV)
TBB_ONV_resid<- resid(TBB_ONV_model)
plot(TBB_ONV$year, TBB_ONV_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBB_ONV_resid)

TBC_ONV$CPUEe <- TBC_ONV$CPUE + 0.01
TBC_ONV_model <- lm(log(CPUEe) ~ year, TBC_ONV)
TBC_ONV_resid<- resid(TBC_ONV_model)
plot(TBC_ONV$year, TBC_ONV_resid, ylab= "Residuals", xlab="Year")
abline(0,0)
hist(TBC_ONV_resid)
### will log transform all CPUE data

###############################################
#Add log transformed CPUE to each data frame
#CPUEe is log transformed (CPUE +0.01) to deal with zeros present in data

APA_ONV$logCPUEe <- log(APA_ONV$CPUE + 0.01)
APA_OV$logCPUEe  <- log(APA_OV$CPUE + 0.01)
APA_S$logCPUEe  <- log(APA_S$CPUE + 0.01)

APB_ONV$logCPUEe <- log(APB_ONV$CPUE + 0.01)
APB_OV$logCPUEe  <- log(APB_OV$CPUE + 0.01)
APB_S$logCPUEe  <- log(APB_S$CPUE + 0.01)

CHA_ONV$logCPUEe <- log(CHA_ONV$CPUE + 0.01)
CHA_OV$logCPUEe  <- log(CHA_OV$CPUE + 0.01)
CHA_S$logCPUEe  <- log(CHA_S$CPUE + 0.01)

CHB_ONV$logCPUEe <- log(CHB_ONV$CPUE + 0.01)
CHB_OV$logCPUEe  <- log(CHB_OV$CPUE + 0.01)
CHB_S$logCPUEe  <- log(CHB_S$CPUE + 0.01)

CHC_ONV$logCPUEe <- log(CHC_ONV$CPUE + 0.01)
CHC_OV$logCPUEe  <- log(CHC_OV$CPUE + 0.01)
CHC_S$logCPUEe  <- log(CHC_S$CPUE + 0.01)

CHD_ONV$logCPUEe <- log(CHD_ONV$CPUE + 0.01)
CHD_OV$logCPUEe  <- log(CHD_OV$CPUE + 0.01)
CHD_S$logCPUEe  <- log(CHD_S$CPUE + 0.01)

CKB_ONV$logCPUEe <- log(CKB_ONV$CPUE + 0.01)
CKB_OV$logCPUEe  <- log(CKB_OV$CPUE + 0.01)
CKB_S$logCPUEe  <- log(CKB_S$CPUE + 0.01)

CKC_ONV$logCPUEe <- log(CKC_ONV$CPUE + 0.01)
CKC_OV$logCPUEe  <- log(CKC_OV$CPUE + 0.01)
CKC_S$logCPUEe  <- log(CKC_S$CPUE + 0.01)

IRA_ONV$logCPUEe <- log(IRA_ONV$CPUE + 0.01)
IRA_OV$logCPUEe  <- log(IRA_OV$CPUE + 0.01)
IRA_S$logCPUEe  <- log(IRA_S$CPUE + 0.01)

IRB_ONV$logCPUEe <- log(IRB_ONV$CPUE + 0.01)
IRB_OV$logCPUEe  <- log(IRB_OV$CPUE + 0.01)
IRB_S$logCPUEe  <- log(IRB_S$CPUE + 0.01)

IRC_ONV$logCPUEe <- log(IRC_ONV$CPUE + 0.01)
IRC_OV$logCPUEe  <- log(IRC_OV$CPUE + 0.01)
IRC_S$logCPUEe  <- log(IRC_S$CPUE + 0.01)

IRD_ONV$logCPUEe <- log(IRD_ONV$CPUE + 0.01)
IRD_OV$logCPUEe  <- log(IRD_OV$CPUE + 0.01)
IRD_S$logCPUEe  <- log(IRD_S$CPUE + 0.01)

IRE_ONV$logCPUEe <- log(IRE_ONV$CPUE + 0.01)
IRE_OV$logCPUEe  <- log(IRE_OV$CPUE + 0.01)
IRE_S$logCPUEe  <- log(IRE_S$CPUE + 0.01)

IRH_ONV$logCPUEe <- log(IRH_ONV$CPUE + 0.01)
IRH_OV$logCPUEe  <- log(IRH_OV$CPUE + 0.01)
IRH_S$logCPUEe  <- log(IRH_S$CPUE + 0.01)

TBA_ONV$logCPUEe <- log(TBA_ONV$CPUE + 0.01)
TBA_OV$logCPUEe  <- log(TBA_OV$CPUE + 0.01)
TBA_S$logCPUEe  <- log(TBA_S$CPUE + 0.01)

TBB_ONV$logCPUEe <- log(TBB_ONV$CPUE + 0.01)
TBB_OV$logCPUEe  <- log(TBB_OV$CPUE + 0.01)
TBB_S$logCPUEe  <- log(TBB_S$CPUE + 0.01)

TBC_ONV$logCPUEe <- log(TBC_ONV$CPUE + 0.01)
TBC_OV$logCPUEe  <- log(TBC_OV$CPUE + 0.01)
TBC_S$logCPUEe  <- log(TBC_S$CPUE + 0.01)

TBD_ONV$logCPUEe <- log(TBD_ONV$CPUE + 0.01)
TBD_OV$logCPUEe  <- log(TBD_OV$CPUE + 0.01)
TBD_S$logCPUEe  <- log(TBD_S$CPUE + 0.01)

TBE_ONV$logCPUEe <- log(TBE_ONV$CPUE + 0.01)
TBE_OV$logCPUEe  <- log(TBE_OV$CPUE + 0.01)
TBE_S$logCPUEe  <- log(TBE_S$CPUE + 0.01)

###############################################
# Correlation between the ecological phenomenon of interest  using all years for which data from both sites are available
#  > All Tampa Bay Zones and Stratums 1996-2014
#  > Appalachicola A,B 1998-2014 
#  > Cedar Key B,C 1996-2014 
#  > Indian River C,D,F 1998-2014
#  > Charlotte Harbor A, B,C,1996-2014  D 2004-2014

# First, add NA to missing years of all data frames so that they can be compared together rather than in steps

    #TB#_S missing 1996, 1997. Will rbind to the beginning. 
newrow_TB <- matrix(c(1,2, 1996,1997), ncol=2, nrow=2 )
colnames(newrow_TB) <- c("X", "year")
TBA_S_up<- data.frame(rbind.fill.matrix(newrow_TB, TBA_S))
TBB_S_up<- rbind.fill.matrix(newrow_TB, TBB_S)
TBC_S_up<- rbind.fill.matrix(newrow_TB, TBC_S)
TBD_S_up<- rbind.fill.matrix(newrow_TB, TBD_S)
TBE_S_up<- rbind.fill.matrix(newrow_TB, TBE_S)

    # CKB_OV missing 2009 and 2014. Will insert 2009 and then rbind 2014 to the end.
insertRow <- function(existingDF, newrow, r) {
existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
existingDF[r,] <- newrow
existingDF
} 
newrow14 <- cbind(14, 2009, "NA", "NA", "NA", "NA")
r14 <- 14
CKB_OV <- data.frame(insertRow(CKB_OV, newrow14, r14))

newrow19 <- data.frame(cbind(19, 2014, "NA", "NA", "NA", "NA"))
names(newrow19) <- names(CKB_OV)
CKB_OV_up <- rbind(CKB_OV, newrow19) #for some reason the insertRow command is not working when its the last row of the dataframe

    #CK#_S missing 1996,1997, 1998, 1999, 2000
newrow_CK <-  matrix(c(1,2,3,4,5, 1996,1997, 1998, 1999, 2000), ncol=2, nrow=5 )
colnames(newrow_CK) <- c("X", "year")
CKB_S_up<- rbind.fill.matrix(newrow_CK, CKB_S)
CKC_S_up<- rbind.fill.matrix(newrow_CK, CKC_S)

    #AP#_S missing 96, 97
newrow_AP <- matrix(c(1,2, 1996,1997), ncol=2, nrow=2 )
colnames(newrow_AP) <- c("X", "year")
APA_S_up <- rbind.fill.matrix(newrow_AP, APA_S)
APB_S_up <- rbind.fill.matrix(newrow_AP, APB_S)

    #AP#_OV missing 96, 97
APA_OV_up <- rbind.fill.matrix(newrow_AP, APA_OV)
APB_OV_up <- rbind.fill.matrix(newrow_AP, APB_OV)

    #AP#_ONV missing 96, 97
APA_ONV_up <- rbind.fill.matrix(newrow_AP, APA_ONV)
APB_ONV_up <- rbind.fill.matrix(newrow_AP, APB_ONV)

    #CHD_OV missing 98,99, 2000,2001,2002,2003
newrow_CH <- matrix(c(3,4,5,6,7,8, 1998,1999,2000,2001,2002,2003 ), ncol=2, nrow=6)
colnames(newrow_CH) <- c("X", "year")
CHD_OV_start <- CHD_OV[1:2,]
CHD_OV_end   <- CHD_OV[3:13,]
CHD_OV_test <- rbind.fill.matrix(newrow_CH,CHD_OV_end) #for some reason this only works if the matrix that needs filling is called first
CHD_OV_up <- rbind(CHD_OV_start, CHD_OV_test)

    #CH#_S missing 96, 97 and CHD_S missing 1996-2003
newrow_CH2 <- matrix(c(1,2,1996,1997), ncol=2, nrow=2)
colnames(newrow_CH2) <- c("X", "year")
newrow_CH3 <- matrix(c(1,2,3,4,5,6,7,8,1996,1997, 1998,1999,2000, 2001,2002,2003), ncol=2, nrow=8)
colnames(newrow_CH3) <- c("X", "year")
CHA_S_up <- data.frame(rbind.fill.matrix(newrow_CH2, CHA_S))
CHB_S_up <- data.frame(rbind.fill.matrix(newrow_CH2, CHB_S))
CHC_S_up <- data.frame(rbind.fill.matrix(newrow_CH2, CHC_S))
CHD_S_up <- data.frame(rbind.fill.matrix(newrow_CH3, CHD_S))


    #IR#_S missing 96,97
newrow_IR <- matrix(c(1,2,1996, 1997), ncol=2, nrow=2)
colnames(newrow_IR)<- c("X", "year")
IRC_S_up<- data.frame(rbind.fill.matrix(newrow_IR, IRC_S))
IRD_S_up<- data.frame(rbind.fill.matrix(newrow_IR, IRD_S))
IRH_S_up<- data.frame(rbind.fill.matrix(newrow_IR, IRH_S))

IRH_OV_up <- data.frame(rbind.fill.matrix(newrow_IR, IRH_OV))
IRH_ONV_up <- data.frame(rbind.fill.matrix(newrow_IR, IRH_ONV))


# Bind series together to then correlate the variable named logCPUEe
#
#1996-2014 
ninetysix_Stratum <- cbind(TBA_ONV$logCPUEe, TBA_OV$logCPUEe, TBA_S_up$logCPUEe,
                           TBB_ONV$logCPUEe, TBB_OV$logCPUEe, TBB_S_up$logCPUEe,
                           TBC_ONV$logCPUEe, TBC_OV$logCPUEe, TBC_S_up$logCPUEe,
                           TBD_ONV$logCPUEe, TBD_OV$logCPUEe, TBD_S_up$logCPUEe,
                           TBE_ONV$logCPUEe, TBE_OV$logCPUEe, TBE_S_up$logCPUEe,
                           CKC_ONV$logCPUEe, CKC_OV$logCPUEe, CKC_S_up$logCPUEe, 
                           CKB_ONV$logCPUEe, CKB_OV$logCPUEe, CKB_S_up$logCPUEe, 
                           CHA_ONV$logCPUEe, CHA_OV$logCPUEe, CHA_S_up$logCPUEe,
                           CHB_ONV$logCPUEe, CHB_OV$logCPUEe, CHB_S_up$logCPUEe,
                           CHC_ONV$logCPUEe, CHC_OV$logCPUEe, CHC_S_up$logCPUEe,
                            CHD_OV_up$logCPUEe, CHD_S_up$logCPUEe,
                           APA_ONV_up$logCPUEe,  APA_OV_up$logCPUEe, APA_S_up$logCPUEe,
                           APB_ONV_up$logCPUEe,  APB_OV_up$logCPUEe, APB_S_up$logCPUEe,
                           IRC_ONV$logCPUEe,IRC_OV$logCPUEe, IRC_S_up$logCPUEe, 
                           IRD_ONV$logCPUEe,IRD_OV$logCPUEe, IRD_S_up$logCPUEe,
                           IRH_ONV$logCPUEe,IRH_OV$logCPUEe, IRH_S_up$logCPUEe)


                   
                   
                   
                  
colnames(ninetysix_Stratum) <- c("TBA_ONV","TBA_OV","TBA_S", 
                                 "TBB_ONV","TBB_OV","TBB_S", 
                                 "TBC_ONV","TBC_OV","TBC_S", 
                                 "TBD_ONV","TBD_OV","TBD_S", 
                                 "TBE_ONV","TBE_OV","TBE_S", 
                                 "CKB_ONV","CKB_OV","CKB_S",
                                 "CKC_ONV","CKC_OV","CKC_S",
                                 "CHA_ONV","CHA_OV","CHA_S",
                                 "CHB_ONV","CHB_OV","CHB_S",
                                 "CHC_ONV","CHC_OV","CHC_S")
                                 
                                 
                                 
                               

library(ltm)
cors_ninetysix_stratum <- rcor.test(ninetysix_STRATUM, p.adjust=TRUE, p.adjust.method="bonferroni", method="pearson")
cors_ninetysix_stratum_mat <- cors_ninetysix_stratum$cor.mat
cors_ninetysix_stratum_pval <- data.frame(cors_ninetysix_stratum$p.values)
cors_ninetysix_stratum_sig <- subset(cors_ninetysix_stratum_pval, pvals <0.05)

