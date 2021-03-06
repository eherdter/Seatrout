# Low frequency- three year running medians 
setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")

#####################################################
## First, by Zone
#####################################################
TB_A <- read.csv("TampaBay_Bay_A_sumrec.csv")
  TB_A$runmed <-runmed(TB_A$CPUE, 3)
  write.csv(TB_A, "TB_A_with_runmed.csv")
  TB_A$resid <- TB_A$CPUE - TB_A$runmed
TB_B <- read.csv("TampaBay_Bay_B_sumrec.csv")
  TB_B$runmed <-runmed(TB_B$CPUE, 3)
  write.csv(TB_B, "TB_B_with_runmed.csv")
  TB_B$resid <- TB_B$CPUE - TB_B$runmed
TB_C <- read.csv("TampaBay_Bay_C_sumrec.csv")
  TB_C$runmed <-runmed(TB_C$CPUE, 3)
  TB_C$resid <- TB_C$CPUE - TB_C$runmed
TB_D <- read.csv("TampaBay_Bay_D_sumrec.csv")
  TB_D$runmed <-runmed(TB_D$CPUE, 3)
  TB_D$resid <- TB_D$CPUE - TB_D$runmed
TB_E <- read.csv("TampaBay_Bay_E_sumrec.csv")
  TB_E$runmed <-runmed(TB_E$CPUE, 3)
  TB_E$resid <- TB_E$CPUE - TB_E$runmed
TB_K <- read.csv("TampaBay_Riv_K_sumrec.csv")
  TB_K$runmed <-runmed(TB_K$CPUE, 3)
  TB_K$resid <- TB_K$CPUE - TB_K$runmed
TB_L <- read.csv("TampaBay_Riv_L_sumrec.csv")
  TB_L$runmed <-runmed(TB_L$CPUE, 3)
  TB_L$resid <- TB_L$CPUE - TB_L$runmed
TB_M <- read.csv("TampaBay_Riv_M_sumrec.csv")
  TB_M$runmed <-runmed(TB_M$CPUE, 3)
  TB_M$resid <- TB_M$CPUE - TB_M$runmed
TB_N <- read.csv("TampaBay_Riv_N_sumrec.csv")
TB_N$runmed <- runmed(TB_N$CPUE, k=3) 
# because I cant figure out how to properly deal with NAs I've decided to just edit in Excel
  write.csv(TB_N, "TB_N_with_runmed.csv")
  TB_N_up <- read.csv("TB_N_with_runmed_and_missing_years_added.csv")


AP_A <- read.csv("Appalachicola_Bay_A_sumrec.csv")
AP_A$runmed <- runmed(AP_A$CPUE, k=3)
  write.csv(AP_A, "AP_A_with_runmed.csv")
  AP_A_up <- read.csv("AP_A_with_runmed_and_missing_years_added.csv")
AP_B <- read.csv("Appalachicola_Bay_B_sumrec.csv")
AP_B$runmed <-runmed(AP_B$CPUE, 3)
  write.csv(AP_B, "AP_B_with_runmed.csv")
  AP_B_up <- read.csv("AP_B_with_runmed_and_missing_years_added.csv")
AP_C <- read.csv("Appalachicola_Riv_C_sumrec.csv")
AP_C$runmed <-runmed(AP_C$CPUE, 3)
  write.csv(AP_C, "AP_C_with_runmed.csv")
  AP_C_up <- read.csv("AP_C_with_runmed_and_missing_years_added.csv")

CK_B <- read.csv("CedarKey_Bay_B_sumrec.csv")
  CK_B$runmed <-runmed(CK_B$CPUE, 3)
CK_C <- read.csv("CedarKey_Bay_C_sumrec.csv")
  CK_C$runmed <-runmed(CK_C$CPUE, 3)
CK_F <- read.csv("CedarKey_Riv_F_sumrec.csv")
CK_F$runmed <-runmed(CK_F$CPUE, 3)
  write.csv(CK_F, "CK_F_with_runmed.csv")
  CK_F_up <- read.csv("CK_F_with_runmed_and_missing_years_added.csv")

CH_A <- read.csv("CharlotteHarbor_Bay_A_sumrec.csv")
  CH_A$runmed <-runmed(CH_A$CPUE, 3)
CH_B <- read.csv("CharlotteHarbor_Bay_B_sumrec.csv")
  CH_B$runmed <-runmed(CH_B$CPUE, 3)
CH_C <- read.csv("CharlotteHarbor_Bay_C_sumrec.csv")
  CH_C$runmed <-runmed(CH_C$CPUE, 3)
CH_D <- read.csv("CharlotteHarbor_Bay_D_sumrec.csv")
CH_D$runmed <-runmed(CH_D$CPUE, 3)
  write.csv(CH_D, "CH_D_with_runmed.csv")
  CH_D_up <- read.csv("CH_D_with_runmed_and_missing_years_added.csv")
CH_M <- read.csv("CharlotteHarbor_Riv_M_sumrec.csv")
  CH_M$runmed <-runmed(CH_M$CPUE, 3)
  write.csv(CH_M, "CH_M_with_runmed.csv")
CH_P <- read.csv("CharlotteHarbor_Riv_P_sumrec.csv")
  CH_P$runmed <-runmed(CH_P$CPUE, 3)

IR_A <- read.csv("IndianRiver_Bay_A_sumrec.csv")
  IR_A$runmed <-runmed(IR_A$CPUE, 3)
IR_B <- read.csv("IndianRiver_Bay_B_sumrec.csv")
  IR_B$runmed <-runmed(IR_B$CPUE, 3)
IR_C <- read.csv("IndianRiver_Bay_C_sumrec.csv")
  IR_C$runmed <-runmed(IR_C$CPUE, 3)
IR_D <- read.csv("IndianRiver_Bay_D_sumrec.csv")
  IR_D$runmed <-runmed(IR_D$CPUE, 3)
IR_E <- read.csv("IndianRiver_Bay_E_sumrec.csv")
  IR_E$runmed <-runmed(IR_E$CPUE, 3)
IR_H <- read.csv("IndianRiver_Bay_H_sumrec.csv")
IR_H$runmed <-runmed(IR_H$CPUE, 3)
  write.csv(IR_H, "IR_H_with_runmed.csv")
  IR_H_up <- read.csv("IR_H_with_runmed_and_missing_years_added.csv")
IR_F <- read.csv("IndianRiver_Riv_F_sumrec.csv")
IR_F$runmed <-runmed(IR_F$CPUE, 3)
  write.csv(IR_F, "IR_F_with_runmed.csv")
  IR_F_up <- read.csv("IR_F_with_runmed_and_missing_years_added.csv")

JX_A <-read.csv("Jax_Riv_A_sumrec.csv")
JX_A$runmed <-runmed(JX_A$CPUE, 3)
  write.csv(JX_A, "JX_A_with_runmed.csv")
  JX_A_up <- read.csv("JX_A_with_runmed_and_missing_years_added.csv")
JX_B <-read.csv("Jax_Riv_B_sumrec.csv")
JX_B$runmed <-runmed(JX_B$CPUE, 3)
  write.csv(JX_B, "JX_B_with_runmed.csv")
  JX_B_up <- read.csv("JX_B_with_runmed_and_missing_years_added.csv")
JX_C <-read.csv("Jax_Riv_C_sumrec.csv")
JX_C$runmed <-runmed(JX_C$CPUE, 3)
  write.csv(JX_C, "JX_C_with_runmed.csv")
  JX_C_up <- read.csv("JX_C_with_runmed_and_missing_years_added.csv")
JX_D <-read.csv("Jax_Riv_D_sumrec.csv")
JX_D$runmed <-runmed(JX_D$CPUE, 3)
  write.csv(JX_D, "JX_D_with_runmed.csv")
  JX_D_up <- read.csv("JX_D_with_runmed_and_missing_years_added.csv")
JX_E <-read.csv("Jax_Riv_E_sumrec.csv")
JX_E$runmed <-runmed(JX_E$CPUE, 3)
  write.csv(JX_E, "JX_E_with_runmed.csv")
  JX_E_up <- read.csv("JX_E_with_runmed_and_missing_years_added.csv")
JX_F <-read.csv("Jax_Riv_F_sumrec.csv")
JX_F$runmed <-runmed(JX_F$CPUE, 3)
  write.csv(JX_F, "JX_F_with_runmed.csv")
  JX_F_up <- read.csv("JX_F_with_runmed_and_missing_years_added.csv")


# Combine all to perform correlation testing
    
year <- data.frame(matrix(c(1996,1997,1998, 1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014), ncol=1, nrow=19))
colnames(year) <- "year"

Zone_runmeds <- cbind(TB_A$runmed, TB_B$runmed, TB_C$runmed, TB_D$runmed, 
                      TB_E$runmed, TB_K$runmed, TB_L$runmed, TB_M$runmed, 
                      TB_N_up$runmed, CH_A$runmed, CH_B$runmed, CH_C$runmed,
                      CH_D_up$runmed, CH_M$runmed, CH_P$runmed, AP_A_up$runmed, 
                      AP_B_up$runmed, AP_C_up$runmed, CK_B$runmed, CK_C$runmed, 
                      CK_F_up$runmed, IR_C$runmed, IR_D$runmed, IR_F_up$runmed,
                      IR_H_up$runmed, JX_A_up$runmed, JX_B_up$runmed, JX_C_up$runmed,
                      JX_D_up$runmed, JX_E_up$runmed, JX_F_up$runmed)

library(hmisc)  #rcorr- missing values are deleted in pairs rather than deleting all rows of x having any missing variables
Low_frequency_Zone_hmisc <- rcorr(Zone_runmeds,type=c("pearson"))
write.csv(Low_frequency_Zone_hmisc$r, "Low_frequency_Zone_hmisc_rval.csv")
Low_frequency_Zone_hmisc_Pval <- Low_frequency_Zone_hmisc$P
write.csv(Low_frequency_Zone_hmisc_Pval, "Low_frequency_Zone_hmisc_Pval.csv")

#adjust p-vales for repeated correlations.. edited matrix to remove top diagonal
Low_frequency_Zone_hmisc_Pval_edited <- as.matrix(read.csv("Low_frequency_Zone_hmisc_Pval_edited.csv"))
P2<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,2]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,2]))
P3<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,3]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,3]))
P4<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,4]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,4]))
P5<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,5]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,5]))
P6<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,6]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,6]))
P7<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,7]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,7]))
P8<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,8]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,8]))
P9<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,9]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,9]))
P10<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,10]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,10]))
P11<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,11]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,11]))
P12<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,12]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,12]))
P13<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,13]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,13]))
P14<-p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,14]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,14]))
P15<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,15]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,15]))
P16<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,16]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,16]))
P17<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,17]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,17]))
P18<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,18]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,18]))
P19<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,19]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,19]))
P20<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,20]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,20]))
P21<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,21]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,21]))
P22<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,22]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,22]))
P23<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,23]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,23]))
P24<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,24]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,24]))
P25<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,25]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,25]))
P26<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,26]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,26]))
P27<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,27]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,27]))
P28<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,28]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,28]))
P29<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,29]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,29]))
P30<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,30]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,30]))
P31<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,31]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,31]))
P32<- p.adjust((Low_frequency_Zone_hmisc_Pval_edited[,32]), method= "bonferroni", n=length(Low_frequency_Zone_hmisc_Pval_edited[,32]))

adjusted_pvals <-cbind(P2, P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23,P24,
                       P25,P26,P27,P28,P29,P30,P31,P32)
write.csv(adjusted_pvals, "Low_frequency_Zone_hmisc_adjusted_Pvals.csv")

######################################################
### Second, by Stratum
######################################################

APA_ONV <- read.csv("APA_ONV_sumrec.csv")
APA_ONV$runmed <-runmed(APA_ONV$CPUE, 3)
  write.csv(APA_ONV, "APA_ONV_with_runmed.csv")
  APA_ONV_up<- read.csv("APA_ONV_with_runmed_and_missing_years_added.csv")

APA_OV <- read.csv("APA_OV_sumrec.csv")
APA_OV$runmed <-runmed(APA_OV$CPUE, 3)
  write.csv(APA_OV, "APA_OV_with_runmed.csv")
  APA_OV_up<- read.csv("APA_OV_with_runmed_and_missing_years_added.csv")

APA_S <- read.csv("APA_S_sumrec.csv")
APA_S$runmed <-runmed(APA_S$CPUE, 3)
  write.csv(APA_S, "APA_S_with_runmed.csv")
  APA_S_up<- read.csv("APA_S_with_runmed_and_missing_years_added.csv")

APB_ONV<- read.csv("APB_ONV_sumrec.csv")
APB_ONV$runmed <-runmed(APB_ONV$CPUE, 3)
  write.csv(APB_ONV, "APB_ONV_with_runmed.csv")
  APB_ONV_up<- read.csv("APB_ONV_with_runmed_and_missing_years_added.csv")

APB_OV <- read.csv("APB_OV_sumrec.csv")
APB_OV$runmed <-runmed(APB_OV$CPUE, 3)
  write.csv(APB_OV, "APB_OV_with_runmed.csv")
  APB_OV_up<- read.csv("APB_OV_with_runmed_and_missing_years_added.csv")

APB_S <- read.csv("APB_S_sumrec.csv")
APB_S$runmed <-runmed(APB_S$CPUE, 3)
  write.csv(APB_S, "APB_S_with_runmed.csv")
  APB_S_up<- read.csv("APB_S_with_runmed_and_missing_years_added.csv")

CHA_ONV <- read.csv("CHA_ONV_sumrec.csv")
CHA_ONV$runmed <-runmed(CHA_ONV$CPUE, 3)
  
CHA_OV <- read.csv("CHA_OV_sumrec.csv")
CHA_OV$runmed <-runmed(CHA_OV$CPUE, 3)

CHA_S <- read.csv("CHA_S_sumrec.csv")
CHA_S$runmed <-runmed(CHA_S$CPUE, 3)
  write.csv(CHA_S, "CHA_S_with_runmed.csv")
  CHA_S_up<- read.csv("CHA_S_with_runmed_and_missing_years_added.csv")

CHB_ONV <- read.csv("CHB_ONV_sumrec.csv")
CHB_ONV$runmed <-runmed(CHB_ONV$CPUE, 3)

CHB_OV <- read.csv("CHB_OV_sumrec.csv")
CHB_OV$runmed <-runmed(CHB_OV$CPUE, 3)

CHB_S <- read.csv("CHB_S_sumrec.csv")
CHB_S$runmed <-runmed(CHB_S$CPUE, 3)
  write.csv(CHB_S, "CHB_S_with_runmed.csv")
  CHB_S_up<- read.csv("CHB_S_with_runmed_and_missing_years_added.csv")

CHC_ONV <- read.csv("CHC_ONV_sumrec.csv")
CHC_ONV$runmed <-runmed(CHC_ONV$CPUE, 3)

CHC_OV <- read.csv("CHC_OV_sumrec.csv")
CHC_OV$runmed <-runmed(CHC_OV$CPUE, 3)

CHC_S <- read.csv("CHC_S_sumrec.csv")
CHC_S$runmed <-runmed(CHC_S$CPUE, 3)
  write.csv(CHC_S, "CHC_S_with_runmed.csv")
  CHC_S_up<- read.csv("CHC_S_with_runmed_and_missing_years_added.csv")

#CHD_ONV <- read.csv("CHD_ONV_sumrec.csv")- this is missing until 2004 and after that CPUE is 0 across the board
CHD_OV<- read.csv("CHD_OV_sumrec.csv")
CHD_OV$runmed <-runmed(CHD_OV$CPUE, 3)
  write.csv(CHD_OV, "CHD_OV_with_runmed.csv")
  CHD_OV_up<- read.csv("CHD_OV_with_runmed_and_missing_years_added.csv")

CHD_S <- read.csv("CHD_S_sumrec.csv")
CHD_S$runmed <-runmed(CHD_S$CPUE, 3)
  write.csv(CHD_S, "CHD_S_with_runmed.csv")
  CHD_S_up<- read.csv("CHD_S_with_runmed_and_missing_years_added.csv")

CKB_ONV <- read.csv("CKB_ONV_sumrec.csv")
CKB_ONV$runmed <- runmed(CKB_ONV$CPUE, 3)

CKB_OV <- read.csv("CKB_OV_sumrec.csv")
CKB_OV$runmed <-runmed(CKB_OV$CPUE, 3)
  write.csv(CKB_OV, "CKB_OV_with_runmed.csv")
  CKB_OV_up<- read.csv("CKB_OV_with_runmed_and_missing_years_added.csv")

CKB_S <- read.csv("CKB_S_sumrec.csv")
CKB_S$runmed <-runmed(CKB_S$CPUE, 3)
  write.csv(CKB_S, "CKB_S_with_runmed.csv")
  CKB_S_up<- read.csv("CKB_S_with_runmed_and_missing_years_added.csv")

CKC_ONV<- read.csv("CKC_ONV_sumrec.csv")
CKC_ONV$runmed <- runmed(CKC_ONV$CPUE, 3)

CKC_OV <- read.csv("CKC_OV_sumrec.csv")
CKC_OV$runmed <- runmed(CKC_OV$CPUE, 3)

CKC_S<- read.csv("CKC_S_sumrec.csv")
CKC_S$runmed <-runmed(CKC_S$CPUE, 3)
  write.csv(CKC_S, "CKC_S_with_runmed.csv")
  CKC_S_up<- read.csv("CKC_S_with_runmed_and_missing_years_added.csv")

#IRA_ONV <- read.csv("IRA_ONV_sumrec.csv")
#IRA_OV <- read.csv("IRA_OV_sumrec.csv")
#IRA_S <- read.csv("IRA_S_sumrec.csv")

#IRB_ONV <- read.csv("IRB_ONV_sumrec.csv")
#IRB_OV <- read.csv("IRB_OV_sumrec.csv")
#IRB_S <- read.csv("IRB_S_sumrec.csv")

IRC_ONV <- read.csv("IRC_ONV_sumrec.csv")
IRC_ONV$runmed <- runmed(IRC_ONV$CPUE, 3)

IRC_OV <- read.csv("IRC_OV_sumrec.csv")
IRC_OV$runmed <-runmed(IRC_OV$CPUE, 3)
  write.csv(IRC_OV, "IRC_OV_with_runmed.csv")
  IRC_OV_up<- read.csv("IRC_OV_with_runmed_and_missing_years_added.csv")

IRC_S <- read.csv("IRC_S_sumrec.csv")
IRC_S$runmed <-runmed(IRC_S$CPUE, 3)
  write.csv(IRC_S, "IRC_S_with_runmed.csv")
  IRC_S_up<- read.csv("IRC_S_with_runmed_and_missing_years_added.csv")


IRD_ONV <- read.csv("IRD_ONV_sumrec.csv")
IRD_ONV$runmed <- runmed(IRD_ONV$CPUE,3)

IRD_OV <- read.csv("IRD_OV_sumrec.csv")
IRD_OV$runmed <- runmed(IRD_OV$CPUE, 3)

IRD_S <- read.csv("IRD_S_sumrec.csv")
IRD_S$runmed <-runmed(IRD_S$CPUE, 3)
  write.csv(IRD_S, "IRD_S_with_runmed.csv")
  IRD_S_up<- read.csv("IRD_S_with_runmed_and_missing_years_added.csv")

#IRE_ONV <- read.csv("IRE_ONV_sumrec.csv")
#IRE_OV <- read.csv("IRE_OV_sumrec.csv")
#IRE_S <- read.csv("IRE_S_sumrec.csv")

IRH_ONV <- read.csv("IRH_ONV_sumrec.csv")
IRH_ONV$runmed <-runmed(IRH_ONV$CPUE, 3)
  write.csv(IRH_ONV, "IRH_ONV_with_runmed.csv")
  IRH_ONV_up<- read.csv("IRH_ONV_with_runmed_and_missing_years_added.csv")

IRH_OV <- read.csv("IRH_OV_sumrec.csv")
IRH_OV$runmed <-runmed(IRH_OV$CPUE, 3)
  write.csv(IRH_OV, "IRH_OV_with_runmed.csv")
  IRH_OV_up<- read.csv("IRH_OV_with_runmed_and_missing_years_added.csv")

IRH_S <- read.csv("IRH_S_sumrec.csv")
IRH_S$runmed <-runmed(IRH_S$CPUE, 3)
  write.csv(IRH_S, "IRH_S_with_runmed.csv")
  IRH_S_up<- read.csv("IRH_S_with_runmed_and_missing_years_added.csv")

TBA_ONV <- read.csv("TBA_ONV_sumrec.csv")
TBA_ONV$runmed <- runmed(TBA_ONV$CPUE, 3)

TBA_OV <- read.csv("TBA_OV_sumrec.csv")
TBA_OV$runmed <- runmed(TBA_OV$CPUE, 3)

TBA_S <- read.csv("TBA_S_sumrec.csv")
TBA_S$runmed <-runmed(TBA_S$CPUE, 3)
  write.csv(TBA_S, "TBA_S_with_runmed.csv")
  TBA_S_up<- read.csv("TBA_S_with_runmed_and_missing_years_added.csv")

TBB_ONV <- read.csv("TBB_ONV_sumrec.csv")
TBB_ONV$runmed <- runmed(TBB_ONV$CPUE, 3)

TBB_OV <- read.csv("TBB_OV_sumrec.csv")
TBB_OV$runmed <- runmed(TBB_OV$CPUE, 3)

TBB_S <- read.csv("TBB_S_sumrec.csv")
TBB_S$runmed <-runmed(TBB_S$CPUE, 3)
  write.csv(TBB_S, "TBB_S_with_runmed.csv")
  TBB_S_up<- read.csv("TBB_S_with_runmed_and_missing_years_added.csv")

TBC_ONV <- read.csv("TBC_ONV_sumrec.csv")
TBC_ONV$runmed <- runmed(TBC_ONV$CPUE, 3)

TBC_OV <- read.csv("TBC_OV_sumrec.csv")
TBC_OV$runmed <- runmed(TBC_OV$CPUE, 3)

TBC_S <- read.csv("TBC_S_sumrec.csv")
TBC_S$runmed <-runmed(TBC_S$CPUE, 3)
  write.csv(TBC_S, "TBC_S_with_runmed.csv")
  TBC_S_up<- read.csv("TBC_S_with_runmed_and_missing_years_added.csv")

TBD_ONV <- read.csv("TBD_ONV_sumrec.csv")
TBD_ONV$runmed <- runmed(TBD_ONV$CPUE,3)

TBD_OV <- read.csv("TBD_OV_sumrec.csv")
TBD_OV$runmed <- runmed(TBD_OV$CPUE,3)

TBD_S <- read.csv("TBD_S_sumrec.csv")
TBD_S$runmed <-runmed(TBD_S$CPUE, 3)
  write.csv(TBD_S, "TBD_S_with_runmed.csv")
  TBD_S_up<- read.csv("TBD_S_with_runmed_and_missing_years_added.csv")

TBE_ONV <- read.csv("TBE_ONV_sumrec.csv")
TBE_ONV$runmed <-runmed(TBE_ONV$CPUE, 3)

TBE_OV <- read.csv("TBE_OV_sumrec.csv")
TBE_OV$runmed <-runmed(TBE_OV$CPUE, 3)

TBE_S <- read.csv("TBE_S_sumrec.csv")
TBE_S$runmed <-runmed(TBE_S$CPUE, 3)
  write.csv(TBE_S, "TBE_S_with_runmed.csv")
  TBE_S_up<- read.csv("TBE_S_with_runmed_and_missing_years_added.csv")

#### Combine all

Stratum_runmeds <- cbind(TBA_ONV$runmed, TBA_OV$runmed, TBA_S_up$runmed,
                            TBB_ONV$runmed, TBB_OV$runmed, TBB_S_up$runmed,
                            TBC_ONV$runmed, TBC_OV$runmed, TBC_S_up$runmed,
                            TBD_ONV$runmed, TBD_OV$runmed, TBD_S_up$runmed,
                            TBE_ONV$runmed, TBE_OV$runmed, TBE_S_up$runmed,
                            CKC_ONV$runmed, CKC_OV$runmed, CKC_S_up$runmed, 
                            CKB_ONV$runmed, CKB_OV_up$runmed, CKB_S_up$runmed, 
                            CHA_ONV$runmed, CHA_OV$runmed, CHA_S_up$runmed,
                            CHB_ONV$runmed, CHB_OV$runmed, CHB_S_up$runmed,
                            CHC_ONV$runmed, CHC_OV$runmed, CHC_S_up$runmed,
                            CHD_OV_up$runmed, CHD_S_up$runmed,
                            APA_ONV_up$runmed,  APA_OV_up$runmed, APA_S_up$runmed,
                            APB_ONV_up$runmed,  APB_OV_up$runmed, APB_S_up$runmed,
                            IRC_ONV$runmed,IRC_OV_up$runmed, IRC_S_up$runmed, 
                            IRD_ONV$runmed,IRD_OV$runmed, IRD_S_up$runmed,
                            IRH_ONV_up$runmed,IRH_OV_up$runmed, IRH_S_up$runmed)

library(hmisc)  #rcorr- missing values are deleted in pairs rather than deleting all rows of x having any missing variables
Low_frequency_Stratum_hmisc <- rcorr(Stratum_runmeds,type=c("pearson"))
write.csv(Low_frequency_Stratum_hmisc$r, "Low_frequency_Stratum_hmisc_rval.csv")
Low_frequency_Stratum_hmisc_Pval <- Low_frequency_Stratum_hmisc$P
write.csv(Low_frequency_Stratum_hmisc_Pval, "Low_frequency_Stratum_hmisc_Pval.csv")

# Load edited Pval matrix. Top half removed. 
Low_frequency_Stratum_hmisc_Pval_edited <- as.matrix(read.csv("Low_frequency_Stratum_hmisc_Pval_edited.csv"))
P2<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,2]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,2]))
P3<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,3]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,3]))
P4<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,4]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,4]))
P5<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,5]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,5]))
P6<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,6]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,6]))
P7<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,7]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,7]))
P8<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,8]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,8]))
P9<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,9]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,9]))
P10<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,10]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,10]))
P11<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,11]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,11]))
P12<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,12]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,12]))
P13<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,13]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,13]))
P14<-p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,14]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,14]))
P15<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,15]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,15]))
P16<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,16]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,16]))
P17<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,17]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,17]))
P18<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,18]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,18]))
P19<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,19]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,19]))
P20<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,20]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,20]))
P21<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,21]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,21]))
P22<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,22]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,22]))
P23<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,23]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,23]))
P24<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,24]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,24]))
P25<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,25]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,25]))
P26<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,26]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,26]))
P27<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,27]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,27]))
P28<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,28]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,28]))
P29<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,29]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,29]))
P30<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,30]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,30]))
P31<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,31]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,31]))
P32<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,32]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,32]))
P33<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,33]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,33]))
P34<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,34]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,34]))
P35<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,35]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,35]))
P36<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,36]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,36]))
P37<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,37]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,37]))
P38<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,38]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,38]))
P39<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,39]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,39]))
P40<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,40]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,40]))
P41<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,41]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,41]))
P42<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,42]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,42]))
P43<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,43]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,43]))
P44<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,44]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,44]))
P45<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,45]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,45]))
P46<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,46]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,46]))
P47<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,47]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,47]))
P48<- p.adjust((Low_frequency_Stratum_hmisc_Pval_edited[,48]), method= "bonferroni", n=length(Low_frequency_Stratum_hmisc_Pval_edited[,48]))

Stratum_adjusted_pvals <-cbind(P2, P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23,P24,
                       P25,P26,P27,P28,P29,P30,P31,P32,P33,P34,P35,P36,P37,P38,P39,P40,P41,P42, P43, P44, P45, P46, P47, P48)

write.csv(Stratum_adjusted_pvals, "Low_frequency_Stratum_hmisc_adjusted_Pvals.csv")



