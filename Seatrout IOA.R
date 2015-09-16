# This is for exploring the indices of abundance and running some statistics on them. 
setwd("~/Desktop/Github Repo/Seatrout")
# load all IOA for YOY. Data are from FIM. 
TB_RIV <- read.csv("TB_RIV.csv", check.names=FALSE)
TB_BAY <- read.csv("TB_BAY.csv", check.names=FALSE)
AP_BAY <- read.csv("AP_BAY.csv", check.names=FALSE) #there are no IOA for YOY in the river
CH_BAY <- read.csv("CH_BAY.csv", check.names=FALSE)
CH_RIV <- read.csv("CH_RIV.csv", check.names=FALSE)
CK_BAY <- read.csv("CK_BAY.csv", check.names=FALSE)
CK_RIV <- read.csv("CK_RIV.csv", check.names=FALSE)
IR_BAY <- read.csv("IR_BAY.csv", check.names=FALSE)
JX     <- read.csv("JX.csv", check.names=FALSE)



 #melt the data so that the sets can easily be fead into ddply
library(reshape2)
meltTB_RIV <- melt(TB_RIV)
meltTB_BAY <- melt(TB_BAY)
meltAP_BAY <- melt(AP_BAY)
meltCH_BAY <- melt(CH_BAY)
meltCH_RIV <- melt(CH_RIV)
meltCK_BAY <- melt(CK_BAY)
meltCK_RIV <- melt(CK_RIV)
meltIR_BAY <- melt(IR_BAY)
meltJX     <- melt(JX)

 #re-name column headings
colnames(meltTB_RIV) <- c("Year", "BootstrapRealization")
colnames(meltTB_BAY) <- c("Year", "BootstrapRealization")
colnames(meltAP_BAY) <- c("Year", "BootstrapRealization")
colnames(meltCH_BAY) <- c("Year", "BootstrapRealization")
colnames(meltCH_RIV) <- c("Year", "BootstrapRealization")
colnames(meltCK_BAY) <- c("Year", "BootstrapRealization")
colnames(meltCK_RIV) <- c("Year", "BootstrapRealization")
colnames(meltIR_BAY) <- c("Year", "BootstrapRealization")
colnames(meltJX)     <- c("Year", "BootstrapRealization")

library(plyr)
TBRIV_sum <- ddply(meltTB_RIV, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
TBBAY_sum <- ddply(meltTB_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))











