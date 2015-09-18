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
meltAP_BAY <- melt(AP_BAY[,3:19]) # no data for 1996 or 1997
meltCH_BAY <- melt(CH_BAY)
meltCH_RIV <- melt(CH_RIV)
meltCK_BAY <- melt(CK_BAY)
meltCK_RIV <- melt(CK_RIV)
meltIR_BAY <- melt(IR_BAY)
meltJX     <- melt(JX[, 6:19]) # no data for 1996-2000

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

# identify median and quantiles as done in the annual report for FIM
library(plyr)
TBRIV_sum <- ddply(meltTB_RIV, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
TBBAY_sum <- ddply(meltTB_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
APBAY_sum <- ddply(meltAP_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
CHBAY_sum <- ddply(meltCH_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
CHRIV_sum <- ddply(meltCH_RIV, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
CKBAY_sum <- ddply(meltCK_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
CKRIV_sum <- ddply(meltCK_RIV, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
IRBAY_sum <- ddply(meltIR_BAY, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))
JX_sum    <- ddply(meltJX,     .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))


 # to do next: (REVIST WITH RAW DATA)
# 1. plot each time series
# 2. test autocorrelation for each
#   a. graphically summarize autocorrelation- lagged scatterplot
#   b. acf/acf2 and correlogram (continue with all time series)
#     b1. how to evaluate the autocorrelation .. http://stats.stackexchange.com/questions/78281/confidence-band-in-correlogram (which assumes normal assumption)
#     b2. what is the distribution of the output from a general linear model??... 

#   c. test for a trend- using time series analysis pdf-Colorado lecture; Mann-Kendall test?

# Useful Resources:
#  http://wwwuser.gwdg.de/~cscherb1/content/Statistics%20Course%20files/A%20short%20introduction%20to%20time%20series%20analysis%20in%20R.pdf
#  http://www.stat.pitt.edu/stoffer/tsa2/Examples.htm#lag.plot1
#  http://www.ltrr.arizona.edu/~dmeko/notes_3.pdf
#  http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture16_TS3.pdf


  #simple plot of each time series.. median vs year
    # change format of "year" from factor to number 
TBBAY_sum$Year <- as.numeric(as.character(TBBAY_sum$Year))
TBRIV_sum$Year <- as.numeric(as.character(TBRIV_sum$Year))
APBAY_sum$Year <- as.numeric(as.character(APBAY_sum$Year))
CHBAY_sum$Year <- as.numeric(as.character(CHBAY_sum$Year))
CHRIV_sum$Year <- as.numeric(as.character(CHRIV_sum$Year))
CKBAY_sum$Year <- as.numeric(as.character(CKBAY_sum$Year))
CKRIV_sum$Year <- as.numeric(as.character(CKRIV_sum$Year))
IRBAY_sum$Year <- as.numeric(as.character(IRBAY_sum$Year))
JX_sum$Year <- as.numeric(as.character(JX_sum$Year))
  
    #plot 
plot(TBRIV_sum$median~TBRIV_sum$Year)
plot(TBBAY_sum$median~TBBAY_sum$Year)
plot(APBAY_sum$median~APBAY_sum$Year)
plot(CHBAY_sum$median~CHBAY_sum$Year)
plot(CHRIV_sum$median~CHRIV_sum$Year)
plot(CKBAY_sum$median~CKBAY_sum$Year)
plot(CKRIV_sum$median~CKRIV_sum$Year)
plot(IRBAY_sum$median~IRBAY_sum$Year)
plot(JX_sum$median~JX_sum$Year)

 #fit a 3 yr moving average to the time series
TBRIV_time <- as.matrix(TBRIV_sum[,1:2])
library(zoo)
plot(TBRIV_time)
rm <-rollmean(TBRIV_time,k=3)
lines(rm)


#lagged scatterplots of each time series
library(astsa)
lag1.plot(TBRIV_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(TBBAY_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(APBAY_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(CHBAY_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(CHRIV_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(CKBAY_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(CKRIV_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(IRBAY_sum$median, 5, corr=TRUE, smooth=TRUE)
lag1.plot(JX_sum$median, 5, corr=TRUE, smooth=TRUE)

# perform autocorrelation function and make correlogram
  #below, the correlogram is an automatic output of the acf2 function
acf2(TBRIV_sum$median)
acf2(TBBAY_sum$median)
acf2(APBAY_sum$median)
acf2(CHBAY_sum$median)
acf2(CHRIV_sum$median)
acf2(CKBAY_sum$median)
acf2(CKRIV_sum$median)
acf2(IRBAY_sum$median)
acf2(JX_sum$median)

# perform Mann-Kendall test - test for monotonic trend in a time series z[t] based on the Kendall rank correlation of z[t]
library(Kendall)
MannKendall(TBRIV_sum$median)
MannKendall(TBBAY_sum$median)
MannKendall(APBAY_sum$median)
MannKendall(CHBAY_sum$median)
MannKendall(CHRIV_sum$median)
MannKendall(CKBAY_sum$median)
MannKendall(CKRIV_sum$median)
MannKendall(IRBAY_sum$median)
MannKendall(JX_sum$median)


#haven for loading SAS variables


