setwd("~/Desktop/Github Repo/Seatrout")
# load Tampa Bay river and bay IOA for YOY. Data are from FIM. 
TB_RIV <- read.csv("TB_RIV.csv", check.names=FALSE)
TB_BAY <- read.csv("TB_BAY.csv", check.names=FALSE)

 #melt the data so that the sets can easily be fead into ddply
meltTB_RIV <- melt(TB_RIV)
meltTB_BAY<- melt(TB_BAY)

 #re-name column headings
colnames(meltTB_RIV) <- c("Year", "BootstrapRealization")
colnames(meltTB_BAY) <- c("Year", "Bootstrap Realization")

library(plyr)
TBRIV_sum <- ddply(meltTB_RIV, .(Year), summarise, median=median(BootstrapRealization), lower=quantile(BootstrapRealization, .25, na.rm=TRUE), upper=quantile(BootstrapRealization, .75, na.rm=TRUE))


TBBAY_sum <-ddply(meltTB_BAY, .(Year), summarise, median = median(Boostrap Realization), ;)

three <- ddply(data_wkdf_3, .(Year.of.Increment.Formation), summarise, N=length(Increment.Width), three.mean=mean(Increment.Width))



# calculate median values and quantiles
library(matrixStats)
TB_RIV_med <-colMedians(as.matrix(TB_RIV))
    # set up lines to calculate quantiles
prob= c(0:4)/4 # defines the percentages that I want

TB_RIV_quan <- lapply(TB_RIV, quantile, probs=prob, name=FALSE)


library(plyr)
TBRIV_sum<- ddply()

TB_BAY_med <- colMedians(as.matrix(TB_BAY))


