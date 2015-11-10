# Script for partitioning by Zone and Stratum #

setwd("~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes ")
# can use these data frames because they came from Seatrout Raw.R
# Example of Origin: 
#TB_RIV_MUn <- subset(TB_RIV_M, !duplicated(Reference))
#write.csv(TB_RIV_MUn, "TB_M.csv")
#TB_RIV_NUn <- subset(TB_RIV_N, !duplicated(Reference))
#write.csv(TB_RIV_NUn, "TB_N.csv")

TB_A <- read.csv("TB_A.csv")
TB_B <- read.csv("TB_B.csv")
TB_C <- read.csv("TB_C.csv")
TB_D <- read.csv("TB_D.csv")
TB_E <- read.csv("TB_E.csv")
TB_K <- read.csv("TB_K.csv")
TB_L <- read.csv("TB_L.csv")
TB_M <- read.csv("TB_M.csv")
TB_N <- read.csv("TB_N.csv")

JX_A <- read.csv("JX_A.csv")
JX_B <- read.csv("JX_B.csv")
JX_C <- read.csv("JX_C.csv")
JX_D <- read.csv("JX_D.csv")
JX_E <- read.csv("JX_E.csv")
JX_F <- read.csv("JX_F.csv")

IR_A <- read.csv("IR_A.csv")
IR_B <- read.csv("IR_B.csv")
IR_C <- read.csv("IR_C.csv")
IR_D <- read.csv("IR_D.csv")
IR_E <- read.csv("IR_E.csv")
IR_F <- read.csv("IR_F.csv")
IR_H <- read.csv("IR_H.csv")

CK_B <- read.csv("CK_B.csv")
CK_C <- read.csv("CK_C.csv")
CK_F <- read.csv("CK_F.csv")

CH_A <- read.csv("CH_A.csv")
CH_B <- read.csv("CH_B.csv")
CH_C <- read.csv("CH_C.csv")
CH_D <- read.csv("CH_D.csv")
CH_M <- read.csv("CH_M.csv")
CH_P <- read.csv("CH_P.csv")

AP_A <- read.csv("AP_A.csv")
AP_B <- read.csv("AP_B.csv")
AP_C <- read.csv("AP_C.csv")
################################################################
# Use raw catch data to make a time series of raw abundance index. 
# First, by Zone.
#   - > monthly recruitment (over recruitment season) over years
#   - > SUM abundance over all months of recruitment season
#   - > chose month with most recruitment (Not sure about this step)
# Then, by Stratum ( shore and offshore (veg and non veg)) within EACH zone ###  SEE BELOW ###
#   - > monthly recruitment (over recruitment season) over years
#   - > SUM abundance over all months of recruitment season
#   - > chose month with most recruitment

########### ZONE####################
# >>>>>>>> Monthly recruitment (over recruitment season) over years
# TotalNumberofAnimalsCollectedinHauls in the summation of the total number of animals collected in the hauls for month by year. 
# MedianNumberofAnimas, MeanNumberofAnimals
# APB_Asum = Appachicola Bay _ zone A (summarised)
# APR_Csum = Appalachicol (River) _ zone C (summarised)

library(plyr)
library(zoo)
#Appalachicola#
APB_Asum <- ddply(AP_A, c("year", "month"), summarise, NumberofHauls= length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_Asum$CPUE <- APB_Asum$TotalNumberofAnimalsCollectedinHauls/APB_Asum$NumberofHauls
APB_Asum$date <- paste(APB_Asum$year, APB_Asum$month, sep="-") #make a date so that I can make a continuous plot of the data 
# APB_Asum$date <- as.numeric(as.character(APB_Asum$date))

library(ggplot2)
plot_APB_Asum <- ggplot(APB_Asum, aes(x=date, y=CPUE, group=1))+
  geom_point(stat='summary', fun.y=sum)+ stat_summary(fun.y=sum, geom="line")+ #over-ride the character format of the date and connect them as if they are numeric; http://stackoverflow.com/questions/16350720/using-geom-line-with-x-axis-being-factors
  xlab("Year")+ ylab("CPUE C.neb") +
  #scale_x_discrete(breaks=2)+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'), axis.text.x=element_text(angle=90))+
  ggtitle( "Zone A")

#Another option for plotting below
#http://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart

APB_Bsum <- ddply(AP_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_Bsum$CPUE <- APB_Bsum$TotalNumberofAnimalsCollectedinHauls/APB_Bsum$NumberofHauls
APR_Csum <- ddply(AP_RIV_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APR_Csum$CPUE <- APR_Csum$TotalNumberofAnimalsCollectedinHauls/APR_Csum$NumberofHauls

CHB_Asum <- ddply(CH_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Asum$CPUE <- CHB_Asum$TotalNumberofAnimalsCollectedinHauls/CHB_Asum$NumberofHauls
CHB_Bsum <- ddply(CH_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Bsum$CPUE <- CHB_Bsum$TotalNumberofAnimalsCollectedinHauls/CHB_Bsum$NumberofHauls
CHB_Csum <- ddply(CH_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Csum$CPUE <- CHB_Csum$TotalNumberofAnimalsCollectedinHauls/CHB_Csum$NumberofHauls
CHB_Dsum <- ddply(CH_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_Dsum$CPUE <- CHB_Dsum$TotalNumberofAnimalsCollectedinHauls/CHB_Dsum$NumberofHauls
CHR_Msum <- ddply(CH_RIV_MUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Msum$CPUE <- CHR_Msum$TotalNumberofAnimalsCollectedinHauls/CHR_Msum$NumberofHauls
CHR_Psum <- ddply(CH_RIV_PUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHR_Psum$CPUE <- CHR_Psum$TotalNumberofAnimalsCollectedinHauls/CHR_Psum$NumberofHauls

CKB_Bsum <- ddply(CK_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_Bsum$CPUE <- CKB_Bsum$TotalNumberofAnimalsCollectedinHauls/CKB_Bsum$NumberofHauls
CKB_Csum <- ddply(CK_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_Csum$CPUE <- CKB_Csum$TotalNumberofAnimalsCollectedinHauls/CKB_Csum$NumberofHauls
CKR_Fsum <- ddply(CK_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKR_Fsum$CPUE <- CKR_Fsum$TotalNumberofAnimalsCollectedinHauls/CKR_Fsum$NumberofHauls

IRB_Asum <- ddply(IR_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Asum$CPUE <- IRB_Asum$TotalNumberofAnimalsCollectedinHauls/IRB_Asum$NumberofHauls
IRB_Bsum <- ddply(IR_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Bsum$CPUE <- IRB_Bsum$TotalNumberofAnimalsCollectedinHauls/IRB_Bsum$NumberofHauls
IRB_Csum <- ddply(IR_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Csum$CPUE <- IRB_Csum$TotalNumberofAnimalsCollectedinHauls/IRB_Csum$NumberofHauls
IRB_Dsum <- ddply(IR_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Dsum$CPUE <- IRB_Dsum$TotalNumberofAnimalsCollectedinHauls/IRB_Dsum$NumberofHauls
IRB_Esum <- ddply(IR_BAY_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Esum$CPUE <- IRB_Esum$TotalNumberofAnimalsCollectedinHauls/IRB_Esum$NumberofHauls
IRB_Hsum <- ddply(IR_BAY_HUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_Hsum$CPUE <- IRB_Hsum$TotalNumberofAnimalsCollectedinHauls/IRB_Hsum$NumberofHauls
IRR_Fsum <- ddply(IR_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRR_Fsum$CPUE <- IRR_Fsum$TotalNumberofAnimalsCollectedinHauls/IRR_Fsum$NumberofHauls


JXR_Asum <- ddply(JX_RIV_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Asum$CPUE <- JXR_Asum$TotalNumberofAnimalsCollectedinHauls/JXR_Asum$NumberofHauls
JXR_Bsum <- ddply(JX_RIV_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Bsum$CPUE <- JXR_Bsum$TotalNumberofAnimalsCollectedinHauls/JXR_Bsum$NumberofHauls
JXR_Csum <- ddply(JX_RIV_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Csum$CPUE <- JXR_Csum$TotalNumberofAnimalsCollectedinHauls/JXR_Csum$NumberofHauls
JXR_Dsum <- ddply(JX_RIV_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Dsum$CPUE <- JXR_Dsum$TotalNumberofAnimalsCollectedinHauls/JXR_Dsum$NumberofHauls
JXR_Esum <- ddply(JX_RIV_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Esum$CPUE <- JXR_Esum$TotalNumberofAnimalsCollectedinHauls/JXR_Esum$NumberofHauls
JXR_Fsum <- ddply(JX_RIV_FUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
JXR_Fsum$CPUE <- JXR_Fsum$TotalNumberofAnimalsCollectedinHauls/JXR_Fsum$NumberofHauls

TBB_Asum <- ddply(TB_BAY_AUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Asum$CPUE <- TBB_Asum$TotalNumberofAnimalsCollectedinHauls/TBB_Asum$NumberofHauls
TBB_Bsum <- ddply(TB_BAY_BUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Bsum$CPUE <- TBB_Bsum$TotalNumberofAnimalsCollectedinHauls/TBB_Bsum$NumberofHauls
TBB_Csum <- ddply(TB_BAY_CUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Csum$CPUE <- TBB_Csum$TotalNumberofAnimalsCollectedinHauls/TBB_Csum$NumberofHauls
TBB_Dsum <- ddply(TB_BAY_DUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Dsum$CPUE <- TBB_Dsum$TotalNumberofAnimalsCollectedinHauls/TBB_Dsum$NumberofHauls
TBB_Esum <- ddply(TB_BAY_EUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_Esum$CPUE <- TBB_Esum$TotalNumberofAnimalsCollectedinHauls/TBB_Esum$NumberofHauls

TBR_Ksum <- ddply(TB_RIV_KUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Ksum$CPUE <- TBR_Ksum$TotalNumberofAnimalsCollectedinHauls/TBR_Ksum$NumberofHauls
TBR_Lsum <- ddply(TB_RIV_LUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Lsum$CPUE <- TBR_Lsum$TotalNumberofAnimalsCollectedinHauls/TBR_Lsum$NumberofHauls
TBR_Msum <- ddply(TB_RIV_MUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Msum$CPUE <- TBR_Msum$TotalNumberofAnimalsCollectedinHauls/TBR_Msum$NumberofHauls
TBR_Nsum <- ddply(TB_RIV_NUn, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBR_Nsum$CPUE <- TBR_Nsum$TotalNumberofAnimalsCollectedinHauls/TBR_Nsum$NumberofHauls

# >>>>>>>>> SUM recruitment over season ###
TBB_A_sumrec <- ddply(TBB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls) )
TBB_A_sumrec$CPUE <- TBB_A_sumrec$TotalCollected/TBB_A_sumrec$TotalNumberofHauls
library(ggplot2)
plot_TBB_A_sumrec <- ggplot(TBB_A_sumrec, aes(x=year, y=CPUE))+ geom_line() + geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone A")
write.csv(TBB_A_sumrec, "TampaBay_Bay_A_sumrec.csv")

TBB_B_sumrec <- ddply(TBB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_B_sumrec$CPUE <- TBB_B_sumrec$TotalCollected/TBB_B_sumrec$TotalNumberofHauls
plot_TBB_B_sumrec <- ggplot(TBB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(TBB_B_sumrec, "TampaBay_Bay_B_sumrec.csv")

TBB_C_sumrec <- ddply(TBB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_C_sumrec$CPUE <- TBB_C_sumrec$TotalCollected/TBB_C_sumrec$TotalNumberofHauls
plot_TBB_C_sumrec <- ggplot(TBB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(TBB_C_sumrec, "TampaBay_Bay_C_sumrec.csv")

TBB_D_sumrec <- ddply(TBB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_D_sumrec$CPUE <- TBB_D_sumrec$TotalCollected/TBB_D_sumrec$TotalNumberofHauls
plot_TBB_D_sumrec <- ggplot(TBB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone D")
write.csv(TBB_D_sumrec, "TampaBay_Bay_D_sumrec.csv")

TBB_E_sumrec <- ddply(TBB_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBB_E_sumrec$CPUE <- TBB_E_sumrec$TotalCollected/TBB_E_sumrec$TotalNumberofHauls
plot_TBB_E_sumrec <- ggplot(TBB_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone E")
write.csv(TBB_E_sumrec, "TampaBay_Bay_E_sumrec.csv")
###### Add in the Multiplot Function ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Plot ##
TBB_ZONE_sumrec_multiplot <- multiplot(plot_TBB_A_sumrec, plot_TBB_B_sumrec, plot_TBB_C_sumrec, plot_TBB_D_sumrec, plot_TBB_E_sumrec,cols=2)

TBR_K_sumrec <- ddply(TBR_Ksum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_K_sumrec$CPUE <- TBR_K_sumrec$TotalCollected/TBR_K_sumrec$TotalNumberofHauls
plot_TBR_K_sumrec <- ggplot(TBR_K_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone K")
write.csv(TBR_K_sumrec, "TampaBay_Riv_K_sumrec.csv")

TBR_L_sumrec <- ddply(TBR_Lsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_L_sumrec$CPUE <- TBR_L_sumrec$TotalCollected/TBR_L_sumrec$TotalNumberofHauls
plot_TBR_L_sumrec <- ggplot(TBR_L_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone L")
write.csv(TBR_L_sumrec, "TampaBay_Riv_L_sumrec.csv")

TBR_M_sumrec <- ddply(TBR_Msum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_M_sumrec$CPUE <- TBR_M_sumrec$TotalCollected/TBR_M_sumrec$TotalNumberofHauls
plot_TBR_M_sumrec <- ggplot(TBR_M_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone M")
write.csv(TBR_M_sumrec, "TampaBay_Riv_M_sumrec.csv")

TBR_N_sumrec <- ddply(TBR_Nsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
TBR_N_sumrec$CPUE <- TBR_N_sumrec$TotalCollected/TBR_N_sumrec$TotalNumberofHauls
plot_TBR_N_sumrec <- ggplot(TBR_N_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone N")
write.csv(TBR_N_sumrec, "TampaBay_Riv_N_sumrec.csv")


TBR_ZONE_sumrec_multiplot <- multiplot(plot_TBR_K_sumrec, plot_TBR_L_sumrec,plot_TBR_M_sumrec,plot_TBR_N_sumrec, cols=2)

JXR_A_sumrec <- ddply(JXR_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_A_sumrec$CPUE <- JXR_A_sumrec$TotalCollected/JXR_A_sumrec$TotalNumberofHauls
plot_JXR_A_sumrec <- ggplot(JXR_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone A")
write.csv(JXR_A_sumrec, "Jax_Riv_A_sumrec.csv")

JXR_B_sumrec <- ddply(JXR_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_B_sumrec$CPUE <- JXR_B_sumrec$TotalCollected/JXR_B_sumrec$TotalNumberofHauls
plot_JXR_B_sumrec <- ggplot(JXR_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(JXR_B_sumrec, "Jax_Riv_B_sumrec.csv")

JXR_C_sumrec <- ddply(JXR_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_C_sumrec$CPUE <- JXR_C_sumrec$TotalCollected/JXR_C_sumrec$TotalNumberofHauls
plot_JXR_C_sumrec <- ggplot(JXR_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(JXR_C_sumrec, "Jax_Riv_C_sumrec.csv")


JXR_D_sumrec <- ddply(JXR_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_D_sumrec$CPUE <- JXR_D_sumrec$TotalCollected/JXR_D_sumrec$TotalNumberofHauls
plot_JXR_D_sumrec <- ggplot(JXR_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone D")
write.csv(JXR_D_sumrec, "Jax_Riv_D_sumrec.csv")

JXR_E_sumrec <- ddply(JXR_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_E_sumrec$CPUE <- JXR_E_sumrec$TotalCollected/JXR_E_sumrec$TotalNumberofHauls
plot_JXR_E_sumrec <- ggplot(JXR_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone E")
write.csv(JXR_E_sumrec, "Jax_Riv_E_sumrec.csv")

JXR_F_sumrec <- ddply(JXR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
JXR_F_sumrec$CPUE <- JXR_F_sumrec$TotalCollected/JXR_F_sumrec$TotalNumberofHauls
plot_JXR_F_sumrec <- ggplot(JXR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(2001,2014), breaks=seq(2001, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone F")
write.csv(JXR_F_sumrec, "Jax_Riv_F_sumrec.csv")


multiplot(plot_JXR_A_sumrec,plot_JXR_B_sumrec,plot_JXR_C_sumrec,plot_JXR_D_sumrec,plot_JXR_E_sumrec,plot_JXR_F_sumrec, cols=2) 

# - Stations A, B, and E of Indian River were only sampled only in October and November
IRB_A_sumrec <- ddply(IRB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_A_sumrec$CPUE <- IRB_A_sumrec$TotalCollected/IRB_A_sumrec$TotalNumberofHauls
plot_IRB_A_sumrec <- ggplot(IRB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone A")
write.csv(IRB_A_sumrec, "IndianRiver_Bay_A_sumrec.csv")

IRB_B_sumrec <- ddply(IRB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_B_sumrec$CPUE <- IRB_B_sumrec$TotalCollected/IRB_B_sumrec$TotalNumberofHauls
plot_IRB_B_sumrec <- ggplot(IRB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(IRB_B_sumrec, "IndianRiver_Bay_B_sumrec.csv")

IRB_C_sumrec <- ddply(IRB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_C_sumrec$CPUE <- IRB_C_sumrec$TotalCollected/IRB_C_sumrec$TotalNumberofHauls
plot_IRB_C_sumrec <- ggplot(IRB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(IRB_C_sumrec, "IndianRiver_Bay_C_sumrec.csv")

IRB_D_sumrec <- ddply(IRB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_D_sumrec$CPUE <- IRB_D_sumrec$TotalCollected/IRB_D_sumrec$TotalNumberofHauls
plot_IRB_D_sumrec <- ggplot(IRB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone D")
write.csv(IRB_D_sumrec, "IndianRiver_Bay_D_sumrec.csv")

IRB_E_sumrec <- ddply(IRB_Esum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_E_sumrec$CPUE <- IRB_E_sumrec$TotalCollected/IRB_E_sumrec$TotalNumberofHauls
plot_IRB_E_sumrec <- ggplot(IRB_E_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone E")
write.csv(IRB_E_sumrec, "IndianRiver_Bay_E_sumrec.csv")

IRB_H_sumrec <- ddply(IRB_Hsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRB_H_sumrec$CPUE <- IRB_H_sumrec$TotalCollected/IRB_H_sumrec$TotalNumberofHauls
plot_IRB_H_sumrec <- ggplot(IRB_H_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone H")
write.csv(IRB_H_sumrec, "IndianRiver_Bay_H_sumrec.csv")

IRR_F_sumrec <- ddply(IRR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
IRR_F_sumrec$CPUE <- IRR_F_sumrec$TotalCollected/IRR_F_sumrec$TotalNumberofHauls
plot_IRR_F_sumrec <- ggplot(IRR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone F")
write.csv(IRR_F_sumrec, "IndianRiver_Riv_F_sumrec.csv")

multiplot(plot_IRB_A_sumrec,plot_IRB_B_sumrec,plot_IRB_C_sumrec,plot_IRB_D_sumrec,plot_IRB_E_sumrec,plot_IRB_H_sumrec,plot_IRR_F_sumrec, cols=3) 

CKB_B_sumrec <- ddply(CKB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKB_B_sumrec$CPUE <- CKB_B_sumrec$TotalCollected/CKB_B_sumrec$TotalNumberofHauls
plot_CKB_B_sumrec <- ggplot(CKB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(CKB_B_sumrec, "CedarKey_Bay_B_sumrec.csv")

CKB_C_sumrec <- ddply(CKB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKB_C_sumrec$CPUE <- CKB_C_sumrec$TotalCollected/CKB_C_sumrec$TotalNumberofHauls
plot_CKB_C_sumrec <- ggplot(CKB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(CKB_C_sumrec, "CedarKey_Bay_C_sumrec.csv")

CKR_F_sumrec <- ddply(CKR_Fsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CKR_F_sumrec$CPUE <- CKR_F_sumrec$TotalCollected/CKR_F_sumrec$TotalNumberofHauls
plot_CKR_F_sumrec <- ggplot(CKR_F_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone F")
write.csv(CKR_F_sumrec, "CedarKey_Riv_F_sumrec.csv")
multiplot(plot_CKB_B_sumrec,plot_CKB_C_sumrec,plot_CKR_F_sumrec, cols=1)

APB_A_sumrec <- ddply(APB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APB_A_sumrec$CPUE <- APB_A_sumrec$TotalCollected/APB_A_sumrec$TotalNumberofHauls
plot_APB_A_sumrec <- ggplot(APB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone A")
write.csv(APB_A_sumrec, "Appalachicola_Bay_A_sumrec.csv")

APB_B_sumrec <- ddply(APB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APB_B_sumrec$CPUE <- APB_B_sumrec$TotalCollected/APB_B_sumrec$TotalNumberofHauls
plot_APB_B_sumrec <- ggplot(APB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(APB_B_sumrec, "Appalachicola_Bay_B_sumrec.csv")

APR_C_sumrec <- ddply(APR_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
APR_C_sumrec$CPUE <- APR_C_sumrec$TotalCollected/APR_C_sumrec$TotalNumberofHauls
plot_APR_C_sumrec <- ggplot(APR_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(APR_C_sumrec, "Appalachicola_Riv_C_sumrec.csv")

multiplot(plot_APB_A_sumrec,plot_APB_B_sumrec,plot_APR_C_sumrec, cols=1)

CHB_A_sumrec <- ddply(CHB_Asum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_A_sumrec$CPUE <- CHB_A_sumrec$TotalCollected/CHB_A_sumrec$TotalNumberofHauls
plot_CHB_A_sumrec <- ggplot(CHB_A_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone A")
write.csv(CHB_A_sumrec, "CharlotteHarbor_Bay_A_sumrec.csv")

CHB_B_sumrec <- ddply(CHB_Bsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_B_sumrec$CPUE <- CHB_B_sumrec$TotalCollected/CHB_B_sumrec$TotalNumberofHauls
plot_CHB_B_sumrec <- ggplot(CHB_B_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone B")
write.csv(CHB_B_sumrec, "CharlotteHarbor_Bay_B_sumrec.csv")

CHB_C_sumrec <- ddply(CHB_Csum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_C_sumrec$CPUE <- CHB_C_sumrec$TotalCollected/CHB_C_sumrec$TotalNumberofHauls
plot_CHB_C_sumrec <- ggplot(CHB_C_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone C")
write.csv(CHB_C_sumrec, "CharlotteHarbor_Bay_C_sumrec.csv")

CHB_D_sumrec <- ddply(CHB_Dsum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHB_D_sumrec$CPUE <- CHB_D_sumrec$TotalCollected/CHB_D_sumrec$TotalNumberofHauls
plot_CHB_D_sumrec <- ggplot(CHB_D_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone D")
write.csv(CHB_D_sumrec, "CharlotteHarbor_Bay_D_sumrec.csv")

CHR_M_sumrec <- ddply(CHR_Msum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHR_M_sumrec$CPUE <- CHR_M_sumrec$TotalCollected/CHR_M_sumrec$TotalNumberofHauls
plot_CHR_M_sumrec <- ggplot(CHR_M_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone M")
write.csv(CHR_M_sumrec, "CharlotteHarbor_Riv_M_sumrec.csv")

CHR_P_sumrec <- ddply(CHR_Psum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls))
CHR_P_sumrec$CPUE <- CHR_P_sumrec$TotalCollected/CHR_P_sumrec$TotalNumberofHauls
plot_CHR_P_sumrec <- ggplot(CHR_P_sumrec, aes(x=year, y=CPUE))+ geom_line()+ geom_point()+
  xlab("Year")+ ylab("CPUE C.neb")+
  scale_x_continuous(limits=c(1996,2014), breaks=seq(1996, 2014, 2))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.background=element_rect(fill='white', colour='black'))+
  ggtitle( "Zone P")
write.csv(CHR_P_sumrec, "CharlotteHarbor_Riv_P_sumrec.csv")

multiplot(plot_CHB_A_sumrec,plot_CHB_B_sumrec,plot_CHB_C_sumrec,plot_CHB_D_sumrec,plot_CHR_M_sumrec,plot_CHR_P_sumrec, cols=2)


####### STRATUM ########
# >>>>>>Partition the data further by shore and offshore. Can use previously defined dataframes.  
#  >>>>>>>By each Estuary
#           -> Bay Zones
#               -> Zone
#                 -> Shore
#                 -> Offshore
#                     -> Veg
#                     -> Unveg
#  No stratum partitioning in rivers
##########################################################
# Use the Stratum variable in the _physical data sheets. Per the procedure manual, S is for shorelines,
# A and B represent offshore seines with A being on vegetated sites and B on non vegetated sites.
# The shoreline is stratified by the presence of over-hanging vegetation
# Rivers are NOT stratified so All of Jax is absent

# 1. Subset by Offshore veg and non-veg and shore
#       > OV= offshore Veg, ONV= offshore non Veg, S= shoreline
#         > summarise by year and month


AP_A_OV <- subset(AP_BAY_AUn, Stratum =="A")
library(plyr)
APA_OV_sum <- ddply(AP_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_OV_sum <- APA_OV_sum$TotalNumberofAnimalsCollectedinHaul/APA_OV_sum$NumberofHauls
AP_B_OV <- subset(AP_BAY_BUn, Stratum=="A")
APB_OV_sum <- ddply(AP_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_OV_sum <- APB_OV_sum$TotalNumberofAnimalsCollectedinHaul/APB_OV_sum$NumberofHauls
AP_A_ONV <- subset(AP_BAY_AUn, Stratum=="B")
APA_ONV_sum <- ddply(AP_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_ONV_sum <- APA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/AP_A_ONV_sum$NumberofHauls
AP_B_ONV <- subset(AP_BAY_BUn, Stratum=="B")
APB_ONV_sum <- ddply(AP_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_ONV_sum <- APB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/APB_ONV_sum$NumberofHauls
AP_A_S <- subset(AP_BAY_AUn, Stratum=="S")
APA_S_sum <- ddply(AP_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APA_S_sum <- AP_A_S_sum$TotalNumberofAnimalsCollectedinHaul/APA_S_sum$NumberofHauls
AP_B_S <- subset(AP_BAY_BUn, Stratum=="S")
APB_S_sum <- ddply(AP_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
APB_S_sum <- APB_S_sum$TotalNumberofAnimalsCollectedinHaul/APB_S_sum$NumberofHauls

CH_A_OV <- subset(CH_BAY_AUn, Stratum="A")
CHA_OV_sum <- ddply(CHA_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_OV_sum <- CHA_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHA_OV_sum$NumberofHauls
CH_B_OV <- subset(CH_BAY_BUn, Stratum="A")
CHB_OV_sum <- ddply(CH_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_OV_sum <- CHB_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHB_OV_sum$NumberofHauls
CH_C_OV <- subset(CH_BAY_CUn, Stratum="A")
CH_C_OV_sum <- ddply(CH_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_OV_sum <- CHC_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHC_OV_sum$NumberofHauls
CH_D_OV <- subset(CH_BAY_DUn, Stratum="A")
CHD_OV_sum <- ddply(CH_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_OV_sum <- CHD_OV_sum$TotalNumberofAnimalsCollectedinHaul/CHD_OV_sum$NumberofHauls
CH_A_ONV <- subset(CH_BAY_AUn, Stratum="B")
CHA_ONV_sum <- ddply(CH_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_ONV_sum <- CHA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHA_ONV_sum$NumberofHauls
CH_B_ONV <- subset(CH_BAY_BUn, Stratum="B")
CHB_ONV_sum <- ddply(CH_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_ONV_sum <- CHB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHB_ONV_sum$NumberofHauls
CH_C_ONV <- subset(CH_BAY_CUn, Stratum="B")
CHC_ONV_sum <- ddply(CH_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_ONV_sum <- CHC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHC_ONV_sum$NumberofHauls
CH_D_ONV <- subset(CH_BAY_DUn, Stratum="B")
CHD_ONV_sum <- ddply(CH_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_ONV_sum <- CHD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CHD_ONV_sum$NumberofHauls
CH_A_S <- subset(CH_BAY_AUn, Stratum="S")
CHA_S_sum <- ddply(CH_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHA_S_sum <- CHA_S_sum$TotalNumberofAnimalsCollectedinHaul/CHA_S_sum$NumberofHauls
CH_B_S <- subset(CH_BAY_BUn, Stratum="S")
CHB_S_sum <- ddply(CH_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHB_S_sum <- CHB_S_sum$TotalNumberofAnimalsCollectedinHaul/CHB_S_sum$NumberofHauls
CH_C_S <- subset(CH_BAY_CUn, Stratum="S")
CHC_S_sum <- ddply(CH_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHC_S_sum <- CHC_S_sum$TotalNumberofAnimalsCollectedinHaul/CHC_S_sum$NumberofHauls
CH_D_S <- subset(CH_BAY_DUn, Stratum="S")
CHD_S_sum <- ddply(CH_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CHD_S_sum <- CHD_S_sum$TotalNumberofAnimalsCollectedinHaul/CHD_S_sum$NumberofHauls


CK_B_OV <- subset(CK_BAY_BUn, Stratum="A")
CKB_OV_sum <- ddply(CK_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_OV_sum <- CKB_OV_sum$TotalNumberofAnimalsCollectedinHaul/CKB_OV_sum$NumberofHauls
CK_C_OV <- subset(CK_BAY_CUn, Stratum="A")
CKC_OV_sum <- ddply(CK_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_OV_sum <- CKC_OV_sum$TotalNumberofAnimalsCollectedinHaul/CKC_OV_sum$NumberofHauls
CK_B_ONV <- subset(CK_BAY_BUn, Stratum="B")
CKB_ONV_sum <- ddply(CK_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_ONV_sum <- CKB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CKB_ONV_sum$NumberofHauls
CK_C_ONV <- subset(CK_BAY_CUn, Stratum="B")
CKC_ONV_sum <- ddply(CK_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_ONV_sum <- CKC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/CKC_ONV_sum$NumberofHauls
CK_B_S <- subset(CK_BAY_BUn, Stratum="S")
CKB_S_sum <- ddply(CK_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKB_S_sum <- CKB_S_sum$TotalNumberofAnimalsCollectedinHaul/CKB_S_sum$NumberofHauls
CK_C_S <- subset(CK_BAY_CUn, Stratum="S")
CKC_S_sum <- ddply(CK_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
CKC_S_sum <- CKC_S_sum$TotalNumberofAnimalsCollectedinHaul/CKC_S_sum$NumberofHauls


IR_A_OV <- subset(IR_BAY_AUn, Stratum="A")
IRA_OV_sum <- ddply(IR_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_OV_sum <- IRA_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRA_OV_sum$NumberofHauls
IR_B_OV <- subset(IR_BAY_BUn, Stratum="A")
IRB_OV_sum <- ddply(IR_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_OV_sum <- IRB_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRB_OV_sum$NumberofHauls
IR_C_OV <- subset(IR_BAY_CUn, Stratum="A")
IRC_OV_sum <- ddply(IR_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_OV_sum <- IRC_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRC_OV_sum$NumberofHauls
IR_D_OV <- subset(IR_BAY_DUn, Stratum="A")
IRD_OV_sum <- ddply(IR_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_OV_sum <- IRD_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRD_OV_sum$NumberofHauls
IR_E_OV <- subset(IR_BAY_EUn, Stratum="A")
IRE_OV_sum <- ddply(IR_E_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_OV_sum <- IRE_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRE_OV_sum$NumberofHauls
IR_H_OV <- subset(IR_BAY_HUn, Stratum="A")
IRH_OV_sum <- ddply(IR_H_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_OV_sum <- IRH_OV_sum$TotalNumberofAnimalsCollectedinHaul/IRH_OV_sum$NumberofHauls
IR_A_ONV <- subset(IR_BAY_AUn, Stratum="B")
IRA_ONV_sum <- ddply(IR_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_ONV_sum <- IRA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRA_ONV_sum$NumberofHauls
IR_B_ONV <- subset(IR_BAY_BUn, Stratum="B")
IRB_ONV_sum <- ddply(IR_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_ONV_sum <- IRB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRB_ONV_sum$NumberofHauls
IR_C_ONV <- subset(IR_BAY_CUn, Stratum="B")
IRC_ONV_sum <- ddply(IR_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_ONV_sum <- IRC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRC_ONV_sum$NumberofHauls
IR_D_ONV <- subset(IR_BAY_DUn, Stratum="B")
IRD_ONV_sum <- ddply(IR_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_ONV_sum <- IRD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRD_ONV_sum$NumberofHauls
IR_E_ONV <- subset(IR_BAY_EUn, Stratum="B")
IRE_ONV_sum <- ddply(IR_E_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_ONV_sum <- IRE_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRE_ONV_sum$NumberofHauls
IR_H_ONV <- subset(IR_BAY_HUn, Stratum="B")
IRH_ONV_sum <- ddply(IR_H_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_ONV_sum <- IRH_ONV_sum$TotalNumberofAnimalsCollectedinHaul/IRH_ONV_sum$NumberofHauls
IR_A_S <- subset(IR_BAY_AUn, Stratum="S")
IRA_S_sum <- ddply(IR_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRA_S_sum <- IRA_S_sum$TotalNumberofAnimalsCollectedinHaul/IRA_S_sum$NumberofHauls
IR_B_S <- subset(IR_BAY_BUn, Stratum="S")
IRB_S_sum <- ddply(IR_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRB_S_sum <- IRB_S_sum$TotalNumberofAnimalsCollectedinHaul/IRB_S_sum$NumberofHauls
IR_C_S <- subset(IR_BAY_CUn, Stratum="S")
IRC_S_sum <- ddply(IR_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRC_S_sum <- IRC_S_sum$TotalNumberofAnimalsCollectedinHaul/IRC_S_sum$NumberofHauls
IR_D_S <- subset(IR_BAY_DUn, Stratum="S")
IRD_S_sum <- ddply(IR_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRD_S_sum <- IRD_S_sum$TotalNumberofAnimalsCollectedinHaul/IRD_S_sum$NumberofHauls
IR_E_S <- subset(IR_BAY_EUn, Stratum="S")
IRE_S_sum <- ddply(IR_E_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRE_S_sum <- IRE_S_sum$TotalNumberofAnimalsCollectedinHaul/IRE_S_sum$NumberofHauls
IR_H_S <- subset(IR_BAY_HUn, Stratum="S")
IRH_S_sum <- ddply(IR_H_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
IRH_S_sum <- IRH_S_sum$TotalNumberofAnimalsCollectedinHaul/IRH_S_sum$NumberofHauls

TB_A_OV <- subset(TB_BAY_AUn, Stratum=="A")
TBA_OV_sum <- ddply(TB_A_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_OV_sum <- TBA_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_OV_sum$NumberofHauls
TB_B_OV <- subset(TB_BAY_BUn, Stratum=="A")
TBB_OV_sum <- ddply(TB_B_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_OV_sum <- TBB_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBB_OV_sum$NumberofHauls
TB_C_OV <- subset(TB_BAY_CUn, Stratum=="A")
TBC_OV_sum <- ddply(TB_C_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_OV_sum <- TBC_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_OV_sum$NumberofHauls
TB_D_OV <- subset(TB_BAY_DUn, Stratum=="A")
TBD_OV_sum <- ddply(TB_D_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_OV_sum <- TBD_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBD_OV_sum$NumberofHauls
TB_E_OV <- subset(TB_BAY_EUn, Stratum=="A")
TBE_OV_sum <- ddply(TB_E_OV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_OV_sum <- TBE_OV_sum$TotalNumberofAnimalsCollectedinHaul/TBE_OV_sum$NumberofHauls
TB_A_ONV <- subset(TB_BAY_AUn, Stratum=="B")
TBA_ONV_sum <- ddply(TB_A_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_ONV_sum <- TBA_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBA_ONV_sum$NumberofHauls
TB_B_ONV <- subset(TB_BAY_BUn, Stratum=="B")
TBB_ONV_sum <- ddply(TB_B_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_ONV_sum <- TBB_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBB_ONV_sum$NumberofHauls
TB_C_ONV <- subset(TB_BAY_CUn, Stratum=="B")
TBC_ONV_sum <- ddply(TB_C_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_ONV_sum <- TBC_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBC_ONV_sum$NumberofHauls
TB_D_ONV <- subset(TB_BAY_DUn, Stratum=="B")
TBD_ONV_sum <- ddply(TB_D_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_ONV_sum <- TBD_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBD_ONV_sum$NumberofHauls
TB_E_ONV <- subset(TB_BAY_EUn, Stratum=="B")
TBE_ONV_sum <- ddply(TB_E_ONV, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_ONV_sum <- TBE_ONV_sum$TotalNumberofAnimalsCollectedinHaul/TBE_ONV_sum$NumberofHauls
TB_A_S <- subset(TB_BAY_AUn, Stratum=="S")
TBA_S_sum <- ddply(TB_A_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBA_S_sum <- TBA_S_sum$TotalNumberofAnimalsCollectedinHaul/TBA_S_sum$NumberofHauls
TB_B_S <- subset(TB_BAY_BUn, Stratum=="S")
TBB_S_sum <- ddply(TB_B_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBB_S_sum <- TBB_S_sum$TotalNumberofAnimalsCollectedinHaul/TBB_S_sum$NumberofHauls
TB_C_S <- subset(TB_BAY_CUn, Stratum=="S")
TBC_S_sum <- ddply(TB_C_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBC_S_sum <- TBC_S_sum$TotalNumberofAnimalsCollectedinHaul/TBC_S_sum$NumberofHauls
TB_D_S <- subset(TB_BAY_DUn, Stratum=="S")
TBD_S_sum <- ddply(TB_D_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBD_S_sum <- TBD_S_sum$TotalNumberofAnimalsCollectedinHaul/TBD_S_sum$NumberofHauls
TB_E_S <- subset(TB_BAY_EUn, Stratum=="S")
TBE_S_sum <- ddply(TB_E_S, c("year", "month"), summarise, NumberofHauls=length(Reference) , TotalNumberofAnimalsCollectedinHauls=sum(number), avgofnumbercollectedinhauls=mean(number), mediannumbercollected=median(number))
TBE_S_sum <- TBE_S_sum$TotalNumberofAnimalsCollectedinHaul/TBE_S_sum$NumberofHauls

# >>>>>>>>> SUM recruitment over season ###
TBE_S_sumrec <- ddply(TBE_S_sum, c("year"), summarise, TotalNumberofHauls=sum(NumberofHauls), TotalCollected=sum(TotalNumberofAnimalsCollectedinHauls) )
TBE_S_sumrec$CPUE <- TBE_S_sumrec$TotalCollected/TBE_S_sumrec$TotalNumberofHauls



