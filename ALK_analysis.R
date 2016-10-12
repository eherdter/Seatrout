# 10/10/2016 This script imports ALK with Bay.xlsx
# Main Objectives of this script: 
# 1. Imports otolith data. 
# 2. Makes bay-specific observed ALK.  
# 3. Makes bay specific smoothed (modeled) ALK with multinomial modeling methods in Ogle (87-).
# 4. Likelihood ratio testing to do among group statistical comparisons - Ogle (102-103)
# 5. Plots observed and smoothed ALK for each bay. 
#################################################################
library(FSA)
library(magrittr)
library(dplyr)
library(nnet)
library(plotrix)

# set working directory
setwd("~/Desktop/Github Repo/Seatrout/Data")

#load the csv file
# subset by which bay I want
# make sure I have just the "aged sample"
# turn mm to cm
# select just a few variables to make it more manageable
# then drop the remaining bay levels still sticking around (droplevels)
# turn tl from mm to cm
# create length categories with FSA package

Agelength_TB<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="TB" & tl>14 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) #, as.fact=TRUE))- can include this when determing ALK below but the smoothed ALK needs to be the nonfactored version of the length categorization variable. 
Agelength_AP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="AP" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) #, as.fact=TRUE))
Agelength_CK<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CK" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_CH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CH" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_IR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="IR" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_JX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="JX" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))


###########################################################
# Make table with observed total numbers at length by age.
###########################################################
(rawfreq_TB <- xtabs(~lcat2+final_age, data=Agelength_TB)) 
#rawfreq_TB_test_df <- as.data.frame(as.matrix(xtabs(~lcat2+final_age, data=TB_test)))
# there appears to be a fish that was assigned an age of 3 but is in the 0-2 length category. Going to remove this because its probably a typo. Specified in above subsetting step as tl>20mm =(2cm).   
rowSums(rawfreq_TB)
(rawfreq_AP <- xtabs(~lcat2+final_age, data=Agelength_AP)) 
rowSums(rawfreq_AP)
(rawfreq_CK <- xtabs(~lcat2+final_age, data=Agelength_CK)) 
rowSums(rawfreq_CK)
(rawfreq_CH <- xtabs(~lcat2+final_age, data=Agelength_CH)) 
rowSums(rawfreq_CH)
(rawfreq_IR <- xtabs(~lcat2+final_age, data=Agelength_IR)) 
rowSums(rawfreq_IR)
(rawfreq_JX <- xtabs(~lcat2+final_age, data=Agelength_JX))
rowSums(rawfreq_JX)

############################################
# Make observed ALK from the above tables.
############################################
#The conditional proportions that form the ALK are calculated by dividing ecah cell of the frequency table by the sum of the corresponding row. 
#These row proportions are constructed by submitting the xtabs() object to prop.table() and including margin=1 to indicate that the proportions are computed by row (page 92). 

#The alkPlot command used for plotting the observed ALK is unable to extend the x axis to the bounds of c(0,80) because xlim is not working. 
# Therefore, in order to produce a plot with an x axis that can span from 0-80 (like what is happening with the length frequency and the smoothed ALK)
# I need to add in "observed" proportions for length categories that were not sampled. 
# I could have added them to the original data frame but I was concerned that in the process of proportion calculations the extra entries would
# affect the proportions or result in proportions that I didn't want. The smoothing process can estimate proportions outside of the range but I wanted 
# to keep the observed plot with just the proportions from the observed data. Therefore, I added in the zero proportion data by writing and editing
# a csv file which I then read below. 

as.data.frame.matrix((prop.table(rawfreq_TB, margin=1))) %>% write.csv("alk_TB.csv")
alk_TB <- read.csv("alk_TB_edit.csv", row.names=1)
names(alk_TB) <- c(1,2,3,4,5,6,7,8,9)
round(alk_TB,3)

as.data.frame.matrix((prop.table(rawfreq_CH, margin=1))) %>% write.csv("alk_CH.csv")
alk_CH <- read.csv("alk_CH_edit.csv", row.names=1)
names(alk_CH) <- c(1,2,3,4,5,6,7,8,9)
round(alk_CH,3)

as.data.frame.matrix((prop.table(rawfreq_CK, margin=1))) %>% write.csv("alk_CK.csv")
alk_CK <- read.csv("alk_CK_edit.csv", row.names=1)
names(alk_CK) <- c(1,2,3,4,5,6,7,8)
round(alk_CK,3)

as.data.frame.matrix((prop.table(rawfreq_AP, margin=1))) %>% write.csv("alk_AP.csv")
alk_AP <- read.csv("alk_AP_edit.csv", row.names=1)
names(alk_AP) <- c(1,2,3,4,5,6,7,8,9,10)
round(alk_AP,3)

as.data.frame.matrix((prop.table(rawfreq_JX, margin=1))) %>% write.csv("alk_JX.csv")
alk_JX <- read.csv("alk_JX_edit.csv", row.names=1)
names(alk_JX) <- c(1,2,3,4,5,6,7,8)
round(alk_JX,3)

as.data.frame.matrix((prop.table(rawfreq_IR, margin=1))) %>% write.csv("alk_IR.csv")
alk_IR <- read.csv("alk_IR_edit.csv", row.names=1)
names(alk_IR) <- c(1,2,3,4,5,6,7,8,9)
round(alk_IR,3)





######################################################################
# Apply ALK.
# 1. Determine age distribution (age distribution with standard errors)
# 2. Mean length-at-age. 
######################################################################


###############################################################################################################
# Produce smoothed ALK from multinomial modeling exercise which can be used to do Likelihood ratio testing. 
#MODELED AGE LENGTH KEYS (aka SMOOTHED ALK)
#-fixes two common issues with Age Length keys detailed on page 92 Ogle. 
#multinomial logistic regression model -Gerritsen et al. 2006. The response variable has more than two levels
###############################################################################################################
#TB
tb <- multinom(final_age~lcat2, data=Agelength_TB, maxit=500)
lens<- seq(10,85, 1)
alk.tb <- predict(tb, data.frame(lcat2=lens), type="probs")
row.names(alk.tb) <- lens
round(alk.tb, 3)




#################################################
#Plot raw data, observed ALK, and modeled ALK
# Multiple options below. 
#################################################
histStack(lcat2~final_age, data=Agelength_TB, col=gray.colors(9, start=0, end=1), breaks=seq(0,80,1), xlim=c(0,80), ylim=c(0,700), xlab="Total Length (cm)", legend.pos="topright", xaxt="n") #remove col argument if we want it in rainbow format
axis(1, at=seq(0, 80, by=4))

rawCK <- histStack(lcat2~final_age, data=Agelength_CK, col=gray.colors(8, start=0, end=1), breaks=seq(0,80,1), ylim=c(0,80), xlab="Total Length (cm)", legend.pos="topright") #remove col argument if we want it in rainbow format
rawCH <- histStack(lcat2~final_age, data=Agelength_CH, col=gray.colors(9, start=0, end=1), breaks=seq(0,80,1), ylim=c(0,250), xlab="Total Length (cm)", legend.pos="topright") #remove col argument if we want it in rainbow format
rawAP <- histStack(lcat2~final_age, data=Agelength_AP, col=gray.colors(10, start=0, end=1), breaks=seq(0,80,1), ylim=c(0,350), xlab="Total Length (cm)", legend.pos="topright") #remove col argument if we want it in rainbow format
rawIR <- histStack(lcat2~final_age, data=Agelength_IR, col=gray.colors(9, start=0, end=1), breaks=seq(0,80,1), ylim=c(0,500), xlab="Total Length (cm)", legend.pos="topright") #remove col argument if we want it in rainbow format
rawJX <- histStack(lcat2~final_age, data=Agelength_JX, col=gray.colors(8, start=0, end=1), breaks=seq(0,80,1), ylim=c(0,100), xlab="Total Length (cm)", legend.pos="topright") #remove col argument if we want it in rainbow format


alkPlot(alk_TB_new, type="barplot", xlab="Total Length (cm)", xlim=c(0,80), showLegend=F, pal="gray") #could remove legend and just reference in figure description
# will tighten xlim but won't expand. Maybe I'll have to put some dummy values in here. 
alkPlot(alk_TB, type="barplot", xlab="Total Length (cm)", showLegend=F, pal="gray") #could remove legend and just reference in figure description

alkPlot # this is what you did without having a good enough answer
methods(alkPlot) # Next step, ask for the method: 'princomp.default'
getAnywhere('alkPlot.default')


#obsTB <- alkPlot(alk_TB, type="area", pal="gray", showLegend=TRUE)
#obsTB <- alkPlot(alk_TB, type="lines", showLegend=TRUE)
#obsTB <- alkPlot(alk_TB, type="splines", showLegend=TRUE, span=0.1)

smoTB <- alkPlot(alk.tb, type="barplot", pal="gray", showLegend=TRUE)
smoTB <- alkPlot(alk.tb, type="area", pal="gray", showLegend=TRUE)
smoTB <- alkPlot(alk.tb, type="lines", showLegend=TRUE)

  
ck <- multinom(final_age~lcat2, data=Agelength_CK, maxit=500)
lens<- seq(14,68, 1)
alk.ck <- predict(ck, data.frame(lcat2=lens), type="probs")
row.names(alk.ck) <- lens
round(alk.ck, 3)

#plot
alkPlot(alk.ck, type="barplot")



#AMONG GROUP STATISTICAL COMPARISONS
#page 102 in Ogle
Agelength_ALL<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK" | bay== "CH" | bay=="IR" |bay=="AP" | bay=="JX"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_ALL, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_ALL, maxit=500) #more complex model

#likelihood ratio test is computed with anova
anova(mod1, mod2)

#bay dropping 
  #removing IR
Agelength_minIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK" | bay== "CH" | bay=="AP" | bay=="JX"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_minIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_minIR, maxit=500) #more complex model

anova(mod1, mod2)
  #still significantly different

  #now remove JX also
Agelength_minIRJX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK" | bay== "CH" | bay=="AP"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_minIRJX, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_minIRJX, maxit=500) #more complex model

anova(mod1, mod2)
  #still significantly different

  #now remove AP also
Agelength_minIRJXAP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK" | bay== "CH"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_minIRJXAP, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_minIRJXAP, maxit=500) #more complex model


anova(mod1, mod2)

#now remove CH also
Agelength_minIRJXAPCH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_minIRJXAPCH, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_minIRJXAPCH, maxit=500) #more complex model

anova(mod1, mod2)
  #still significantly different

#one on one comparison
#TB vs CK 
Agelength_TBCK<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "CK" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_TBCK, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_TBCK, maxit=500) #more complex model

anova(mod1, mod2)

#TB vs CH
Agelength_TBCH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "CH" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_TBCH, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_TBCH, maxit=500) #more complex model

anova(mod1, mod2)

#TB vs AP
Agelength_TBAP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "AP" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_TBAP, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_TBAP, maxit=500) #more complex model

anova(mod1, mod2)

#TB vs JX
Agelength_TBJX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "JX" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_TBJX, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_TBJX, maxit=500) #more complex model

anova(mod1, mod2)


#TB vs IR
Agelength_TBIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "IR" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_TBIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_TBIR, maxit=500) #more complex model

anova(mod1, mod2)

#CK vs CH

Agelength_CKCH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CK" |  bay== "CH" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKCH, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKCH, maxit=500) #more complex model

anova(mod1, mod2)

#CK Vs AP
Agelength_CKAP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CK" |  bay== "AP" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKAP, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKAP, maxit=500) #more complex model

anova(mod1, mod2)

#CK Vs JX
Agelength_CKJX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CK" |  bay== "JX" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKJX, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKJX, maxit=500) #more complex model

anova(mod1, mod2)

#CK Vs IR
Agelength_CKIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CK" |  bay== "IR" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKIR, maxit=500) #more complex model

anova(mod1, mod2)


#CH Vs AP
Agelength_CKAP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CH" |  bay== "AP" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKAP, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKAP, maxit=500) #more complex model

anova(mod1, mod2)

#CH Vs JX
Agelength_CKJX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CH" |  bay== "JX" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKJX, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKJX, maxit=500) #more complex model

anova(mod1, mod2)


#CH Vs IR
Agelength_CKIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "CH" |  bay== "IR" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_CKIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_CKIR, maxit=500) #more complex model

anova(mod1, mod2)

#AP Vs JX
Agelength_APJX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "AP" |  bay== "JX" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_APJX, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_APJX, maxit=500) #more complex model

anova(mod1, mod2)


#AP Vs JX
Agelength_APIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "AP" |  bay== "IR" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_APIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_APIR, maxit=500) #more complex model

anova(mod1, mod2)

#JX Vs IR
Agelength_JXIR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "JX" |  bay== "IR" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_JXIR, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_JXIR, maxit=500) #more complex model

anova(mod1, mod2)



