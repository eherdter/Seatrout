# 10/10/2016 This script imports ALK with Bay.xlsx
# Main Objectives of this script: 
# 1. Separate age-length data by bay
# 2. Use methods in Ogle (87-) to do Age Length Key Analysis

library(FSA)
library(magrittr)
library(dplyr)
library(nnet)

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

Agelength_TB<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="TB" & tl>20 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2)) #, as.fact=TRUE))- can include this when determing ALK below but the smoothed ALK needs to be the nonfactored version of the length categorization variable. 
Agelength_AP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="AP" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2 )) #, as.fact=TRUE))
Agelength_CK<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CK" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2)) # as.fact=TRUE))
Agelength_CH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CH" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2)) # as.fact=TRUE))
Agelength_IR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="IR" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2)) # as.fact=TRUE))
Agelength_JX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="JX" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2)) # as.fact=TRUE))


#MAKE SIMPLE ALK
(alk.freq_TB <- xtabs(~lcat2+final_age, data=Agelength_TB)) 
# there appears to be a fish that was assigned an age of 3 but is in the 0-2 length category. Going to remove this because its probably a typo. Specified in above subsetting step in first step as tl>20mm =(2cm).   
rowSums(alk.freq_TB)
(alk.freq_AP <- xtabs(~lcat2+final_age, data=Agelength_AP)) 
rowSums(alk.freq_AP)
(alk.freq_CK <- xtabs(~lcat2+final_age, data=Agelength_CK)) 
rowSums(alk.freq_CK)
(alk.freq_CH <- xtabs(~lcat2+final_age, data=Agelength_CH)) 
rowSums(alk.freq_CH)
(alk.freq_IR <- xtabs(~lcat2+final_age, data=Agelength_IR)) 
rowSums(alk.freq_IR)
(alk.freq_JX <- xtabs(~lcat2+final_age, data=Agelength_JX))
rowSums(alk.freq_JX)

#The conditional proportions that form the ALK are calculated by dividing ecah cell of the frequency table by the sum of the corresponding row. 
# These row proportions are constructed by submitting the xtabs() object to prop.table() and including margin=1 to indicate that the proportions are computed by row (page 92). 

alk_TB <- prop.table(alk.freq_TB, margin=1)
round(alk_TB,3)

#Mean Length at Age
    # done with additional length data


sis<- read.csv("SiscowetMI2004.csv")

#MODELED AGE LENGTH KEYS (aka SMOOTHED ALK)
#-fixes two common issues with Age Length keys detailed on page 92 Ogle. 
#multinomial logistic regression model -Gerritsen et al. 2006. The response variable has more than two levels

tb <- multinom(final_age~lcat2, data=Agelength_TB, maxit=500)
lens<- seq(14,72, 2)
alk.sm <- predict(tb, data.frame(lcat2=lens), type="probs")
row.names(alk.sm) <- lens
round(alk.sm, 3)

#AMONG GROUP STATISTICAL COMPARISONS
#page 102 in Ogle
Agelength_ALL<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" | bay== "CK" | bay== "CH" | bay=="IR" |bay=="AP" | bay=="JX"),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=2))) # as.fact=TRUE))
mod1 <- multinom(final_age~lcat2, data=Agelength_ALL, maxit=500) #simple model
mod2 <- multinom(final_age~lcat2*bay,data=Agelength_ALL, maxit=500) #more complex model

#likelihood ratio test is computed with anova
anova(mod1, mod2)



#do some bay dropping to see which one is causing the difference
#read Stari et al. 2010 and review code on a more flexible continuation ratio logit model