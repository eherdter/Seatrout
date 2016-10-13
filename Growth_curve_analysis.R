# 10/13/2016 This script imports ALK with Bay.xlsx
# Main Objectives of this script: 
# 1. Imports otolith data. 
# 2. Makes bay-specific vonB growth models with data from ages 1 - N



# 3. Makes bay specific smoothed (modeled) ALK with multinomial modeling methods in Ogle (87-).
# 4. Likelihood ratio testing to do among group statistical comparisons - Ogle (102-103)
# 5. Plots observed and smoothed ALK for each bay. 
# 6. Calculates proportional age distribution
# 7. Determines mean length -at-age (otolith database) and produces plots
# 8. Test length frequency distributions (mrfss, mrip)- ask about whether they have ages
#################################################################

library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)

#load the csv file
# subset by which bay I want
# make sure I have just the "aged sample"
# turn mm to cm
# select just a few variables to make it more manageable
# then drop the remaining bay levels still sticking around (droplevels)
# turn tl from mm to cm
# create length categories with FSA package
#age is >0

Agelength_TB<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="TB" & tl>14 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) #, as.fact=TRUE))- can include this when determing ALK below but the smoothed ALK needs to be the nonfactored version of the length categorization variable. 
Agelength_AP<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="AP" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) #, as.fact=TRUE))
Agelength_CK<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CK" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_CH<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="CH" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_IR<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="IR" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))
Agelength_JX<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="JX" & tl>0 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1)) # as.fact=TRUE))

#Determine a list of starting values

(svTyp_TB <- vbStarts(tl~final_age, data=Agelength_TB))
(svTyp_CK <- vbStarts(tl~final_age, data=Agelength_CK))
(svTyp_CH <- vbStarts(tl~final_age, data=Agelength_CH))
(svTyp_AP <- vbStarts(tl~final_age, data=Agelength_AP))
(svTyp_IR <- vbStarts(tl~final_age, data=Agelength_IR))
(svTyp_JX <- vbStarts(tl~final_age, data=Agelength_JX))

# Compare VGBFs between groups- requires fitting multiple models to determine which parameters between each group is different. 
# For simplicities sake, I will only compare 2 bays at a time. Can refer to ALK_analysis.R for a list of bay to bay comparisons. 
# Refer to page 236 in Ogle for the family of models that must be considered when examining the differences in VBGF among groups. 
# The nested relationships among these models allows use of likelihoo ratio and extra sum of squares tests.

# Define the generic models to compare
vbLKt <- tl~Linf[bay]*(1-exp(-K[bay]*(final_age-t0[bay])))
vbLK <- tl~Linf[bay]*(1-exp(-K[bay]*(final_age-t0)))
vbLt <- tl~Linf[bay]*(1-exp(-K*(final_age-t0[bay])))
vbKt <- tl~Linf*(1-exp(-K[bay]*(final_age-t0[bay])))
vbL <-  tl~Linf[bay]*(1-exp(-K*(final_age-t0)))
vbK <-  tl~Linf*(1-exp(-K[bay]*(final_age-t0)))
vbt <- tl~Linf*(1-exp(-K*(final_age-t0[bay])))
vb0 <- tl~Linf*(1-exp(-K*(final_age-t0)))

#Fitting linf, K and t0 requires starting values for Linf, K and t0 for each group. I will assume they are in the general area. 
# So I will duplicate. 
Agelength_TBCK<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)),tl>20 & final_age >0 & (bay== "TB" |  bay== "CK" ),select=c(specimennumber, bay, tl, final_age)) %>% mutate(tl=tl/10, lcat2 =lencat(tl, w=1))) # as.fact=TRUE))

#Determine starting values for simplest model (no difference in any of the parameters between groups)
sv0 <- vbStarts(tl~final_age, data=Agelength_TBCK)

#Duplicate so that each bay has its own set of starting values for the most complex model. 
svLKt <- Map(rep,sv0,c(2,2,2))

#Fit to [LKt] model then to the simplest model [vb0]
fitLKt <- nls(vbLKt, data=Agelength_TBCK, start=svLKt)
residPlot(fitLKt, col=rgb(0,0,0,1/3))

fit0 <- nls(vb0, data=Agelength_TBCK, start=sv0)

# Likelihood ratio and extra for comparing the two models are calculated with lrt() and extraSS().
lrt(fit0, com=fitLKt, com.name="All pars differ", sim.name="No pars differ")
extraSS(fit0, com=fitLKt, com.name="All pars diff", sim.names="No pars diff")

#Likelihood ratio and extra sums of square test both suggest a significant difference
# between the most complex and the simplest. So there is evidence that there is some
# difference in the parameters between TB and CK

#Determine starting values for each model scenario
svLK <- Map(rep,sv0,c(2,2,1))
svLt <- Map(rep,sv0,c(2,1,2))
svKt <- Map(rep,sv0,c(1,2,2))

fitLK <- nls(vbLK, data=Agelength_TBCK, start=svLK)
fitLt <- nls(vbLt, data=Agelength_TBCK, start=svLt)
fitKt <- nls(vbKt, data=Agelength_TBCK, start=svKt)

#Compare the nested models

lrt(fitLK, fitLt, fitKt, com=fitLKt, com.name="All pars diff", sim.names=c("Linf, K diff", "Linf,t0 diff", "K, t0, diff"))

# ***** Any nested model that is not statistically different from[L,K,t0] is considered "better" because it fits equally (statistically) well, but is more parsimonious.
#       If two models are better than [L, K, t0] then the one with the greatest LL is chosen to be the best of the nested models. 

# Therefore in above example Model 2 [Kt] is chosen and will be used as the basis for further model reduction. 

svL <- Map(rep,sv0,c(2,1,1))
svt <- Map(rep,sv0,c(1,1,2))

fitL <- nls(vbL, data=Agelength_TBCK, start=svL)
fitt <- nls(vbt, data=Agelength_TBCK, start=svt)
lrt(fitL, fitt, com=fitKt, com.name = "Linf, t0 diff", sim.names=c("Linf dif", "t0 dif"))

svK <- Map(rep, sv0,c(1,2,1))
fitK <- nls(vbK, data=Agelength_TBCK, start=svK)

# COULD ALSO try AIC
cbind(AIC(fitLKt,fitLK,fitLt,fitKt, fitL, fitK, fitt, fit0), BIC(fitLKt, fitLK, fitLt, fitKt, fitL, fitK, fitt, fit0))