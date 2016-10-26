#*************
# Edited on 10/25/2016 when I put data from my FWRI scratch folder in my GitHub on my personal computer -EH
#**************

# Produces indices ##
# *** NOTE. This is R code that I used at FWRI. Currently, it does not produce the indices on my computer.
# I produced them at FWRI and then copied them to my computer. They are located here:
# ("~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaLogNormal Indices and Diagnostics"). 



#must run with R 3.1 for lsmeans to work
rm(list=ls());rm(.SavedPlots);graphics.off();gc();windows(record=T)
setwd("~/Desktop/Github Repo/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData")
library(haven)
library(dplyr)
library('MASS')
library('statmod')
library('lsmeans')
source('cpue_functions.r')

################################################################################
#
#                         setup
#
################################################################################
region  = 'ck'
#for(region in c('nw','sw','ne','se')){
#region='nw'
m       = paste(region,'yoy')

# cutoff percent for the minimum reduction in mean deviance - if used
cutoff.percent = 0.5
cutoff         = cutoff.percent / 100
alpha          = 0.05    #signifcance level for correlation

################################################################################
#
#                         import and prepare master dataset
#
################################################################################
d = read_sas("ck_yoy_bay_cn_c.sas7bdat") 
names(d) = tolower(names(d))
str(d)

#name the response variable- which is the number of YOY C.Neb
  d$response = d$number

#concactenate some habitat variables - basically use the dominant type or one I think is most important for juv red drum
  d$bot_x     = ifelse(d$bstr==1,'structure',ifelse(d$bsan>0 | d$bmud>0,'mudsand','unk'))
  d$bveg_x    = ifelse(substr(d$bveg,1,3) %in% c('Alg','SAV'),'SAV','noveg')
  d$shore_max = max.col(d[,c('man','eme','ter','str')],ties.method='first')
  d$shore_x   = ifelse(d$shore_max==1,'mangrove',ifelse(d$shore_max==2,'emerg',ifelse(d$shore_max==3,'terrest','struct')))

#log the continuous variables
  d      = subset(d,temperature>0 & salinity>0)
  d$temp = log(d$temperature)
  d$salt = log(d$salinity)


#for young of year fish, only worried about recruitment window for Estuary of interest!!
# MUST CHANGE THESE FOR EACH ESTUARY  
  d = subset(d,month %in% c(5,6,7,8,9,10,11))

#some region specific filters
if(region=='ck'){
  d = subset(d,z %in% c('CKB','CKC') & bot_x != 'unk' & year>=1997)
}
# Region CKF is the river region  
  
# #if(region=='sw'){
# #  d = subset(d,z %in% c('CHA','CHB','CHC','CHM','CHP','TBA','TBB','TBC','TBD','TBE','TBK','TBL','TBM') & year>=1990)
# #}
# 
# if(region=='ne'){
#   d = subset(d,bot_x != 'unk' & shore_x != 'mangrove' & year>=2002)
# }
# if(region=='se'){
#   d = subset(d,z %in% c('IRA','IRB','IRC','IRD','IRE','IRF','IRH') & year>=1996)
# }

#which columns to keep; keep response (number) and the potential explanatory variables
  d = d[,c('response','year','month','bveg_x','bot_x','shore_x','z','temp','salt')]

#correct column classes
  str(d)
  d$year    = as.factor(d$year)
  d$month   = as.factor(d$month)
  d$bveg_x  = as.factor(d$bveg_x)
  d$bot_x   = as.factor(d$bot_x)
  d$shore_x = as.factor(d$shore_x)
  d$z       = as.factor(d$z)
  str(d)

#drop empty levels
  d[] <- lapply(d,function(x) if(is.factor(x)) factor(x) else x)

#only complete cases
  d = d[complete.cases(d),]

#explore data - look at these and add more filters if necessary
  with(d,tapply(response,list(year,month),sum))
  with(d,tapply(response,list(year,z),sum))
  with(d,tapply(response,list(year,bveg_x),sum))
  with(d,tapply(response,list(year,bot_x),sum))
  with(d,tapply(response,list(year,shore_x),sum))

#years index
  years = as.numeric(levels(d$year))

#create positive and binomial datasets
  d.pos = d[d$response>0,] #positive data set
  d.bin = d
  d.bin$response = ifelse(d$response>0,1,0) #binomial data set

################################################################################
#
#                         MODEL SELECTION
#
################################################################################
#-------------------------------positive model----------------------------------
#run model
  glm.full.pos = glm(log(response) ~ .,family=gaussian,data=d.pos)
  glm.base.pos = glm(log(response) ~ year,family=gaussian,data=d.pos)
#step to simplify model and get appropriate order, step function selects a formula-based model by AIC
  #the text(variable) in the right side of the lower component is always included in the model; the right side of model (all x values) is placed in upper. So basically, lower is saying always include this and upper is like here is a list of all possibilities
  glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=~year),direction='forward',trace=1,k=log(nrow(d.pos)))
#the anova variables is a deviance and AIC table
  glm.step.pos$anova
  #cut.formula         <- find.formula(glm.step.pos$anova)
  #cut.formula[2]   # deviance table
#final model - may choose to reduce based on deviance and cutoff in above step
  glm.final.pos 
  glm.final.pos = glm.full.pos 
  
  
  summary(glm.final.pos)
#calculate the LSMeans for the proportion of positive trips
  lsm.p.glm   = lsmeans(glm.final.pos, "year", data = d.pos)
  LSMeansPos  = summary(lsm.p.glm)

#-------------------------------binomial model----------------------------------
#run models
  glm.full.bin = glm(response ~ .,family=binomial,data=d.bin)
  glm.base.bin = glm(response ~ year,family=binomial,data=d.bin)
#step to simplify model and get appropriate order
  glm.step.bin = step(glm.base.bin,scope=list(upper=glm.full.bin,lower=~year),direction='forward',trace=1,k=log(nrow(d.bin)))
#the anova variables is a deviance and AIC table
  glm.step.bin$anova
  #cut.formula         <- find.formula(glm.step.bin$anova)
  #cut.formula[2]   # deviance table
#final model - may choose to reduce based on deviance and cutoff in above step
  
  glm.final.bin  
  
  
  glm.final.bin = glm.full.bin 
  summary(glm.final.bin)
  anova(glm.final.bin)
#calculate the LSMeans for the proportion of positive trips
  
  # proportion of the positive trips out of the whole data set is best described by simply defining binary data. 
  # we dont care specifically how many trips were positive in each year or exactly what the number was we just care whehter the trips
  # were positive or not. Making a binomial dataset shows only positives and negatives and is helpful
  # for us to get a total proportion of how many trips were actually positive trips out of all of the trips
  
  lsm.b.glm   = lsmeans(glm.final.bin, "year", data = d.bin)
  LSMeansProp = summary(lsm.b.glm)
  
  
  dbin_2000 <- subset(d.bin, year=2000)
  mean(dbin_2000$response) #= 0.1187
  

################################################################################
#
#                         CATCH RATE
#
################################################################################
#determining the number of positive trips per year
  sample.size.pos = as.data.frame(table(d.pos$year))
  num.pos         = sample.size.pos$Freq
#calculating the nominal catch rates
  SampleSize  = tapply(d.bin$response, d.bin$year, length)
  NominalMean = tapply(d.bin$response, d.bin$year, mean)
  NominalSD   = tapply(d.bin$response, d.bin$year, sd)
  NominalCV   = NominalSD /NominalMean
  estimate.0  = cbind(years, LSMeansPos$lsmean, LSMeansPos$SE, LSMeansProp$lsmean, LSMeansProp$SE, num.pos)
  estimate    = as.data.frame(estimate.0)
  names(estimate) = c("year", "LSM.pos","se.pos", "LSM.ppt","se.ppt","num.pos")
  estimate

#creating the index through Monte Carlo simulations
# checking whether the lsmeans are correlated
  check.rho   = cor.test(estimate$LSM.pos, estimate$LSM.ppt)
  rho         = check.rho$estimate
  correlated  = (check.rho$p.value <= alpha)
#loop through years
  num.yr      = length(years)
  index.dist  = matrix(data=NA,nrow=num.yr,ncol=9)  #make blank matrix for the results to go into
  set.seed(12345)
  for (i in 1:num.yr) {
    rn.1   <- rnorm(10000,0,1) #10,000 random numbers with mean 0 and variance =1 
    rn.2   <- rnorm(10000,0,1) 
    temp.1 <- estimate$LSM.ppt[i] + rn.1 * estimate$se.ppt[i]
    if (correlated) {
      temp.2 <- estimate$LSM.pos[i] + estimate$se.pos[i] * (rho*rn.1 + rn.2*sqrt(1-rho^2))
    } else {
      temp.2 <- estimate$LSM.pos[i] + rn.2 * estimate$se.pos[i]
    }
    prop.pos <- exp(temp.1)/(1 +exp(temp.1))
    pos      <- exp(temp.2)
    temp    <- prop.pos * pos
    l.temp  = log(temp)
    index.dist[i,1] <- mean(temp) #mean
    index.dist[i,2] <- sd(temp) #standard dev
    index.dist[i,3] <- sd(temp)/mean(temp) #CV
    index.dist[i,4:8] <- quantile(temp,probs=c(0.025,0.25,0.50,0.75,0.975)) # low, qtr 1 median, qtr 3 up 95
    index.dist[i,9] = sqrt(var(l.temp)) #log se ?
  }

index <- as.data.frame(cbind(as.character(estimate$year),SampleSize,estimate$num.pos,index.dist, NominalMean,NominalSD,
                             NominalCV ))
names(index) <- c("year","Total.num.trips","Num.pos","Mean","std.dev", "CV","Low.95","Qtr.1","Median","Qtr.3","Up.95",
                  "log se","NominalMean", "NominalSD", "NominalCV" )

################################################################################
#
#                         plots
#
################################################################################ 
#Binomial Residuals: randomized quantile residuals using qres.binom from statmod package:
#Quantile residuals are based on the idea of inverting the estimated distribution
#function for each observation to obtain exactly standard normal residuals. In
#the case of discrete distributions, such as the binomial and Poisson, some
#randomization is introduced to produce continuous normal residuals.
#Quantile residuals are the residuals of choice for generalized linear models in
#large dispersion situations when the deviance and Pearson residuals can be
#grossly non-normal. Quantile residuals are the only useful residuals for binomial
#or Poisson data when the response takes on only a small number of distinct values.
 pdf(paste('plots_',region,'_yoy.pdf',sep=''),onefile=T)
   glm.final.bin$formula
   par(mfrow=c(4,2),oma=c(0,0,2,0),mar=c(4,4,2,2))
   ylabs <- 'Rand. Quant. Resids.'
   hist(qres.binom(glm.final.bin),main='',xlab='Rand. Quant. Resids.')
   for(i in 2:ncol(glm.final.bin$model)){
     plot(glm.final.bin$model[,i],qres.binom(glm.final.bin),xlab=names(glm.final.bin$model)[i],ylab=ylabs)
   }
   title('Randomized Quantile Residuals for Binomial Model',outer=T,cex.main=2)
 #binomial diagnostic plots
   par(mfrow=c(2,2))
   plot(glm.final.bin,sub.caption='Diagnostic Plots for Binomial Submodel')
 #plots of effect by each factor as predicted by the model
   glm.final.pos$formula
   par(mfrow=c(2,2), mex=.5, mai=c(.4,.4,.2,.3))
   termplot(glm.final.pos,se=T,cex=0.75)
   mtext("Plots Positive Catch Observations",side=3, line=-1.5,outer=TRUE)
 #residual plots of positive model
   par(mfrow=c(3,2),oma=c(0,0,2,0),mar=c(4,4,2.5,2))
   hist(residuals(glm.final.pos),main='',xlab='Standardized Residuals')
   for(i in 2:ncol(glm.final.pos$model)){
     plot(glm.final.pos$model[,i],residuals(glm.final.pos),xlab=names(glm.final.pos$model)[i],ylab=ylabs)
   }
   title('Standardized Residuals for Positive Model',outer=T,cex.main=2)
 #default diagnostic plots of positive model
   par(mfrow=c(2,2))
   plot(glm.final.pos,sub.caption='Diagnostic Plots of Positive Submodel')
 #index plot
  x         <- as.numeric(as.character(index[ -nrow(index),"year"]))
  y         <- index.dist[-nrow(index.dist), 4:8]
  y1        <- index.dist[-nrow(index.dist),8]
  x.label   <- "Year"
  y.axis    <- c(0,1.05*max(y1))
  y.label   <- "Number per Sample"
  box.color <- "yellow"
  pt.labels <- as.numeric(as.character(index[ -nrow(index),"Num.pos"]))
png(paste('index_',region,'_YOY.png',sep=''),width=7,height=7,units='in',res=300)
  par(mfrow=c(1,1))
  boxplot(y ~ x, main = ifelse(region=='nw','Northwest',ifelse(region=='sw','Southwest',ifelse(region=='ne','Northeast','Southeast'))),
    ylim = y.axis, range = 0,
    xlab = x.label, ylab = y.label , col=box.color)
  text(seq(1:(num.yr-1)), y1,labels=pt.labels,pos=3,offset=0.5,cex=0.7)
dev.off()

################################################################################
#
#                         output
#
################################################################################ 
write.csv(index,paste('index_',region,'_yoy.csv',sep=''),row.names=F)
}


################################################################################
#
#         FIT POSITIVE CPUE TO A DISTRIBUTION, EITHER GAMMA OR LOGNORMAL
#
################################################################################
# #use the fitdistr fxn in the MASS package.  I did it by hand below just to check
# #get positive cpue values. Smaller AIC is better.
#   d1_pos  = as.numeric(d$response[d$response>0])
#   d1_pos[!complete.cases(d1_pos)]
# #log-normal
#   lnfit <- fitdistr(d1_pos,'log-normal')
# #gamma
#   gmfit <- fitdistr(d1_pos,'gamma')
#   1/gmfit$estimate[2]
# #build AIC table
#   AICtable <- data.frame(matrix(nrow=2,ncol=4))
#   rownames(AICtable) <- c('lognormal','gamma')
#   colnames(AICtable) <- c('logLik','npar','AIC','deltaAIC')
#   AICtable[,1] <- c(lnfit$loglik,gmfit$loglik)
#   AICtable[,2] <- c(length(lnfit$estimate),length(gmfit$estimate))
#   AICtable[,3] <- c(AIC(lnfit),AIC(gmfit))
#   AICtable[,4] <- max(AICtable$AIC) - AICtable$AIC
#   AICtable

