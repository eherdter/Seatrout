library(FSA)
library(magrittr)
library(dplyr)
library(nnet)
# set working directory
setwd("~/Desktop/Github Repo/Seatrout/Data")


Agelength_TB<- droplevels(subset(as.data.frame(read.csv("ALK with Bay.csv", header=T)), bay=="TB"  & tl>199 &tl<251 & final_age >0, select=c(specimennumber, bay, tl, final_age))) %>% mutate(tl=tl/10, length =lencat(tl, w=1), age=final_age) #, as.fact=TRUE))- can include this when determing ALK below but the smoothed ALK needs to be the nonfactored version of the length categorization variable. 
#missinglengths <- data.frame( matrix(c(NA,NA ,NA ,NA ,15,NA, 
                           NA,NA,NA,NA,18,NA,
                           NA,NA,NA,NA,68,NA), ncol=6, nrow=3))
#names(missinglengths) <- c("specimennumber", "bay", "tl", "final_age", "length", "age")
#test4<- rbind(Agelength_TB, missinglengths)
alk.freq_TB <- xtabs(~length+age, data=Agelength_TB) 
#missing length classes 68, 2-13,15, and 18 so going to add them in as dummy variables 
# in Agelength_TB so that even all non sampled length classes are included in the matrix




#transpose the matrix so it's in the format required below (mat[i,j]= i is legth, j is age)
agelengthmatrix = t(alk.freq_TB)
ages = c(1:3)
lmin=20#Tampa Bay
lmax=25 #Tampa Bay 

matrix.mat <- as.matrix(mat)

#calculated age distributions at length from the product of the likelihoods
#output a list pmat- the ALK,

pi2p = function(x) {
    z =1-x
    cz=cumprod(c(1,z))
    dz= -diff(cz)
    c(dz,cz[length(cz)])
}

#calculates the ALK
#mat = matrix -mat[i,j] = number of fish length i, and age j
#ages=list or vector of ages to be modelled
#lmin, lmax=integers - minimum/maximum length to be modelled

ord.regr=function(mat, ages, lmin, lmax) {
    N=ages
    rr=lmin:lmax
    agemin=min(ages)
    agemax=max(ages)
    pimat=matrix(NA, agemax-agemin, length(rr))
    resdf=matrix(NA, length(ages))
    resdev=matrix(NA, length(ages))
    len=data.frame(rr)
    names(len)="length"
    if(length(ages)>2) {
      for (j in agemin:(agemax-2)){
        dmat=cbind(mat[,j], apply(mat[,(j+1):agemax],1,sum),mat[,1])
        ff=apply(dmat[,1:2],1,sum)
        dmat=dmat[ff>0,]
        dmat=data.frame(dmat)
        names(dmat) = c("S", "F", "length")
        z=glm(cbind(S,F)~length,binomial, data=dmat)
        pimat[j-agemin+1,] =predict(z,newdata=len,type="resp")
      }}
    j=agemax-1
    dmat=cbind(mat[,(j)], mat[,(j+1)], mat[,1])
    ff=apply(dmat[,1:2],1,sum)
    dmat=dmat[ff>0,]
    dmat=data.frame(dmat)
    names(dmat)=c("S","F", "length")
    z=glm(cbind(S,F)~length,binomial, data=dmat)
    pimat[j-agemin+1,] =predict(z,newdata=len, type="resp")
    pmat= apply(pimat,2,pi2p)
    par(mfrow=c(2,1), mar=c(2,2,2,1)+.1)
    matplot(rr, t(pmat), type="l", lty=1, lwd=2, col=rainbow(length(ages)))
    legend(30, 0.9, paste("Age", c(1,2,3,4), sep="-"), lty=1, lwd=2, col=rainbow(length(ages)))
    text(78,0.1, "LENGTH (cms)")
    barplot(pmat, names.arg=rr, space=0, col=rainbow(length(ages)))
    colnames(pmat) =lmin:lmax
    rownames(pmat) = N
          pmat
}
#pmat is the ALK matrix 
#Calculates the Likelihood of the ALK pmat - output is a real number
#pmat = pmat output from ord.regr
#mat = matrix of observed data. mat[,1] is the length class then mat[i, j+1] is the number of fish of length i, age j
Lik = function(mat, pmat, ages) {
      agemin=min(ages)
      lik = matrix(NA, nrow=length(ages), ncol=1)
      for (age in ages) {
          lks=0
    for (l in mat[,1]) {
      if(l<=80) {
        #ensure the right prob is multiplied by the right number
        #lk=log(pmat[age, l+1]) *mat[mat[,1]==1,age+1] #when lmin=0
        lk =log(pmat [age,l])*mat[mat[,1]==l, age+1]
        lks=lks+lk
      }}
      lik[age-agemin+1,] =lks
      }
    rownames(lik) =ages
    lik
}


par(mfrow=c(2,1), mar=c(2,2,2,1)+.1)
matplot(rr, t(pmat), type="1", lty=1, lwd=2, col=rainbow(length(ages)))
legend(0.5, 0.9, paste("Age", c(2,34,56,7), sep="-"), lty=1, lwd=2, col=rainbow(ages))
text(78, 0.1, "LENGTH (cms)")
barplot(pmat, names.arg=rr, space=0, col=rainbow(length(ages)))
colnames(pmat) =lmin:lmax
rownames(pmat) = N

