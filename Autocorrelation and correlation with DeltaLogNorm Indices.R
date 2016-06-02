# 5/24/2016 This file imports the indices of abundance produced from the DeltaLogNormal....R file.
# Main Objectives of this script: 
# 1. Tests interseries correlation (autocorrelation). If interseries correlation exists 
#   then significance tests must be adjusted based on degrees of freedom (Pyper et al. 2001)
# 2. Pearson product-moment correlation (Mueter et al. 2002, Field and Ralston 2005, Pyper et al. 2001, Peterman et al. 1998, Myers at al. 1997)

# library(dplyr) NOTE: loading dplyr conflicts with the lag1.plot in astsa. DO NOT LOAD before using ASTSA. 
library(astsa)

setwd("~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaLogNormal Indices and Diagnostics")

#Import the indices and select the year, mean and std.dev for graphing purposes
ap_bay_mean<- subset(read.csv("index_apbay_yoy.csv", header=T), select=c(year, Mean))
  names(ap_bay_mean) <- c("year", "ap_bay_mean")
  # ap does not have a riv component because of so many 1 or 0 positive trips in the FIM data. The csv is available but its mostly NaNs

ch_bay_mean<- subset(read.csv("index_chbay_yoy.csv", header=T), select=c(year, Mean))
  names(ch_bay_mean) <- c("year", "ch_bay_mean")
ch_riv_mean<- subset(read.csv("index_chriv_yoy.csv", header=T), select=c(year, Mean))
  names(ch_riv_mean) <- c("year", "ch_riv_mean")


ck_bay_mean<- subset(read.csv("index_ckbay_yoy.csv", header=T), select=c(year, Mean))
  names(ck_bay_mean) <- c("year", "ck_bay_mean")
  # Riv is same as AP above

ir_bay_mean<- subset(read.csv("index_irbay_yoy.csv", header=T), select=c(year, Mean))
  names(ir_bay_mean) <- c("year", "ir_bay_mean")
  # Riv is same as AP above

tb_bay_mean<- subset(read.csv("index_tbbay_yoy.csv", header=T), select=c(year, Mean))
  names(tb_bay_mean) <- c("year", "tb_bay_mean")
tb_riv_mean<- subset(read.csv("index_tbriv_yoy.csv", header=T), select=c(year, Mean))
  names(tb_riv_mean) <- c("year", "tb_riv_mean")

jx_riv_mean<- subset(read.csv("index_jxriv_yoy.csv", header=T), select=c(year, Mean))
  names(jx_riv_mean) <- c("year", "jx_riv_mean")
  # No bay zones for northern indian river

#1A. LAGGED SCATTERPLOTS of each time series
# From Notes_3, GEOS 585A, Spring 2015 handout printed from arizona (website above)
# " An attribute of the lagged scatterplot is that it can display autocorrelation regardless of the form of the
# dependence on the past values. An assumption of linear dependence is not necessary."
# Such nonlinear dependence might not be effectively summarized by other methods (such as the acf function)

#1A.1 Determine Critical level of correlation for 95% significance (alpha = 0.5) r= 0+- 2/sqrt(N)
#AP = +-2/sqrt(18)= 0.47
#CH = +-2/sqrt(27)= 0.38
#CK = +-2/sqrt(20)= 0.44
#IR = +- 2/sqrt(26) =0.39
#TB = +- 2/sqrt(27) = 0.38 ***
#Jx= +- 2/sqrt(14) = 0.53

lag1.plot(ap_bay_mean, 4, corr=TRUE)
lag1.plot(ch_bay_mean, 4, corr=TRUE)
lag1.plot(ch_riv_mean, 4, corr=TRUE)
lag1.plot(ck_bay_mean, 4, corr=TRUE)
lag1.plot(ir_bay_mean, 4, corr=TRUE)
lag1.plot(tb_bay_mean, 4, corr=TRUE) #autocorrelation at 1st and 2nd lag
lag1.plot(tb_riv_mean, 4, corr=TRUE)
lag1.plot(jx_riv_mean, 4, corr=TRUE)

#1B. ACF and correlograms
acf2(ap_bay_mean$ap_bay_mean)
acf2(ch_bay_mean$ch_bay_mean)
acf2(ch_riv_mean$ch_riv_mean)
acf2(ck_bay_mean$ck_bay_mean)
acf2(ir_bay_mean$ir_bay_mean)
acf2(tb_bay_mean$tb_bay_mean)
acf2(tb_riv_mean$tb_riv_mean)
acf2(jx_riv_mean$jx_riv_mean)

#little evidence of interseries correlation so can use standard significance test for pairwise correlations


# 2.Pearson correlation
# 2A. join all dataframes together
library(dplyr)
test <- full_join(ap_bay_mean, ch_bay_mean, by='year') %>% 
            full_join(.,ch_riv_mean, by='year' ) %>%
            full_join(.,ck_bay_mean, by='year' ) %>%
            full_join(.,ir_bay_mean, by='year' ) %>%
            full_join(.,tb_bay_mean, by='year' ) %>%
            full_join(.,tb_riv_mean, by='year' ) %>%
            full_join(.,jx_riv_mean, by='year' ) 
mat<- as.matrix(arrange(test, year)) 
mattest=mat[,-1]

# 2B. Computer pearson correlations
library(Hmisc)  #rcorr- missing values are deleted in pairs rather than deleting all rows of x having any missing variables

corr_mat <- rcorr(mattest, type="pearson")
rho_mat <- corr_mat$r #rho values
n_mat <- corr_mat$n #number of samples used to compute correlation
P_mat <- corr_mat$P #P values

#unwrap rho_mat into a vector
library(gdata)
rho_vec <- as.data.frame(lowerTriangle(rho_mat, diag=FALSE, byrow=FALSE))
p_vec   <- as.data.frame(lowerTriangle(P_mat, diag=FALSE, byrow=FALSE))
n_mat <- as.data.frame(lowerTriangle(n_mat, diag=FALSE, byrow=FALSE))
rho_P_vec <- cbind(rho_vec, p_vec, n_mat)
rownames <- c("chb_AP", "chr_AP", "ck_AP", "ir_AP", "tbb_AP", "tbr_AP", "jx_AP", 
             "chr_CHB", "ck_CHB", "ir_CHB", "tbb_CHB", "tbr_CHB", "jx_CHB", 
             "ck_CHR", "ir_CHR", "tbb_CHR", "tbr_CHR", "jx_CHR", 
             "ir_CK", "tbb_CK", "tbr_CK", "jx_CK",
             "tbb_IR", "tbr_IR", "jx_IR", 
             "tbr_TBB", "jx_TBB",
             "jx_TBR")
             
# for example, chb_AP means the distance between charlotteharbor bay and appalachicola        


row.names(rho_P_vec)<- rownames
data.frame(rho_P_vec)
colnames(rho_P_vec)<-  c("rho", "P", "N")

#export
write.csv(rho_P_vec, '~/Desktop/Github Repo/Seatrout/Data/Exported R Dataframes/rho_P_vector.csv')
