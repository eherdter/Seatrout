####################
# 10/26/2016
# Purpose: To produce adjusted nominal catch indices using the Delta Method
# This method uses the Delta method for determining the nominal catch rate of Spotted Seatrout in the FIM catch
# Uses methods described in Chyan-huei et al 1992

# positive haul= hauls with Seatrout
# zero haul= hauls without any Seatrout 

# Positive Data = total numbers of Seatrout in a year/ total numbers of positive hauls in year
# Proportion Positive = total number of positive hauls / total numbers of all hauls (positive and zero) 

# Adjusted Index = Positive Data * Proportion Positive

###########################
# Set working directory
###########################
setwd("~/Desktop/Github Repo/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData")
library(haven)
library(dplyr)
###########################
# Import Data Sets
###########################
# These data sets were produced using the spp_comb_5_13_EG_2bays_yoy_2015.sas program which is stored in my scratch folder
# Bay and river stations were denoted by the gear sampling  code. There is a hard copy of this script in my green folder. 


# select the important recruitment months for each zone and also check on gear codes
# adjust selected gear if it was not chosen in the first place
# _C$month => depends on recruitment window in each estuary
#               => Jax 5<=x<=11
#               => nor. IRL 5<=x<=11
#               => CK  5<=x<=11
#               => TB  4<=x<=10
#               => CH  4<=x<=10
#               => AP  6<=x<=10
# also check on zones to make sure the bays and rivers are stratefying correctly => depends on each estuary (for a more in depth description of monthly sampling within bay or riverine see Annual Report)
#               => TB BAY(A-E), RIVERINE (K-N)
#               => CH BAY(A-D), RIVERINE (M-P)
#               => nor.IRL BAY(A-E, H), RIVERINE (F)
#               => AP BAY(A-B), RIVERINE (C)
#               => Jax RIVERINE (A-F)
#               => CK BAY(B-C), RIVERINE (F)

ap = subset(read_sas("ap_yoy_bay_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) #gear 20
apr = subset(read_sas("ap_yoy_riv_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) #gear 23

ck = subset(read_sas("ck_yoy_bay_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11)) #gear 20
ckr = subset(read_sas("ck_yoy_riv_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11) & Zone == "F") #gear 22, 23

ch = subset(read_sas("ch_yoy_bay_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(19,20)) #for some reason gear hadnt been selected so I needed to subset here
chr = subset(read_sas("ch_yoy_riv_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(22,23)) #again same problem as above

tb = subset(read_sas("tb_yoy_bay_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(19,20), select=c(year, bay, Gear, number, Reference))
tbr = subset(read_sas("tb_yoy_riv_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10) & Gear %in% c(22,23), select=c(year, bay, Gear,number, Reference))

nametb <- names(tb)
nametbr <- names(tbr)
# columns are misordered so they wont join. I selected columns specifically. 

ir = subset(read_sas("ir_yoy_riv_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11) & Gear %in% c(22,23))
irb = subset(read_sas("ir_yoy_bay_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11) & Gear %in% c(19,20))

jx = subset(read_sas("jx_yoy_riv_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11))
#no bay data for jax so no jxb

#combine the data sets because some of the rivers were missing data and some gear/zones were partitioning weird
ck_all = rbind(ck,ckr)
ch_all = rbind(ch, chr)
ap_all = rbind(ap,apr)
ir_all <- rbind(ir, irb)
tb_all = rbind(tb, tbr)
jx_all <- jx
##############################################
# MAKE POSITIVE SET
# to determine the total number of positive huals and the total number of fish in all the positive hauls
##########################
#ap.pos<- ap %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#apr.pos <- apr %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

ap_all.pos <- ap_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)


# apr.pos doesnt have any positives for 2003, 2005 or 2013 which causes a problem when trying to make the indices because the years are of unequal length

#ck.pos<- ck %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#ckr.pos<- ckr %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

ck_all.pos<- ck_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#ch.pos<- ch %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#chr.pos<- chr %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

ch_all.pos<- ch_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#tb.pos<- tb %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#tbr.pos<- tbr %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

tb_all.pos <-  tb_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)


#ir.pos<- ir %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)
# missing positive years of data for 98,99,00,01,03,14,15

#irb.pos<- irb %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              #mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

ir_all.pos <-  ir_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)


jx_all.pos<- jx_all %>% subset(number>0) %>% group_by(year) %>% summarize(totalnumberpositivehauls=length(unique(Reference)), TotalNumberOfSeatroutInPosHauls=sum(number))  %>% 
              mutate(positive = TotalNumberOfSeatroutInPosHauls/totalnumberpositivehauls)

#########################################################
# Make Bionomial set for Identifying Positive/Zeros Hauls
#########################################################

# ap.bin = ap %>% mutate(HaulCategory= ifelse(ap$number>0,1,0)) %>% group_by(year) %>%
#           summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#           mutate(ProportionPositive = TotalPosHauls/TotalHauls)
 
# apr.bin = apr %>% mutate(HaulCategory= ifelse(apr$number>0,1,0)) %>% group_by(year) %>%
#           summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#           mutate(ProportionPositive = TotalPosHauls/TotalHauls)


ap_all.bin = ap_all %>% mutate(HaulCategory= ifelse(ap_all$number>0,1,0)) %>% group_by(year) %>% 
      summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
      mutate(ProportionPositive = TotalPosHauls/TotalHauls)

# ck.bin = ck %>% mutate(HaulCategory= ifelse(ck$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)
# 
# ckr.bin = ckr %>% mutate(HaulCategory= ifelse(ckr$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)

ck_all.bin <- ck_all %>% mutate(HaulCategory= ifelse(ck_all$number>0,1,0)) %>% group_by(year) %>% 
        summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
        mutate(ProportionPositive = TotalPosHauls/TotalHauls)


# tb.bin = tb %>% mutate(HaulCategory= ifelse(tb$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)
# 
# tbr.bin = tbr %>% mutate(HaulCategory= ifelse(tbr$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)

tb_all.bin <- tb_all %>% mutate(HaulCategory= ifelse(tb_all$number>0,1,0)) %>% group_by(year) %>% 
          summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
          mutate(ProportionPositive = TotalPosHauls/TotalHauls)

# ch.bin = ch %>% mutate(HaulCategory= ifelse(ch$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)
# 
# chr.bin = chr %>% mutate(HaulCategory= ifelse(chr$number>0,1,0)) %>% group_by(year) %>% 
#         summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#         mutate(ProportionPositive = TotalPosHauls/TotalHauls)

ch_all.bin <- ch_all %>% mutate(HaulCategory= ifelse(ch_all$number>0,1,0)) %>% group_by(year) %>% 
          summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
          mutate(ProportionPositive = TotalPosHauls/TotalHauls)

# ir.bin = ir %>% mutate(HaulCategory= ifelse(ir$number>0,1,0)) %>% group_by(year) %>% 
#           summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#           mutate(ProportionPositive = TotalPosHauls/TotalHauls)
# 
# irb.bin = irb %>% mutate(HaulCategory= ifelse(irb$number>0,1,0)) %>% group_by(year) %>% 
#   summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#   mutate(ProportionPositive = TotalPosHauls/TotalHauls)

ir_all.bin <- ir_all %>% mutate(HaulCategory= ifelse(ir_all$number>0,1,0)) %>% group_by(year) %>% 
        summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
        mutate(ProportionPositive = TotalPosHauls/TotalHauls)

# jx.bin = jx %>% mutate(HaulCategory= ifelse(jx$number>0,1,0)) %>% group_by(year) %>% 
#           summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
#            mutate(ProportionPositive = TotalPosHauls/TotalHauls)

jx_all.bin <- jx_all %>% mutate(HaulCategory= ifelse(jx_all$number>0,1,0)) %>% group_by(year) %>% 
  summarize(TotalPosHauls= sum(HaulCategory), TotalHauls = length(unique(Reference))) %>%
  mutate(ProportionPositive = TotalPosHauls/TotalHauls)

############################
# Produce Adjusted Indices 
############################

AP <- cbind(ap_all.bin$year, data.frame(ap_all.pos$positive*ap_all.bin$ProportionPositive)) 
names(AP) <- c('year', 'index')
write.csv(AP, "~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/AP_yoy_index.csv")

#APR <- cbind(apr.bin$year, data.frame(apr_sum$positive*apr.bin$ProportionPositive)) 
#names(APR) <- c('year', 'index')

CH <- cbind(ch_all.bin$year, data.frame(ch_all.pos$positive*ch_all.bin$ProportionPositive))
#CHR <- cbind(chr.bin$year, data.frame(chr.pos$positive*chr.bin$ProportionPositive))
names(CH) <- c('year', 'index')
write.csv(CH,"~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/CH_yoy_index.csv" )

CK <- cbind(ck_all.bin$year, data.frame(ck_all.pos$positive*ck_all.bin$ProportionPositive))
#CKR <- cbind(ckr.bin$year, data.frame(ckr.pos$positive*ckr.bin$ProportionPositive))
names(CK) <- c('year', 'index')
write.csv(CK,"~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/CK_yoy_index.csv" )


TB <- cbind(tb_all.bin$year, data.frame(tb_all.pos$positive*tb_all.bin$ProportionPositive))
#TBR <- cbind(tbr.bin$year, data.frame(tbr.pos$positive*tbr.bin$ProportionPositive))
names(TB) <- c('year', 'index')
write.csv(TB,"~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/TB_yoy_index.csv" )


IR <- cbind(ir_all.bin$year, data.frame(ir_all.pos$positive*ir_all.bin$ProportionPositive))
#IR <- cbind(ir.bin$year, data.frame(ir.pos$positive*ir.bin$ProportionPositive))
names(IR) <- c('year', 'index')
write.csv(IR,"~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/IR_yoy_index.csv" )


JX <- cbind(jx_all.bin$year, data.frame(jx_all.pos$positive*jx_all.bin$ProportionPositive))
#JXB <- cbind(jxb.bin$year, data.frame(jxb.pos$positive*jxb.bin$ProportionPositive))
names(JX) <- c('year', 'index')
write.csv(JX,"~/Desktop/Github Repo/Seatrout/Data/Indices/DeltaMethod Indices/JX_yoy_index.csv" )
