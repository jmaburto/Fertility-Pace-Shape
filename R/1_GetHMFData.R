####### Program for getting most recent data from HFD
############# Written by JMA, thanks to Tim Riffe
############# 23/09/2019
library(HMDHFDplus)
library(data.table)

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Fertility-Pace-Shape")

# get all countries in HMD
XYZ <- getHFDcountries()
# set your username for HMD
us <- "jmaburto@sdu.dk"
# set your password
pw <- "50070"

# get all births and exposure available from HFD
HFD.period <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Births          <- readHFDweb(x,"birthsRR",username=us,password=pw)
  Exposure        <- readHFDweb(x,"exposRR",username=us,password=pw)
  Births$Exposure <- Exposure$Exposure
  Births$PopName  <- x
  Births    
}, us = us, pw = pw))

# convert to data.table
HFD.period <- data.table(HFD.period)
HFD.period <- HFD.period[,c(6,1,2,3,5)]
names(HFD.period) <- c('country','year','age','births','exposure')

#TFR in period
TFR.period <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  tfr          <- readHFDweb(x,"tfrRR",username=us,password=pw)
  tfr$PopName  <- x
  tfr    
}, us = us, pw = pw))


#Mean age at birth in period
MAB.period <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  mab          <- readHFDweb(x,"mabRR",username=us,password=pw)
  mab$PopName  <- x
  mab    
}, us = us, pw = pw))

MAB.period <- data.table(MAB.period)
names(MAB.period) <- c('year','mab','mab40','country')

#standard deviation in mean age at birth in period
SD.period <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  sdb          <- readHFDweb(x,"sdmabRR",username=us,password=pw)
  sdb$PopName  <- x
  sdb    
}, us = us, pw = pw))

SD.period <- data.table(SD.period)
names(SD.period) <- c('year','sdmab','sdmab40','country')



################### cohort data
XYZ2 <- XYZ[-c(5,6)]

#completed cohort fertility
CCF.cohort <- do.call(rbind,lapply(XYZ2, function(x, us, pw){
  cat(x,"\n")
  ccf          <- readHFDweb(x,"tfrVH",username=us,password=pw)
  ccf$PopName  <- x
  ccf    
}, us = us, pw = pw))

CCF.cohort <- data.table(CCF.cohort)
CCF.cohort <- CCF.cohort[,c(1,2,3,5)]
names(CCF.cohort) <- c('cohort','ccf','ccf40','country')


#Mean age at birth in cohort
MAB.cohort <- do.call(rbind,lapply(XYZ2, function(x, us, pw){
  cat(x,"\n")
  mab.cohort          <- readHFDweb(x,"mabVH",username=us,password=pw)
  mab.cohort$PopName  <- x
  mab.cohort    
}, us = us, pw = pw))

MAB.cohort <- data.table(MAB.cohort)
MAB.cohort <- MAB.cohort[,c(1,2,3,5)]
names(MAB.cohort) <- c('cohort','cmab','cmab40','country')


#standard deviation in mean age at birth in cohort
SD.cohort <- do.call(rbind,lapply(XYZ2, function(x, us, pw){
  cat(x,"\n")
  sd.mab.cohort          <- readHFDweb(x,"sdmabVH",username=us,password=pw)
  sd.mab.cohort$PopName  <- x
  sd.mab.cohort    
}, us = us, pw = pw))

SD.cohort <- data.table(SD.cohort)
SD.cohort <- SD.cohort[,c(1,2,3,5)]
names(SD.cohort) <- c('cohort','sd.cmab','sd.cmab40','country')




# get all births and exposure available from HFD
HFD.cohort <- do.call(rbind,lapply(XYZ2, function(x, us, pw){
  cat(x,"\n")
  Births          <- readHFDweb(x,"birthsVH",username=us,password=pw)
  Exposure        <- readHFDweb(x,"exposVH",username=us,password=pw)
  Births$Exposure <- Exposure$Exposure
  Births$PopName  <- x
  Births    
}, us = us, pw = pw))

# convert to data.table
HFD.cohort <- data.table(HFD.cohort)
HFD.cohort <- HFD.cohort[,c(6,1,2,3,5)]
names(HFD.cohort) <- c('country','cohort','age','births','exposure')


#get data by parity
HFD.cohort.birth.parity <- do.call(rbind,lapply(XYZ2[-9], function(x, us, pw){
  cat(x,"\n")
  Births          <- readHFDweb(x,"birthsVHbo",username=us,password=pw)
  Births$PopName  <- x
  Births    
}, us = us, pw = pw))

# convert to data.table
HFD.cohort.birth.parity <- data.table(HFD.cohort.birth.parity)
HFD.cohort.birth.parity <- HFD.cohort.birth.parity[,c(10,1:8)]
names(HFD.cohort.birth.parity) <- c('country','cohort','age','total.births','birth1','birth2','birth3','birth4','birth5p')


# asfr by birth order
HFD.cohort.asfr.parity <- do.call(rbind,lapply(XYZ2[-9], function(x, us, pw){
  cat(x,"\n")
  Births          <- readHFDweb(x,"asfrVHbo",username=us,password=pw)
  Births$PopName  <- x
  Births    
}, us = us, pw = pw))

# convert to data.table
HFD.cohort.asfr.parity <- data.table(HFD.cohort.asfr.parity)
HFD.cohort.asfr.parity <- HFD.cohort.asfr.parity[,c(10,1:8)]
names(HFD.cohort.asfr.parity) <- c('country','cohort','age','asfr','asfr1','asfr2','asfr3','asfr4','asfr5p')

#load('Data/HFD_Data.RData')
# save the data
save(HFD.period,HFD.cohort,TFR.period,MAB.period,SD.period,
     CCF.cohort,MAB.cohort,SD.cohort,HFD.cohort.birth.parity,HFD.cohort.asfr.parity,file="Data/HFD_Data.RData")

load('Data/HFD_Data.RData')

unique(HFD.cohort$country)
