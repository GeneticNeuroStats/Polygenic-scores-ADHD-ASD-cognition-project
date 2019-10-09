################################################################
# DATA MANAGEMENT
################################################################
 #Set working directory 
setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/DESCRIPTIVOS") 

#Load packages
library(foreign)
library(psych)
library(nlme)
library(lme4)
library(nnet)
library(ggplot2)
library(reshape2)
library(dplyr)

################################################################
# GENETIC DATA
################################################################
#Loading PCs BREATHE
pcs<-read.table("pcs.txt", header=T)
pcs<-pcs[,c(2:12)] 
pcs$idnum<-pcs$IDNUM 
pcs<-pcs[,c(12,2:11)]

#Loading PGS ADHD (Demontis et al. 2018 Nat Gen)
adhd<-read.table("PRS_ADHD_BREATHE_scaled_20190509.txt", header=T)
head(adhd)
colnames(adhd)[which(names(adhd) == "IID")] <- "idnum"  
colnames(adhd)[which(names(adhd) == "ADHD_0_01")] <- "pT_0_01_adhd"  
colnames(adhd)[which(names(adhd) == "ADHD_0_05")] <- "pT_0_05_adhd"
colnames(adhd)[which(names(adhd) == "ADHD_0_1")] <- "pT_0_1_adhd"
colnames(adhd)[which(names(adhd) == "ADHD_0_5")] <- "pT_0_5_adhd"
colnames(adhd)[which(names(adhd) == "ADHD_1")] <- "pT_1_adhd"
str(adhd)  #check format variables

#histogram PRS
par(mfrow=c(2,3))
hist(adhd$pT_0_01_adhd)
hist(adhd$pT_0_05_adhd)
hist(adhd$pT_0_1_adhd)
hist(adhd$pT_0_5_adhd)
hist(adhd$pT_1_adhd)

#Scale PRS
adhd$zpT_0_01_adhd<-scale(adhd$pT_0_01_adhd)
adhd$zpT_0_05_adhd<-scale(adhd$pT_0_05_adhd)
adhd$zpT_0_1_adhd<-scale(adhd$pT_0_1_adhd)
adhd$zpT_0_5_adhd<-scale(adhd$pT_0_5_adhd)
adhd$zpT_1_adhd<-scale(adhd$pT_1_adhd)

#histogram  scaled PRS
par(mfrow=c(2,3))
hist(adhd$zpT_0_01_adhd)
hist(adhd$zpT_0_05_adhd)
hist(adhd$zpT_0_1_adhd)
hist(adhd$zpT_0_5_adhd)
hist(adhd$zpT_1_adhd)

adhd<-adhd[,c(1,7:11)]  

#Loading PGS ASD (Grove et al. 2019 Nat Gen)
asd<-read.table("PRS_ASD_BREATHE_scaled_20190509.txt", header=T)
head(asd)
colnames(asd)[which(names(asd) == "IID")] <- "idnum"  
colnames(asd)[which(names(asd) == "ASD_0_01")] <- "pT_0_01_asd"  
colnames(asd)[which(names(asd) == "ASD_0_05")] <- "pT_0_05_asd"
colnames(asd)[which(names(asd) == "ASD_0_1")] <- "pT_0_1_asd"
colnames(asd)[which(names(asd) == "ASD_0_5")] <- "pT_0_5_asd"
colnames(asd)[which(names(asd) == "ASD_1")] <- "pT_1_asd"

str(asd)  #check format variables

#Scale PRS
asd$zpT_0_01_asd<-scale(asd$pT_0_01_asd)
asd$zpT_0_05_asd<-scale(asd$pT_0_05_asd)
asd$zpT_0_1_asd<-scale(asd$pT_0_1_asd)
asd$zpT_0_5_asd<-scale(asd$pT_0_5_asd)
asd$zpT_1_asd<-scale(asd$pT_1_asd)

#histogram  scaled PRS
par(mfrow=c(2,3))
hist(asd$zpT_0_01_asd)
hist(asd$zpT_0_05_asd)
hist(asd$zpT_0_1_asd)
hist(asd$zpT_0_5_asd)
hist(asd$zpT_1_asd)

asd<-asd[,c(1,7:11)]  #keeping only scaled PRS
head(asd)

### MERGE PCS + PRS FOR ADHD & ASD
head(pcs)
head(adhd)
gen<-merge(pcs,adhd, by="idnum")
gen<-merge(gen,asd, by="idnum")
dim(gen)  
head(gen) 

################################################################
# PHENOTYPIC DATA: PHENO (WIDE FORMAT) + COVARS BREATHE
################################################################
breathew<-read.dta("BREATHE_Sofia_wide_20190411.dta")
dim(breathew)  
colnames(breathew)
ids<-c("84102","172210","172301","174225","183111","234109","332208",NA)  #idnums to remove
breathe1<-breathew[!(breathew$idnum %in% ids),]  #remove
dim(breathe1)
#remove old polygenic socres+PCs+variables not needed
breathe1<-breathe1[,c(1:51,67:98,65,162)]
dim(breathe1)  

################################################################
# MERGE GENETIC + PHENOTYPIC DATA
################################################################
breathe2<-merge(breathe1,gen, by.x="idnum",by.y="idnum", all.x=T)
dim(breathe2)  #2897 105

#Generate var "gwas_incl" according to data on genetics yes/no. With genotyped data
breathe2$gwas_incl<-NA
breathe2$gwas_incl<- ifelse(is.na(breathe2$PC1), 0, 1)
breathe2$gwas_incl<-as.factor(breathe2$gwas_incl)
table(breathe2$gwas_incl)
#Re-order vars
breathe2<-breathe2[,c(1,2,13,17,9,5,14,18,10,6,11,15,7,3,12,16,8,4,23,31,39,47,53,55,57,59,24,32,40,48,25,33,41,49,26,34,42,50,27,35,43,51,20,28,36,44,52,54,56,58,21,29,37,45,22,30,38,46,19,60:63,65,64,66,85,84,67:83,106,86:105)]
dim(breathe2)  #2897   106

#Check numeric & factors
str(breathe2)
breathe2[43:58] <- lapply(breathe2[43:58], as.numeric)
breathe2[69:74] <- lapply(breathe2[69:74], as.numeric)
breathe2[72:78] <- lapply(breathe2[72:78], as.numeric)
breathe2[80:81] <- lapply(breathe2[80:81], as.numeric)
str(breathe2)

write.table(breathe2, "BREATHE_wide_TFM_Sofia_20190415.txt", col.names=T, row.names=F, quote=F, sep="\t")
