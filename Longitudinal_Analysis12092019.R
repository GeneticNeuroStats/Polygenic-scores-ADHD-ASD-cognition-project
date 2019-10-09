library(foreign)
#install.packages("psych")
library(psych)
#install.packages("nlme")
library(nlme)
#install.packages("nnet")
#install.packages("lme4")
library(lme4)
require(nnet)
require(ggplot2)
require(reshape2)
library(dplyr)
#install.packages("r2glmm")
library(r2glmm)


setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/TABLAS")
###############################################################################################
#Script para analizar trayectorias:
##############################################################################################  

setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/longitudinal_analysis")
datlong <- read.dta("BREATHE_long_TFM_Sofia_20190612.dta")
datlong$ones_v
dim(datlong)

#TABLA DE RESULTADOS LONGITUDINAL ANALYSIS

#VISITAS:
datones<-subset(datlong, ones_v>90) #IMPORTANT, eliminar outliers!
dim(datones)

#Determinant
Scores<- names(datones[,86:95]) 

#Outcome 

dparaules3<-datones$dparaules3_v
dnumeros3<-datones$dnumeros3_v
hitrtse<- datones$hitrtse_v
colnames(datones)
final_outcomes<-names(datones[,c(7,5,13)])

##################################################################
### analyses 5 thresholds prsice GLOBAL
##################################################################
outcomes<-names(datones[,c(7,5,13)]) 
prs<-names(datones[,86:95]) 


##############################################################################################
#RESULTADOS ANALISIS LONGITUDINAL TENIENDO EN CUENTA LOS PRS Y AÑADIENDO PVALUE AJUSTADO Y R2
#############################################################################################

#install.packages("piecewiseSEM")
library( piecewiseSEM)
library(lme4)
library(nlme)

coefs=as.data.frame(matrix(NA,nrow=1,ncol=10))
colnames(coefs)=c("time","Outcome","model","Determinant","BetaNoStand", "SE", "P","R2","R2PRS","N")

count=1
for(j in 1:length(outcomes)) {
  print(outcomes[j])
  results=NULL
  for(i in 1:length(prs)) {
    print(prs[i])
    ff <- paste(outcomes[j],"~",prs[i],"*as.numeric(edat_v)+factor(sexe_total)+factor(meduc)+PC1+PC2+PC3+PC4",sep="")  
    fit <- lme(fixed=as.formula(ff), random=~1|escola/idnum,data=datones,na.action=na.omit)
    s = summary(fit)
    res = s$tTable
    R2 = r2beta(fit, partial=TRUE,method="sgv",data=datones)
    #Save analysis details and coefficients
    coefs[count,1]=gsub(" ","_",Sys.time())
    coefs[count,2]=outcomes[j]
    coefs[count,3]=ff
    coefs[count,4]=prs[i]
    coefs[count,5:7]=res[2,c(1,2,5)]
    coefs[count,8]=R2$Rsq[1]
    coefs[count,9]=R2$Rsq[3]
    coefs[count,10]=fit$dims$N[1]
    
    count=count+1
    
  }
}
View(coefs)
#coefs$Padj<-  p.adjust(coefs$P, method="fdr")
View(coefs)
setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/ANALYSIS_DEF_27082019/ULTIMOS_ANALYSIS_120919")
write.table(coefs,file="table_longitudinal_analysis_PRS_12092019.txt",quote=TRUE,dec=".",row.names=TRUE,col.names=TRUE)
