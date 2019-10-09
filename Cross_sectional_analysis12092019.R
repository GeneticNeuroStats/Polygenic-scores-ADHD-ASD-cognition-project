setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS")

library(foreign)
library(nlme)
library(psych)
library(r2glmm)

breathe_wide<-read.dta("BREATHE_wide_TFM_Sofia_20190612.dta")
colnames(breathe_wide)


#VISIT 1:
breathe_wide$ones_v1 > 90  

breathe_wide$hitrtse_v1
breathe_wide$include_v1<- ifelse(breathe_wide$ones_v1>= 90, breathe_wide$hitrtse_v1,NA)

#VISIT 2:
breathe_wide$hitrtse_v2
breathe_wide$include_v2<- ifelse(breathe_wide$ones_v2 >= 90, breathe_wide$hitrtse_v2,NA)

#VISIT 3:

breathe_wide$hitrtse_v3
breathe_wide$include_v3<- ifelse(breathe_wide$ones_v3 >= 90, breathe_wide$hitrtse_v3,NA)

#VISIT 4: 

breathe_wide$hitrtse_v4
breathe_wide$include_v4<- ifelse(breathe_wide$ones_v4>= 90, breathe_wide$hitrtse_v4,NA)

colnames(breathe_wide)

breathe_wide[,c("include_v1","include_v2","include_v3","include_v4")]

#AHOR VAMOS A VER LA EDAD. Hay una edad diferente según cada visita al estar en meses

breathe_wide$edat_v1
breathe_wide$edat_v2
breathe_wide$edat_v3
breathe_wide$edat_v4

#Determinant
Scores<- names(breathe_wide[,97:106]) #determinant (en el script de natalia son los snps)

#Outcome (para natalia en su script es el volumen)

dparaules3<-names(breathe_wide[,7:10])
dnumeros3<-names(breathe_wide[,15:18])
hitrtse<- names(breathe_wide[,27:30])
include<-names(breathe_wide[,107:110]) #estos equivalen a los hit reactions con un valor de one > 90

final_outcomes<-names(breathe_wide[,c(7,8,9,10,15,16,17,18,107,108,109,110)])

###############################################################################

#RESULTADOS CROSS-SECTIONALES TENIENDO EN CUENTA LOS PRS Y CALCULANDO P ADJ Y R2
##################################################################################

View(breathe_wide)
coefs=as.data.frame(matrix(NA,nrow=1,ncol=10))
colnames(coefs)=c("DateTime","Outcome","Determinant","Model","BetaNoStand", "SE", "P","R2","R2PRS","N")
#colnames(coefs)=c("DateTime","Outcome","Determinant","Model","BetaU1", "SE1", "T1", "P1", "BetaU2","SE2","T2","P2") #original nat
count=1

for(j in 1:length(final_outcomes)) {
  print(final_outcomes[j])
  results=NULL
  for(i in 1:length(Scores)) {
    print(Scores[i])
    n<-unlist(strsplit(final_outcomes[j],split=NULL))   #formula que te permite sacar los caracteres de un string 
    start <- length(n)-1
    stop <- length(n)
    nvisit <- substr(final_outcomes[j], start, stop)
    ff <- paste(final_outcomes[j],"~",Scores[i],"+edat_",nvisit,"+sexe_total+meduc+PC1+PC2+PC3+PC4",sep="") 
    fit <- lme(fixed=as.formula(ff), random=~1|escola,data=breathe_wide, method="ML",na.action=na.omit, control = lmeControl(opt="optim"))
    s = summary(fit)
    res = s$tTable
    R2 = r2beta(fit, partial=TRUE,method="sgv",data=breathe_wide)
    
    #Save analysis details and coefficients
    coefs[count,1]=gsub(" ","_",Sys.time())
    coefs[count,2]=final_outcomes[j]
    coefs[count,3]=Scores[i]
    coefs[count,4]=ff
    coefs[count,5:7]=res[2,c(1,2,5)]
    coefs[count,8]= R2$Rsq[1]
    coefs[count,9]= R2$Rsq[3]
    coefs[count,10]=fit$dims$N[1]
    
    count=count+1
    
  }
}

#coefs$Padj<-  p.adjust(coefs$P, method="fdr")
View(coefs) 
#coefs_def<-coefs[,c(2,3,5,6,7,9,8)]

setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/ANALYSIS_DEF_27082019/ULTIMOS_ANALYSIS_120919")
write.table(coefs,file="table_cross_analysis_PRS_12092019.txt",quote=TRUE,dec=".",row.names=TRUE,col.names=TRUE)
