library(foreign)
library(nlme)
library(psych)
library(r2glmm)

setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS")


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




breathe_final<-breathe_wide[,c(1,2,7,8,9,10,15,16,17,18,60,61,62,63,64,68,87,88,89,90,97:106,107,108,109,110,83)] 
View(breathe_final)                               
names(breathe_final)
dim(breathe_final)
withoutADHDchildren<-breathe_final[breathe_final$tdah =="No",]
View(withoutADHDchildren)
dim(na.omit(withoutADHDchildren))
names(withoutADHDchildren)
Nasnumber<-is.na(withoutADHDchildren$zpT_0_01_adhd)
table(Nasnumber) #1500 sin NAs
table(is.na(withoutADHDchildren$dparaules3_v1))
#from a total of 2897, 174 (2723) where excluded to repeat the analysis. 
# Cross-sectional model
Scores<- names(breathe_wide[,97:106]) #determinant (en el script de natalia son los snps)

#Outcome (para natalia en su script es el volumen)

dparaules3<-names(breathe_wide[,7:10])
dnumeros3<-names(breathe_wide[,15:18])
hitrtse<- names(breathe_wide[,27:30])
include<-names(breathe_wide[,107:110]) #estos equivalen a los hit reactions con un valor de one > 90

final_outcomes<-names(breathe_wide[,c(7,8,9,10,15,16,17,18,107,108,109,110)])

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
    nvisit <- substr(final_outcomes[j], start, stop)#con esto consigo que me coja los dos ultimos digitos 
    ff <- paste(final_outcomes[j],"~",Scores[i],"+edat_",nvisit,"+sexe_total+meduc+PC1+PC2+PC3+PC4",sep="") #¿que variables faltan? 
    fit <- lme(fixed=as.formula(ff), random=~1|escola,data=withoutADHDchildren, method="ML",na.action=na.omit, control = lmeControl(opt="optim"))
    s = summary(fit)
    res = s$tTable
    R2 = r2beta(fit, partial=TRUE,method="sgv",data=withoutADHDchildren)
    
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
setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/ANALYSIS_DEF_27082019/ULTIMOS_ANALYSIS_120919")
write.table(coefs, "Sensitivity_analysis.txt",sep="\t")

#HEATMAP PLOTS
# PLOT CROSS-SECTIONAL RESULTS
#################################################################################################################
setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/ANALYSIS_DEF_27082019/ULTIMOS_ANALYSIS_120919")

all<- read.csv( "Sensitivity_analysis.txt", sep="")  #home

dim(all)  
all$sign = "" 
all$sign[all$P < 0.05] = "*" 

#Plot
library(ggplot2)
all$Outcome
all$Determinant
#all$Determinant<-factor(all$Determinant, levels = unique(all$Determinant))
all$Outcome<-factor(all$Outcome, levels=unique(all$Outcome))
all$Determinant<-factor(all$Determinant, levels=unique(all$Determinant))
plot<-ggplot() + geom_tile(data=all, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("Cognitive Measurements") + ylab("Cognitive Measurements") +  
  geom_text(data=all, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="Plot_crossectional_20190529PRS.pdf", plot=plot)

#WORKING MEMORY WORDS
paraules<-all[c(1:10),]
paraules$sign = "" 
paraules$sign[paraules$P < 0.05] = "*" 

paraules$Outcome<-factor(paraules$Outcome, levels=unique(paraules$Outcome))
paraules$Determinant<-factor(paraules$Determinant, levels=unique(paraules$Determinant))
plot<-ggplot() + geom_tile(data=paraules, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("") + ylab("Cognitive Measurements") +  
  geom_text(data=paraules, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="sensitivityanalysis_paraulesPRS.pdf", plot=plot)

#WORKING MEMORY NUMBERS
numeros<-all[c(41:50),]
numeros$sign = "" 
numeros$sign[numeros$P < 0.05] = "*"
numeros$Outcome<-factor(numeros$Outcome, levels=unique(numeros$Outcome))
numeros$Determinant<-factor(numeros$Determinant, levels=unique(numeros$Determinant))
plot<-ggplot() + geom_tile(data=numeros, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("") + ylab("Cognitive Measurements") +  
  geom_text(data=numeros, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="Plot_crossectional_10072019_numerosPRS.pdf", plot=plot)

#INNATTENTION

hitrt<-all[c(81:90),]

hitrt$sign = "" 
hitrt$sign[hitrt$P < 0.05] = "*"
hitrt$Outcome<-factor(hitrt$Outcome, levels=unique(hitrt$Outcome))
hitrt$Determinant<-factor(hitrt$Determinant, levels=unique(hitrt$Determinant))

plot<-ggplot() + geom_tile(data=hitrt, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("") + ylab("Cognitive Measurements") +  
  geom_text(data=hitrt, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="Plot_crossectional_10072019_hitrtPRS.pdf", plot=plot)


#COMBINANDO TODOS
visit1<-all[c(1:10,41:50,81:90),]
  
visit1$sign = "" 
visit1$sign[all$P < 0.05] = "*" 
visit1$Outcome<-factor(visit1$Outcome, levels=unique(visit1$Outcome))
visit1$Determinant<-factor(visit1$Determinant, levels=unique(visit1$Determinant))

plot<-ggplot() + geom_tile(data=visit1, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("Scores") + ylab("Cognitive Measurements") +  
  geom_text(data=visit1, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="Plot_crossectional_10072019_hitrtPRS.pdf", plot=plot)

#Desglosando los de la 1 visita

