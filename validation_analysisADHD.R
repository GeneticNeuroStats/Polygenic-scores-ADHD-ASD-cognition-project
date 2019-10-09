#ASSOCIATION WITH  behavioral measurements

library(foreign)
library(nlme)
library(psych)
library(r2glmm)
setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS")
dir()

breathe_wide<-read.dta("BREATHE_wide_TFM_Sofia_20190612.dta")

colnames(breathe_wide)

breathe_wide$ADHD
#Determinant
Scores<- names(breathe_wide[,97:106]) #determinant (en el script de natalia son los snps)
Scores_adhd<-Scores[1:5]
#Outcome (para natalia en su script es el volumen)

ADHD<-names(breathe_wide[,78])
inattention<-names(breathe_wide[,76])
hyperactivity<- names(breathe_wide[,77])


final_outcomes<-names(breathe_wide[,c(76,77,78)])


# Cross-sectional model

coefs=as.data.frame(matrix(NA,nrow=1,ncol=10))
colnames(coefs)=c("DateTime","Outcome","Determinant","Model","BetaNoStand", "SE", "P","R2","R2PRS","N")

count=1

for(j in 1:length(final_outcomes)) {
  print(final_outcomes[j])
  results=NULL
  for(i in 1:length(Scores_adhd)) {
    print(Scores[i])
    #hay problemas con la edat
    ff <- paste(final_outcomes[j],"~",Scores_adhd[i],"+edat_v1+sexe_total+meduc+PC1+PC2+PC3+PC4",sep="") #¿que variables faltan? 
    fit <- lme(fixed=as.formula(ff), random=~1|escola,data=breathe_wide, method="ML",na.action=na.omit, control = lmeControl(opt="optim"))
    s = summary(fit)
    res = s$tTable
    R2 = r2beta(fit, partial=TRUE,method="sgv",data=breathe_wide)
    
    
    #Save analysis details and coefficients
    coefs[count,1]=gsub(" ","_",Sys.time())
    coefs[count,2]=final_outcomes[j]
    coefs[count,3]=Scores_adhd[i]
    coefs[count,4]=ff
    coefs[count,5:7]=res[2,c(1,2,5)]
    coefs[count,8]= R2$Rsq[1]
    coefs[count,9]= R2$Rsq[3]
    coefs[count,10]=fit$dims$N[1]
    
    count=count+1
    
  }
}

?apply
coefs$R2perc<-coefs$R2PRS*100
View(coefs)
#coefs$Padj<-  p.adjust(coefs$P, method="fdr")

setwd("~/MASTER OMICS/Master's degree Final Proyect/BREATHE prs/ANALYSIS/ANALYSIS_DEF_27082019/ULTIMOS_ANALYSIS_120919")


write.table(coefs, "validationAnalysisADHD_120919.txt",sep="\t")


validation_analysis<-coefs
dim(validation_analysis)  
validation_analysis$sign = "" 
validation_analysis$sign[validation_analysis$P < 0.05] = "*" 

validation_analysis$Determinant<-c("ADHD PT 0.01","ADHD PT 0.05","ADHD PT 0.1","ADHD PT 0.5","ADHD PT 1")

#Plot
library(ggplot2)

#all$Determinant<-factor(all$Determinant, levels = unique(all$Determinant))
validation_analysis$Outcome<-factor(validation_analysis$Outcome, levels=unique(validation_analysis$Outcome))
validation_analysis$Determinant<-factor(validation_analysis$Determinant, levels=unique(validation_analysis$Determinant))

plot<-ggplot() + geom_tile(data=validation_analysis, aes(x=as.factor(Determinant), y=as.factor(Outcome), fill=BetaNoStand), colour="gray") + 
  scale_fill_gradient2(name="Standardized Coefficient", low="skyblue2", mid="white", high="firebrick3") + theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust=0.5)) + 
  xlab("Polygenic Risk Scores") + ylab("") +  
  geom_text(data=validation_analysis, aes(x=as.factor(Determinant), y=as.factor(Outcome), label=sign))
plot

ggsave(filename="Plot_crossectional_20190529PRS.pdf", plot=plot)

#PLOT VARIANCES
library(ggplot2)
df<-coefs

df$P<-round(df$P,3)
colnames(df)

ggplot(data=df,aes(x=Determinant,y=R2perc))+geom_bar(stat="identity",wigth=0.5,fill="steelblue")+geom_text(aes(label=P),vjust=-0.3,size=3.5)+theme_minimal()

inattention<-df[1:5,]
inattention$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")

PlotBAR<-ggplot(data=inattention,aes(x=Determinant,y=R2perc,color=R2perc))+geom_bar(stat="identity",wigth=0.5)+geom_text(aes(label=P),vjust=-0.3,size=3.5,color="black")+labs(x="Polygenic Risk score",y="R-squared (%)")+theme_minimal()
PlotBAR+scale_colour_gradient(low = "skyblue2",high = "firebrick3")

inattention$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")

write.table(inattention, "barplot_inattention.txt",sep="\t")
PlotBAR <- ggplot(inattention, aes(x = Determinant, y = R2perc, fill=R2perc)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="#FF8888",high="#FF0000")+
  geom_text(aes(label=paste("P-value: ", P, "\n", "R2: ", round(R2perc,2), "%", sep="")),vjust=-0.3,size=3.8,color="black")+labs(x="Inattention",y="")+theme_minimal() 
P1<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1)) + labs(fill = "R2%")

hyperactivity<-df[6:10,]
hyperactivity$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")
PlotBAR <- ggplot(hyperactivity, aes(x = Determinant, y = R2perc, fill=R2perc)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="#FF8888",high="#FF0000")+
  geom_text(aes(label=paste("P-value: ", P, "\n", "R2: ", round(R2perc,2), "%", sep="")),vjust=-0.3,size=3.8,color="black")+labs(x="Hyperactivity",y="")+theme_minimal() 
P2<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1)) + labs(fill = "R2%")

ADHD<-df[11:15,]
ADHD$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")
PlotBAR <- ggplot(ADHD, aes(x = Determinant, y = R2perc, fill=R2perc)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient(low="#FF8888",high="#FF0000")+
  geom_text(aes(label=paste("P-value: ", P, "\n", "R2: ", round(R2perc,2), "%", sep="")),vjust=-0.3,size=3.8,color="black")+labs(x="ADHD symptoms",y="")+theme_minimal() 
P3<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1)) + labs(fill = "R2%")
library(gridExtra)
grid.arrange(P3,P2,P1,ncol=1)

install.packages("gridExtra")
library(gridExtra)
library(grid)
?grid.arrange

inattention<-df[1:5,]
inattention$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")

P1 <- ggplot(inattention, aes(x = Determinant, y = R2perc)) + 
  geom_bar(stat = "identity",fill="#FF8888")+geom_text(aes(label=paste("P-value:", P, sep="")),vjust=-0.3,size=4.8,color="black")+labs(x="Inattention",y="R-square (%)")+theme_minimal()+theme(text = element_text(size = 15)) + ylim(c(0, 1)) 

#P1<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1))

hyperactivity<-df[6:10,]
hyperactivity$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")
P2 <- ggplot(hyperactivity, aes(x = Determinant, y = R2perc)) + 
  geom_bar(stat = "identity",fill="#FF8888")+geom_text(aes(label=paste("P-value:", P, sep="")),vjust=-0.3,size=4.8,color="black")+labs(x="Hyperactivity",y="R-square (%)")+theme_minimal()+theme(text = element_text(size = 15)) + ylim(c(0, 1)) 
# P2 <- ggplot(hyperactivity, aes(x = Determinant, y = R2perc, fill=P)) + 
#   geom_bar(stat = "identity")+
#   scale_fill_gradient(low="#FF8888",high="#FF0000")+
#   geom_text(aes(label=paste("P-value:", P, sep="")),vjust=-0.3,size=3.8,color="black")+labs(x="Hyperactivity",y="R-square (%)")+theme_minimal() 
# P2<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1)) + labs(fill = "P-value")

ADHD<-df[11:15,]
ADHD$Determinant<-c("PRS-0.01","PRS-0.05","PRS-0.1","PRS-0.5","PRS-1")
P3 <- ggplot(ADHD, aes(x = Determinant, y = R2perc)) + 
  geom_bar(stat = "identity",fill="#FF8888")+geom_text(aes(label=paste("P-value:", P, sep="")),vjust=-0.3,size=4.8,color="black")+labs(x="ADHD symptoms",y="R-square (%)")+theme_minimal()+theme(text = element_text(size = 15)) + ylim(c(0, 1))  
# PlotBAR <- ggplot(ADHD, aes(x = Determinant, y = R2perc, fill=P)) + 
#   geom_bar(stat = "identity")+
#   scale_fill_gradient(low="#FF8888",high="#FF0000")+
#   geom_text(aes(label=paste("P-value:", P, sep="")),vjust=-0.3,size=3.8,color="black")+labs(x="ADHD symptoms",y="R-square (%)")+theme_minimal() 
# P3<-PlotBAR + theme(text = element_text(size = 13)) + ylim(c(0, 1)) + labs(fill = "P-value")
library(gridExtra)
grid.arrange(P3,P2,P1,ncol=1)
