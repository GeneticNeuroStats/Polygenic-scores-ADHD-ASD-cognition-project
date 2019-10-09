#Correlation Plots
#Cognition measures correlation

#Load packages
library(corrplot)

final_outcomes<-breathe_wide[,c(7,8,9,10,15,16,17,18,107,108,109,110)]
names(final_outcomes)
dim(final_outcomes)
outcomes_withoutNA<-na.omit(final_outcomes)
dim(outcomes_withoutNA) #1937
names(outcomes_withoutNA)
names(outcomes_withoutNA) = c("Working memory(words) V1","Working memory(words) V2","Working memory(words) V3","Working memory(words) V4","Working memory(numbers) V1",  "Working memory(numbers) V2","Working memory(numbers) V3","Working memory(numbers) V4","inattentiveness(HRT-SE) V1",                         "inattentiveness(HRT-SE) V2","inattentiveness(HRT-SE) V3","inattentiveness(HRT-SE) V4")
colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7", #FFFFFF", "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))

int_color<-colorRampPalette(c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#FFFFFF","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
M<-cor(outcomes_withoutNA)
plot1<-cor.mtest(outcomes_withoutNA,conf.level=0.95)
table(plot1$p>0.05)
corrplot(M,method = "color",tl.cex =0.6 ,tl.col = "black",col=int_color(50), p.mat = plot1$p, sig.level = 0.05,insig = "blank")

#Cognitive measures + behavioural measures correlation

#Load packages
library(corrplot)

names(breathe_wide)
data_plot2<-breathe_wide[,c(7,8,9,10,15,16,17,18,107,108,109,110,76,77,78)]
names(data_plot2)
dim(data_plot2)
data2_withoutNA<-na.omit(data_plot2)
dim(data2_withoutNA) 
names(data2_withoutNA)<-c("Working memory(words) V1","Working memory(words) V2","Working memory(words) V3","Working memory(words) V4","Working memory(numbers) V1",
                          "Working memory(numbers) V2","Working memory(numbers) V3","Working memory(numbers) V4","inattentiveness(HRT-SE) V1",
                          "inattentiveness(HRT-SE) V2","inattentiveness(HRT-SE) V3","inattentiveness(HRT-SE) V4","Inattention","Hyperactivity","ADHD")
M<-cor(data2_withoutNA)
plot2<-cor.mtest(data2_withoutNA,conf.level=0.95)
table(plot2$p>0.05)
corrplot(M,method = "color",tl.cex =0.6 ,tl.col = "black",col=int_color(50), p.mat = plot2$p, sig.level = 0.05,insig = "blank")
Polygenic Risk Scores correlation

#Load packages
library(corrplot)

scores<-breathe_wide[,c(97:106)]
scores<-breathe_wide[,c(97:106)]
scores_withoutNA<-na.omit(scores)

names(scores_withoutNA)<-c("PRS-ADHD 0.01","PRS-ADHD 0.05","PRS-ADHD 0.1","PRS-ADHD 0.5","PRS-ADHD 1","PRS-ASD 0.01","PRS-ASD 0.05",
                           "PRS-ASD 0.1","PRS-ASD 0.5","PRS-ASD 1")
M3<-cor(scores_withoutNA)
plot3<-cor.mtest(scores_withoutNA,conf.level=0.95)
table(plot3$p>0.05)
corrplot(M3,method = "color",tl.cex =0.6 ,tl.col = "black",col=int_color(50), p.mat = plot3$p, sig.level = 0.05,insig = "blank")
