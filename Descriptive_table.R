Descriptive Table

#Load packages
library(foreign)
library(psych)
library(nlme)
library(nnet)
library(lme4)
library(ggplot2)
library(reshape2)
library(dplyr)
library(finalfit)
library(table1)
library(boot)

breathe2 <- read.dta("BREATHE_wide_TFM_Sofia_20190612.dta")
str(breathe2)
breathe2[43:58] <- lapply(breathe2[43:58], as.numeric)
breathe2[69:74] <- lapply(breathe2[69:74], as.numeric)
breathe2[72:78] <- lapply(breathe2[72:78], as.numeric)
breathe2[80:81] <- lapply(breathe2[80:81], as.numeric)
breathe2$gwas_incl<-as.factor(breathe2$gwas_incl)
str(breathe2)

#Keep only relevant vars for description
names(breathe2)
breathe3<-breathe2[,c(3:64,67:83,86:106)]
names(breathe3)

################################################################
# Table 1: Included vs. excluded in analyses (based on GWAS data #available)
################################################################

explanatory <- c("sexe_total","edat_v1","meduc","dparaules3_v1","dnumeros3_v1","hitrtse_v1","Inattention","Hyperactivity","ADHD") 
dependent <- "gwas_incl" # outcome
breathe2 %>%
summary_factorlist(dependent, explanatory,p=TRUE,colum=TRUE, cont="mean",
                     add_dependent_label=TRUE,na_include =TRUE,total_col=TRUE)-> table1

View(breathe3)
write.table(table1, file=paste0(cohort,"_table_descriptives_by_GWAS_available_", format(Sys.Date(), "%Y%m%d"),".txt"), 
            col.names=T, row.names=F, quote=F, sep="\t") 

################################################################

breathe_D<-breathe3

breathe_D$meduc<-factor(breathe_D$meduc,levels=c("No sap llegir ni escriure","Sense estudis o primaris incomplerts", "Estudis primaris (EGB, primària, ESO)","Estudis secundaris (BUP, COU, FP, Batxillerat)","Estudis universitaris"), labels=c("Does not know to read nor to write","Without or incomplete studies","Primary","Secondary","University"))
levels(breathe_D$sexe_total) 

label(breathe_D$dparaules3_v1)<- "Working memory (words)"
label(breathe_D$dnumeros3_v1)<-"Working memory (numbers)"
label(breathe_D$hitrtse_v1)<-"Hit Reaction Time"
# label(breathe_D$alerting_v1)<-"Alerting"
# label(breathe_D$orienting_v1)<-"Orienting"
# label(breathe_D$conflict_v1)<-"Conflict"
label(breathe_D$Inattention)<-"Inattention"
label(breathe_D$Hyperactivity)<-"Hyperactivity"
label(breathe_D$edat_v1)<-"Age"
units(breathe_D$edat_v1)<-"years"
label(breathe_D$sexe_total)<-"Sexe"
units(breathe_D$sexe_total)<-"Sex"
label(breathe_D$meduc)<-"Maternal education"
# label(breathe_D$x94_isvur_)<-"Socioeconomic status"
names(breathe_D)

breathe_D$gwas_incl<-factor(breathe_D$gwas_incl,levels=c("0","1"),labels=c("excluded (0)","included (1)"))
#breathe_D$gwas_incl<-factor(breathe_D$gwas_incl,levels=1:3, labels=c("excluded (0)","included (1)","p-value"))

# P-values columns

levels(breathe_D$gwas_incl)
rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- breathe_D[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ breathe_D$gwas_incl)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(breathe_D$gwas_incl)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}
rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}

descriptive_table<-table1(~ sexe_total+edat_v1+meduc+x94_isvur_+dparaules3_v1+dnumeros3_v1+hitrtse_v1+alerting_v1+orienting_v1+conflict_v1+Inattention+Hyperactivity+ADHD| gwas_incl, data=breathe_D, overall="Total",topclass="Rtable1-zebra")

write.table(descriptive_table,file="descriptive_table_08082019.txt", col.names=T, row.names=F, quote=F, sep="\t")
 
