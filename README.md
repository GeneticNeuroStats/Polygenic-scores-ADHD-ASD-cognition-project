# Supplemental Scripts. 
## Publication: Polygenic Risk Score for Child-Onset Psychiatric Disorders is associated with cognitive abilities during childhood.
### Status: In preparation
### Maintainers:  Sofia Aguilar-Lacasaña <sofia.aguilar@isglobal.org>, Natalia Vilor-Tejedor <natalia.vilortejedor@crg.eu>

## Objective

I am going to calculate Polygenic Risk Scores of ASD and ADHD.  (Incluir objetivo del paper).

## File's Description

(Describir los ficheros incluidos en el repositorio). Que contienen, para que sirven.




# POLYGENIC RISK SCORES: ADHD (Esta parte hacia abajo debe ir en otro file y incluirla dentro del repositorio)

R --file=PRSice_v1.25.R -q --args \
plink ./plink_1.9_linux_160914 \
base adhd_eur_jun2017 \
target BREATHE_QC_FINAL \
pheno.file phenotype.txt \
covary T \
ancestry.dim PCA \
fastscore T \
barchart.levels 0.01,0.05,0.1,0.5,1 \
report.individual.scores T \
ggfig F \
cleanup F


# Arguments: 

 ##Base: GWAS results file from the meta-analysis of ADHD by the Psychiatric Genomics Consortium (PGC) and the Lundbeck Foundation Initiative Psychiatric Research (iPSYCH) released in June 2017.  20183 ADHD cases and 35191 controls where collected from 12 cohorts. Genome version: hg19.We use the  European ancestry meta-analysis (19,099 cases, 34,194 controls). 
 
Subjects and SNPs were included in the analyses based on the following quality control parameters:
- SNP call rate > 0.95 (before sample removal)
- subject call rate > 0.98 (> 0.95 for the iPSYCH samples)
- autosomal heterozygosity deviation (| Fhet | < 0.2)
- SNP call rate > 0.98 (after sample removal)
- difference in SNP missingness between cases and controls < 0.02 
- SNP Hardy-Weinberg equilibrium (HWE) (P > 10−6 in controls or P > 10−10 in cases).
- Related individuals were removed
- Genetic outliers were excluded based on principal component analysis. 
- Non-genotyped markers were imputed using the 1000 Genomes Project Phase 3 reference panel. For trio cohorts, pseudocontrols
were defined from phased haplotypes prior to imputation.
- GWAS was conducted in each cohort using logistic regression with the imputed additive genotype dosages.
- Principal components were included as covariates to correct for population stratification, along with relevant study-specific covariates where applicable, and variants with imputation INFO score < 0.8 or minor allele frequency (MAF) < 0.01 were excluded. 
- The GWAS were then metaanalyzed using an inverse-variance weighted fixed effects model. 
- Association results were considered only for variants with an effective sample size greater than 70% of the full metaanalysis, leaving 8,047,421 variants in the final meta-analysis.


In this file, A1 corresponds to reference allele (may or may not be the minor allele) and A2 corresponds to alternative allele (tengo que darles la vuelta!!!!).  This file is formed by different columns:

- CHR Chromosome (hg19) where the SNP is found
- SNP Marker name
- BP Base pair location (hg19)
- A1 Reference allele for OR (may or may not be minor allele)
- A2 Alternative allele
- INFO Imputation information score
- OR Odds ratio for the effect of the A1 allele (binary phenotype)
- SE Standard error of the log(OR)
- P P-value for association test in the meta-analysis


##Target: The target data set is supplied in PLINK binary format , with the extensions .bed, .bim, .fam - where .bed contains compressed genotype data. Missing phenotype data can be coded as NA, or -9 for binary traits. BREATHE_QC_FINAL is the name of the file. 
There are 1667 children after the QC taking into account: 

- the minimum call rate
- a maximum of 4 SD heterozigosity
- sample relaeness excluding proportions of identity by-state above 0.185 
- gender disordance excluding mismatch infomration
- population stratification...

##pheno.file: Location of the file containing external phenotype data, if it is not coded in the genotype file. This file must have two columns, individual ID and phenotype, with no header line. Missing data is coded NA, and NA and -9 for binary traits. If NA, phenotypes will be extracted from the genotype data. Default value is NA

##covary: T. covariates are used when testing model fit of polygenic score on phenotype. 

##ancestry.dim PCA. We specify the method that we want. 

##clump.snps: With this argument, base SNPs will be clumped to remove linkage desequilibrium. In our case, we do not add this argument, so is calculated by default T.  

##fastscore: if this argument is true (T), scores will only be calculated at the thresholds specified by barchart.levels.

##barchart.levels: Thresholds which should be plotted on the bar chart. 0.01,0.05,0.1,0.5,1

##report.individual.scores: if this argument is true (T), a file will be produced containing every individual's polygenic risk score at every threshold (or just the most predictive threshold and written to the working directory.

##ggfig: If T, ggplot2 will be used to generate figures. If F, base graphics will be used to generate figures.We select F. 

##cleanup: if this argument is False(F), all temporary files will not be removed at the end of the analysis. We select F. 

##plink: the location and name of the binary file for executing plnk. ./plink_1.9_linux_160914 \


# POLYGENIC RISK SCORES: AUTISM

R -q --file=./PRSice_v1.25.R --args \
base autism_GWAS_def \
target BREATHE_QC_FINAL \
slower 0 \
supper 0.5 \
sinc 0.01 \
covary F \
clump.snps F \
debug.mode T \
barchart.levels 0.01,0.05,0.1,0.5,1 \
fastscore T \
report.individual.scores T \
report.best.scores.only F \
no.regression T \
cleanup F \
plink ./plink_1.9_linux_160914 \

#PRSice_v1.25.R : R file where is the script that is used to calculate Polygenic Risk Scores

# Arguments: 

 ##Base: GWAS results file from the meta-analysis of ASD by the Psychiatric Genomics Consortium (PGC) and the Lundbeck Foundation Initiative Psychiatric Research (iPSYCH) released in November 2017.  Samples of European acestry (18,382 cases, 27,969 controls. Genome version: hg19.

QC???


In this file, A1 corresponds to reference allele (may or may not be the minor allele) and A2 corresponds to alternative allele (tengo que darles la vuelta!!!!).  This file is formed by different columns:

- CHR Chromosome (hg19)
- SNP Marker name
- BP Base pair location (hg19)
- A1 Reference allele for OR (may or may not be minor allele)
- A2 Alternative allele
- INFO Imputation information score
- OR Odds ratio for the effect of the A1 allele
- SE Standard error of the log(OR)
- P P-value for association test in the meta-analysis


##Target: The target data set is supplied in PLINK binary format , with the extensions .bed, .bim, .fam - where .bed contains compressed genotype data. Missing phenotype data can be coded as NA, or -9 for binary traits. BREATHE_QC_FINAL is the name of the file. 
There are 1667 children after the QC taking into account: 

- the minimum call rate
- a maximum of 4 SD heterozigosity
- sample relaeness excluding proportions of identity by-state above 0.185 
- gender disordance excluding mismatch infomration
- population stratification...

##pheno.file: Location of the file containing external phenotype data, if it is not coded in the genotype file. This file must have two columns, individual ID and phenotype, with no header line. Missing data is coded NA, and NA and -9 for binary traits. If NA, phenotypes will be extracted from the genotype data. Default value is NA

##covary: T. covariates are used when testing model fit of polygenic score on phenotype. 

##ancestry.dim PCA. We specify the method that we want. 

##clump.snps: With this argument, base SNPs will be clumped to remove linkage desequilibrium. In our case, we do not add this argument, so is calculated by default T.  

##fastscore: if this argument is true (T), scores will only be calculated at the thresholds specified by barchart.levels.

##barchart.levels: Thresholds which should be plotted on the bar chart. 0.01,0.05,0.1,0.5,1

##report.individual.scores: if this argument is true (T), a file will be produced containing every individual's polygenic risk score at every threshold (or just the most predictive threshold and written to the working directory.

##ggfig: If T, ggplot2 will be used to generate figures. If F, base graphics will be used to generate figures.We select F. 

##cleanup: if this argument is False(F), all temporary files will not be removed at the end of the analysis. We select F. 

##plink: the location and name of the binary file for executing plnk. ./plink_1.9_linux_160914 \
