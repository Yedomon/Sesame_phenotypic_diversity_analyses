##--Script for Core collection inference
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | Department of Genomics |RDA | Republic of South Korea



## Installation
install.packages("rJava")
install.packages("corehunter")

## Loading
library(rJava)
library(corehunter)


## Import the data
data_pheno = read.csv("corehunter_datapheno.csv", h = T, sep = ",")

## convert into corehunter class

phenot <- phenotypes(data_pheno)


## Infer the core collection

core <- sampleCore(phenot)

## Results

core


### core collection evaluation

data_pheno_core_collection = read.csv("core_collection_quanti.csv", h = T, sep = ",")
data_pheno_whole_collection = read.csv("whole_collection_quanti.csv", h = T, sep = ",")
View(data_pheno_core_collection)
View(data_pheno_whole_collection)


### Install pastecs package 

install.packages("pastecs")


### Load pastecs


library(pastecs)


### Stats for core collection

stat.desc(data_pheno_core_collection[2:17])
stat.desc(data_pheno_whole_collection[2:17])


### test difference between whole and core collection for each trait

### First let's check the data normality 


### Data importation

data_whole_core = read.csv("data_anova_t_test_wilcoxon_others.csv", h = T, sep = ",")

View(data_whole_core)


### Check the normality test quickly with the pastecs package

library(pastecs)

res.norm = stat.desc(data_whole_core[1:16], norm = TRUE, basic=FALSE, desc=FALSE)

write.csv(res.norm, "res.normality.csv")


### Try with dlookr

library(dlookr)


normality(data_whole_core[1:16])

## We've got the same results. Now let's plot the distribution

plot_normality(data_whole_core[1:16])


## Only PAL are normally distributed. So, let's run the Student's paired t-test.



library(dplyr) # To select what I want

attach(data_whole_core)
PAL_whole
PAL_whole = data_whole_core %>%
  select(PAL) %>%
  filter(Type == "Whole")


PAL_core = data_whole_core %>%
  select(PAL) %>%
  filter(Type == "Core")

### Run the t test
t.test(PAL_whole$PAL, PAL_core$PAL)



## Let' run the Wilcoxon signed-rank test for none normally distributed data

## CNU

CNU_whole = data_whole_core %>%
  select(CNU) %>%
  filter(Type == "Whole")


CNU_core = data_whole_core %>%
  select(CNU) %>%
  filter(Type == "Core")



wilcox.test(CNU_core$CNU, CNU_whole$CNU)


# PHE 


PHE_whole = data_whole_core %>%
  select(PHE) %>%
  filter(Type == "Whole")


PHE_core = data_whole_core %>%
  select(PHE) %>%
  filter(Type == "Core")



wilcox.test(PHE_core$PHE,PHE_whole$PHE)





# BNU case

BNU_whole = data_whole_core %>%
  select(BNU) %>%
  filter(Type == "Whole")


BNU_core = data_whole_core %>%
  select(BNU) %>%
  filter(Type == "Core")



wilcox.test(BNU_core$BNU,BNU_whole$BNU)



# DIA


DIA_whole = data_whole_core %>%
  select(DIA) %>%
  filter(Type == "Whole")


DIA_core = data_whole_core %>%
  select(DIA) %>%
  filter(Type == "Core")



wilcox.test(DIA_core$DIA,DIA_whole$DIA)




# DBI



DBI_whole = data_whole_core %>%
  select(DBI) %>%
  filter(Type == "Whole")


DBI_core = data_whole_core %>%
  select(DBI) %>%
  filter(Type == "Core")



wilcox.test(DBI_core$DBI,DBI_whole$DBI)






# DSW



DSW_whole = data_whole_core %>%
  select(DSW) %>%
  filter(Type == "Whole")


DSW_core = data_whole_core %>%
  select(DSW) %>%
  filter(Type == "Core")



wilcox.test(DSW_core$DSW,DSW_whole$DSW)



# TSW



TSW_whole = data_whole_core %>%
  select(TSW) %>%
  filter(Type == "Whole")


TSW_core = data_whole_core %>%
  select(TSW) %>%
  filter(Type == "Core")



wilcox.test(TSW_core$TSW,TSW_whole$TSW)


# FLO



FLO_whole = data_whole_core %>%
  select(FLO) %>%
  filter(Type == "Whole")


FLO_core = data_whole_core %>%
  select(FLO) %>%
  filter(Type == "Core")



wilcox.test(FLO_core$FLO,FLO_whole$FLO)


# MAT



MAT_whole = data_whole_core %>%
  select(MAT) %>%
  filter(Type == "Whole")


MAT_core = data_whole_core %>%
  select(MAT) %>%
  filter(Type == "Core")



wilcox.test(MAT_core$MAT,MAT_whole$MAT)


# FTM



FTM_whole = data_whole_core %>%
  select(FTM) %>%
  filter(Type == "Whole")


FTM_core = data_whole_core %>%
  select(FTM) %>%
  filter(Type == "Core")



wilcox.test(FTM_core$FTM,FTM_whole$FTM)


# CLE


CLE_whole = data_whole_core %>%
  select(CLE) %>%
  filter(Type == "Whole")


CLE_core = data_whole_core %>%
  select(CLE) %>%
  filter(Type == "Core")



wilcox.test(CLE_core$CLE,CLE_whole$CLE)





# CWI 



CWI_whole = data_whole_core %>%
  select(CWI) %>%
  filter(Type == "Whole")


CWI_core = data_whole_core %>%
  select(CWI) %>%
  filter(Type == "Core")



wilcox.test(CWI_core$CWI,CWI_whole$CWI)




# HIN

attach(data_whole_core)
glm.HIN <- glm(HIN ~ Type , family = "poisson")

anova(glm.HIN, test = "Chisq")


# CNLA

glm.CNLA <- glm(CNLA ~ Type, family = poisson())

anova(glm.CNLA, test = "Chisq")


#NLC


glm.NLC <- glm(NLC ~ Type, family = poisson())

anova(glm.NLC, test = "Chisq")
