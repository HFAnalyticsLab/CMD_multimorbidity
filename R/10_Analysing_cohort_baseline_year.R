# Describes size of the cohort and their characteristics for the baseline year (2015/2016) for the sample WITH linked HES data
# Prepared by Mai Stafford
# Created 21st Feb 2020
# Last edit date: 21st Feb 2020


# Load libraries ----------------------------------------------------------

## @knitr libraries

library(plyr) #load before tidyverse
library(tidyverse)
library(here)
library(data.table)
library(table1)
library(haven)
library(margins)

source('filepaths.R') #get folder path for a previous project: HCUcomorbid
#HCUcomorbid: Comorbidities of high cost users of the NHS


#Read  data into R and select appropriate year
cprd2015 <- readRDS(here("Analysis","Processed_data","CPRDoutcomes2015_2016.rds"))
cprd2015 <- cprd2015 %>% 
  filter(incohort == T)

# merge BMI and smoking
BMI <- readRDS(here("Analysis","Processed_data","BMI2015_2016.rds"))
keepvars <- c("patid", "smoking_status")
smoking <- readRDS(here("Analysis","Processed_data","smoking2015_2016.rds"))[keepvars]
cprd2015 <- left_join(cprd2015, BMI, by="patid")
cprd2015 <- left_join(cprd2015, smoking, by="patid")

#Data derivations
cprd2015 <- cprd2015 %>% 
  mutate(gender = factor(gender)) %>% 
  mutate(age = 2016 - yob) %>%
  mutate(imd2015g = factor(imd, labels=c("Q1 least dep", "Q2", "Q3", "Q4", "Q5"))) %>% 
  mutate(numconds=select(.,c(ALC:ANO, AST:DEM, DIB:THY)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(numMHconds=select(.,c(ALC, ANO, DEM, LEA, PSM)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))


cprd2015$agegroup <- cut(cprd2015$age, breaks=c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
                            labels=c("18-24y", "25-34y", "35-44y", "45-54y", "55-64y", "65-74y", "75+y"))  
cprd2015$agegroup3 <- cut(cprd2015$age, breaks=c(-Inf, 44, 64, Inf),
                             labels=c("18-44y", "45-64y", "65+y"))  
cprd2015$condsgroup <- cut(cprd2015$numconds, breaks=c(-Inf, 0, 1, 2, Inf),
                              labels=c("CMD only", "CMD+1", "CMD+2", "CMD+3+"))  
cprd2015$gender <- ordered(cprd2015$gender, levels=c(1,2), labels=c("Men", "Women"))



# demo descriptives -------------------------------------------------------

## @knitr demo descriptives

# Edit Table 1 to remove max and min
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}


#Demographics tab in excel
#Scale of the issue
#How many people with CMD have MM?

table1(~factor(gender) + factor(agegroup) + factor(imd2015g) + factor(ethnicity) + factor(condsgroup), data=cprd2015) 
table1(~ factor(condsgroup) | gender*agegroup3, data=cprd2015) 


# compare with random sample (from previous project ISAC: 17_150 - Comorbidities of high cost users of the NHS) -----

## @knitr compare with random sample

randomsample <- read_sas(paste0(HCUcomorbid, 'ltsuse.sas7bdat')) #load SAS data from previous project
randomadults <- subset(randomsample, startage>17)
randomadults <- randomadults %>% 
  mutate(gender = factor(gender)) %>%
  mutate(numMHconds=select(.,c(depanx, ALC, ANO, DEM, LEA, PSO)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))

randomadults$gender <- ordered(randomadults$gender, levels=c(1,2), labels=c("Men", "Women"))

randomadults$agegroup <- cut(randomadults$startage, breaks=c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
                             labels=c("18-24y", "25-34y", "35-44y", "45-54y", "55-64y", "65-74y", "75+y"))  
randomadults$agegroup3 <- cut(randomadults$startage, breaks=c(-Inf, 44, 64, Inf),
                          labels=c("18-44y", "45-64y", "65+y"))  
randomadults$condsgroup <- cut(randomadults$total, breaks=c(-Inf, 1, 2, 3, Inf),
                               labels=c("0 conditions", "1 condition", "2+ conditions", "3+ conditions"))  

table1(~factor(gender) + factor(agegroup) + factor(condsgroup), data=randomadults)
table1(~ factor(condsgroup) | gender*agegroup3, data=randomadults)



# conditions by demo -------------------------------------------

## @knitr conditions by demo


# types of condition -------------------------------------------

## @knitr types of condition

# Prevalence of each of the 36 conditions in the CMD sample
table1(~factor(ALC) + factor(ANO) + factor(AST) + factor(ATR) + factor(BLI) + factor(BRO) + factor(CAN) + factor(CHD) + factor(CKD) + factor(CLD)
 +factor(CON) + factor(COP) + factor(DEM) + factor(DIB) + factor(DIV) + factor(EPI) + factor(HEF) + factor(HEL) + factor(HYP) + factor(IBD) + factor(IBS)
 +factor(LEA) + factor(MIG) + factor(MSC) + factor(PEP) + factor(PNC) + factor(PRK) + factor(PRO) + factor(PSM) + factor(PSO) + factor(PVD)
 +factor(RHE) + factor(SCZ) + factor(SIN) + factor(STR) + factor(THY) + factor(cardiovasc),  data=cprd2015)

# identify top 10 most prevalent
condits <- select(cprd2015, c(ALC:THY, cardiovasc))
top10_overall <- condits %>% 
  summarize_all(list(mean))
ranktop10 <- rank(top10_overall)


# check same conditions most prevalent by gender
table1(~factor(ALC) + factor(ANO) + factor(AST) + factor(ATR) + factor(BLI) + factor(BRO) + factor(CAN) + factor(CHD) + factor(CKD) + factor(CLD)
       +factor(CON) + factor(COP) + factor(DEM) + factor(DIB) + factor(DIV) + factor(EPI) + factor(HEF) + factor(HEL) + factor(HYP) + factor(IBD) + factor(IBS)
       +factor(LEA) + factor(MIG) + factor(MSC) + factor(PEP) + factor(PNC) + factor(PRK) + factor(PRO) + factor(PSM) + factor(PSO) + factor(PVD)
       +factor(RHE) + factor(SCZ) + factor(SIN) + factor(STR) + factor(THY) + factor(cardiovasc) | gender, data=cprd2015)

# check same conditions most prevalent by agegroup
table1(~factor(ALC) + factor(ANO) + factor(AST) + factor(ATR) + factor(BLI) + factor(BRO) + factor(CAN) + factor(CHD) + factor(CKD) + factor(CLD)
       +factor(CON) + factor(COP) + factor(DEM) + factor(DIB) + factor(DIV) + factor(EPI) + factor(HEF) + factor(HEL) + factor(HYP) + factor(IBD) + factor(IBS)
       +factor(LEA) + factor(MIG) + factor(MSC) + factor(PEP) + factor(PNC) + factor(PRK) + factor(PRO) + factor(PSM) + factor(PSO) + factor(PVD)
       +factor(RHE) + factor(SCZ) + factor(SIN) + factor(STR) + factor(THY) + factor(cardiovasc) | agegroup3, data=cprd2015)

# types of condition in random sample -------------------------------------------

## @knitr types of condition in random sample

# Comparison with random sample
table1(~factor(ALC) + factor(ANO) + factor(AST) + factor(ATR) + factor(BLI) + factor(BRO) + factor(CAN) + factor(CHD) + factor(CKD) + factor(CLD)
 +factor(CON) + factor(COP) + factor(DEM) + factor(DIB) + factor(DIV) + factor(EPI) + factor(HEF) + factor(HEL) + factor(HYP) + factor(IBD) + factor(IBS)
 +factor(LEA) + factor(MIG) + factor(MSC) + factor(PNC) + factor(PRK) + factor(PRO) + factor(OPS) + factor(PSO) + factor(PVD)
 +factor(RHE) + factor(SCZ) + factor(SIN) + factor(STR) + factor(THY) + factor(cardiovasc),  data=randomadults)



# Mean number of additional conditions by age and sex - CMD cohort
numconds_by_sexage <- cprd2015 %>% 
  group_by(gender, agegroup) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_sexage, here("Analysis","Analysis_Results","numconds_by_sexage.csv"))
mylinreg <- lm(numconds ~ gender*age, data=cprd2015) # test for signif gender difference in assoc between numconds and age
summary(mylinreg)


# check if this is driven by prostate conditions in men
cprd2015 <- cprd2015 %>% 
  mutate(numcondsnopro=select(.,c(ALC:ANO, AST:DEM, DIB:PRK, PSM:THY)) %>% rowSums(na.rm=TRUE))
numcondsnopro_by_sexage <- cprd2015 %>% 
  group_by(gender, agegroup) %>% 
  summarize(N=length(numcondsnopro), mean_numconds=mean(numcondsnopro), se_numconds=sd(numcondsnopro)/sqrt(length(numcondsnopro)))
write_csv(numcondsnopro_by_sexage, here("Analysis","Analysis_Results","numcondsnopro_by_sexage.csv"))

# Which additional conditions contribute to age and sex pattern in total number of conditions
# check those in the top 10 conditions
table1(~factor(ALC) + factor(AST) + factor(CKD) +factor(CON) + factor(DIB) + factor(HEL) + factor(HYP) + factor(IBS) | gender*agegroup, data=cprd2015)
table1(~factor(MIG) + factor(PNC) + factor(PSM) + factor(SIN) + factor(THY) + factor(cardiovasc) | gender*agegroup, data=cprd2015)


# Mean number of additional conditions by age and sex - random sample
random_numconds_by_sexage <- randomadults %>% 
  group_by(gender, agegroup) %>% 
  summarize(N=length(total), mean_numconds=mean(total), se_numconds=sd(total)/sqrt(length(total)))
write_csv(random_numconds_by_sexage, here("Analysis","Analysis_Results","random_numconds_by_sexage.csv"))
mylinreg <- lm(total ~ gender*startage, data=randomadults) # test for signif gender difference in assoc between numconds and age
summary(mylinreg)

# _________________________________________________________________
# IMD - CMD cohort with linked data
table1(~ factor(condsgroup) | agegroup3*imd2015g, data=cprd2015linked) 
table1(~ factor(condsgroup) | gender*imd2015g, data=cprd2015linked)
mylinreg <- lm(numconds ~ agegroup3*imd, data=cprd2015linked) # test SE gradient within each age band
summary(mylinreg)


# Mean number of additional conditions by IMD 
cprd2015linked <- cprd2015 %>%
  filter(hes_e == 1 & death_e == 1 & lsoa_e == 1)
numconds_by_sexIMD <- cprd2015linked %>% 
  group_by(gender, imd2015g) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_sexIMD, here("Analysis","Analysis_Results","numconds_by_IMD.csv"))

cprd2015imdyoung <- filter(cprd2015linked, agegroup3=="18-44y" & (imd2015g=="Q1 least dep" | imd2015g=="Q5"))
cprd2015imdmid <- filter(cprd2015linked, agegroup3=="45-64y" & (imd2015g=="Q1 least dep" | imd2015g=="Q5"))
cprd2015imdold <- filter(cprd2015linked, agegroup3=="65+y" & (imd2015g=="Q1 least dep" | imd2015g=="Q5"))


table1(~factor(PNC) + factor(IBS) + factor(AST) +factor(HEL) + factor(PSM) + factor(ALC) + factor(THY) + factor(SIN) + factor(PSO) + factor(MIG) | imd2015g, data=cprd2015imdyoung)
table1(~factor(PNC) + factor(HYP) + factor(IBS) +factor(AST) + factor(DIB) + factor(HEL) + factor(THY) + factor(cardiovasc) + factor(ALC) + factor(CKD) | imd2015g, data=cprd2015imdmid)
table1(~factor(HYP) + factor(PNC) + factor(cardiovasc) +factor(CKD) + factor(HEL) + factor(DIB) + factor(DIV) + factor(CON) + factor(IBS) + factor(AST) | imd2015g, data=cprd2015imdold)


numconds_by_ageIMD <- cprd2015linked %>% 
  group_by(agegroup, imd2015g) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_ageIMD, here("Analysis","Analysis_Results","numconds_by_ageIMD.csv"))

fit <- lm(numconds ~ agegroup + imd2015g, data=cprd2015linked)
summary(fit)

# Mean number of additional conditions by ethnicity - CMD cohort with linked data
numconds_by_ageethnic <- cprd2015linked %>% 
  group_by(ethnic, agegroup) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_ageethnic, here("Analysis","Analysis_Results","numconds_by_ageethnic.csv"))



# Conditions by IMD -------------------------------------------

## @knitr conditions by IMD
mylogit <- glm(PNC ~ gender + agegroup + imd2015g, family="binomial", data=cprd2015linked)
summary(mylogit)
PNCout <- exp(cbind(OR=coef(mylogit), confint(mylogit)))


# Describe numbers with key additional conditions by sex*IMD or ethnicity
# CVD (=CHD + STR + PVD)
table1(~factor(PNC) + factor(HYP) + factor(IBS) + factor(HEL) + factor(AST) + factor(cardiovasc) + factor(CKD) + factor(DIB) + factor(CON) + factor(DIV) + factor(PRO) + factor(ALC) | gender*imd2015g, data=cprd2015linked)
table1(~factor(PNC) + factor(HYP) + factor(IBS) + factor(HEL) + factor(AST) + factor(cardiovasc) + factor(CKD) + factor(DIB) + factor(CON) + factor(DIV) + factor(PRO) + factor(ALC) | ethnic, data=cprd2015linked) # some small numbers

# _________________________________________________________________


# key additional conditions by demo in random sample-------------------------------------------

## @knitr key additional conditions in random sample

# Describe numbers with key additional conditions by sex and age in random sample
randomadults <- randomadults %>%
  mutate(subst=select(.,c(ALC, PSO)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(substance=ifelse(subst==2, 1, subst)) %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))

table1(~factor(substance) + factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | gender, data=randomadults)
table1(~factor(substance) + factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | agegroup, data=randomadults)


# Describe those with 3+ conditions in more detail
table1(~factor(BMI_categorical) + factor(smoking_status) | gender*condsgroup, data=cprd2015) # NB lots of missing data for BMI and smoking. Instead look at likelihood of obesity and current smoking

cprd2015$obese = cut(cprd2015$BMI_calc, breaks = c(0, 30, 100), 
                      labels = c('<=30', '>30'),
                      ordered_result = TRUE, right = FALSE)
mylogit <- glm(obese ~ condsgroup, family="binomial", data=cprd2015)
summary(mylogit)

cprd2015 <- cprd2015 %>%
  mutate(smokstatus=ifelse(smoking_status=="Missing", NA, 
                         ifelse(smoking_status=="smoker", 1, 0)))
mylogit <- glm(smokstatus ~ condsgroup, family="binomial", data=cprd2015)
summary(mylogit)


# Specific conditions that are highly prevalent for those with CMD and multiple additional conditions
# Adjusting for age and gender, which of these conditions is more common among those with 3+ additional conditions vs those with 1 additional condition?
cprd2015_1plus <- filter(cprd2015, condsgroup=="CMD+3+" | condsgroup=="CMD+2" | condsgroup=="CMD+1" )
mylogit <- glm(ATR ~ condsgroup + gender + agegroup, family="binomial", data=cprd2015_1plus) # substitute different outcomes
summary(mylogit)
# Conditions with logOR >2: ATR, BLI, cardiovasc, CKD, COP, DIB, DIV, PNC, RHE
# Conditions with logOR >3: CON, HEF



# combinations of mh and ph conditions -------------------------------------------

## @knitr combinations of mh and ph conditions

# list all combinations of MH conditions and combinations of physical conditions   
cprd2015$MHcomb <- paste(cprd2015$ALC,cprd2015$ANO,cprd2015$DEM,cprd2015$LEA,cprd2015$PSM,cprd2015$PNC) # concatenate addit MH condits and pain





