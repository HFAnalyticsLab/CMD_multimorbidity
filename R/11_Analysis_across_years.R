# Describes number and type of conditions by year
# Prepared by Mai Stafford
# Created 20th Dec 2019
# Last edit date: 25th Feb 2020


#Load libraries
library(plyr) #load before tidyverse
library(here)
library(tidyverse)
library(data.table)
library(table1)
library(haven)
library(margins)


#Read data into R
trends <- readRDS(here("Analysis","Processed_data","CPRDtrends.rds"))
trends <- trends %>% 
  filter(incohort == T)


#---------------------------------------------------------------  
#Data derivations
  trends <- trends %>% 
    mutate(yearnw = str_sub(studyyear, -4, -1)) %>%
    mutate(yearnow = as.numeric(yearnw)) %>%
    mutate(gender = factor(gender)) %>% 
    mutate(age = yearnow - yob) %>%
    mutate(numconds=select(.,c(ALC:ANO, AST:DEM, DIB:THY)) %>% rowSums(na.rm=TRUE)) %>%
    mutate(numMHconds=select(.,c(ALC, ANO, DEM, LEA, PSM)) %>% rowSums(na.rm=TRUE)) %>%
    mutate(comorbidg=ifelse(numconds==0, 1, 
                            ifelse(numconds==numMHconds, 2, 3)
                            )
           ) %>%
    mutate(comorbidg=factor(comorbidg, labels=c("CMD only", "addit MH condits only", "Other addit condits"))) %>%
    mutate(imd2015g = factor(imd, labels=c("Q1 least dep", "Q2", "Q3", "Q4", "Q5"))) %>% 
    mutate(elig_linkage = ifelse(hes_e == 1 & death_e == 1 & lsoa_e == 1, 1, 0)) %>%
    mutate(elig_linkage = replace_na(elig_linkage, 0))
           

  trends$agegroup <- cut(trends$age, breaks=c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
                                    labels=c("18-24y", "25-34y", "35-44y", "45-54y", "55-64y", "65-74y", "75+y"))  
  trends$agegroup3 <- cut(trends$age, breaks=c(-Inf, 44, 64, Inf),
                          labels=c("18-44y", "45-64y", "65+y"))  
  trends$condsgroup <- cut(trends$numconds, breaks=c(-Inf, 0, 1, 2, Inf),
                               labels=c("CMD only", "CMD+1", "CMD+2", "CMD+3+"))  
  trends$gender <- ordered(trends$gender, levels=c(1,2), labels=c("Men", "Women"))
  trends$region <- ordered(trends$region, labels=c("NE", "NW", "Yorks & Humber", "E Mids", "W Mids", "East of Engl", "SW", "SC", "London", "SE coast", "NI", "Scotland", "Wales"))  
  

  
  trendslinked <- trends %>% 
      filter(hes_e == 1 & death_e == 1 & lsoa_e == 1)
  



# How has the scale, demographic patterning and nature of these additional conditions changed over time?

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

#4.1  Table1_trends Demographics of CMD cohort over time
table1(~factor(elig_linkage)  | studyyear, data=trends, render.continuous=my.render.cont)
table1(~factor(region)  | studyyear, data=trends, render.continuous=my.render.cont)
table1(~factor(gender) + factor(agegroup) + factor(imd2015g) + numconds + factor(condsgroup) | studyyear, data=trends, render.continuous=my.render.cont)
table1(~factor(gender) + factor(agegroup) + factor(imd2015g) + numconds + factor(condsgroup) | studyyear, data=trendslinked, render.continuous=my.render.cont) # compare linked and whole dataset


# Notes on trends in number of additional conditions
# isd.scotland.org/Health-Topics/General-Practice/Quality-And-Outcomes-Framework/Revisions-To-QOF.asp   has some useful info on QOF changes. 
# In QOF 2013-2014, there was an increase in points for DEP001 (% patients with new diagnosis of depression in previous year who had a biopsychosocial assessment) and DEP002 (% patients with new diagnosis of depression in previous year who had a review between 10-35 days after diagnosis)
# There was a retirement of DEP01 (% of patients on diabetes register or CHD register for whom case finding for depression had been undertaken) and also retirement of some BP, CHD, diabetes, kidney disease, and epilepsy indicators


# More detailed look at changes in number of conditions over time by demog chars
# Mean number of conditions by age and sex over time   
numconds_by_sexageyear <- trends %>% 
  group_by(gender, agegroup3, studyyear) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_sexageyear, here("Analysis","Analysis_Results","numconds_by_sexageyear.csv"))


# Mean number of conditions by IMD and sex over time   
numconds_by_sexIMDyear <- trendslinked %>% 
  group_by(gender, imd2015g, studyyear) %>% 
  summarize(N=length(numconds), mean_numconds=mean(numconds), se_numconds=sd(numconds)/sqrt(length(numconds)))
write_csv(numconds_by_sexIMDyear, here("Analysis","Analysis_Results","numconds_by_sexIMDyear.csv"))


# Test for trend in SE gradient controlling for possible differences in age, gender, region. 
linreg <- lm(numconds ~  imd2015g + gender + region + agegroup3*studyyear, data=trends)
summary(linreg)
linreg <- lm(numconds ~ agegroup3 + gender + region + studyyear*imd2015g, data=trends)
summary(linreg)
linreg <- lm(numconds ~ agegroup3 + region + imd2015g + gender*studyyear, data=trends)
summary(linreg)




# Specific conditions over time
# people at high risk for COVID19 
# CVD (=CHD + STR + PVD)
# Heartcond = CVD + HEF
# lung condition (AST + COP + BRO)

BMI <- readRDS(here("Analysis","Processed_data","BMI2015_2016.rds"))
BMI$obese = cut(BMI$BMI_calc, breaks = c(0, 39.9999, 100), 
                     labels = c('<40', '>=40'),
                     ordered_result = TRUE, right = FALSE)

trends <- left_join(trends, BMI, by="patid")

trends <- trends %>%
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov)) %>%
  mutate(heartc=select(.,c(CHD, STR, PVD, HEF)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(heartcond=ifelse(heartc>1, 1, heartc)) %>%
  mutate(lungc=select(.,c(AST, COP, BRO)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(lungcond=ifelse(lungc>1, 1, lungc)) %>%
  mutate(neuroc=select(.,c(MSC, PRK)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(neurocond=ifelse(neuroc>1, 1, neuroc)) %>%
  mutate(obese_binary=ifelse(BMI_calc>=40, 1, 0)) %>%
  mutate(highr=select(.,c(LEA, lungcond, heartcond, HYP, DIB, CKD, CLD, neurocond, obese_binary)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(highrisk=ifelse(highr>1, 1, highr)) %>%
  mutate(highr_nohyp=select(.,c(LEA, lungcond, heartcond, DIB, CKD, CLD, neurocond, obese_binary)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(highrisk_nohyp=ifelse(highr_nohyp>1, 1, highr_nohyp))

  
table1(~factor(highrisk) + factor(highrisk_nohyp) + factor(LEA) + factor(lungcond) + factor(heartcond) + factor(HYP) + factor(DIB) + factor(CKD) + factor(CLD) + factor(neurocond) + factor(obese) | studyyear, data=trends) # How to plot prevalence of each of these by year?



trendslinkedyoung <- trendslinked %>%
  filter(agegroup3=="18-44y")
trendslinkedmid <- trendslinked %>%
  filter(agegroup3=="45-64y")
trendslinkedolder <- trendslinked %>%
  filter(agegroup3=="65+y")

table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendslinkedyoung)
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendslinkedmid)
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendslinkedolder)

trendslinkedmen <- trendslinked %>%
  filter(gender=="Men")
trendslinkedwomen <- trendslinked %>%
  filter(gender=="Women")
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendslinkedmen)
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendslinkedwomen)


# SE ineq in total and specific conditions over time 
trendsaffl <- trendslinked %>% 
  filter(imd2015_5==1)
trendsdepr <- trendslinked %>% 
  filter(imd2015_5==5)
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendsaffl)
table1(~factor(ALC) + factor(PSO) + factor(PNC) + factor(cardiovasc) + factor(COP) + factor(CAN) + factor(DIB) | studyyear, data=trendsdepr)


  

# Visualisation
##size of total cohort, percent per gender
  trendslinked %>% 
    count(yearnow,gender) %>%
    group_by(yearnow) %>%
    transmute(gender, percent=100*n/sum(n)) %>%
    ggplot(aes(x=yearnow, y=percent, group=gender)) +
    geom_line(aes(colour=gender)) +
    geom_point(aes(colour=gender))
  
  ##size of total cohort, percent per age
  trendslinked %>% 
    count(yearnow,agegroup) %>%
    group_by(yearnow) %>%
    transmute(agegroup, percent=100*n/sum(n)) %>%
    ggplot(aes(x=yearnow, y=percent, group=agegroup)) +
    geom_line(aes(colour=agegroup)) +
    geom_point(aes(colour=agegroup))
  
  ##size of total cohort, percent per number of conditions
  trendslinked %>% 
    count(yearnow,condsgroup) %>%
    group_by(yearnow) %>%
    transmute(condsgroup, percent=100*n/sum(n)) %>%
    ggplot(aes(x=yearnow, y=percent, group=condsgroup)) +
    geom_line(aes(colour=condsgroup)) +
    geom_point(aes(colour=condsgroup))

  
  ##plotting number of conditions - percentage per year+gender
  trendslinked %>%
    group_by(yearnow, gender, condsgroup) %>% 
    summarise(n=n()) %>% 
    group_by(yearnow, gender) %>% 
    mutate(percent = 100*n/sum(n)) %>% 
    unite("xy",c(gender, condsgroup), remove=F) %>% 
    ggplot(aes(x=yearnow, y=percent, group=xy)) +
    geom_line(aes(colour=condsgroup)) +
    geom_point(aes(colour=condsgroup)) +
    facet_wrap(~gender)
  
  ##plotting number of conditions - percentage per year+age group band
  trendslinked %>%
    group_by(yearnow, agegroup, condsgroup) %>% 
    summarise(n=n()) %>% 
    group_by(yearnow, agegroup) %>% 
    mutate(percent = 100*n/sum(n)) %>% 
    unite("xy",c(agegroup, condsgroup), remove=F) %>% 
    ggplot(aes(x=yearnow, y=percent, group=xy)) +
    geom_line(aes(colour=condsgroup)) +
    geom_point(aes(colour=condsgroup)) +
    facet_wrap(~agegroup)
  
  ##plotting types of conditions - percentage per year+ gender
  trendslinked %>%
    group_by(yearnow, gender, comorbidg) %>% 
    summarise(n=n()) %>% 
    group_by(yearnow, gender) %>% 
    mutate(percent = 100*n/sum(n)) %>% 
    unite("xy",c(gender, comorbidg), remove=F) %>% 
    ggplot(aes(x=yearnow, y=percent, group=xy)) +
    geom_line(aes(colour=comorbidg)) +
    geom_point(aes(colour=comorbidg)) +
    facet_wrap(~gender)
  
  ##plotting types of conditions - percentage per year+ age group
  trendslinked %>%
    group_by(yearnow, agegroup, comorbidg) %>% 
    summarise(n=n()) %>% 
    group_by(yearnow, agegroup) %>% 
    mutate(percent = 100*n/sum(n)) %>% 
    unite("xy",c(agegroup, comorbidg), remove=F) %>% 
    ggplot(aes(x=yearnow, y=percent, group=xy)) +
    geom_line(aes(colour=comorbidg)) +
    geom_point(aes(colour=comorbidg)) +
    facet_wrap(~agegroup)
  
  ##UP TO HERE BUT THIS PLOT DOESN'T LOOK V GOOD - NEED TO PUT ADDITIONAL SPECIFIC CONDITIONS ON AND ONLY SHOW PREVALENCE OF ALC=1
  ##plotting specific conditions - percentage per year+ age group
  trendslinked %>%
    group_by(yearnow, agegroup3, ALC) %>% 
    summarise(n=n()) %>% 
    group_by(yearnow, agegroup3) %>% 
    mutate(percent = 100*n/sum(n)) %>% 
    unite("xy",c(agegroup3, ALC), remove=F) %>% 
    ggplot(aes(x=yearnow, y=percent, group=xy)) +
    geom_line(aes(colour=ALC)) +
    geom_point(aes(colour=ALC)) +
    facet_wrap(~agegroup3)
  