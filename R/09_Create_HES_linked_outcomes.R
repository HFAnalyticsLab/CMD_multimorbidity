##tidying primary and secondary care data for combined dataset. 
library(tidyverse)

# Read in files and combine -----------------------------------------------------------
#Open data for linked CMD cohort 
outcomes_cprd <- readRDS(here("Analysis","Processed_data","cprd_outcomes_2015_2016.rds"))
patient_years <- readRDS(here("Analysis","Processed_data","patient_study_years.rds"))
outcomes_hes <- readRDS(here("Analysis","Processed_data","HESoutcomes.rds"))

acsc_admissions <- readRDS(here("Analysis","Processed_data","ACSadmissions.rds"))

outcomes_linked <- outcomes_cprd %>% 
  filter(incohort ==TRUE) %>% 
  filter(hes_e == 1 & death_e == 1 & lsoa_e == 1)  %>% 
  left_join(patient_years, by="patid") %>% 
  filter(years_in_study_cprd>0) %>% ## need to check this is right
  left_join(outcomes_hes) %>% 
  filter(!is.na(APCspells_adj)) %>%  # eligible for linkage but no data available
  left_join(acsc_admissions, by = "patid")

# create factors of demographic variables --------------------------------

outcomes_linked <- outcomes_linked %>% 
  mutate(gender = factor(gender)) %>% 
  mutate(age = 2016 - yob) %>%
  mutate(numconds=select(.,c(ALC:ANO, AST:DEM, DIB:THY)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(numMHconds=select(.,c(ALC, ANO, DEM, LEA, PSM)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(comorbidg=ifelse(numconds==0, 1, 
                          ifelse(numconds==numMHconds, 2, 3)
  )
  )

outcomes_linked$agegroup <- cut(outcomes_linked$age, breaks=c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
                                labels=c("18-24y", "25-34y", "35-44y", "45-54y", "55-64y", "65-74y", "75+y"))  
outcomes_linked$agegroup3 <- cut(outcomes_linked$age, breaks=c(-Inf, 44, 64, Inf),
                                 labels=c("18-44y", "45-64y", "65+y"))  
outcomes_linked$condsgroup <- cut(outcomes_linked$numconds, breaks=c(-Inf, 0, 1, 2, Inf),
                                  labels=c("CMD only", "CMD+1", "CMD+2", "CMD+3+"))  
outcomes_linked$gender <- ordered(outcomes_linked$gender, levels=c(1,2), labels=c("Men", "Women"))
outcomes_linked$imd2015_5 <- as.factor(outcomes_linked$imd2015_5)


# Adjust primary care + ACSC variables -----------------------------------------

## Adjust for length of follow-up by calculating as mean use per year
outcomes_linked <- outcomes_linked %>%
  mutate(TotCons_adj=TotCons/years_in_study_cprd) %>%   # Need to check if this analysis is highly driven by people with very short time in the study
  mutate(TotMHRefs_adj=TotMHRefs/years_in_study_cprd) %>%
  mutate(AllDrugs_adj=AllDrugs/years_in_study_cprd) %>%
  mutate(PsychDrugs_adj=PsychDrugs/years_in_study_cprd) %>% 
  mutate(ACSadms_adj=ACSadms/years_in_study_cprd) 


# Create factors for primary care variables -------------------------------


outcomes_linked$TotCons_adjg <- cut(outcomes_linked$TotCons_adj, breaks=c(-Inf, 0, 5, 10, 15, 20, Inf),
                                    labels=c("None", "1-5", "6-10", "11-15", "16-20", "21+"))  # Some patients with >150 consultations over 2 years - feasible? CHeck the raw data


outcomes_linked$TotMHRefs_adjg <- cut(outcomes_linked$TotMHRefs_adj, breaks=c(-Inf, 0, 1, Inf),
                                      labels=c("None", "1", "2+"))  # A handful of patients with >150 consultations

outcomes_linked$TotMHRefs_binary<-outcomes_linked$TotMHRefs_adjg!='None'


outcomes_linked$AllDrugs_adjg <- cut(outcomes_linked$AllDrugs_adj, breaks=c(-Inf, 0, 5, 10, 15,20, Inf),
                                     labels=c("None", "1-5", "6-10", "11-15", "16-20", "21+")) # Some patients with >40 Drugs
outcomes_linked$PsychDrugs_adjg <- cut(outcomes_linked$PsychDrugs_adj, breaks=c(-Inf, 0, 1, Inf),
                                       labels=c("None", "1", "2+")) 


outcomes_linked$PlusOnePsychDrugs<-outcomes_linked$PsychDrugs_adj > 1


# create factors of secondary care outcomes -------------------------------
 
five_plus_hes_outcomes <- c('APCspells_adj', 'APCmh_spells_adj','APCemerg_adj','APCelec_adj','OPmiss_adj','OPmh_miss_adj', 'AEatts_adj','AEmh_att_adj')

thirty_plus_hes_outcomes <- c('APClos_adj','APCmh_los_adj','OPappts_adj','OPmh_appts_adj')


outcomes_linked <- outcomes_linked %>% 
  mutate(APClos_adj = as.numeric(APClos_adj)) %>%
  mutate_at(five_plus_hes_outcomes, .funs = list(cat=~cut(., breaks=c(-Inf, 0, 1, 2, Inf),labels=c("None", "1", "2", "3+")))) %>% 
  mutate_at(paste0(five_plus_hes_outcomes,"_cat"), .funs = list(~fct_relevel(., "None", "1", "2", "3+"))) %>% 
  mutate_at(thirty_plus_hes_outcomes, .funs = list(cat=~cut(., breaks=c(-Inf, 0, 5, 10, 15, 20, 30, Inf),
                                                            labels=c("None", "1-5", "6-10", "11-15", "16-20", "21-30", "31+")))) %>% 
  mutate_at(paste0(thirty_plus_hes_outcomes,"_cat"), .funs = list(~fct_relevel(., "None", "1-5", "6-10", "11-15", "16-20", "21-30", "31+")))

outcomes_linked$APCspells_binary <- outcomes_linked$APCspells_adj_cat!='None'
outcomes_linked$APCmh_spells_binary<-outcomes_linked$APCmh_spells_adj_cat!='None'
outcomes_linked$AEatts_binary <- outcomes_linked$AEatts_adj_cat!='None'
outcomes_linked$AEmh_att_binary<-outcomes_linked$AEmh_att_adj_cat!='None'
outcomes_linked$OPmh_appts_binary<-outcomes_linked$OPmh_appts_adj_cat!='None'
outcomes_linked$OPappts_binary<-outcomes_linked$OPappts_adj_cat!='None'
outcomes_linked$ACSadms_binary<-outcomes_linked$ACSadms_adj>0
outcomes_linked$APCemerg_binary<-outcomes_linked$APCemerg_adj_cat!='None'
outcomes_linked$APCelec_binary<-outcomes_linked$APCelec_adj_cat!='None'


saveRDS(outcomes_linked, here('Analysis','Processed_data','cprd_hes_linked_outcomes.rds')) 
