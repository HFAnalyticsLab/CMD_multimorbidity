##tidying primary and secondary care data for combined dataset. 
library(tidyverse)

# Read in files and combine -----------------------------------------------------------
#Open data for linked CMD cohort 
outcomes_cprd <- readRDS(here("Analysis","Processed_data","CPRDoutcomes2015_2016.rds"))
outcomes_hes <- readRDS(here("Analysis","Processed_data","HESoutcomes.rds"))

outcomes_linked <- outcomes_cprd %>% 
  filter(incohort ==TRUE) %>% 
  filter(hes_e == 1 & death_e == 1 & lsoa_e == 1)  %>% 
  filter(!is.na(imd)) %>% 
  filter(years_in_study_cprd>0) %>% ## remove those with no data during study period
  left_join(outcomes_hes)  


outcomes_linked$ethnic <- as.factor(outcomes_linked$ethnic)

# Adjust primary care  -----------------------------------------

## Adjust for length of follow-up by calculating as use per year

primary_cols <- c('TotCons','TotMHRefs','AllDrugs','PsychDrugs')
  
outcomes_linked <- outcomes_linked %>% mutate_at(primary_cols, list(adj= ~ ./ years_in_study_cprd ))

# Adjust secondary care + ACSC variables -----------------------------------------


#list names of columns to be adjusted by time in follow-up
secondary_cols <- c('APCemerg','APCelec','APCmat','APCday','APCoth','APClos','APCmh_spells','APCmh_los', 'APCspells','AEseen','AEplan', 'AEunplan','AEoth','AEmh_att','AEatts','OPmiss','OPappts','OPmh_miss','OPmh_appts')

#Add variables adjusted for length of time in follow-up (to give 'per year' values)
outcomes_linked <- outcomes_linked %>% mutate_at(secondary_cols, list(adj= ~ ./ years_in_study_cprd ))

#limit adjusted values to 200
outcomes_linked$TotCons_adj[outcomes_linked$TotCons_adj>200]<-200
outcomes_linked$AllDrugs_adj[outcomes_linked$AllDrugs_adj>200]<-200
outcomes_linked$OPappts_adj[outcomes_linked$OPappts_adj>200]<-200
outcomes_linked$APCspells_adj[outcomes_linked$APCspells_adj>200]<-200
outcomes_linked$AEatts_adj[outcomes_linked$AEatts_adj>200]<-200


# Create factors for primary care variables -------------------------------
outcomes_linked$PlusOnePsychDrugs<-outcomes_linked$PsychDrugs_adj > 1

# create factors of secondary care outcomes -------------------------------

outcomes_linked$APCspells_binary <- outcomes_linked$APCspells_adj > 0
outcomes_linked$APCmh_spells_binary<-outcomes_linked$APCmh_spells_adj > 0
outcomes_linked$AEatts_binary <- outcomes_linked$AEatts_adj > 0
outcomes_linked$AEmh_att_binary<-outcomes_linked$AEmh_att_adj > 0
outcomes_linked$OPmh_appts_binary<-outcomes_linked$OPmh_appts_adj > 0
outcomes_linked$OPappts_binary<-outcomes_linked$OPappts_adj > 0
outcomes_linked$APCemerg_binary<-outcomes_linked$APCemerg_adj > 0
outcomes_linked$APCelec_binary<-outcomes_linked$APCelec_adj > 0


saveRDS(outcomes_linked, here('Analysis','Processed_data','cprd_hes_linked_outcomes.rds')) 

