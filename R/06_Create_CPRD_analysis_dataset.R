
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Create CPRD analysis datasets 
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'purrr', 'data.table','tidyverse') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source(here('filepaths.R')) #get folder path for linked IMD data: (1) linkdata, (2) moreIMDdata

#_________________________________________

#Counting outcomes for 2015/16 cohort ----

cohort <- fread(here('Data','patient2015_2016.csv'))[, .(patid)] #read in patids of 2015/16 cohort
staff <- fread(here('Data','staff2015_2016.csv')) #read in GP staff data

#Consultations ----
#~~~~~~~~~~~~~~~~~~
#consultation types are described in the CPRD lookup file COT.txt
#staff role types are described in the CPRD lookup file ROL.txt

# Face to face consultation codes:	Home consultation codes:		Telephone consultation codes:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~~~~~~~~~~    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clinic,1,                         Home Visit,27,              Telephone call from a patient,10,
# Follow-up/routine visit,3,        Hotel Visit,28,             Telephone call to a patient,21,
# Night visit , practice,6,         Nursing Home Visit,30,      Triage,33,
# Out of hours, Practice,7,         Residential Home Visit,31,  Telephone Consultation,55,
# Surgery consultation,9,           Twilight Visit,32,
# Acute visit,11,                   Night Visit,50,
# Emergency Consultation,18,
# Initial Post Discharge Review,48,
# 
# GP codes from rol:				      Nurse codes from rol:				  Other clinician codes from rol:
# ~~~~~~~~~~~~~~~~~~				      ~~~~~~~~~~~~~~~~~~~~~				  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Senior Partner,1,               Practice Nurse,11,            Physiotherapist,26,
# Partner,2,                      Other Nursing & Midwifery,54  Other Health Care Professional,33
# Assistant,3,
# Associate,4, 
# Locum,7,
# GP Registrar,8,
# Sole Practitioner,10,
# Salaried Partner,47,
# GP Retainer,50,
# Other Students,53

#This uses a lot of RAM in our project (~30-40GB)
consult <- staff[fread(here('Data','consult2015_2016.csv')), on = .(staffid)][ #read in consultations and join on staff (keeping all consultations)
  , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
    eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')][ #restrict to study follow-up period
      constype %in% c(1,3,6,7,9,11,18,48,27,28,30,31,32,50,10,21,33,55) #keep certain consultation types
        & role %in% c(1,2,3,4,7,8,10,47,50,53,11,54,26,33)][ #keep certain staff roles
          , duration := ifelse(duration == 0, 0.5, duration)][ #make minimum duration of a consultation 0.5 minutes
            , duration := ifelse(duration > 60, 60, duration)][ #make maximum duration of a consultation 60 minutes
              , duration := ifelse(is.na(duration), 0, duration)][ #make duration zero where it is missing (no joined consultations)
                , .(TotCons = .N, TotDur = sum(duration)), by = .(patid)][ #summarise consultations by patient
                  cohort, on=.(patid)][ #join onto cohort dataset
                    , TotCons:=ifelse(is.na(TotCons), 0, TotCons)][ #make zero any patients with no joined consultations
                      , TotDur:=ifelse(is.na(TotDur), 0, TotDur)]  #make zero any patients with no joined durations

gc() #clean up memory


#Referrals ----
#~~~~~~~~~~~~~~

readtomed <- fread(here('Lookups','medical.txt')) #read in readcode to medcode lookup file

refcodes <- fread(here('Lookups', 'Appendix_3_MH_Referral_Read_Codes.txt')) %>% #read in list of read codes for referrals we are interested in
              merge(readtomed, by.x = 'code', by.y = 'readcode', all.x = TRUE) %>% .[['medcode']] #join on lookup file and return medcodes as a vector

referral <- fread(here('Data','referral2015_2016.csv'))[ #read in referrals dataset for 2015/16 cohort
              medcode %in% refcodes][ #reduce to those medcodes we are interested in
                , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
                  eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')] #restrict to study follow-up period

referral_type <- referral[, .(patid, medcode, flag = 1)] %>% #per patid flag of each MH referral given
                  dcast(., ... ~ medcode, value.var = 'flag', fill = '0', fun.aggregate = length) #cast by medcode and count records
saveRDS(referral_type, here('Data','Var_freq_checking','mental_health_referral_types_2015_2016.rds')) ##save for consideration later

referral <- referral[, .(TotMHRefs = .N), by = .(patid)][ #count MH referrals by patient
              cohort, on = .(patid)][ #join onto cohort dataset
                , TotMHRefs:=ifelse(is.na(TotMHRefs), 0, TotMHRefs)]  #make zero any patients with no joined referrals

gc() #clean up memory


#Therapy ----
#~~~~~~~~~~~~

prodtodrug <- fread(here('Lookups','product.txt'))[ #read in prodcode to drug substance lookup file
                , .(prodcode, productname, drugsubstance, bnfchapter)] #keep only the columns needed

#NB: there are some drugs in the Appendix_3_Psych_Prod_Codes list that do not have a matching drug substance in the prodtodrug lookup
psychprods <- fread(here('Lookups', 'Appendix_4_Psych_Prod_Codes.txt')) %>% #read in list of codes for psychiatric drugs we are interested in
                .[, .(prodcode, psych = TRUE)] #keep just the prodcode but add a flag for when merged with therapy below

therapy <- fread(here('Data','therapy2015_2016.csv'))[ #read in therapy dataset for 2015/16 cohort
    , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')] %>% #restrict to study follow-up period
        merge(prodtodrug, by = 'prodcode', all.x = TRUE) %>% #join on product and drug names in prodtodrug
          merge(psychprods, by = 'prodcode', all.x = TRUE) %>% #join on flag from psychprods to identify psychiatric drugs
            .[, psych:=ifelse(is.na(psych), FALSE, psych)] #change psych flag to FALSE where no record was joined

gc() #clean up memory

#check counts by drug substance for missing values
missdrugs <- therapy[is.na(drugsubstance) | drugsubstance=='', .N, keyby=.(psych, prodcode, productname, drugsubstance, bnfchapter)] 
View(missdrugs)

a <- therapy[!is.na(drugsubstance) & drugsubstance!=''] #reduce to those records with a drug substance
b <- a[, .N, by = .(patid, drugsubstance)][, .(AllDrugs = .N), by = .(patid)] #count drug substances by patient (excluding missing values)
c <- a[psych == TRUE, .N, by=.(patid, drugsubstance)][, .(PsychDrugs = .N), by = .(patid)] #count psych drug substances by patient (excluding missing values)
d <- a[psych == FALSE, .N, by=.(patid, drugsubstance)][, .(OtherDrugs = .N), by = .(patid)] #count non-psych drug substances by patient (excluding missing values)

therapy <- list(cohort, b, c, d) %>% reduce(merge, all.x = TRUE, by = 'patid') %>% #merge the different counts onto the cohort
            .[is.na(AllDrugs), AllDrugs := 0] %>% #change any remaining NAs to zeroes...
              .[is.na(PsychDrugs), PsychDrugs := 0] %>%
                .[is.na(OtherDrugs), OtherDrugs := 0]
        
psych_drug_substance_type <- a[psych == TRUE][, .(patid, drugsubstance, flag = 1)] %>% #per patid flag of each drugsubstance given
                              dcast(., ... ~ drugsubstance, value.var = 'flag', fill = '0', fun.aggregate = length) #cast by drug substance and count records
saveRDS(psych_drug_substance_type, here('Data','Var_freq_checking','psych_drug_substance_type_types_2015_2016.rds')) #save for consideration later

psych_bnfchapter_type <- a[psych == TRUE][, .(patid, bnfchapter, flag = 1)] %>% #per patid flag of each bnf chapter given
                          dcast(., ... ~ bnfchapter, value.var = 'flag', fill = '0', fun.aggregate = length) #cast by BNF chapter and count records
saveRDS(psych_bnfchapter_type, here('Data','Var_freq_checking','psych_bnfchapter_type_types_2015_2016.rds')) #save for consideration later

#_____________________

#Coding ethnicity ---- [ this relies on info from both cprd and hes ]

#Using CPRD clinical data...

ethcodes <- fread(here('Lookups', 'res56-ethnicity.csv')) [, .(readcode, ethnic5)] %>% #read in ethnicity read codes
                    merge(readtomed, by = 'readcode', all.x = TRUE) #join on readtomed

clinical <- fread(here('Data','clinical2015_2016.csv'))[ #read in clinical data for 2015/16 cohort
              , .(patid, medcode)] #restrict to medcode and patid

ethCPRD <- clinical[ethcodes, on = .(medcode)][ #join onto ethcodes (keeping only records matching with ethcodes)
            , .N, by = .(patid, ethnic5)][ #count codings by patient and ethnicity
              , max := max(N), by = .(patid)][ #identify max count by patid
                max == N][ #restrict to those ethnicities that are equal to their max count
                  , count := .N, by = .(patid)][ #count number of remaining records per patient (to identify ties)
                    count == 1, .(patid, ethnic5)] #keep only patients with no ties, and keep only patid and ethnic5

gc() #clean up memory

#Using HES data...

ethHES <- fread(paste0(linkdata, 'hes_patient_19_178.txt'))[ #read in linked HES patient data
            , .(patid, gen_ethnicity)] %>% #restrict to patid and gen_ethnicity
              merge(ethCPRD, all = TRUE, by = 'patid') #merge onto ethCPRD keeping all records (outer join)
                
#Combining and recoding...

ethnic <- ethHES[cohort, on = .(patid)][ #merge onto cohort
            , ethnic := ifelse(is.na(ethnic5), gen_ethnicity, ethnic5)][ #recode the two sources into one, then change to ethnic5 coding
              ethnic == 'Chinese', ethnic :=  'Other'][ #recode the two sources into one based on ethnic5...
                ethnic %in% c('Bl_Afric', 'Bl_Other', 'Bl_Carib'), ethnic := 'Black/Black British'][
                  ethnic %in% c('Indian', 'Pakistani', 'Bangladesi', 'Oth_Asian'), ethnic := 'Asian/British Asian'][
                    ethnic == 'Unknown', ethnic := NA][
                      , .(patid, ethnic)] #restrict to patid and recoded ethnicity

#_______________________________________________________________________

#Merge everything together to create 2015/16 outcomes analysis file ----

cprd_1516 <- readRDS(here('Analysis', 'Processed_data', 'CPRDtrends.rds')) %>% #read trends RDS file (for all the useful info contained in it)
  .[studyyear=='2015_2016'] #reduce dataset to just 2015/16 cohort

cprd_1516 <- cprd_1516 %>%
  group_by(patid) %>%
  filter(incohort == TRUE) %>% 
  mutate(study_end=c("01/11/2018")) %>%
  mutate(study_end=as.Date(study_end,"%d/%m/%Y")) %>%
  mutate(tod=as.Date(tod,"%Y-%m-%d")) %>%
  mutate(dod=as.Date(tod,"%Y-%m-%d")) %>%
  mutate(lcd=as.Date(lcd,"%d/%m/%Y")) %>%
  mutate(censoring_date_cprd=min(tod, dod, lcd, study_end, na.rm=TRUE)) %>%
  mutate(years_in_study_cprd=round(as.numeric(censoring_date_cprd - as.Date('2016-11-01'))/365, 2)) %>%
  ungroup()


# create factors of demographic variables --------------------------------

cprd_1516 <- cprd_1516 %>% 
  mutate(gender = factor(gender)) %>% 
  mutate(age = 2016 - yob) %>%
  mutate(numconds=select(.,c(ALC:ANO, AST:DEM, DIB:THY)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(numMHconds=select(.,c(ALC, ANO, DEM, LEA, PSM)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(comorbidg=ifelse(numconds==0, 1, 
                          ifelse(numconds==numMHconds, 2, 3))) %>% 
  mutate(cardiov=select(.,c(CHD, STR, PVD)) %>% rowSums(na.rm=TRUE)) %>%
  mutate(cardiovasc=ifelse(cardiov>1, 1, cardiov))

cprd_1516$agegroup3 <- cut(cprd_1516$age, breaks=c(-Inf, 44, 64, Inf),
                                 labels=c("18-44y", "45-64y", "65+y"))  
cprd_1516$condsgroup <- cut(cprd_1516$numconds, breaks=c(-Inf, 0, 2, Inf),
                           labels=c("CMD only", "CMD+1/2", "CMD+3+"))  
cprd_1516$gender <- ordered(cprd_1516$gender, levels=c(1,2), labels=c("Men", "Women"))
cprd_1516$imd_5f <- as.factor(cprd_1516$imd)


# create 3 categories of IMD for visualisation purposes
cprd_1516 <- cprd_1516 %>% 
  mutate(imd_3=as.factor(case_when(
    imd %in% 1 ~ 1,
    imd %in% 2:4 ~ 2,
    imd %in% 5 ~ 3,
  )))


results <- list(cprd_1516, ethnic, consult, referral, therapy) %>% reduce(merge, all.x = TRUE, by = 'patid') #merge everything together

saveRDS(results, here('Analysis', 'Processed_data', 'CPRDoutcomes2015_2016.rds')) #save to file


#Clean up objects
rm(a, b, c, d, clinical, cohort, consult, ethcodes, ethCPRD, ethHES, ethnic, missdrugs, prodtodrug, psych_bnfchapter_type, 
   psych_drug_substance_type, psychprods, readtomed, refcodes, referral, referral_type, staff, therapy, cprd_1516)

gc() #clean up memory




