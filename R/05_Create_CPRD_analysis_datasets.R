
# =========================================================================================
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Create CPRD analysis datasets for time trends and outcomes
# Author: Dr Will Parry
# =========================================================================================

#Setup - Load required packages and set directory/folders ----

pkgs <- c('data.table', 'purrr') #package list
lapply(pkgs, library, character.only=T) #load packages
setwd('C:/MyProject/') #set the directory for your project

#_____________________________________________

#Create patient level time trends dataset ----

cohort <- list.files('.', pattern = 'CohortCounts', full.names = TRUE) %>% #list cohort eligibility files
            map(fread, select = c('studyyear', 'patid', 'gender', 'yob', 'incohort')) #read them all into a list

camconds <- list.files('./data/', pattern = 'CamConds', full.names = TRUE) %>% #list the camconds files
              map(readRDS) #read them all into a list
			  
dt <- map2(cohort, camconds, merge, by='patid', all = TRUE) %>% rbindlist() #merge the datasets together and bind into one

saveRDS(dt, 'CPRDtimetrends.rds') #save as RDS data file
rm(cohort, camconds) #remove datasets which are no longer needed

#_________________________________________

#Counting outcomes for 2015/16 cohort ----

dt <- dt[studyyear=='2015_2016'] #reduce time trends dataset to just 2015/16 cohort


#Consultations ----
#~~~~~~~~~~~~~~~~~~
#consultation types are described in the CPRD lookup file COT.txt
#staff role types are described in the CPRD lookup file ROL.txt

# Face to face consultation codes:	Home consultation codes:	Telephone consultation codes:
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
# GP codes from rol:			Nurse codes from rol:				  	Other clinician codes from rol:
# ~~~~~~~~~~~~~~~~~~			~~~~~~~~~~~~~~~~~~~~~				  	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Senior Partner,1,				Practice Nurse,11,			   			Physiotherapist,26,
# Partner,2,					Other Nursing & Midwifery,54			Other Health Care Professional,33
# Assistant,3,
# Associate,4, 
# Locum,7,
# GP Registrar,8,
# Sole Practitioner,10,
# Salaried Partner,47,
# GP Retainer,50,
# Other Students,53

#This uses a lot of RAM in our project (~30-40GB)
consult <- fread('./data/consult2015_2016.csv')[ #read in consultations dataset for 2015/16 cohort
  , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
    eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')] #restrict to study follow-up period

staff <- fread('./data/staff2015_2016.csv') #read in GP staff data
gc()

dt <- staff[consult, on = .(staffid)][ #join staff data onto consultation data (keeping all consultations)
  constype %in% c(1,3,6,7,9,11,18,48,27,28,30,31,32,50,10,21,33,55) #keep certain consultation types
  & role %in% c(1,2,3,4,7,8,10,47,50,53,11,54,26,33)][ #keep certain staff roles
      , duration := ifelse(duration == 0, 0.5, duration)][ #make minimum duration of a consultation 0.5 minutes
        , duration := ifelse(duration > 60, 60, duration)][ #make maximum duration of a consultation 60 minutes
          , duration := ifelse(is.na(duration), 0, duration)][ #make duration zero where it is missing (no joined consultations)
            , .(TotCons = .N, TotDur = sum(duration)), by = .(patid)][ #summarise consultations by patient
              dt, on=.(patid)][ #join onto cohort dataset
                , TotCons:=ifelse(is.na(TotCons), 0, TotCons)][ #make zero any patients with no joined consultations
                  , TotDur:=ifelse(is.na(TotDur), 0, TotDur)]  #make zero any patients with no joined durations
gc()

#Referrals ----
#~~~~~~~~~~~~~~
#NB: readtomed lookup file is currently taken from an old project (2017). Might need updating!
readtomed <- fread('./data/medical.txt') #read in readcode to medcode lookup file

#read in the list of read codes for referrals we are interested in
refcodes <- fread('./data/Appendix_3_MH_Referral_Read_Codes.txt') %>% #read in
              merge(readtomed, by.x = 'code', by.y = 'readcode', all.x = TRUE) %>% .[['medcode']] #join on lookup file and return medcodes as a vector

referral <- fread('./data/referral2015_2016.csv')[ #read in referrals dataset for 2015/16 cohort
              medcode %in% refcodes][ #reduce to those medcodes we are interested in
                , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
                  eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')][ #restrict to study follow-up period
                    , .(TotRefs = .N), by = .(patid)][ #count referrals by patient
                      dt, on = .(patid)][ #join onto cohort dataset
                        , TotRefs:=ifelse(is.na(TotRefs), 0, TotRefs)]  #make zero any patients with no joined referrals
gc()

#Therapy ----
#~~~~~~~~~~~~
prodtodrug <- fread('./data/product.txt')[ #read in prodcode to drug substance lookup file
                , .(prodcode, productname, drugsubstance, bnfchapter)] #keep only the columns needed

psychprods <- fread('./data/Appendix_4_Psych_Prod_Codes.txt') %>% #read in prod codes for psychiatric drugs
                .[, .(prodcode, psych = TRUE)] #keep just the prodcode but add a flag for when merged with therapy below

therapy <- fread('./data/therapy2015_2016.csv')[ #read in therapy dataset for 2015/16 cohort
    , eventdate := as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      eventdate >= as.Date('2016-11-01') & eventdate < as.Date('2018-11-01')] %>% #restrict to study follow-up period
        merge(prodtodrug, by = 'prodcode', all.x = TRUE) %>% #join on product and drug names in prodtodrug
          merge(psychprods, by = 'prodcode', all.x = TRUE) %>% #join on flag from psychprods to identify psychiatric drugs
            .[, psych:=ifelse(is.na(psych), FALSE, psych)] #change psych flag to FALSE where no record was joined
gc()

#Check products with no drug substance listed
missdrugs <- therapy[is.na(drugsubstance) | drugsubstance=='', .N, keyby=.(psych, prodcode, productname, drugsubstance, bnfchapter)] 
View(missdrugs)

a <- therapy[!is.na(drugsubstance) & drugsubstance!=''] #reduce to those records with a drug substance
b <- a[, .N, by = .(patid, drugsubstance)][, .(AllDrugs = .N), by = .(patid)] #count drug substances by patient
c <- a[psych == TRUE, .N, by=.(patid, drugsubstance)][, .(PsychDrugs = .N), by = .(patid)] #count psych drug substances by patient
d <- a[psych == FALSE, .N, by=.(patid, drugsubstance)][, .(OtherDrugs = .N), by = .(patid)] #count non-psych drug substances by patient

#__________________________________________

#Create 2015/16 outcomes analysis file ----

results <- Reduce(function(x, y) merge(x, y, all = TRUE, by = 'patid'), list(referral, b, c, d)) #merge all results together
setcolorder(results, c("patid", "studyyear", "gender", "yob", "incohort", "TotRefs", "TotCons", 
                       "TotDur", "AllDrugs", "PsychDrugs", "OtherDrugs")) #reorder columns
 
drugcols <- c('AllDrugs', 'PsychDrugs', 'OtherDrugs') #list drug result columns so...
for(i in drugcols){results[is.na(get(i)), (i):=0]} #we can change any NA counts to zeroes

saveRDS(results, 'CPRDoutcomes2015_2016.rds') #save to file


