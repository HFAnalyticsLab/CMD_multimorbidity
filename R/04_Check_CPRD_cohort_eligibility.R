
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Check CPRD data and eligibility of patients for study
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'readxl', 'data.table', 'purrr') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source('filepaths.R') #get folder path for CPRD linkage eligibility data: linkdata
savedir <- here('Data', 'Cohort_checking') #create path to save results of checking
dir.create(savedir, showWarnings = FALSE) #create folder if it doesn't exist

#______________________________________________________

#Load cohort identification prodcodes and medcodes ----

#Appendix_2_CMD_Codes_No_Z_Drugs.xlsx contains the medcodes and prodcodes we use to identify our study cohorts
medcodes <- read_xlsx(here('Lookups','Appendix_2_CMD_Codes_No_Z_Drugs.xlsx'), 'cmd_med_codes') #read medcodes
prodcodes <- read_xlsx(here('Lookups','Appendix_2_CMD_Codes_No_Z_Drugs.xlsx'), 'cmd_prod_codes') #read prodcodes

#___________________________________________________________________________________

#Perform various checks on CPRD data and determine patient elgibility for study ----

years <- 2009:2017 #we have 10 cohorts from 2008/09 to 2017/18
for(i in years){ #for each start year...

  #Read data into R - because of the large files, this takes a while to run
  patient <- fread(here('Data', paste0('patient', i, '_', i + 1, '.csv'))) #read CPRD patient data
  print(paste0('Patient ', i, '_', i + 1,':')) #print heading (to console)
  print(paste0(' - Rows: ', nrow(patient)))  #print number of rows
  print(paste0(' - Unique IDs: ', length(unique(patient[['patid']])))) #check number of unique patients
  print(paste0(' - Names: ', paste0(names(patient), collapse = ', '))) #print field names

  therapy <- fread(here('Data', paste0('therapy', i, '_', i + 1, '.csv'))) #read CPRD therapy data
  print(paste0('Therapy ', i, '_', i + 1,':')) #print heading
  print(paste0(' - Rows: ', nrow(therapy)))  #print number of rows
  print(paste0(' - Names: ', paste0(names(therapy), collapse = ', '))) #print field names

  clinical <- fread(here('Data', paste0('clinical', i, '_', i + 1, '.csv'))) #read CPRD clinical data
  print(paste0('Clinical ', i, '_', i + 1,':')) #print heading
  print(paste0(' - Rows: ', nrow(clinical)))  #print number of rows
  print(paste0(' - Names: ', paste0(names(clinical), collapse = ', '))) #print field names

  
  #Checking patients' demography, registration and quality
  print(paste0(i, '_', i + 1))
  print(patient[, .N, keyby = .(yob)]) #tablulate birth years
  print(patient[, .N, by = .(gender)]) #tabulate gender
  print(patient[, .N, by = .(accept)]) #tabulate acceptable CPRD quality


  #Checking registration dates
  patient[, `:=`(crd = as.Date(crd, format = '%d/%m/%Y'), tod = as.Date(tod, format = '%d/%m/%Y'))] #change dates to date format
  crds <- dcast(patient[, .N, by = .(year(crd), month(crd))], year ~ month, value.var = 'N', fill = 0) #check when registration started
  #View(crds, title = paste0('crds_', i, '_', i + 1)) #view crds
  tods <- dcast(patient[, .N, by = .(year(tod), month(tod))], year ~ month, value.var = 'N', fill = 0) #check when patient transferred out
  #View(tods, title = paste0('tods_', i, '_', i + 1)) #view tods


  #Flag problematic regsitration dates (for cohort eligibility)
  patient[, `:=`(goodcrd = (crd <= as.Date(paste0(i, '-11-01'))), goodtod = ifelse(is.na(tod), TRUE, (tod >= as.Date(paste0(i + 1, '-11-01')))))]


  #Set up data table to store results in
  store <- data.table(patid = integer(), type = character(), studyyear = character(), count = integer())


  #Stages for identifying cohort using therapy records ----
  #1 - get date of first relevant therapy record in year
  #2 - calculate date one year forward to look for additional records
  #3 - join relevant records onto #1 and count by patid
  therapy <- therapy[prodcode %in% prodcodes$prodcode][ #restrict to relevant prodcodes
    , eventdate:=as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      , .(daycount = .N), by = .(patid, eventdate)] #collapse multiple prescriptions received on one day (count as 1)

  r_therapy <- therapy[eventdate >= as.Date(paste0(i, '-11-01')) & eventdate < as.Date(paste0(i + 1, '-11-01'))][ #restrict to specific year
    order(eventdate), .SD[1], by = patid][ #take first date for each patient by subsetting the data
      , c('startdate', 'eventdate') := list(eventdate, NULL)][ #rename first eventdate as startdate and remove eventdate
        , enddate:=startdate + 365][ #add 365 days to first therapy record to get end date for each patient
          # !!! NB: no accounting for leap years at the moment (e.g. affects 2016)
          therapy, on = .(patid = patid), nomatch = NULL][ #merge therapy back on with inner join (nomatch = NULL)
            eventdate >= startdate & eventdate < enddate, .(count = .N), by = patid][ #count records in new date range by patient
              patient[, .(patid)], on = .(patid = patid)][ #merge the counts onto the full set of patids
                is.na(count), count := 0][ #set any with no count (i.e. count = NA) to zero
                  , c('studyyear', 'type') := list(paste0(i, '_', i + 1), 'therapy')] #add study year and type (therapy) columns

  store <- rbindlist(list(store, r_therapy), use.names = T) #bind counts onto the end of the results table
  rm(therapy, r_therapy) #remove large objects in memory

  #Stages for identifying cohort using clinical records ----
  # 1 - count relevant clinical records in year by patid
  # 2 - join onto patient dataset
  clinical <- clinical[medcode %in% medcodes$medcode][ #restrict to relevant medcodes
    , eventdate:=as.Date(eventdate, format = '%d/%m/%Y')] #change eventdate to date format

  r_clinical <- clinical[eventdate >= as.Date(paste0(i, '-11-01')) & eventdate < as.Date(paste0(i + 1, '-11-01')), #restrict to specific year
    .(count = .N), by = patid][ #count relevant records by patient
      patient[, .(patid)], on = .(patid = patid)][ #merge the counts onto the full set of patids
        is.na(count), count := 0][ #set any with no count (i.e. count = NA) to zero
          , c('studyyear', 'type') := list(paste0(i, '_', i + 1), 'clinical')] #add study year and type (clinical) columns

  store <- rbindlist(list(store, r_clinical), use.names = T) #bind counts onto the end of the results table
  rm(clinical, r_clinical) #remove large objects in memory

  #Get Cambridge multimorbidity score to identify patients with serious mental health problems (SCZ)
  camSCZ <- readRDS(here('Data', paste0('camconds', i, '_', i + 1, '.rds'))) %>% .[, .(patid, noSCZ = ifelse(SCZ == 0, TRUE, FALSE))]


  #Cast the results to separate the counts by type and identify those patients meeting our eligibility criteria:
  #Read (med) code in the study year OR 4+ prodcodes in the year following the first to occur in the study year
  results <- dcast(store, patid + studyyear ~ type, value.var = 'count')[ #cast by type
    patient[, .(patid, gender, yob, goodcrd, goodtod, tod)], on = .(patid = patid)][ #add on variables from original patient dataset
      camSCZ, on = .(patid = patid)][ #add on noSCZ condition flag from Cambridge multimorbidity score
        , incohort := (((clinical > 0) | (therapy >= 4)) & goodcrd & goodtod & noSCZ)] #flag those passing eligibility criteria


  freqtable <- with(results, table(incohort, studyyear, useNA = 'ifany')) #tabulate results
  proptable <- prop.table(freqtable, margin = 2) #tabulate as column percentages


  #Save results to new folder as CSVs
  write.csv(results, paste0(savedir, '/CohortCounts', i, '_', i + 1, '.csv'), row.names = F) #save the results to file
  write.csv(freqtable, paste0(savedir, '/FreqTable', i, '_', i + 1, '.csv')) #write the frequency table to file
  write.csv(proptable, paste0(savedir, '/PropTable', i, '_', i + 1, '.csv')) #write the proportions table to file

  gc() #clean up memory
  
}

#________________________________________________________________

#Read all cohort counts and create eligibility summary table ----

link_elig <- fread(paste0(linkdata, '19_178_linkage_eligibility_gold.txt')) #read in CPRD linkage eligibility data

eligibility <- list.files(here('Data', 'Cohort_checking'), pattern = 'CohortCounts[0-9]*_[0-9]*\\.csv', full.names = TRUE) %>% #list cohort count files
                map(fread) %>% rbindlist(fill = TRUE) %>% #read the files and bind them together
                  merge(link_elig, all = TRUE, by = 'patid') %>% #merge on the CPRD linkage eligibility data
                    .[, `:=`(clin = clinical > 0, ther = therapy >= 4)] %>% #add flags to identify if the clinical or therapy conditions are met
                      .[, .N, keyby = .(studyyear, gender, clin, ther, goodcrd, goodtod, noSCZ, hes_e, death_e, lsoa_e, incohort)] #check counts by eligibilty criteria

saveRDS(eligibility, paste0(savedir, '/eligibility.rds')) #save to file

#Describe number of eligible men and women identified by therapy / by diagnosis or symptoms / by both
elig_descr1 <- list.files(here('Data', 'Cohort_checking'), pattern = 'CohortCounts[0-9]*_[0-9]*\\.csv', full.names = TRUE) %>% #list cohort count files
  map(fread) %>% rbindlist(fill = TRUE) %>% #read the files and bind them together
  merge(link_elig, all = TRUE, by = 'patid') %>% #merge on the CPRD linkage eligibility data
  .[, `:=`(clin = clinical > 0, ther = therapy >= 4)] %>% #add flags to identify if the clinical or therapy conditions are met
  .[, .N, keyby = .(studyyear, gender, clin, ther, incohort)] #check counts by eligibilty criteria

elig_descr2 <- subset(elig_descr1, incohort==T)
write.csv(elig_descr2, here('Data', 'Cohort_checking', 'elig_describe.csv'))






