
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Create dataset to examine trends in the CPRD over time
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'purrr', 'data.table') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source('filepaths.R') #get folder path for linked IMD data: (1) linkdata, (2) moreIMDdata

#_____________________________________________

#Create patient level time trends dataset ----

practices <- list.files(here('Data'), pattern = 'practice[0-9]*_[0-9]*\\.csv', full.names = TRUE) %>% #list the practice files
  map(fread) %>% rbindlist(fill = TRUE) %>% #read them all into a list object and bind together
  .[, .SD[1], by = .(pracid)] #keep one record per practice (practice data changes over years)

cohort <- list.files(here('Data', 'Cohort_checking'), pattern = 'CohortCounts[0-9]*_[0-9]*\\.csv', full.names = TRUE) %>% #list cohort eligibility files
            map(fread) #read them all into a list object

camconds <- list.files(here('Data'), pattern = 'camconds[0-9]*_[0-9]*\\.rds', full.names = TRUE) %>% #list the camconds files
              map(readRDS) #read them all into a list object

cohort <- map2(cohort, camconds, merge, by = 'patid', all = TRUE) %>% rbindlist(fill = TRUE) %>% #merge them together separately and then bind into one
            .[, pracid := patid %% 1000] %>% #use remainder division to extract pracid from patid
              merge(practices, all.x = TRUE, by = 'pracid') #merge practices onto cohort

rm(camconds, practices) #remove camconds and practices now not needed


#NB: no reason for two separate IMD files below (we had originally extracted the 2015/16 cohort from CPRD as a trial run)
imd1516 <- fread(paste0(linkdata, 'patient_imd2015_19_178.txt'))[, .(patid, imd2015_5)] #read linked IMD data for patients in our 2015/16 cohort
imd <- fread(paste0(moreIMDdata, 'patient_imd2015_19_178_request2.txt'))[, .(patid, imd2015_5)] %>%  #read linked IMD data for all other years
        rbind(imd1516) %>% .[, .SD[1], by = .(patid)] %>% #bind the two IMD sources together and take first record for each patid
          setnames('imd2015_5', 'imd') #change name of IMD field

linkage <- fread(paste0(linkdata, '19_178_linkage_eligibility_gold.txt'))[, .(patid, hes_e, death_e, lsoa_e)] #read in linkage eligibility data

dod <- fread(paste0(linkdata, 'death_patient_19_178.txt'))[ #read in death registration data 
        ,.(patid, dod = as.Date(dod, format = '%d/%m/%Y'))] #format dates of death as date format variable


trends <- list(cohort, imd, linkage, dod) %>% reduce(merge, all.x = TRUE, by = 'patid') #merge everything onto cohort

saveRDS(trends, here('Analysis', 'Processed_data', 'CPRDtrends.rds')) #save RDS file to folder
rm(trends, cohort, imd1516, imd, linkage, dod) #remove datasets
gc() #clean up memory



