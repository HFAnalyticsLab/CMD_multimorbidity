
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Creating the Cambridge Multimorbidity Score for each cohort using CamCodeList
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'data.table', 'tidyverse') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project

#______________________________________________________________________________________

#Read in and process CPRD data for each year (used up to 60GB RAM in our project!) ----

codelist <- readRDS(here('Lookups', 'CamCodeList.rds')) #read in CamCodeList
medcodes <- codelist[type == 'MEDCODES'] #Make medcode list
prodcodes <- codelist[type == 'PRODCODES'] #Make prodcode list

years <- 2008:2017 #we have 10 cohorts, from 2008/09 to 2017/18
for(i in years){ #for each start year
  
  starttime <- Sys.time() #set start time
  enddate <- as.Date(paste0(i + 1, '-11-01')) #set end date for current period (e.g. for 2008_2009, i=2008, enddate=2009-11-01)
 
  #Process clinical data to identify CAN (cancer) - dealt with separately because identification is slightly different to the rest
  clinical <- fread(here('Data', paste0('clinical', i, '_', i + 1, '.csv'))) #read in combined CPRD clinical data, which contains medcodes
  cancer <- clinical[medcodes[cond == 'CAN'], on = .(medcode = code)][ #restrict to relevant medcodes using a join
    , eventdate:=as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      order(eventdate), .SD[1], by = .(patid, cond,  ref, read, special, logic, ud)][ #retain oldest cancer record per patient
        eventdate < enddate & eventdate >= (enddate - read)] #see if oldest was in last 5 years
  
  
  #Process clinical data to identify all other conditions by medcode (overwriting original object in memory)
  clinical <- clinical[medcodes, on = .(medcode = code)][ #restrict to relevant medcodes using a join
    , eventdate:=as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      eventdate < enddate & eventdate >= (enddate - read)][ #restrict to required date range
        cond != 'CAN', .(mcount = .N), by = .(patid, cond, ref, special, logic, ud)] #remove cancer (done above) and count by patient/condition
  
  
  #Process therapy data to identify conditions by prodcode
  therapy <- fread(here('Data', paste0('therapy', i, '_', i + 1, '.csv'))) #read in the therapy data, which contains prod codes
  therapy <- therapy[prodcodes, on = .(prodcode = code)][ #restrict to relevant prodcodes using a join
    , eventdate:=as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
      , days:= ifelse(ref == 'SCZ176', 99999, 365)][ #add historical days over which to restrict (SCZ176 is a special case)
        eventdate < enddate & eventdate >= (enddate - days)][ #restrict to required date range
          , .(pcount = .N), by = .(patid, cond, ref, Rx, special, logic, ud)][ #count records by patient and condition
            pcount >= Rx] #restrict using Rx field
  
  
  #Process test data to identify patients with CKD by eGFR test results
  eGFR <- fread(here('Data', paste0('test', i, '_', i + 1, '.csv'))) %>% #read in the test data
    .[enttype == 466 & data2 > 0 & data2 <= 250] #restrict to eGFR readings (for CKD) which are not ridiculous
  eGFR <- eGFR[, eventdate:=as.Date(eventdate, format = '%d/%m/%Y')][ #change eventdate to date format
    order(-eventdate), n := 1:.N, by = patid][n <= 2][ #index tests by patient and date, then keep two most recent
      , .(max = max(data2)), by = patid][ #identify max test value by patient
        max < 60, .(cond = 'CKD', ref = 'CKD147', eGFR = max), by = patid] #keep patients where test value is below 60mL/min
  
  
  #Use logic to derive final patient multimorbidity
  results <- rbindlist(list(cancer, clinical, therapy, eGFR), use.names = T, fill = T)[ #bind our medcode, prodcode & eGFR tables together
    , .(patid, ref, flag = 1)] %>% #restrict to required variables and add a flag variable in (to indicate the ref exists)
    dcast(., ... ~ ref, value.var = 'flag', fill = '0') %>% #cast table wide (where patients do not have a condition, fill with 0)
    .[, `:=` (PNC = ifelse(PNC166 != 0 | (PNC167 != 0 & EPI155 == 0), 1, 0), #include logic where necessary and flag condition...
              IBS = ifelse(IBS161 != 0 | IBS162 != 0, 1, 0),
              SCZ = ifelse(SCZ175 != 0 | SCZ176 !=0, 1, 0),
              ANX = ifelse(ANX140 != 0 | ANX141 !=0, 1, 0),
              DEP = ifelse(DEP152 != 0 | DEP153 !=0, 1, 0),
              PSO = ifelse(PSO171 != 0 & PSO172 !=0, 1, 0),
              EPI = ifelse(EPI155 != 0 & EPI156 !=0, 1, 0),
              AST = ifelse(AST127 != 0 & AST142 !=0, 1, 0),
              PNC166 = NULL, PNC167 = NULL, IBS161 = NULL, IBS162 = NULL, #get rid of all the ref columns used with logic rules 
              SCZ175 = NULL, SCZ176 = NULL, ANX140 = NULL, ANX141 = NULL,
              DEP152 = NULL, DEP153 = NULL, PSO171 = NULL, PSO172 = NULL,
              EPI155 = NULL, EPI156 = NULL, AST127 = NULL, AST142 = NULL)]
  
  
  #Tidying up, joining with full cohort and saving the results
  condrefs <- names(results) %>% tail(-1) %>% sort() #get column names, remove patid and order alphabetically
  condcodes <- substr(condrefs, 1, 3) #create new column names (using just the condition letters)
  setnames(results, condrefs, condcodes) #rename the condition columns to only use first three characters
  setcolorder(results, c('patid', condcodes)) #set the column order in results to alphabetical
  
  results[, conds := rowSums(.SD), .SDcols = 2:length(condcodes)] #add a count of how many conditions are flagged
  patient <- fread(here('Data', paste0('patient', i, '_', i + 1, '.csv')), select = 'patid') #read in patid from full cohort patient data
  results <- results[patient, on = .(patid = patid)] #merge with patient spine
  results[is.na(results)] <- 0 #set any missing values to zero (for patients with no conditions in results)
  
  savepath <- here('Data', paste0('camconds', i, '_', i + 1, '.rds')) #create full filepath to save to
  saveRDS(results, savepath) #save to file
  
  print(Sys.time() - starttime) #print the time taken (takes about 10 minutes per year of data in our project)
  gc() #clean up memory using a garbage collection

}





