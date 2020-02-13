
# =========================================================================================
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Create linked HES analysis dataset
# Author: Dr Will Parry
# =========================================================================================

#Setup - Load required packages and set directory/folders ----

pkgs <- c('data.table', 'stringr', 'purrr') #package list
lapply(pkgs, library, character.only=T) #load packages
setwd('C:/MyProject/data/HES') #set working directory to where HES extracts are stored

#___________________________________________________

#Read in HES data, edit and save using function ----

linkcohort <- fread("19_178_linkage_eligibility_gold.txt")[ #read in CPRD cohort patids and dates of death
				hes_e == 1, .(patid)] #limit to those CPRD patients successfully linked to HES
dod <- fread("death_patient_19_178.txt")[,.(patid, dod)] #read in ONS death dates


#Function for reading in HES data and editing date fields
createDT <- function(filename, keepcols, datecols){
  dt <- fread(filename, select = keepcols) #read data file keeping required columns
  for (i in datecols){ #for each date field...
    dt[, c(i):=.(as.Date(get(i), format = '%d/%m/%Y'))] #format as a date type
  } 
  return(dt) #output data table
}

#_______________________________________

#APC - admitted patient care spells ----

apc <- createDT(filename = 'hes_episodes_19_178.txt', keepcols = c('patid', 'admidate', 'epistart', 'epiend', #create data table
        'discharged', 'eorder', 'epidur', 'admimeth', 'admisorc', 'classpat'), 
          datecols = c('admidate', 'epistart', 'epiend', 'discharged'))[
            epiend >= epistart & discharged >= admidate & eorder == 1][ #ensure dates make sense and restrict to first episode in spell
              !admisorc %in% c(51,52,53,98,99) & !admimeth %in% c('81', '2B')][ #remove admissions from wards, unknown admissions, and transfers
                epistart >= as.Date('2016-11-01') & epistart < as.Date('2018-11-01')][ #restrict to spells starting in study follow-up
                  , `:=`(APCemerg = (admimeth %in% c('21','22','23','24','25','2A','2D')), #categorise admissions...
                         APCelec = (admimeth %in% c('11','12','13') & classpat %in% c(1,4)),
                         APCmat = (admimeth %in% c('31','32','2C','82','83')),
                         APCday = (admimeth %in% c('11','12','13') & classpat %in% c(2,3)),
                         APCoth = (admimeth == '28'),
                         APClos = (discharged - epistart))][ #calculate spell duration
                    order(patid, epistart, -APClos)][ #sort data so longest duration for a patient admitted on the same day is at the top
                      , .SD[1], by = .(patid, epistart)][ #restrict to first record for each patient admission date combination
                        linkcohort, on = .(patid)][ #join onto HES-linked cohort
                          , lapply(.SD, sum, na.rm = TRUE), by = .(patid), #sum categorised admissions and spell durations by patid 
                            .SDcols = c('APCemerg','APCelec','APCmat','APCday','APCoth','APClos')][
                              , APCspells := sum(APCemerg, APCelec, APCmat, APCday, APCoth), by = .(patid)] #add total of categorised spells

#____________________________________________

#AE - accident and emergency attendances ----

ae <- dod[createDT(filename = 'hesae_attendance_19_178.txt', keepcols = c('patid', 'arrivaldate', 'aeattenddisp', 'aeattendcat'), #create data table
            datecols = 'arrivaldate'), on = .(patid)][ #join dates of death on by patid (see start of previous line)
              is.na(dod) | arrivaldate <= as.Date(dod, format = '%d/%m/%Y')][ #make sure attendance dates are not after death dates
                arrivaldate >= as.Date('2016-11-01') & arrivaldate < as.Date('2018-11-01')][ #restrict to attendances in study follow-up
                , `:=`(AEseen = (aeattenddisp %in% c(1:11,14)), #categorise attendances...
                       AEplan = (aeattendcat == 2),
                       AEunplan = (aeattendcat %in% c(1,3)),
                       AEoth = (aeattendcat == 9))][
                  linkcohort, on = .(patid)][ #join onto HES-linked cohort
                  , lapply(.SD, sum, na.rm = TRUE), by = .(patid), #sum categorised attendances
                    .SDcols = c('AEseen','AEplan','AEunplan','AEoth')][
                    , AEatts := sum(AEplan, AEunplan, AEoth), by = .(patid)] #add total of categorised attendances

#_________________________________

#OP - outpatient appointments ----

opclin <- createDT(filename = 'hesop_clinical_19_178.txt', keepcols = c('patid', 'attendkey', 'tretspef'), datecols = NULL) #create data table

opapp <- dod[createDT(filename = 'hesop_appointment_19_178.txt', keepcols = c('patid', 'attendkey', 'apptdate', 'firstatt', #create data table
				'attended'), datecols = 'apptdate'), on = .(patid)][ #join dates of death on by patid (see start of previous line)
                        apptdate >= as.Date('2016-11-01') & apptdate < as.Date('2018-11-01')][ #restrict to appointments in study follow-up period
                          is.na(dod) | apptdate <= as.Date(dod, format = '%d/%m/%Y')] #make sure appointments are before any deaths
                        
op <- opclin[opapp, on = .(patid, attendkey)][ #merge clinical data on to the appointments data (so that we have the tretspef field)
        tretspef != '&' & !firstatt %in% c('X','9') & attended != 9][ #remove appointments with 'Not known' values
          , `:=`(OPatts = (attended %in% c(5,6)), OPmiss = (attended %in% c(2,3,4,7)))][ #categorise appointments as attended or missed
            linkcohort, on = .(patid)][ #join onto HES-linked cohort
              , lapply(.SD, sum, na.rm =TRUE), by = .(patid), .SDcols = c('OPatts','OPmiss')][ #sum categorised appointments
                , OPappts := sum(OPatts, OPmiss), by = .(patid)] #add total of categorised appointments

#_________________________

#Save merged outcomes ----

saveRDS(apc[op, on = .(patid)][ae, on = .(patid)], '../../HESoutcomes.rds') #merge and save all the HES outcomes to main project folder



