
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Create linked HES analysis dataset
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'purrr', 'data.table') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source('filepaths.R') #get folder path for linked HES data: linkdata

#__________________________________________

#Read in cohort of HES linked patients ----

trends <- readRDS(here('Analysis', 'Processed_data', 'CPRDtrends.rds')) #read in CPRDtrends dataset

HESpats <- trends[studyyear == '2015_2016' & hes_e == 1, .(patid, dod, hes_e, studyyear)][ #restrict to HES-linked patids for 2015/16 cohort
              dod > as.Date('2016-11-01') | is.na(dod)][ #dates of death must be greater than the start of the follow-up period (or NA)
                dod > as.Date('2018-11-01') | is.na(dod), dod := as.Date('2018-11-01')][ #make NA death dates & those after study end equal study end
                , studydays := 365*2 - (as.Date('2018-11-01') - dod)][ #calculate time in study in days
                  , studyyears := round(studydays/365, 3)] #add study time in years

#_________________________________________________________

#Function for reading in data and editing date fields ----

createDT <- function(filename, keepcols, datecols){
  dt <- fread(paste0(linkdata, filename), select = keepcols) #read data file keeping required columns
  for (i in datecols){ #for each date field...
    dt[, c(i):=.(as.Date(get(i), format = '%d/%m/%Y'))] #format as a date type
  } 
  return(dt) #output data table
}
#_______________________________________

#APC - admitted patient care spells ----

apc_diag <- createDT(filename = 'hes_diagnosis_epi_19_178.txt', keepcols = c('patid', 'epikey', 'ICD', 'd_order'), datecols = NULL)[ #create data table
              , `:=`(ICDch = substr(ICD, 1, 1), ICDsec = substr(ICD, 2, 3))][ #create ICD chapter and section fields
                , `:=`(APC_mh_diag = ((ICDch == 'F') & (d_order==1)) | #create flag for psychiatric diagnoses (1) Chp.F + first diagnosis, or...
                                ((ICDch == 'X') & (ICDsec %between% c(60,84))) | #(2) Chp.X + Sec.60 to 84 , or...
                                ((ICDch == 'Y') & (ICDsec %between% c(10,34)) & ICD != 'Y33.9'))][ #(3) Chp.Y + Sec.10 to 34 + ICD NOT 'Y33.9'
                  , lapply(.SD, any, na.rm =TRUE), by = .(patid, epikey), .SDcols = c('APC_mh_diag')] #identify episodes where any APC_mh_diag is TRUE

apc_admis <- createDT(filename = 'hes_episodes_19_178.txt', keepcols = c('patid', 'admidate', 'epistart', 'epiend','epikey', #create data table
        'discharged', 'eorder', 'admimeth', 'admisorc', 'classpat', 'tretspef'), datecols = c('admidate', 'epistart', 'epiend', 'discharged'))[
            epiend >= epistart & discharged >= admidate & eorder == 1][ #ensure dates make sense and restrict to first episode in spell (i.e. admission)
              !admisorc %in% c(51,52,53,98,99) & !admimeth %in% c('81', '2B')][ #remove admissions from wards, unknown admissions, and transfers
                epistart >= as.Date('2016-11-01') & epistart < as.Date('2018-11-01')] #restrict to spells starting in study follow-up

apc <- apc_diag[apc_admis, on = .(patid, epikey)][ #join previously categorised diagnoses onto admissions by patid and epikey
                  , `:=`(APCemerg = (admimeth %in% c('21','22','23','24','25','2A','2D')), #categorise the admissions... emergency
                         APCelec = (admimeth %in% c('11','12','13') & classpat %in% c(1,4)), #elective
                         APCmat = (admimeth %in% c('31','32','2C','82','83')), #maternity
                         APCday = (admimeth %in% c('11','12','13') & classpat %in% c(2,3)), #day case
                         APCoth = (admimeth == '28'), #other
                         APClos = (discharged - epistart), #calculate length of stay
                         APCmh_spells = ((tretspef %between% c(700,730)) | APC_mh_diag == TRUE), #identify mental health spells
                         APCmh_los = ifelse(((tretspef %between% c(700,730)) | APC_mh_diag == TRUE), (discharged - epistart), NA))][ #calculate spell duration
                    order(patid, epistart, -APClos)][ #sort data so longest duration for a patient admitted on the same day is at the top
                      , .SD[1], by = .(patid, epistart)][ #restrict to first record for each patient admission date combination
                        HESpats, on = .(patid)][ #join onto HES-linked cohort
                          , lapply(.SD, sum, na.rm = TRUE), by = .(patid), #sum categorised admissions and spell durations by patid 
                            .SDcols = c('APCemerg','APCelec','APCmat','APCday','APCoth','APClos', 'APCmh_spells', 'APCmh_los')][
                              , APCspells := sum(APCemerg, APCelec, APCmat, APCday, APCoth), by = .(patid)] #add total of categorised spells
 
#____________________________________________

#AE - accident and emergency attendances ----

ae_diag <-createDT(filename = 'hesae_diagnosis_19_178.txt', keepcols = c('patid', 'aekey', 'diag2', 'diagscheme'), datecols = NULL)[ #create data table
            diagscheme == 1][, `:=`(AEpsychdiag = (diag2 == 35))][ #restrict to those using A&E diagnosis scheme (~99%) and identify psychiatric diagnoses
              , lapply(.SD, any, na.rm = TRUE), by = .(patid, aekey), .SDcols = c('AEpsychdiag')] #identify attendances where any diagnosis is psychiatric

ae_attend <- HESpats[createDT(filename = 'hesae_attendance_19_178.txt', keepcols = c('patid','aekey', 'arrivaldate', #create data table 
              'aeattenddisp', 'aeattendcat', 'aepatgroup'), datecols = 'arrivaldate'), on = .(patid)][ #join on HESpats by patid
                is.na(dod) | arrivaldate <= as.Date(dod, format = '%d/%m/%Y')][ #make sure attendances are before any deaths
                  arrivaldate >= as.Date('2016-11-01') & arrivaldate < as.Date('2018-11-01')][ #restrict to attendances in study follow-up
                    , `:=`(AEseen = (aeattenddisp %in% c(1:11,14)), #categorise attendances... seen (i.e. did not leave before being seen)
                      AEplan = (aeattendcat == 2), #planned follow-up attendance
                      AEunplan = (aeattendcat %in% c(1,3)), #initial attendance or unplanned follow-up attendance
                      AEoth = (aeattendcat == 9), #other (attendance where it is not known if it was initial or planned)
                      AEselfharm = (aepatgroup == 30))] #reason for attendance is deliberate self-harm

ae <- ae_diag[ae_attend, on = .(patid, aekey)][ #join diagnoses onto attendances
        , AEmh_att := any(AEpsychdiag, AEselfharm, na.rm = TRUE), by = .(patid, aekey)][ #identify any mental health attendances by diagnosis or self-harm reason
          HESpats, on = .(patid)][ #join onto HES-linked cohort
            , lapply(.SD, sum, na.rm = TRUE), by = .(patid), #sum categorised attendances by patient
              .SDcols = c('AEseen','AEplan','AEunplan','AEoth','AEmh_att')][
                , AEatts := sum(AEplan, AEunplan, AEoth), by = .(patid)] #add total of categorised attendances 
                      
#_________________________________

#OP - outpatient appointments ----

op_clin <- createDT(filename = 'hesop_clinical_19_178.txt', keepcols = c('patid', 'attendkey', 'tretspef'), datecols = NULL) #create data table

op_app <- HESpats[createDT(filename = 'hesop_appointment_19_178.txt', keepcols = c('patid', 'attendkey', 'apptdate', #create data table
            'firstatt', 'attended'), datecols = 'apptdate'), on = .(patid)][ #join on HESpats by patid
              apptdate >= as.Date('2016-11-01') & apptdate < as.Date('2018-11-01')][ #restrict to appointments in study follow-up period
                is.na(dod) | apptdate <= as.Date(dod, format = '%d/%m/%Y')] #make sure appointments are before any deaths
                        
op <- opclin[op_app, on = .(patid, attendkey)][ #merge clinical data onto appointments (as we need the tretspef field)
        tretspef != '&' & !firstatt %in% c('X','9') & attended != 9][ #remove appointments with 'Not known' values
          , `:=`(OPatts = (attended %in% c(5,6)), #categorise attendances... attended appointment
                 OPmiss = (attended %in% c(2,3,4,7)), #missed appointment
                 OPmh_atts = (attended %in% c(5,6) & tretspef %between% c(700,730)), #attended mental health appointment
                 OPmh_miss = (attended %in% c(2,3,4,7) & tretspef %between% c(700,730)))][ #missed mental health appointment
            HESpats, on = .(patid)][ #join onto HES-linked cohort
              , lapply(.SD, sum, na.rm =TRUE), by = .(patid), .SDcols = c('OPatts','OPmiss','OPmh_atts','OPmh_miss')][ #sum categorised appointments by patient
                , OPappts := sum(OPatts, OPmiss), by = .(patid)] #add total of categorised appointments

#___________________________________________

#Join together, adjust and save to file ----

hes_outcomes <- HESpats[apc, on = .(patid)][ae, on = .(patid)][op, on = .(patid)] #join everything onto the HES-linked cohort

#list names of columns to be adjusted by time in follow-up
cols <- c('APCemerg','APCelec','APCmat','APCday','APCoth','APClos','APCmh_spells','APCmh_los', 'APCspells','AEseen','AEplan',
          'AEunplan','AEoth','AEmh_att','AEatts','OPatts','OPmiss','OPmh_atts','OPmh_miss','OPappts')

#Add variables adjusted for length of time in follow-up (to give 'per year' values)
hes_outcomes <- hes_outcomes[, (paste0(cols, '_adj')) := lapply(.SD, function(x) x / as.numeric(hes_outcomes[['studyyears']])), .SDcols = cols]

#Save outcomes
saveRDS(hes_outcomes, here('Analysis', 'Processed_data', 'HESoutcomes.rds')) #merge and save the HES outcomes



