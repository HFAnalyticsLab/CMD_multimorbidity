
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Count potentially avoidable emergency admissions for ACS conditions
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#NB: this method is based on the NHS Digital CCG Outcome Indicator Set: Indicator 2.6 v1.15
#However, we did not have the EPISTAT field in our linked HES data (so it is ignored)
#At the time of coding, the document detailing Indicator 2.6 was available at:
#https://files.digital.nhs.uk/BB/6DD6C7/CCG_2.6_I00757_S.pdf

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'data.table', 'stringr') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source('filepaths.R') #get folder path for linked HES data: linkdata

#___________________________________________________

#Read in cohort of HES linked patients ----

trends <- readRDS(here('Analysis', 'Processed_data', 'CPRDtrends.rds')) #read in CPRDtrends dataset

HESpats <- trends[studyyear == '2015_2016' & hes_e == 1, .(patid, dod, hes_e, studyyear)][ #restrict to HES-linked patids for 2015/16 cohort
  dod > as.Date('2016-11-01') | is.na(dod)][ #dates of death must be greater than the start of the follow-up period (or NA)
    dod > as.Date('2018-11-01') | is.na(dod), dod := as.Date('2018-11-01')][ #make NA death dates & those after study end equal study end
      , studydays := 365*2 - (as.Date('2018-11-01') - dod)][ #calculate time in study in days
        , studyyears := round(studydays/365, 3)] #add study time in years

#_____________________________________________________________

#Function for reading in HES data and editing date fields ----

createDT <- function(filename, keepcols, datecols){
  dt <- fread(paste0(linkdata, filename), select = keepcols) #read data file keeping required columns
  for (i in datecols){ #for each date field...
    dt[, c(i):=.(as.Date(get(i), format = '%d/%m/%Y'))] #format as a date type
  } 
  return(dt) #output data table
}
#____________________________________________

#Get spell information from APC episodes ----

epis <- createDT(filename = 'hes_episodes_19_178.txt', keepcols = c('patid', 'epikey', 'spno', 'epistart', 'epiend', #create data table
        'admidate', 'discharged', 'eorder', 'epitype', 'admimeth', 'admisorc', 'classpat'), 
          datecols = c('epistart', 'epiend', 'admidate', 'discharged'))[
            epiend >= epistart & discharged >= admidate & eorder == 1][ #ensure dates make sense and restrict to first episode in spell
              !admisorc %in% c(51,52,53,98,99) & classpat == 1 & epitype == 1 & #exclude transfers/unknowns, include only ordinary/general admissions
                admimeth %in% c('21','22','23','24','25','28','2A','2B','2C','2D')][ #restrict to emergency admissions only
                  epistart >= as.Date('2016-11-01') & epistart < as.Date('2018-11-01')][ #restrict to spells starting in study follow-up
                    , los := discharged - epistart][ #calculate spell duration
                      order(patid, epistart, -los)][ #sort data so longest duration for a patient admitted on the same day is at the top
                        , .SD[1], by = .(patid, epistart)][ #restrict to first record for each patient admission date combination
                          , .(patid, epikey, epistart)] #retain spell identifiers only

#_______________________________________

#Get diagnosis information from APC ----

diags <- createDT(filename = 'hes_diagnosis_epi_19_178.txt', keepcols = c('patid','spno','epikey','epistart', #create data table
          'ICD','ICDx','d_order'), datecols = 'epistart')[epis, on = .(patid, epikey, epistart)][ #join onto spell information
            , ICDn := substr(paste0(str_remove_all(ICD, '[:punct:]'), str_remove_all(ICDx, '[:punct:]')), 1, 4)][ #create punctuation-free ICD string
              d_order == 1, `:=`(d4_1 = ICDn, d3_1 = substr(ICDn, 1, 3))][ #where record is a primary diagnosis, create 4 and 3-character versions of ICD code
                order(patid, epikey, epistart, d_order)][ #order the data by episode and diagnosis order
                , d_cat := paste0(substr(ICDn,1,3), collapse = ','), by = .(patid, epikey, epistart)][ #create string of 3-character diagnosis codes by spell
                  d_order == 1, .(patid, epikey, epistart, d4_1, d3_1, d_cat)] #restrict to one record per patient-spell and retain required fields

#________________________________________

#Get operations information from APC ----

procs <- createDT(filename = 'hes_procedures_epi_19_178.txt', keepcols = c('patid','spno','epikey','epistart', #create data table
          'OPCS','p_order'), datecols = 'epistart')[epis, on = .(patid, epikey, epistart)][ #join onto spell information
            , OP3 := substr(OPCS, 1, 3)][ #create 3-character operation code
              order(patid, epikey, epistart, p_order)][ #order the data by episode and operation order
                , op_cat := paste0(OP3, collapse = ','), by = .(patid, epikey, epistart)][ #create string of 3-character operation codes
                  p_order == 1, .(patid, epikey, epistart, op_cat)] #restrict to one record per patient-spell and retain required fields

#____________________________________________________

#Identify potentially preventable ACS admissions ----

acs <- procs[diags, on = .(patid, epikey, epistart)][ #join the procedures onto the diagnoses
          is.na(op_cat), op_cat := ''][ #replace missing values of op_cat with a zero-length string
            , `:=`( #create categorisations for logical conditions that meet indicator...
                    a = d4_1 %in% c('B180','B181') & !grepl('D57', d_cat),
                    b = d3_1 == 'J45' | d4_1 == 'J46X',
                    c = (d3_1 == 'I50' | d4_1 %in% c('I110','J81X','I130')) & 
                        !grepl('K0|K1|K2|K3|K4|K50|K52|K55|K56|K57|K60|K61|K66|K67|K68|K69|K71', op_cat),
                    d = d3_1 %in% c('E10','E11','E12','E13','E14'),
                    e = d3_1 %in% c('J41','J43','J44') | d4_1 %in% c('J42X','J47X') |
                        (d3_1 == 'J20' & grepl('J41|J42|J43|J44|J47', d_cat)),
                    f = d3_1 %in% c('I20','I25') & !grepl('A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|V|W|X0|X1|X2|X4|X5', op_cat),
                    g = d3_1 %in% c('D51','D52') | d4_1 %in% c('D501','D508','D509'),
                    h = d4_1 %in% c('I10X','I119') & !grepl('K0|K1|K2|K3|K4|K50|K52|K55|K56|K57|K60|K61|K66|K67|K68|K69|K71|K73|K74', op_cat),
                    i = d3_1 %in% c('I48','G40','G41','F00','F01','F02','F03')
                  )][
              , acs := any(a,b,c,d,e,f,g,h,i), by = .(patid, epikey, epistart)][ #check if any of the conditions are met by patient-spell
                HESpats, on = .(patid)][ #join onto HES-linked patients
                  , .(ACSadms = sum(acs, na.rm = TRUE)), by = .(patid)] #sum ACS admissions by patient

#Save ACS admissions as RDS file
saveRDS(acs, here('Analysis', 'Processed_data', 'ACSadmissions.rds'))




  