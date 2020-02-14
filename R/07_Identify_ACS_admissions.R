
# =========================================================================================
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Identifying admissions for Ambulatory Care Sensitive (ACS) conditions
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

#_______________________________________________________________________

#Identifying potentially avoidable admissions due to ACS conditions ----

#This method is based on the paper by XXXXX available here:
#

#Process episodes to get admissions information
epis <- createDT(filename = 'hes_episodes_19_178.txt', keepcols = c('patid', 'epikey', 'spno', 'epistart', 'epiend', #create data table
        'admidate', 'discharged', 'eorder', 'epitype', 'admimeth', 'admisorc', 'classpat'), datecols = c('epistart',
          'epiend', 'admidate', 'discharged'))[
            epiend >= epistart & discharged >= admidate & eorder == 1][ #ensure dates make sense and restrict to first episode in spell
              !admisorc %in% c(51,52,53,98,99) & classpat == 1 & epitype == 1 & #remove admissions from wards/unknown admissions, ensure general/ordinary admission
              admimeth %in% c('21','22','23','24','25','28','2A','2B','2C','2D')][ #restrict to actual admissions (non-maternity)
                epistart >= as.Date('2016-11-01') & epistart < as.Date('2018-11-01')][ #restrict to spells starting in study follow-up
                , los := discharged - epistart][ #calculate spell duration
                 order(patid, epistart, -los)][ #sort data so longest duration for a patient admitted on the same day is at the top
                 , .SD[1], by = .(patid, epistart)][ #restrict to first record for each patient admission date combination
                  , .(patid, epikey, epistart)] #retain identifying fields for admission


diags <- createDT(filename = 'hes_diagnosis_epi_19_178.txt', keepcols = c('patid','spno','epikey','epistart', #create data table
          'ICD','ICDx','d_order'), datecols = 'epistart')[epis, on = .(patid, epikey, epistart)][ #join onto epis table of admissions
            , ICDn := substr(paste0(str_remove_all(ICD, '[:punct:]'), str_remove_all(ICDx, '[:punct:]')), 1, 4)][ #concatenate and clean ICD code fields
              d_order==1, `:=`(d4_1 = ICDn, d3_1 = substr(ICDn, 1, 3))][ #for primary diagnoses, store 4 character and 3 character ICD code
                order(patid, epikey, epistart, d_order)][ #sort the dataset by diagnosis order
                , d_cat := paste0(substr(ICDn,1,3), collapse = ','), by = .(patid, epikey, epistart)][ #create string of comma delimited diagnoses by admission
                  d_order == 1, .(patid, epikey, epistart, d4_1, d3_1, d_cat)] #retain one row per admission (using the primary diagnosis row)


procs <- createDT(filename = 'hes_procedures_epi_19_178.txt', keepcols = c('patid','spno','epikey','epistart', #create data table
          'OPCS','p_order'), datecols = 'epistart')[epis, on = .(patid, epikey, epistart)][ #join onto epis table of admissions
            , OP3 := substr(OPCS, 1, 3)][ #create 3 character operation code
              order(patid, epikey, epistart, p_order)][ #sort the dataset by procedure order
                , op_cat := paste0(OP3, collapse = ','), by = .(patid, epikey, epistart)][ #create string of comma delimited procedures by admission
                  p_order == 1, .(patid, epikey, epistart, op_cat)] #retain one row per admission (using the first procedure row)


cacs <- procs[diags, on = .(patid, epikey, epistart)][ #join the procedures onto the diagnoses by admission
          is.na(op_cat), op_cat := ''][ #for those rows with no procedures, replace NAs with empty strings
            , `:=`( #add various ACS categorisations...
                    a = d4_1 %in% c('B180','B181') & !grepl('D57', d_cat),
                    b = d3_1 == 'J45' | d4_1 == 'J46X',
                    c = (d3_1 == 'I50' | d4_1 %in% c('I110','J81X','I130')) & 
                        !grepl('K0|K1|K2|K3|K4|K50|K52|K55|K56|K57|K60|K61|K66|K67|K68|K69|K71', op_cat),
                    d = d3_1 %in% c('E10','E11','E12','E13','E14'),
                    e = d3_1 %in% c('J41','J43','J44') | d4_1 %in% c('J42X','J47X') |
                        (d3_1 == 'J20' & grepl('J41|J42|J43|J44|J47', d_cat)),
                    f = d3_1 %in% c('I20','I25') & !grepl('A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|V|W|X0|X1|X2|X4|X5', op_cat),
                    g = d3_1 %in% c('D51','D52') | d4_1 %in% c('D501','D508','D509'),
                    h = d4_1 %in% c('I10X','I119') & !grepl('K0|K1|K2|K3|K4|K50|K52|K55|K56|K57|K60|K61|K66|K67|K68|K69|K71', op_cat),
                    i = d3_1 %in% c('I48','G40','G41','F00','F01','F02','F03')
                  )][
              , acs := any(a,b,c,d,e,f,g,h,i), by = .(patid, epikey, epistart)][ #identify if any categorisations are TRUE
                linkcohort, on = .(patid)][ #join onto the original HES-linked cohort
                  , .(ACSadms = sum(acs, na.rm = TRUE)), by = .(patid)] #sum the ACS admissions by patid


saveRDS(cacs, './Analysis/ACSadmissions.rds') #save ACS admissions as RDS file




  