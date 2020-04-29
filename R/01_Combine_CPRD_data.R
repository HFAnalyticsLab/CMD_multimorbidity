
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Combining CPRD data extracts to create single CSV files by type of dataset
# Author: Dr Will Parry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup - Load required packages and set directory/folders ----

pkgs <- c('here', 'data.table', 'tidyverse') #package list
lapply(pkgs, library, character.only=T) #load packages

here() #check here sees root directory for project
source('filepaths.R') #get folder paths for raw data: (1) rawCPRDdata, (2) clin_medcodes0809
datafolders <- dir(path = rawCPRDdata, pattern = 'D042.\\d_extract', full.names = TRUE) #create list of data folders
#we have several folders for different years of data which are numbered (\\d is regex pattern for single digit)

#____________________________________________________________________________

#Create function to read and bind data tables together depending on type ----

createDT <- function(type, datafolder, saveas, keepcols){
  savepath <- here('Data', paste0(saveas, str_sub(datafolder, -9), '.csv')) #build savepath (last 9 chars of folder names are years: 2008-2009, etc.)
  flist <- type %>% list.files(path = datafolder, pattern = ., full.names = T) #list files in folder of type 
  flist %>% map(fread, select = keepcols) %>% rbindlist(fill = T) %>% #read files and bind them together
    fwrite(file = savepath) #write combined data to savepath
  gc() #clean up memory
}

#_________________________________________________________________________________

#Run function for various data types and years of data (separate datafolders) ----

#NB: this can take a very long time for types with many very large files. If your system does not have a lot of RAM, 
#it may not be able to cope (tops out at around 20GB in our project, but depends on amount of CPRD data to combine).
#Typically, the large CPRD extract types could have up to ten separate 1GB files to combine for each year.

for(i in datafolders){ #for each of the years of CPRD data contained in separate folders...
  
  start_time <- Sys.time() #record start time
  
  #we've commented out most of the types so that we can run them one at a time...
  
  createDT(type = 'Extract_Clinical', datafolder = i, saveas = 'clinical', #many very large files
            keepcols = c('patid', 'eventdate', 'medcode', 'adid'))
  #														
  # createDT(type = 'Extract_Additional', datafolder = i, saveas = 'additional', 
  #          keepcols = c('patid', 'enttype', 'adid', 'data1', 'data2', 'data3', 'data4', 'data5', 'data6', 'data7'))
  #
  # createDT(type = 'Extract_Consultation', datafolder = i, saveas = 'consult', #many very large files
  #          keepcols = c('patid', 'eventdate', 'constype', 'staffid', 'duration'))
  # 
  # createDT(type = 'Extract_Patient', datafolder = i, saveas = 'patient',
  #          keepcols = c('patid', 'gender', 'yob', 'crd', 'tod', 'toreason', 'accept'))
  # 
  # createDT(type = 'Extract_Referral', datafolder = i, saveas = 'referral',
  #          keepcols = c('patid', 'eventdate', 'constype', 'medcode', 'source'))
  # 
  # createDT(type = 'Extract_Staff', datafolder = i, saveas = 'staff',
  #          keepcols = c('staffid', 'role'))
  # 
  # createDT(type = 'Extract_Test', datafolder = i, saveas = 'test', #many very large files
  #         keepcols = c('patid', 'eventdate', 'enttype', 'data2'))
  # 
  # createDT(type = 'Extract_Therapy', datafolder = i, saveas = 'therapy', #many very large files
  #         keepcols = c('patid', 'eventdate', 'prodcode'))
  # 
  # createDT(type = 'Extract_Practice', datafolder = i, saveas = 'practice',
  #         keepcols = c('pracid', 'region', 'lcd', 'uts'))
  
  print(Sys.time() - start_time) #time taken to read and save all data for one year ~30 minutes on our system
  
}

#________________________________________________________________

#Addendum: additional code for dealing with a faulty extract ----

#Below is an adhoc section of additional code used to add medcodes to one year of clinical data. This is because
#the medcode column was missing in the original extract and was later extracted separately from CPRD

clin <- fread(here('Data', 'clinical2008_2009.csv')) #read in problematic clinical file created before using createDT
names(clin) #check names of fields

clin2 <- list.files(clin_medcodes0809, pattern = 'Extract_Clinical', full.names = TRUE) %>% #list files in clin_medcodes0809 folder
  map(fread) %>% rbindlist(fill = T) #read and bind them together
names(clin2) #check names of fields

clin3 <- cbind(clin, clin2) #bind datasets together
names(clin3) #check names of fields
names(clin3) <- c('patid', 'eventdate', 'patid2', 'medcode') #rename fields so patid is not in there twice
nrow(clin)==nrow(clin2)==nrow(clin3) #check number of rows

clin3[, check := (patid==patid2)][, .(count = .N), by = .(check)] #quick check that records align

fwrite(clin3[, .(patid, eventdate, medcode)], here('Data', 'clinical2008_2009.csv')) #write to file
rm(clin, clin2, clin3) #remove large data objects
gc() #clean up memory





