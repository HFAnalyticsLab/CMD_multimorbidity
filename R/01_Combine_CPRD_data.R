
# =========================================================================================
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Combining CPRD data extracts to create single CSV files by type of dataset
# Author: Dr Will Parry
# =========================================================================================

#Setup - Load required packages and set directory/folders ----

pkgs <- c('data.table', 'stringr', 'purrr') #package list
lapply(pkgs, library, character.only=T) #load packages

setwd('C:/MyProject/data/') #set the data directory for your project
datadir <- './CPRD_raw/' #change this to where your downloaded CPRD extracts are stored
datafolders <- paste('CPRD_extract', 2008:2017, 2009:2018, sep = '_') #create list of data folders 
#(in our project, we have separate CPRD extract folders for each cohort year between 2008/09 and 2017/18)

#____________________________________________________________________________

#Create function to read and bind data tables together depending on type ----

createDT <- function(type, datafolder, saveas, keepcols){

  savepath <- paste0(saveas, str_sub(datafolder, -9), '.csv') #build savepath from saveas and year suffix
  
  type %>% list.files(path = paste0(datadir, datafolder), pattern = ., full.names = T) %>% #list files in folder of type 
    map(fread, select = keepcols) %>% rbindlist(fill = T) %>% #read files and bind them together
	  fwrite(file = savepath) #write combined data to savepath

}
#_________________________________________________________________________________

#Run function for various data types and years of data (separate datafolders) ----

#NB: this can take a long time for types with many very large files. If your system does not have a lot of RAM, 
#it may not be able to cope (tops out at around 20GB in our project, but depends on amount of CPRD data to combine).
#Typically, our CPRD extract had up to ten 1GB files in each cohort year, for large datasets such as consultations or therapy.

for(i in datafolders){ #for each of the years of CPRD data contained in separate folders...
  
  start_time <- Sys.time() #record start time
  
  createDT(type = '*Extract_Clinical*', datafolder = i, saveas = 'clinical', #use wildcards to find files of a particular type
           keepcols = c('patid', 'eventdate', 'medcode'))
  
  createDT(type = '*Extract_Consultation*', datafolder = i, saveas = 'consult', 
           keepcols = c('patid', 'eventdate', 'constype', 'staffid', 'duration'))

  createDT(type = '*Extract_Patient*', datafolder = i, saveas = 'patient', 
           keepcols = c('patid', 'gender', 'yob', 'crd', 'tod', 'toreason', 'accept'))

  createDT(type = '*Extract_Referral*', datafolder = i, saveas = 'referral', 
           keepcols = c('patid', 'eventdate', 'constype', 'medcode', 'source'))

  createDT(type = '*Extract_Staff*', datafolder = i, saveas = 'staff', 
           keepcols = c('staffid', 'role'))

  createDT(type = '*Extract_Test*', datafolder = i, saveas = 'test', 
           keepcols = c('patid', 'eventdate', 'enttype', 'data2'))

  createDT(type = '*Extract_Therapy*', datafolder = i, saveas = 'therapy', 
           keepcols = c('patid', 'eventdate', 'prodcode'))

  print(Sys.time() - start_time) #time taken to read and save all data for one year ~30 minutes in our project
  
}



