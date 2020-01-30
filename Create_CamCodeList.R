
# =========================================================================================
# Project: CMD_multimorbidity - Common mental disorders and additional long-term conditions
# Purpose: Preparing code list for use in applying the Cambridge Multimorbidity Score
# Author: Dr Will Parry
# =========================================================================================

#Setup - Load required packages and set directory/folders ----

pkgs <- c('xml2', 'rvest', 'stringr', 'purrr', 'data.table') #package list
lapply(pkgs, library, character.only=T) #load packages

setwd('C:/MyProject/') #set the working directory for your project
savedir <- './data/Cambridge_files/' #folder for saving Cambridge reference files
dir.create(savedir, recursive = TRUE) #create folder if it doesn't exist

#_________________________________________________________________________________________

#Access the web page containing the Cambridge code lists and find the file hyperlinks ----

camsite <- 'https://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/v11/' #NB: location could change
filelinks <- read_html(camsite) %>% #read Cambridge code list web page
  html_nodes("a") %>% html_attr("href") %>% #find all links and get URLs
  str_subset("zip") %>% .[1:46] #restrict to zip files for 'Current Code Lists on 38 Common Conditions'

#_______________________________________

#Download and extract the zip files ----

files <- str_split(filelinks, "/V11/", simplify=T)[,2] #split the online paths to extract the zip filenames
saveas <- map2_chr(savedir, files, paste0) #create filepaths to save to savedir
map2(filelinks, saveas, download.file) #download the files from cambridge and save
map(saveas, unzip) #unzip the zip files
map(saveas, unlink) #delete the original zip files

#________________________________________________________________________

#Create a single long data table of codes for the various conditions ----

descriptions <- list.files(savedir, '*DESCRIPTION*', full.names=T) %>% #list files with DESCRIPTION in the filename
  map(fread) %>% rbindlist(use.names=T, fill=T) %>% #read them all and bind them together
  .[, .SD[1], by = SCHEMA_NUMBER] %>% #take first row for each schema number to remove duplicates
  .[, codes:=strsplit(ALLCODES, split = ';', fixed = T)] %>% #split ALLCODES into vector of codes
  .[, ref:=paste0(`CONDITION CODE`, SCHEMA_NUMBER)] #create a reference code from the condition code and schema number

logicpath <- 'https://raw.githubusercontent.com/HFAnalyticsLab/CMD_multimorbidity/master/code_lists/Cam_UD_based_logic.csv'
download.file(logicpath, './data/Cam_UD_based_logic.csv') #download logic file
logic <- fread('./data/Cam_UD_based_logic.csv') #read into data table

#'Cam_UD_based_logic.csv' was created from unique values of the USAGE DEFINITION field in the Cambridge descriptions
#The following additional fields were added...
#read - contains the number of days of history to include when looking for read codes 
#Rx - contains the number of prescriptions required as defined by the prod codes
#logic - contains a logical operator for how to combine read and prod criteria
#special - an aide-memoire, identifies conditions where the rules are unusual and must be dealt with separately

descriptions <- descriptions %>% merge(logic, all.x = T) #merge in logic rules

codelist <- with(descriptions, 
						list(cond = `CONDITION CODE`, #create list of inputs to make long data table
							ref = ref, 
							type = TYPE, 
							code = codes, 
							read = read, 
							Rx = Rx,
							logic = logic,
							special = special, 
							ud = `USAGE DEFINITION`)) %>% 
    pmap_dfr(data.table) #send inputs to pmap_dfr to create and join data tables together

codelist[, code:=as.numeric(code)] #convert code to numeric variable (previously character)

saveRDS(codelist, './data/CamCodeList.rds') #save to data directory
#nrow(codelist) == nrow(unique(codelist)) #there seem to be a few repetitions in the codelist?

