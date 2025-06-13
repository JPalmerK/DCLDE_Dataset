# Collate annotations for DCLDE 2026 data

rm(list =ls())
library(lubridate)
library(dplyr)

# Load the QAQC function
source('/TestFx.R')

# Data sets to collate
# 
# 1) Ocean Networks Canada 
# 2) Viers
# 3) DFO Cetacean Research Program (Pilkington)
# 4) DFO Whale Detection and Localization (Yerk)
# 5) SMRU
# 6) VPFA
# 7) Scripps
# 8) UAF

############################################################################
# Final output column names
#
colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider',
           'AnnotationLevel', 'FilePath', 'FileOk')

# Options for class/species column, killer whales, humpbacks, abiotic sounds, 
# unidentified biological sounds

ClassSpeciesList = c('KW', 'HW', 'AB', 'UndBio')

# Acceptable options for annotation levels
AnnotationLevelList = c('File', 'Detection', 'Call')

# Acceptable options for ecotype/population
EcotypeList = c('SRKW', 'TKW', 'OKW', 'NRKW', 'SAR')



###############################################################################
# 1) ONC- 


ONC_anno = read.csv('E:/DCLDE/ONC/Annotations/ONC_annotations.csv')
ONC_anno$ClassSpecies = ONC_anno$Species
ONC_anno$origEcotype = ONC_anno$Ecotype# Reclass for use later
ONC_anno$Ecotype = NA

ONC_anno$KW_certain = NA
# Antying with an 'or' in the species goes to unidentified biological sounds
ONC_anno$ClassSpecies[grepl("\\|", ONC_anno$Species)]= 'UndBio'

# Humpbacks to humpback
ONC_anno$ClassSpecies[grepl("Mn", ONC_anno$Species)]= 'HW'

# Orcas to killer whales
ONC_anno$ClassSpecies[grepl("Oo", ONC_anno$Species)]= 'KW'

# Basic Info
ONC_anno$Provider= 'ONC'
ONC_anno$AnnotationLevel = 'Call'

# Add Ecotype to the killer whales where provided
ONC_anno$Ecotype[ONC_anno$origEcotype == 'SRKW'] ='SRKW'
ONC_anno$Ecotype[ONC_anno$origEcotype == 'TKW'] ='TKW'
ONC_anno$Ecotype[ONC_anno$origEcotype == 'OKW'] ='OKW'
ONC_anno$Ecotype[ONC_anno$origEcotype == 'BKW'] ='TKW'

# Set the ecotype to NA for uniedentified biologcal sounds
ONC_anno$Ecotype[ONC_anno$ClassSpecies== 'UndBio']= NA

# No unsure killer whale included, so all KW are labeled as certian
ONC_anno$KW = ifelse(ONC_anno$ClassSpecies=='KW',1,0)

# Set time
ONC_anno$FileBeginSec= as.numeric(ONC_anno$Left.time..sec.)
ONC_anno$FileEndSec= as.numeric(ONC_anno$Right.time..sec.)
ONC_anno$HighFreqHz= as.numeric(ONC_anno$Top.freq..Hz.)
ONC_anno$LowFreqHz= as.numeric(ONC_anno$Bottom.freq..Hz.)
ONC_anno$Dep='BarkleyCanyon'

# Numerous formatting errors in the timestamps from the origional annotations
# So deal with these by hand if present (some data culled at authors request
# so how relevent this is now is TBD)

ONC_anno <- ONC_anno %>%
  mutate(filename = as.character(Soundfile),  # Make sure the column is treated as character
         UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z).*", "\\1", filename),
                          format = "%Y%m%dT%H%M%S.%OSZ",  tz = 'UTC'))

# Get the time of the annotation
ONC_anno$UTC = ONC_anno$UTC+ seconds(ONC_anno$FileBeginSec)

ONC_anno$dur = as.numeric(ONC_anno$FileEndSec)  - 
  as.numeric(ONC_anno$FileBeginSec)
ONC_anno$end_time = ONC_anno$UTC+ seconds(ONC_anno$dur)

# Sort and then identify overlaps- some data were duplicated
ONC_anno <- ONC_anno %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))

# There are typos in the datates and files fix them all manually
badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20130615T062356.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20130615T062356', 
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.061)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140402T032959.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140402T032959', 
                                  format="%Y%m%dT%H%M%S", tz="UTC")+seconds(0.061)

# Was this date hand entered? Appears to be 60 rather than 06
badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140701T605216.144Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140701T065216',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.144)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140702T054216.391Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140702T054216',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.391)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140804T1345424.361Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140804T1345424',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.361)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140903T231409.061Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140903T231409',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.061)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141004T102246.170Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141004T102246',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.170)+ONC_anno$FileBeginSec[badidx]

badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141103T174455.370.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141103T174455',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.370)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140104T140150.099Z-HPF.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140104T140150',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.099)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20140202T131456.257Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20140104T140150',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.099)+ONC_anno$FileBeginSec[badidx]


badidx = which(ONC_anno$Soundfile=='ICLISTENHF1251_20141004T102246.170Z.wav')
ONC_anno$UTC[badidx] = as.POSIXct('20141004T102246',
                                  format="%Y%m%dT%H%M%S", tz="UTC")+
  seconds(0.170)+ONC_anno$FileBeginSec[badidx]


# Set the folder path and check that everyting is fine
dayFolderPath = 'E:/DCLDE/ONC/Audio/BarkleyCanyon'
ONC_anno$FilePath = file.path(dayFolderPath,
                              ONC_anno$Soundfile)

# Check all annotations have files associated
ONC_anno$FileOk  = file.exists(ONC_anno$FilePath)
if (all(ONC_anno$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


ONC_anno$FileUTC =ONC_anno$UTC- seconds(as.numeric(ONC_anno$FileBeginSec))


# Remove unnecessary columns
ONC_anno= ONC_anno[,colOut]

rm(list= c('missingData', 'badidx', 'dayFolderPath'))


runTests(ONC_anno, EcotypeList, ClassSpeciesList)


############################################################################
# DFO Cetacean Research Program
############################################################################

# No seconds in UTC
DFO_CRP1 = read.csv('E:/DCLDE/DFO_CRP/Annotations/annot_H50bjRcb_SM_det.csv')
DFO_CRP2 = read.csv('E:/DCLDE/DFO_CRP/Annotations/annot_KkHK0R2F_SM_det.csv')


DFO_CRP1$Dep='WVanIsl'
DFO_CRP2$Dep='NorthBC'

# Combine annotations
DFO_CRP = rbind(DFO_CRP1, DFO_CRP2)


DFO_CRP$UTC = as.POSIXct(DFO_CRP$utc, format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')+
  seconds((as.numeric(DFO_CRP$utc_ms)/1000))

table(DFO_CRP$sound_id_species)

DFO_CRP$ClassSpecies = DFO_CRP$sound_id_species

# Clean up the abiotic sounds. Uncharted wreck is new favorite annotations
DFO_CRP$ClassSpecies[
  DFO_CRP$ClassSpecies %in% c('Vessel Noise', 'Unknown', '', 'Mooring Noise', 
                              'Chain?', 'ADCP', 'Anchor Noise', 'Clang',
                              'Vessel Noise?', 'Chain','No sound data',
                              "Blast/Breach", "Fishing Gear", "Breach",
                              'Rubbing',"Anthro", "Nearby Uncharted Wreck",
                              'Water Noise', "Water Noise", "Mooring" ,
                              "Nothing","Mooring?", "Naval Sonar",
                              "Rocks?", "Rock?")] = 'AB'

# Add to KW class but set to uncertain later
DFO_CRP$ClassSpecies[DFO_CRP$ClassSpecies %in% c("KW/PWSD?", "HW/KW?","KW?")]=
  'KW'

# Add to humpback
DFO_CRP$ClassSpecies[DFO_CRP$ClassSpecies %in% c("HW/GW", "HW/PWSD","HW/PWSD?",
                                                 "NPRW?HW?", 'HW?', "HW/GW?")] ='HW'

DFO_CRP$ClassSpecies[!DFO_CRP$ClassSpecies %in% ClassSpeciesList] = 'UndBio'
DFO_CRP$KW = ifelse(DFO_CRP$ClassSpecies == 'KW', 1,0)

# Set up the uncertainty
DFO_CRP$KW_certain= NA
DFO_CRP$KW_certain[DFO_CRP$KW==1] =1
DFO_CRP$KW_certain[DFO_CRP$sound_id_species %in% c("KW/PWSD?","HW/KW?",
                                                   "KW?")]=0


# Add Ecotype- note uncertain ecotypes getting their own guess
DFO_CRP$Ecotype = as.factor(DFO_CRP$kw_ecotype)
levels(DFO_CRP$Ecotype)<-c(NA, 'NRKW', 'NRKW', 'OKW', 'OKW', 'SRKW', 'SRKW',
                            'TKW', 'TKW', NA)
DFO_CRP$Ecotype[DFO_CRP$KW ==0]<-NA

# If the species is unsure then the ecotype is unsure
DFO_CRP$Ecotype[DFO_CRP$KW_certain==0]<- NA

# If the species is unsure then the ecotype is unsure
DFO_CRP$Ecotype[DFO_CRP$KW_certain==0]<- NA

# Fill out rest of the columns
DFO_CRP$Soundfile = DFO_CRP$filename
DFO_CRP$FileBeginSec = DFO_CRP$start
DFO_CRP$FileEndSec = DFO_CRP$end
DFO_CRP$LowFreqHz = DFO_CRP$freq_min
DFO_CRP$HighFreqHz = DFO_CRP$freq_max

DFO_CRP$Provider = 'DFO_CRP'
DFO_CRP$AnnotationLevel = 'Detection'

DFO_CRP$dur = DFO_CRP$FileEndSec  - DFO_CRP$FileBeginSec
DFO_CRP$end_time = DFO_CRP$UTC+ seconds(DFO_CRP$dur)

# Sort and then identify overlaps
DFO_CRP <- DFO_CRP %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))


dayFolderPath_WV = 'E:/DCLDE/DFO_CRP/Audio/WVanIsl'
dayFolderPath_NBC = 'E:/DCLDE/DFO_CRP/Audio/NorthBc'

# Dummy
DFO_CRP$FilePath = 'blarg'

DFO_CRP1$Dep='WVanIsl'
DFO_CRP2$Dep='NorthBc'


WVIidx = which(DFO_CRP$Dep == 'WVanIsl')
NBCidx = which(DFO_CRP$Dep != 'WVanIsl')

DFO_CRP$FilePath[WVIidx] =file.path(dayFolderPath_WV, DFO_CRP$Soundfile[WVIidx])
DFO_CRP$FilePath[NBCidx] = file.path(dayFolderPath_NBC, DFO_CRP$Soundfile[NBCidx])


DFO_CRP$FileOk  = file.exists(DFO_CRP$FilePath) 

# Make sure all audio files are present for all annotations
if (all(DFO_CRP$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}

runTests(DFO_CRP, EcotypeList, ClassSpeciesList)


rm(list= c('DFO_CRP1', 'DFO_CRP2'))


DFO_CRP = DFO_CRP[, c(colOut)]



############################################################################
# DFO WLDP (Yurk)
############################################################################

# Load the annotation files
file_list <- list.files(path = 'E:/DCLDE/DFO_WDLP/Annotations/merged_annotations/', 
                        pattern = '*csv', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
DFO_WDLP <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.csv(file)
  if (nrow(data) > 0) {
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# Deployment names
levels(DFO_WDLP$Dep)<-c('CarmanahPt', 'StrGeoN1', 'StrGeoN2','StrGeoS1',
                        'StrGeoS2','SwanChan')

# Fucking PAMGuard artifacts, remove any with negative duration
DFO_WDLP = DFO_WDLP[DFO_WDLP$duration>0,]
DFO_WDLP = DFO_WDLP[!duplicated(DFO_WDLP),]

# Standardize formatting
DFO_WDLP$Soundfile = DFO_WDLP$soundfile
DFO_WDLP$LowFreqHz = DFO_WDLP$lf
DFO_WDLP$HighFreqHz = DFO_WDLP$hf
DFO_WDLP$UTC = as.POSIXct( DFO_WDLP$date_time_utc,  
                           format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')

DFO_WDLP$FileBeginSec = DFO_WDLP$elapsed_time_seconds
DFO_WDLP$FileEndSec = DFO_WDLP$FileBeginSec+DFO_WDLP$duration/1000

DFO_WDLP$Ecotype = as.factor(DFO_WDLP$species)
levels(DFO_WDLP$Ecotype)<-c(NA, 'NRKW', 'SRKW', 'TKW', NA, NA)

DFO_WDLP$ClassSpecies = as.factor(DFO_WDLP$species)
levels(DFO_WDLP$ClassSpecies)<-c('HW', 'KW', 'KW', 'KW', 'UndBio', 'AB')

DFO_WDLP$KW = ifelse(DFO_WDLP$ClassSpecies== 'KW', 1,0)
DFO_WDLP$KW_certain = NA
DFO_WDLP$KW_certain[DFO_WDLP$KW==1]<-1
DFO_WDLP$Provider = 'DFO_WDLP'


DFO_WDLP$AnnotationLevel = 'Detection'

DFO_WDLP$dur = DFO_WDLP$FileEndSec  - DFO_WDLP$FileBeginSec
DFO_WDLP$end_time = DFO_WDLP$UTC+ seconds(DFO_WDLP$dur)

# Sort and then identify overlaps
DFO_WDLP <- DFO_WDLP %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))

# Add a new column for deployment folder
DFO_WDLP$DepFolder = DFO_WDLP$Dep
levels(DFO_WDLP$DepFolder)<-c('CarmanahPt',
                              'StrGeoN1',
                              'StrGeoN1',
                              'StrGeoS1',
                              'StrGeoS1',
                              'SwanChan')

# Filepaths
dayFolderPath = 'E:/DCLDE/DFO_WDLP/Audio'

DFO_WDLP$FilePath = 
  file.path(dayFolderPath, 
            DFO_WDLP$DepFolder, 
            DFO_WDLP$Soundfile)

DFO_WDLP$FileOk  = file.exists(DFO_WDLP$FilePath) 

# Make sure all audio files are present for all annotations
if (all(DFO_WDLP$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}

runTests(DFO_WDLP, EcotypeList, ClassSpeciesList)
DFO_WDLP = DFO_WDLP[, colOut]
rm(list=c('dayFolderPath','dayFolderPath_NBC','dayFolderPath_WV','NBCidx','WVIidx'))
   
###############################################################################
# Orca Sound Data

OrcaSound = read.csv('E:/DCLDE/Orcasound/Annotations/ModifiedAnnotations.csv')
OrcaSound$start_time_s = as.numeric(OrcaSound$start_time_s) 

OrcaSound$DateTime <- with(OrcaSound, paste(date, pst_or_master_tape_identifier))

OrcaSound$UTC <-as.POSIXct(OrcaSound$DateTime[1], 
                           tz = "America/Los_Angeles", 
                           format = "%m/%d/%Y %H:%M:%S")

# Every. single. podcast. 'round'. is. labeled. differently. Glaurung kill me 
# now but skip the Oedipus stuff. 

for(ii in 1:length(unique(OrcaSound$dataset))){
  
  podcastId = unique(OrcaSound$dataset)[ii]
  idx = which(OrcaSound$dataset ==podcastId)
  
  dateVals = OrcaSound$date[idx]
  timeVals = OrcaSound$pst_or_master_tape_identifier[idx]
  
  if(ii %in% c(1,2,3,5,6,7,8,9)){
    UTCDate = as.POSIXct(paste(dateVals, timeVals), 
                         tz = "America/Los_Angeles", 
                         format = "%m/%d/%Y %H:%M:%S")+
      OrcaSound$start_time_s[idx]}
  if(ii %in% c(4)){
    UTCDate = as.POSIXct(paste(dateVals, timeVals), tz = "America/Los_Angeles", 
                         format = "%Y_%m_%d %H:%M:%S")+
      OrcaSound$start_time_s[idx]}
  
  OrcaSound$UTC[idx]<-UTCDate
}

#Now convert all times from pacific to UTC
attr(OrcaSound$UTC, "tzone") <- "UTC" 

# Add class species info
OrcaSound$ClassSpecies = OrcaSound$Species
OrcaSound$ClassSpecies[OrcaSound$ClassSpecies == 'Oo'] = 'KW'
OrcaSound$ClassSpecies[OrcaSound$ClassSpecies == 'Noise'] = 'AB'

OrcaSound$Provider = 'OrcaSound'
OrcaSound$KW = ifelse(OrcaSound$ClassSpecies== 'KW',1,0)
OrcaSound$KW_certain = ifelse(OrcaSound$ClassSpecies== 'KW',1,NA)
OrcaSound$Dep = OrcaSound$location
OrcaSound$Soundfile = OrcaSound$wav_filename
OrcaSound$LowFreqHz =NaN
OrcaSound$HighFreqHz =NaN

OrcaSound$FileBeginSec = OrcaSound$start_time_s
OrcaSound$FileEndSec = OrcaSound$start_time_s+OrcaSound$duration_s


OrcaSound$AnnotationLevel = ifelse(OrcaSound$ClassSpecies == 'KW',
                                   'Call', 'File')
levels(OrcaSound$location)


# Filepaths
dayFolderPath = 'E:/DCLDE/OrcaSound/Audio'
OrcaSound$FilePath = 
  file.path(dayFolderPath,OrcaSound$Dep, OrcaSound$Soundfile)

OrcaSound$FileOk  = file.exists(OrcaSound$FilePath) 

# Make sure all audio files are present for all annotations
if (all(OrcaSound$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


OrcaSound = OrcaSound[,c(colOut)]

runTests(OrcaSound, EcotypeList, ClassSpeciesList)


###########################################################################
# SIMRES
##########################################################################


# multiple selection tables
file_list <- list.files(path = 'E:/DCLDE/SIMRES/Annotations/', 
                        pattern = '*txt', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
SIMRES <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.table(file, header = TRUE, sep = '\t')
  if (nrow(data) > 0) {
    data$Dep <- basename(file)  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# Clean out blank rows
SIMRES = SIMRES[!is.na(SIMRES$Selection),]

SIMRES$ClassSpecies = as.factor(SIMRES$Sound.ID.Species)
levels(SIMRES$ClassSpecies)<-c('AB', 'HW','KW',  'KW', 'KW')


#check SIMRES files in UTC
SIMRES$UTC = as.POSIXct(sub(".*_(\\d{8}T\\d{6}\\.\\d{3}Z)_.*", "\\1", 
                            SIMRES$Begin.File),  
                        format = "%Y%m%dT%H%M%S.%OSZ",
                        tz = 'UTC')+seconds(SIMRES$File.Offset..s.)

SIMRES$Ecotype = as.factor(SIMRES$KW.Ecotype)
levels(SIMRES$Ecotype)<- c(NA, NA, 'SRKW', 'SRKW')

SIMRES$KW =ifelse(SIMRES$ClassSpecies=='KW', 1, 0)

SIMRES$KW_certain = NA
SIMRES$KW_certain[SIMRES$KW ==1] =1
SIMRES$KW_certain[SIMRES$Confidence %in% c('low')]=0


SIMRES$Soundfile = SIMRES$Begin.File
SIMRES$Dep = 'Tekteksen'
SIMRES$LowFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$HighFreqHz = SIMRES$Low.Freq..Hz.
SIMRES$FileBeginSec = SIMRES$File.Offset..s.
SIMRES$FileEndSec = SIMRES$File.Offset..s.+SIMRES$Delta.Time..s.
SIMRES$Provider = 'SIMRES'
SIMRES$AnnotationLevel = 'Call'


# Filepaths- wackadoodle for SIMRES
dayFolderPath = 'E:/DCLDE/SIMRES/Audio/'

# 1. List all files in the target directory
allAudio <- list.files(path = dayFolderPath, pattern = "\\.flac$", 
                       full.names = TRUE, recursive = TRUE)

# 2. Check if each file exists and get the full path
SIMRES$FilePath <- sapply(SIMRES$Soundfile, function(filename) {
  # Find the full path of the file (if it exists)
  full_path <- allAudio[grep(paste0("/", filename, "$"), allAudio)]
  if (length(full_path) > 0) return(full_path[1]) else return(NA)
})

SIMRES <- SIMRES %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(SIMRES$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


runTests(SIMRES, EcotypeList, ClassSpeciesList)


SIMRES= SIMRES[, colOut]
rm(list= c('file_list', 'dayFolderPath'))

############################################################################
# VFPA - JASCO Strait of Georgia (Labeled "Roberts Bank" on Globus server 
# corrected for NCEI)
############################################################################

# Strait fo Georgia
VPFA_SoG<- read.csv('E:/DCLDE/VFPA/Annotations/annot_RB_man_det.csv')

VPFA_SoG <- VPFA_SoG %>%
  mutate(
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}.\\d{3}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )


VPFA_SoG$ClassSpecies=VPFA_SoG$sound_id_species
VPFA_SoG$ClassSpecies[VPFA_SoG$sound_id_species == 'KW?'] ='KW'
VPFA_SoG$ClassSpecies[VPFA_SoG$sound_id_species %in% c('KW?', "HW/KW?",
                                                       'KW/PWSD?')]<-'KW'
VPFA_SoG$ClassSpecies[VPFA_SoG$sound_id_species %in% c( "HW?")]<-'HW'

VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies %in% 
                        c("Vessel Noise", "Vessel Noise?",  "Noise", 
                          "Sonar","UN")]= 'AB'

VPFA_SoG$ClassSpecies[VPFA_SoG$ClassSpecies %in% 
                        c("PWSD","FS")]= 'UndBio'



VPFA_SoG$KW = ifelse(VPFA_SoG$ClassSpecies == 'KW',1,0)
VPFA_SoG$KW_certain= NA
VPFA_SoG$KW_certain[VPFA_SoG$KW==1] =1
VPFA_SoG$KW_certain[VPFA_SoG$KW==1 & 
                      grepl("\\?", VPFA_SoG$sound_id_species)]<-0
VPFA_SoG$KW_certain[VPFA_SoG$sound_id_species %in% c('KW?', "HW/KW?",
                                                     'KW/PWSD?')]<-0



VPFA_SoG$UTC = VPFA_SoG$UTC+ seconds(as.numeric(VPFA_SoG$start))

# Add Ecotype 
VPFA_SoG$Ecotype = NA
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'KWT?'] ='TKW'
VPFA_SoG$Ecotype[VPFA_SoG$kw_ecotype == 'TKW?'] ='TKW'


colnames(VPFA_SoG)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                           'FileEndSec', 'Soundfile')


VPFA_SoG$AnnotationLevel = 'Call'

VPFA_SoG$dur = VPFA_SoG$FileEndSec-VPFA_SoG$FileBeginSec

VPFA_SoG$end_time = VPFA_SoG$UTC+ seconds(VPFA_SoG$dur)

VPFA_SoG$Dep='StraitofGeorgia'
VPFA_SoG$Provider = 'JASCO_VFPA_ONC'

# Sort and then identify overlaps
VPFA_SoG <- VPFA_SoG %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))


# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:/DCLDE/VFPA/Audio/StraitofGeorgia//',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Create subset of high pass filtered files
hpf_df <- audio.files[grep("hpf", audio.files$filename, ignore.case = TRUE), ]


# Day folder
dayFolderPath = 'E:/DCLDE/VFPA/Audio/StraitofGeorgia/'
VPFA_SoG$FilePath = file.path(dayFolderPath,
                              VPFA_SoG$Soundfile)

VPFA_SoG <- VPFA_SoG %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(VPFA_SoG$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}



VPFA_SoG =VPFA_SoG[,colOut]

runTests(VPFA_SoG, EcotypeList, ClassSpeciesList)

############################################################################
# VFPA - JASCO Boundary Pass
############################################################################


# Boundary Pass
VPFA_BoundaryPass<- read.csv('E:/DCLDE/VFPA/Annotations/annot_BP_man_det.csv')
VPFA_BoundaryPass <- VPFA_BoundaryPass %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )



# Remove 'duplicate' and 'repeat' annotations
VPFA_BoundaryPass= VPFA_BoundaryPass[
  !VPFA_BoundaryPass$sound_id_species %in% c('Repeat', 'Duplicate'),]


VPFA_BoundaryPass$ClassSpecies<- VPFA_BoundaryPass$sound_id_species


VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$sound_id_species %in%
                                 c('KW?', "HW/KW?",  "KW/HW?","KW/PWSD?")] ='KW'

VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$sound_id_species %in% 
                                 c( "HW?")]= 'HW'

VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$sound_id_species %in% 
                                 c("Vessel Noise", "Vessel Noise?",  "Noise", 
                                   "Sonar","UN", "NN", "BACKGROUND", 'UNK')]= 'AB'

VPFA_BoundaryPass$ClassSpecies[VPFA_BoundaryPass$sound_id_species %in% 
                                 c("PWSD","FS",  "PWSD?")]= 'UndBio'
VPFA_BoundaryPass$KW = ifelse(VPFA_BoundaryPass$ClassSpecies== 'KW', 1,0)
VPFA_BoundaryPass$KW_certain= NA
VPFA_BoundaryPass$KW_certain[VPFA_BoundaryPass$KW==1] =1
UncertainKWidx = which(VPFA_BoundaryPass$KW==1 & 
                         grepl("\\?", VPFA_BoundaryPass$sound_id_species))
VPFA_BoundaryPass$KW_certain[UncertainKWidx]=0

# Get time in UTC
VPFA_BoundaryPass$UTC = VPFA_BoundaryPass$UTC+ 
  seconds(as.numeric(VPFA_BoundaryPass$start))

# Add Ecotype 
VPFA_BoundaryPass$Ecotype = NA
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'KWT?'] ='TKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'TKW?'] ='TKW'
VPFA_BoundaryPass$Ecotype[VPFA_BoundaryPass$kw_ecotype == 'TKW'] ='TKW'



colnames(VPFA_BoundaryPass)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                    'FileEndSec', 'Soundfile')



VPFA_BoundaryPass$AnnotationLevel = 'Call'
VPFA_BoundaryPass$dur = VPFA_BoundaryPass$FileEndSec-VPFA_BoundaryPass$FileBeginSec
VPFA_BoundaryPass$end_time = VPFA_BoundaryPass$UTC+ seconds(VPFA_BoundaryPass$dur)
VPFA_BoundaryPass$Dep='BoundaryPass'
VPFA_BoundaryPass$Provider = 'JASCO_VFPA'


# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:/DCLDE/VFPA/BoundaryPass/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_BoundaryPass$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annotations')
  VPFA_BoundaryPass$FileOk= TRUE
}else{
  print('Missing data')
  VPFA_BoundaryPass$Soundfile[which(!VPFA_BoundaryPass$Soundfile %in% audio.files$Soundfile)]
}


# Day folder
dayFolderPath = 'E:/DCLDE/VFPA/Audio/BoundaryPass'
VPFA_BoundaryPass$FilePath = file.path(dayFolderPath,
  VPFA_BoundaryPass$Soundfile)


VPFA_BoundaryPass <- VPFA_BoundaryPass %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(VPFA_BoundaryPass$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


VPFA_BoundaryPass = VPFA_BoundaryPass[,colOut]
runTests(VPFA_BoundaryPass, EcotypeList, ClassSpeciesList)

############################################################################
# VFPA - JASCO Haro Strait North
############################################################################


# Haro Strait North
VPFA_HaroNB<- read.csv('E:/DCLDE/VFPA/Annotations/annot_VFPA-HaroStrait-NB_SM_coarse.csv')
VPFA_HaroNB <- VPFA_HaroNB %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )

VPFA_HaroNB$ClassSpecies<- VPFA_HaroNB$sound_id_species

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$sound_id_species %in%
                                 c('KW?', "HW/KW?",  "KW/HW?","KW/PWSD?")] ='KW'

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$sound_id_species %in% 
                                 c( "HW?")]= 'HW'

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$sound_id_species %in% 
                           c("Vessel Noise", "Vessel Noise?",  "Noise", 
                             "Sonar","UN", "BELL","VESSEL", "UNK")]= 'AB'

VPFA_HaroNB$ClassSpecies[VPFA_HaroNB$sound_id_species %in% 
                                 c("PWSD","FS",  "PWSD?")]= 'UndBio'

VPFA_HaroNB$KW_certain= NA
VPFA_HaroNB$KW_certain[VPFA_HaroNB$KW==1] =1

UncertainKWidx = which(VPFA_HaroNB$KW==1 & 
                         grepl("\\?", VPFA_HaroNB$sound_id_species))
VPFA_HaroNB$KW_certain[UncertainKWidx]=0



# Get time in UTC
VPFA_HaroNB$UTC = VPFA_HaroNB$UTC+ 
  seconds(as.numeric(VPFA_HaroNB$start))

# Add Ecotype 
VPFA_HaroNB$Ecotype = NA
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'KWT?'] ='TKW'
VPFA_HaroNB$Ecotype[VPFA_HaroNB$kw_ecotype == 'TKW?'] ='TKW'



colnames(VPFA_HaroNB)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                             'FileEndSec', 'Soundfile')

VPFA_HaroNB$AnnotationLevel = 'Call'
VPFA_HaroNB$dur = VPFA_HaroNB$FileEndSec-VPFA_HaroNB$FileBeginSec
VPFA_HaroNB$end_time = VPFA_HaroNB$UTC+ seconds(VPFA_HaroNB$dur)
VPFA_HaroNB$Dep='HaroStraitNorth'
VPFA_HaroNB$Provider = 'JASCO_VFPA'

# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:/DCLDE/VFPA/Audio/VFPA-HaroStrait-NB/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroNB$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annotations')
  VPFA_HaroNB$FileOk=TRUE
}else{
  print('Missing data')
  VPFA_HaroNB$Soundfile[which(!VPFA_HaroNB$Soundfile %in% audio.files$Soundfile)]
}



# Day folder
dayFolderPath = 'E:/DCLDE/VFPA/Audio/VFPA-HaroStrait-NB/'
VPFA_HaroNB$FilePath = file.path(dayFolderPath,
  VPFA_HaroNB$Soundfile)

# VPFA_HaroNB$FilePath = file.path(dayFolderPath, format(
#   VPFA_HaroNB$UTC-seconds(VPFA_HaroNB$FileBeginSec),"%Y%m%d"),
#   VPFA_HaroNB$Soundfile)

VPFA_HaroNB <- VPFA_HaroNB %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroNB$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}

VPFA_HaroNB= VPFA_HaroNB[,colOut]
runTests(VPFA_HaroNB, EcotypeList, ClassSpeciesList)

############################################################################
# VFPA - JASCO Haro Strait South
############################################################################

# Haro Strait South
VPFA_HaroSB<- read.csv('E:/DCLDE/VFPA/Annotations/annot_VFPA-HaroStrait-SB_SM_coarse.csv')
VPFA_HaroSB <- VPFA_HaroSB %>%
  mutate(
    KW = as.numeric(grepl("KW", sound_id_species)),
    UTC = as.POSIXct(sub(".*(\\d{8}T\\d{6}Z).*", "\\1", filename), 
                     format = "%Y%m%dT%H%M%S", tz= 'UTC')
  )



VPFA_HaroSB$ClassSpecies<- VPFA_HaroSB$sound_id_species


VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$sound_id_species %in%
                           c('KW?', "HW/KW?",  "KW/HW?","KW/PWSD?")] ='KW'

VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$sound_id_species %in% 
                           c( "HW?")]= 'HW'

VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$sound_id_species %in% 
                           c("Vessel Noise", "Vessel Noise?",  "Noise", 
                             "Sonar","UN", "BELL","VESSEL", "UNK")]= 'AB'

VPFA_HaroSB$ClassSpecies[VPFA_HaroSB$sound_id_species %in% 
                           c("PWSD","FS",  "PWSD?")]= 'UndBio'

VPFA_HaroSB$KW_certain= NA
VPFA_HaroSB$KW_certain[VPFA_HaroSB$KW==1] =1

UncertainKWidx = which(VPFA_HaroSB$KW==1 & 
                         grepl("\\?", VPFA_HaroSB$sound_id_species))
VPFA_HaroSB$KW_certain[UncertainKWidx]=0



# Get time in UTC
VPFA_HaroSB$UTC = VPFA_HaroSB$UTC+ 
  seconds(as.numeric(VPFA_HaroSB$start))

# Add Ecotype 
VPFA_HaroSB$Ecotype = NA
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'SRKW'] ='SRKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'SRKW?'] ='SRKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'KWT?'] ='TKW'
VPFA_HaroSB$Ecotype[VPFA_HaroSB$kw_ecotype == 'TKW?'] ='TKW'


colnames(VPFA_HaroSB)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                        'FileEndSec', 'Soundfile')


VPFA_HaroSB$AnnotationLevel = 'Call'
VPFA_HaroSB$dur = VPFA_HaroSB$FileEndSec-VPFA_HaroSB$FileBeginSec
VPFA_HaroSB$end_time = VPFA_HaroSB$UTC+ seconds(VPFA_HaroSB$dur)
VPFA_HaroSB$Dep='HaroStraitSouth'
VPFA_HaroSB$Provider = 'JASCO_VFPA'

# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:/DCLDE/VFPA/Audio/VFPA-HaroStrait-SB/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroSB$Soundfile %in% audio.files$Soundfile)){
  print('All data present for annotations')
  VPFA_HaroSB$FileOk = TRUE
}else{
  print('Missing data')
  VPFA_HaroSB$Soundfile[which(!VPFA_HaroSB$Soundfile %in% audio.files$Soundfile)]
}

# Day folder
dayFolderPath = 'E:/DCLDE/VFPA/Audio/VFPA-HaroStrait-SB/'
VPFA_HaroSB$FilePath = file.path(dayFolderPath, 
  VPFA_HaroSB$Soundfile)


VPFA_HaroSB <- VPFA_HaroSB %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(VPFA_HaroSB$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}

VPFA_HaroSB= VPFA_HaroSB[, colOut]
runTests(VPFA_HaroSB, EcotypeList, ClassSpeciesList)

###########################################################################
# SCRIPPS
############################################################################


# Get a list of files matching the pattern 'annot_Malahat'
file_list <- list.files(path = 'E:/DCLDE/Scripps/Annotations', 
                        pattern = '*txt', full.names = TRUE)


# Read and concatenate the CSV files with filename as a separate column (if non-empty)
scripps <- do.call(rbind, lapply(file_list, function(file) {
  data <- read.table(file, header = TRUE, sep = '\t')
  if (nrow(data) > 0) {
    data$Dep <- as.factor(basename(file))  # Add filename as a new column
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

scripps$Dep<- as.factor(scripps$Dep)

# Cape Elizabeth and Quinault Canyon
levels(scripps$Dep)<-c("Cpe_Elz", "Quin_Can")

# Set the initial ecotypes then match the format
scripps$Ecotype <- as.factor(gsub(".*_(.*)\\.wav", "\\1", scripps$Begin.File))
levels(scripps$Ecotype)<-c('TKW', 'TKW', 'TKW', 'TKW', 'OKW', 'SRKW',
                           'SRKW','TKW', 'TKW')
scripps$Ecotype[scripps$ClassSpecies != ""]<- NaN

scripps$ClassSpecies= as.factor(scripps$ClassSpecies)
levels(scripps$ClassSpecies)<-c('KW', 'AB', 'AB', 'HW', 'KW')

scripps$KW = ifelse(scripps$ClassSpecies=='KW',1,0)
scripps$KW_certain = ifelse(scripps$ClassSpecies=='KW',1,NA)

scripps$Soundfile = scripps$Begin.File
scripps$LowFreqHz = scripps$Low.Freq..Hz.
scripps$HighFreqHz = scripps$Low.Freq..Hz.
scripps$FileBeginSec = scripps$Beg.File.Samp..samples./200000
scripps$FileEndSec = scripps$FileBeginSec+(scripps$End.Time..s.- scripps$Begin.Time..s)
scripps$Provider = 'SIO'
scripps$AnnotationLevel = 'Call'
scripps$FilePath = paste0('E:/DCLDE/Scripps/Audio/', scripps$Dep, '/',
                          scripps$Soundfile)


#check SIMRES files in UTC
scripps$UTC = as.POSIXct(sub(".*_(\\d{6}_\\d{6})_.*", "\\1",
                             scripps$Begin.File),  
                        format = "%y%m%d_%H%M%S",
                        tz = 'UTC')

scripps$FileOk = file.exists(scripps$FilePath)

# Make sure all audio files are present for all annotations
if (all(scripps$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}

scripps = scripps[, colOut]
runTests(scripps, EcotypeList, ClassSpeciesList)

#############################################################################
# SMRU Consulting
#############################################################################

# SMRU data were annotated by species encounters

SMRU_SRKW<- read.csv('E:/DCLDE/SMRU/annotations/annot_LimeKiln-Encounters_man_det.csv')
SMRU_HW<- read.csv('E:/DCLDE/SMRU/annotations/annot_LimeKiln-Humpback_man_det.csv')

SMRU <- rbind(SMRU_HW, SMRU_SRKW)

SMRU <- SMRU %>%
  mutate(
    UTC = as.POSIXct(sub(".*_(\\d{8}_\\d{6}_\\d{3})\\..*", "\\1", filename), 
               format = "%Y%m%d_%H%M%S_%OS", tz = "UTC")
  )

# Some annotations witout sound species ID, remove
SMRU = SMRU[SMRU$sound_id_species != "",]

SMRU$ClassSpecies=SMRU$sound_id_species
SMRU$ClassSpecies[SMRU$sound_id_species %in% 
                    c('KW?', "KW", "HW/KW?", "KW/PWSD","KW/PWSD?",
                      "PWSD/KW?")]<-'KW'

SMRU$ClassSpecies[SMRU$sound_id_species %in% c( "HW?", "HW", "HB", "HB?")]<-'HW'

SMRU$ClassSpecies[SMRU$ClassSpecies %in% 
                        c("Vessel Noise", "Vessel Noise?",  "Noise", 
                          "Sonar","UN", "Unknown", "Mooring", "surface noise",
                          "Vessel noise", "Unknown?", "Mooring noise", 
                          "Wave noise", "Vessel  Noise", "Surf noise",
                          "Waves", "Mooring ", "Mooring?")]= 'AB'

SMRU$ClassSpecies[SMRU$ClassSpecies %in% 
                        c("PWSD","FS", "Seal?", "Fish", "Fish?", "FISH",
                          "Snapping shrimp/urchin", 
                          "Snapping shrimp or urchin" )]= 'UndBio'


# Set the KW class and certainty
SMRU$KW = ifelse(SMRU$ClassSpecies == 'KW',1,0)
SMRU$KW_certain= NA
SMRU$KW_certain[SMRU$KW==1] =1
SMRU$KW_certain[SMRU$KW==1 & 
                      grepl("\\?", SMRU$sound_id_species)]<-0


SMRU$UTC = SMRU$UTC+ seconds(as.numeric(SMRU$start))

# Add Ecotype 
SMRU$Ecotype = NA
SMRU$Ecotype[SMRU$kw_ecotype == 'SRKW'] ='SRKW'
SMRU$Ecotype[SMRU$kw_ecotype == 'SRKW?'] ='SRKW'
SMRU$Ecotype[SMRU$kw_ecotype == 'KWT?'] ='TKW'
SMRU$Ecotype[SMRU$kw_ecotype == 'TKW?'] ='TKW'


colnames(SMRU)[c(5,6,3,4,1)]<-c('LowFreqHz','HighFreqHz','FileBeginSec',
                                    'FileEndSec', 'Soundfile')


SMRU$AnnotationLevel = 'Call'

SMRU$dur = SMRU$FileEndSec-SMRU$FileBeginSec
SMRU$end_time = SMRU$UTC+ seconds(SMRU$dur)

SMRU$Dep='LimeKiln'
SMRU$Provider = 'SMRUConsulting'

# Sort and then identify overlaps
SMRU <- SMRU %>%
  arrange(Dep,UTC) %>%
  mutate(overlap = lead(UTC) <= lag(end_time, default = first(end_time)))


# List which files are not in annotations list
audio.files = data.frame(
  filename = list.files('E:/DCLDE/SMRU/Audio/Lime Kiln/',
                        pattern ='.wav', recursive = TRUE, include.dirs = TRUE))
audio.files$Soundfile =basename(audio.files$filename)

# Day folder
dayFolderPath = 'E:/DCLDE/SMRU/Audio/Lime Kiln/'
SMRU$FilePath = file.path(dayFolderPath,
                          SMRU$Soundfile)

SMRU <- SMRU %>%
  mutate(FileOk = file.exists(FilePath))

# Make sure all audio files are present for all annotations
if (all(SMRU$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}



SMRU =SMRU[,colOut]

runTests(SMRU, EcotypeList, ClassSpeciesList)



#############################################################################
#University of Alaska data not organized by location. UAFdata.r used to
#organize at least into individual hydrophones or field locations

# New Audio Location (where to move the organized files too)
new_root ='E:/DCLDE/UAF/Audio'

# Annotations location (already moved all the annotations here)
annot_root =  'E:/DCLDE/UAF/Annotations/'

# Load the deployment info
UAF_depInfo = read.csv('E:/DCLDE/UAF/Meta/Myers_DCLDE_2026_files.csv')


# Get a list of annotation files
file_list <- list.files(path = annot_root,
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)

# Get a list of annotation files
file_list <- list.files(path = annot_root,
                        pattern = '.txt', full.names = TRUE,
                        recursive = TRUE)


# Read and concatenate the selection tables  with file name as a separate column (if non-empty)
UAF <- do.call(rbind, lapply(file_list, function(file) {
  
  # read the selection table
  data <- read.table(file, header = TRUE, sep = '\t')
  
  # get the audio file name
  audioFile<- basename(file)
  
  # Hydrophone id
  Hyd <- strsplit(audioFile, "\\.")[[1]][[1]]
  
  if (nrow(data) > 0) {
    
    # Add filename as a new column and get deployment from the filename
    data$Dep <- as.factor(strsplit(dirname(file),'/')[[1]][4])  
    data$Hyd <- as.factor(Hyd)
    parts <- strsplit(audioFile, "\\.")[[1]][1:2]
    filename <- paste(parts[1], parts[2], sep = ".")
    filename <- paste0(filename, ".wav")
    
    #AudioFile is one of the required column headings
    data$AudioFile =filename
    
    # For ecotype later on
    data$FolderName = dirname(file)
    return(data)
  } else {
    return(NULL)  # Return NULL for empty data frames
  }
}))

# remove 'table' from the field recordings 
UAF$AudioFile <- gsub("\\.Table1", "", UAF$AudioFile)
# and "table1"... shoot me in the face please
UAF$AudioFile <- gsub("\\.Table", "", UAF$AudioFile)


# Merge the files
UAF = merge(UAF, UAF_depInfo, by.x = 'AudioFile',
            by.y='FileName', all.x=TRUE)

UAF$FilePath<- file.path('E:/DCLDE/UAF/Audio', UAF$Location, UAF$AudioFile)


# UTC is the UTC plus the file offset
UAF$UTC<- as.POSIXct(UAF$UTC)+seconds(UAF$Begin.Time..s.)

UAF$Soundfile<- UAF$AudioFile
UAF$AnnotationLevel<-'Call'


ClassSpeciesList = c('KW', 'HW', 'AB', 'UndBio')
AnnotationLevelList = c('File', 'Detection', 'Call')
EcotypeList = c('SRKW', 'TKW', 'OKW', 'NRKW', 'SAR')

# Ok Field recordings don't have an ecotype or population with them...Based
# on the filename Resident and Southern Alaska resident
UAF$Ecotype[is.na(UAF$Ecotype)]<- 'Resident'
UAF$finalizedEcotype =as.factor(UAF$Population)

levels(UAF$finalizedEcotype)<-c('TKW', 'TKW', 'OKW', 'SAR')
UAF$Ecotype = UAF$finalizedEcotype


############################################################################
# Finish up required headings
###########################################################################
colOut = c('Soundfile','Dep','LowFreqHz','HighFreqHz','FileEndSec', 'UTC',
           'FileBeginSec','ClassSpecies','KW','KW_certain','Ecotype', 'Provider',
           'AnnotationLevel', 'FilePath', 'FileOk')


UAF$HydId<-UAF$Hyd
UAF$HydId<-as.factor(UAF$HydId)
levels(UAF$HydId)[9:59]<-'Field'


UAF$Dep<- UAF$Location
# Two different hydrophones used in field deployments not recognized in the
# file names
nonFiledIdx = which(UAF$Dep != 'Field')
idxHTI = which(UAF$UTC< as.POSIXct('2021-06-15') & UAF$Dep == 'Field')
idxSoundTrap = which(UAF$UTC> as.POSIXct('2021-06-15') & UAF$Dep == 'Field')

UAF$Dep[nonFiledIdx]<- paste0(UAF$Dep[nonFiledIdx], '_', UAF$HydId[nonFiledIdx])
UAF$Dep[idxHTI] = 'Field_HTI'
UAF$Dep[idxSoundTrap]= 'Field_SondTrap'


UAF$KW = 1 # only KW annotated
UAF$ClassSpecies= 'KW' 
UAF$KW_certain = 1 # Only annotated calls that were certainly KW

UAF$Soundfile<- UAF$AudioFile
UAF$FileEndSec = UAF$Begin.Time..s.
UAF$FileEndSec = UAF$End.Time..s.
UAF$Provider <- 'UAF_NGOS'
UAF$AnnotationLevel<- 'Call'
UAF$FilePath = UAF$audio_path
UAF$LowFreqHz = UAF$Low.Freq..Hz.
UAF$HighFreqHz = UAF$High.Freq..Hz.
UAF$FileBeginSec = UAF$Begin.Time..s.

# Change deployment to be the combination of the location
# and they hydrophone
UAF$temp = as.factor(paste0(UAF$Location,"_", UAF$HydId))
UAF$temp <- factor(UAF$temp, levels(UAF$temp)[c(2,3,4,6,5,7,9,8,10,1)])

UAF$FilePath = file.path('E:/DCLDE/UAF/Audio', UAF$Location, UAF$AudioFile)
# Check that all files are found
UAF$FileOk  = file.exists(UAF$FilePath)


# Make sure all audio files are present for all annotations
if (all(UAF$FileOk)){
  print('All data present for annotations')}else{print('Missing data')}


runTests(UAF, EcotypeList, ClassSpeciesList)


UAF= UAF[,colOut]


###########################################################################
allAnno = rbind(DFO_CRP, DFO_WDLP, OrcaSound, ONC_anno, scripps, SIMRES,
                VPFA_BoundaryPass, VPFA_HaroNB, VPFA_HaroSB, VPFA_SoG, SMRU,
                UAF)

# Change 'Deployment' to dataset since towed arrays added
colnames(allAnno)[colnames(allAnno)=='Dep']<- 'Dataset'
