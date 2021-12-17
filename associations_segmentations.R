# to segment thermals of eagles
# 25.11.21
setwd("C:/Users/Tess Bronnvik/Desktop/Improvement_and_Golden_Eagles")

library(tidyverse)
library(move)
library(lubridate)
library(doParallel)
library(plyr)

nodes <- detectCores()-2
CL <- makeCluster(nodes)
doParallel::registerDoParallel(CL)

source("associate_ACCinfoToGPS.R") #For ACCtoGPS function

# Define function to find errors in the list after the "try"
is.error <- function(x) inherits(x, "try-error")

### retrieve one eagle from the data

# the names of the acc files
accf <- list.files("D:/Golden_Eagle_data/ACC_data")

# the names of the gps files
gpsf <- list.files("D:/Golden_Eagle_data/moves")

# the names of the life stage file
lsf <- read.csv("Goldeneagles10_2021.csv", stringsAsFactors = F)
lsf <- lsf$id

# the names of the birds in acc files
acc_names <- sub(".RData", "", accf)
acc_names <- separate(as.data.frame(acc_names), col = "acc_names", 
                      into = c("acc_name","eobs"), sep = "_")
acc_names <- acc_names$acc_name

# the names and ids of the birds on MoveBank
load("loginStored.RData")
eagleStudyId <- 282734839

# the names of all the individuals in the study
allInds <- getMovebank("individual", login=loginStored, study_id=eagleStudyId)
allInds <- allInds %>% dplyr::select(id,local_identifier, timestamp_start)
allInds <- separate(allInds, col = "local_identifier", into = c("bank_name","eobs"), 
                    sep = " \\(", remove = F)
allInds$bank_name <- sub(" ", "\\.", allInds$bank_name)
allInds$bank_name <- sub("Art.San ", "", allInds$bank_name)
allInds$bank_name <- gsub("_", "\\.", allInds$bank_name)

# the names that are in life stage data and in acc data
birds <- lsf[lsf %in% acc_names]
#birds <- allInds[allInds$local_identifier %in% sub("_completeDF_thermalVSother_windEst.rdata", "", fls),]
birds <- birds[birds$bank_name %in% lsf,]

# make a frame that contains: 1) the acc name & 2) the gps name, but only of the 28

# the MoveBank info for the 28
eagles <- allInds[which(allInds$bank_name %in% birds),]

# organize the frame a bit
colnames(eagles)[1] <- "gps_name"
colnames(eagles)[3] <- "acc_name"
eagles$eobs <- sub("eobs ", "", sub("\\)", "", eagles$eobs))
eagles$gps_file_name <- paste0(eagles$gps_name, ".RData")
eagles$acc_file_name <- paste0(eagles$acc_name, "_", eagles$eobs, ".RData")

# "Sampuoir2.19" in Svea's data is not present in the ACC data, but Appennino18 (eobs 6462) is
# "Siat19" in Svea's data is not present in the ACC data, but Mals18 (eobs 6225) is
# Stürfis20", "Sampuoir1.19", and "Nalps19" are all on MoveBank and in Svea's data, but did not yield acc/gps

# "Sampuoir2 19 (eobs 6462)", "Nalps19 (eobs 5861)", "Stürfis20 (eobs 7049)"
# fall out at line 178, 

# lsf[!lsf %in% acc_names]
# "Nalps19" "Sampuoir1.19" "Sampuoir2.19" "Siat19" 

save(eagles, file = "eagle_names.RData")

# the birds that are in the MoveBank data but not the ACC data from Elham
missings <- allInds$bank_name[!sub("\\-", "\\.", sub("\\.21", "", gsub("_", "\\.", allInds$bank_name))) %in% acc_names]
missings[2] <- NA
missings <- na.omit(missings)


# access only the data from one individual 


# call in a move object
# call in the same acc
# associate
# see whether it sticks
# if it does, bind it
# segment the remaining list
# discard low-res data
# save locally

### load GPS
load(paste0("D:/Golden_Eagle_data/moves/", eagles[1,5]))

### load ACC
load(paste0("D:/Golden_Eagle_data/", eagles[1,6]))


paste0(sub("\\)", "", sub("\\(eobs", "", gsub(" ", "_", eagle_ms@idData$local_identifier))), ".RData")
### associate those

# get the tags of the birds in Martina's segments
seg_fls <- list.files("D:/Golden_Eagle_data/goldenEagles_wind", full.names = T)
eagles <- data.frame()

for (i in seg_fls) {
  load(i)
  bird_names <- c(unique(burstsWindDF$individual.local.identifier), unique(burstsWindDF$tag.local.identifier))
  eagles <- rbind(eagles, bird_names)
}
colnames(eagles) <- c("local_identifier", "tag")

# get the tags of the birds in Elham's ACC data
acc_fls <- list.files("D:/Golden_Eagle_data/ACC_data")
acc_names <- sub(".RData", "", acc_fls)
acc_tags <- substr(acc_names, nchar(acc_names) - 3, nchar(acc_names))
acc_data <- as.data.frame(acc_names)
acc_data <- cbind(acc_data, acc_tags)


for (i in 1:nrow(eagles)) {
  # load GPS
  load(seg_fls[i]) # the file from Martina
  names(burstsWindDF) <- gsub("\\.", "_", names(burstsWindDF))
  gps <- burstsWindDF
  rm(burstsWindDF)
  # load ACC
  load(paste0("D:/Golden_Eagle_data/ACC_data/", 
              list.files("D:/Golden_Eagle_data/ACC_data", 
              pattern = as.character(unique(gps$tag_local_identifier)))))
  acc <- ind
  rm(ind)
  names(acc) <- gsub("\\.", "_", names(acc))
  acc$timestamp <- as.POSIXct(as.character(acc$timestamp), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  # Order both datasets by timestamp
  acc <- acc[order(acc$timestamp),]
  gps <- gps[order(gps$timestamp),]
  # Subset only acc locations within the time range of the gps for the interpolation
  acc <- acc[acc$timestamp > min(gps$timestamp) & acc$timestamp < max(gps$timestamp),]
  # Calculate vedba per ACC burst
  try(if(nrow(acc) > 0 & length(grep("accelerations_raw", names(acc)))==1){  
    # Extract column names (sometimes they differ depending on tags)
    axesCol <- grep("acceleration_axes", names(acc), value=T)
    accRawCol <- grep("accelerations_raw", names(acc), value=T)
    sampFreqCol <- grep("acceleration_sampling_frequency_per_axis", names(acc), value=T)
    # Exclude ACC data that have only X or Y axis
    acc <- acc[which(!as.character(acc[, axesCol]) %in% c("X","Y")),]
    # Exclude rows with missing acc data
    acc <- acc[which(complete.cases(acc[, accRawCol])),]
    # Associate to each gps location the ACC EVENT ID of the acc values closest in time (within 5 min)
    names(acc)[names(acc) == "event_id"] <- "acc_event_id"
    acc$acc_event_id <- as.character(acc$acc_event_id)
    # nrow(acc)==length(unique(acc$acc_event_id))
    ColsToAssociate <- c("acc_event_id")
    time_tolerance <- 5*60 # 5 mins in seconds
    #create the empty columns that I want to fill in during the loop
    gps[,ColsToAssociate] <- NA
    gps$diff.acc.time <- NA
    gps$acc.closest.timestamp <- NA
    gpsAcc <- ACCtoGPS(acc, gps, ColsToAssociate, timeTolerance = time_tolerance)
    # Subset the acc dataset only to the acc_event_id associated to the gps information
    accSub <- acc[which(acc$acc_event_id %in% gpsAcc$acc_event_id),]
    # Calculate acc info (vedba etc) for this subset of acc data (split acc strings and calculate mean and cumulative vedba)
    if(nrow(accSub)>0 & length(unique(accSub[, axesCol]))==1){ #Continue only if number of acc axes doesn't vary within the same individual
      accDf_vedba <- do.call(rbind, lapply(1:nrow(accSub), function(j){
        #print(j)
        Naxes <- nchar(as.character(accSub[j, axesCol]))
        accMx <- matrix(as.integer(unlist(strsplit(as.character(accSub[j, accRawCol]), " "))), ncol=Naxes, byrow = T)
        n_samples_per_axis <- nrow(accMx)
        if(accSub[j, axesCol] == "Z"){
          vedba <- sqrt((accMx[,1]-mean(accMx[,1]))^2)
        }
        if(accSub[j, axesCol] %in% c("XY","XZ","YZ")){
          vedba <- sqrt((accMx[,1]-mean(accMx[,1]))^2 + (accMx[,2]-mean(accMx[,2]))^2)
        }
        if(accSub[j, axesCol] == "XYZ"){
          vedba <- sqrt((accMx[,1]-mean(accMx[,1]))^2 + (accMx[,2]-mean(accMx[,2]))^2 + (accMx[,3]-mean(accMx[,3]))^2)
        }
        row <- cbind(accSub[j,], n_samples_per_axis, meanVedba=mean(vedba, na.rm=T), cumVedba=sum(vedba, nrm=T))
        return(row)
      }))
      # Merge the resulting columns (vedba etc) to the gps data based on acc_event_id
      if(nrow(accDf_vedba)>0){
        gpsAcc <- merge(gpsAcc, accDf_vedba[,c("acc_event_id","n_samples_per_axis", "meanVedba", "cumVedba")], 
                        by="acc_event_id", all.x=T)
        print(table(is.na(gpsAcc$cumVedba)))
        return(gpsAcc)
      }
    }
  })
  save(gpsAcc, file = paste0("C:/Users/Tess Bronnvik/Desktop/Improvement_and_Golden_Eagles/associated/", unique(gps$individual_local_identifier), ".RData"))
  print(paste0("Associated GPS and ACC for ", unique(gps$individual_local_identifier)), quote = FALSE)
}

objs <- list.files("C:/Users/Tess Bronnvik/Desktop/Improvement_and_Golden_Eagles/associated")
gpsAcc_ls <- list()
for (i in objs) {
  load(paste0("associated/",i))
  gpsAcc_ls[[i]] <- gpsAcc
}

# Remove potential individuals that returned errors during download
gpsAcc_ls <- gpsAcc_ls[!vapply(gpsAcc_ls, is.error, logical(1))]
# Exclude empty elements from the list, bind and save
gpsAcc_ls <- gpsAcc_ls[which(!sapply(gpsAcc_ls, is.null))]
#if(length(gpsAcc_ls)>0){
#  gpsAccDf <- do.call(rbind, gpsAcc_ls)
#  save(gpsAccDf, file="goldenEagles_movebankDownload_gps&acc.rdata")
#}

#_________________________________________________________
### segment the track to isolate thermals
#_________________________________________________________


gpsAcc_ls <- gpsAcc_ls[sapply(gpsAcc_ls, n.locs)>=30] #keep only individuals with more than 30 locations (needed for segmentation)
names(gpsAcc_ls) <- sub(".RData", "", names(gpsAcc_ls))
lapply(names(gpsAcc_ls), function(x){
  mv <- gpsAcc_ls[[x]]
  mv$timelag.sec <- c(NA, timeLag(mv, units="secs"))
  mv$altitude.diff <- c(NA, (mv$height_above_ellipsoid[-1] - mv$height_above_ellipsoid[-nrow(mv)]))
  mv$vert.speed <- mv$altitude.diff/mv$timelag.sec
  mv$turn.angle <- c(NA, turnAngleGc(mv), NA)
  mv$step.length <- c(NA, distance(mv))
  mv$gr.speed <- c(NA, speed(mv))
  animalID <- unique(gpsAcc_ls[[x]]@idData$individual_id)
  if(substr(x,1,1)=="X"){x <- gsub("X","",x)} #When you split a movestack, if the id starts with a number R adds a X, which we don't want
  save(mv, file = paste0("prepped/", animalID,"_gpsNoDup_moveObj_", Sys.Date(), ".RData"))
})


#_________________________________________________________
### Select bursts of continuous 1 sec resolution data ####
#_________________________________________________________

minResol <- 1 # only 1 sec timelag
minBurstDuration <- 30 # we want bursts of at least 30 secs

swV <- 2 #smoothing window of 5 seconds (< min burst duration, 2 before 2 after each loc) for vertical speed for later classification
swT <- 14 #smoothing window of 29 seconds for thermalling behavior (according to Rolf/Bart's paper)

fls <- list.files("prepped/")
fls_todo <- fls[!sub("_gpsNoDup_moveObj_2021-11-28.RData", "", fls) %in% sub("_classifiedBursts_df.RData", "", list.files("segments/"))]

llply(fls_todo, function(f){
  print(f)
  load(paste0("prepped/",f)) #object mv
  animalID <- mv@idData$individual_id #paste(strsplit(f, "/|_")[[1]][2:3], collapse = "_") 
  # with cumsum R assigns the same value to all consecutive locations for which the condition is false (timelag <= 1 sec)
  mv$burstID <- c(0, cumsum(mv$timelag.sec[2:n.locs(mv)] != minResol))  #from row 2 (since the first is NA)
  # with table we can count all locations with the same ID (each one is a separate burst) and keep those high resolution bursts that have at least a certain number of locations (= minBurstDuration)
  burstDuration <- as.data.frame(table(mv$burstID))
  burstsToKeep <- burstDuration$Var1[which(burstDuration$Freq >= minBurstDuration)]
  # use those to subset the move obj and keep only the high resolution bursts
  HRmv <- mv[which(mv$burstID %in% burstsToKeep),]
  if(nrow(HRmv)>0){
    HRdf_bursts <- as.data.frame(HRmv)
    # Remove unnecessary columns
    HRdf_bursts <- HRdf_bursts[,-grep("mag|orientation|coords|timestamps|start.timestamp|optional|import|visible|algorithm|battery|decoding|accuracy|manually|activity|checksum|acceleration",
                                      colnames(HRdf_bursts))]
    # Split each individual dataframe by burst ID
    burst_ls_corr <- split(HRdf_bursts, HRdf_bursts$burstID)
    # Keep only bursts with at least 40 s (30 of smoothing window will be NA) 
    burst_ls_corr_sub <- burst_ls_corr[which(sapply(burst_ls_corr, nrow) >= 40)]
    # Compute smoothed turning angle separately for each burst
    HRdf <- do.call(rbind, lapply(burst_ls_corr_sub, function(b){
      b$vertSpeed_smooth <- NA
      b$turnAngle_smooth <- NA
      for(i in (swV+1):(nrow(b)-swV)){
        b$vertSpeed_smooth[i] <- mean(b$vert.speed[(i-swV):(i+swV)], na.rm=T)}
      for(i in (swT+1):(nrow(b)-swT)){
        b$turnAngle_smooth[i] <- max(abs(cumsum(b$turn.angle[(i-swT):(i+swT)])))}
      return(b) # return df with smoothed variables
    }))
    # Classify soaring only based on vertical speed
    HRdf <- HRdf[complete.cases(HRdf$vertSpeed_smooth),]
    kmeanV <- kmeans(HRdf$vertSpeed_smooth, 2)   #Get the two clusters
    soarId <- which.max(aggregate(HRdf$vertSpeed_smooth~kmeanV$cluster, FUN=mean)[,2]) # which one is the soaring one?
    soarClust <- rep("glide", length(kmeanV$cluster))
    soarClust[which(kmeanV$cluster==soarId)] <- "soar"
    HRdf$soarClust <- factor(soarClust, levels=c("soar","glide"))  
    # Now classify thermaling only based on turning angle (cumulated to a 30 s time window in previous step)
    HRdf$thermalClust <- "other"
    HRdf$thermalClust[which(HRdf$gr.speed >= 2 & HRdf$turnAngle_smooth >= 300)] <- "circular"
    HRdf$thermalClust[which(HRdf$gr.speed >= 2 & HRdf$soarClust=="soar" & HRdf$thermalClust != "circular")] <- "linear"
    HRdf$thermalClust <- factor(HRdf$thermalClust, levels=c("circular","linear","other"))
    # Save classified dataframe per individual
    save(HRdf, file = paste0("segments/",animalID,"_classifiedBursts_df.RData"))
  }
}, .parallel=F)


#_________________________________________________________
### Add life stage information
#_________________________________________________________

load("eagle_names.RData")
eagles <- eagles[order(eagles$acc_name),]

stages <- read.csv("Goldeneagles10_2021.csv", stringsAsFactors = F)
stages <- stages[order(stages$id),]
stages <- stages[which(stages$id %in% eagles$acc_name),]
# get the fledging date column re-arranged
stages <- separate(stages, col = "date_fledging", c("day_f", "month_f", "year_f"), sep = "\\.", remove = F)
stages$date_fledging <- paste(stages$year_f, stages$month_f, stages$day_f, sep = "-")
stages$dt_fledge <- as.POSIXct(paste0(stages$date_fledging, " ", stages$time_fledging), tz = "UTC")
# get the emigration date column re-arranged
stages <- separate(stages, col = "date_emigration", c("day_e", "month_e", "year_e"), sep = "\\.", remove = F)
stages$date_emigration <- paste(stages$year_e, stages$month_e, stages$day_e, sep = "-")
stages$dt_emigrate <- paste0(stages$date_emigration, " ", stages$time_emigration)
#set all the missing emigration dates to today so that it is impossible for a bird to have timestamps > emigration date
stages$dt_emigrate <- as.POSIXct(sub("NA-NA-NA NA", Sys.time(), stages$dt_emigrate), tz = "UTC")


seg_fls <- list.files("segments/")

for (i in 1:length(seg_fls)) {
  # get the associated and segmented ACC & GPS data from a single id
  load(paste0("segments/", seg_fls[i]))
  # for each id, match stages to move, then say before fledge, after fledge, and after emigration
  HRdf$stage <- NA
  # timestamps less than date of fledging are classified as 1 (pre-fledging)
  HRdf$stage[which(HRdf$timestamp < stages$dt_fledge[which(stages$id == eagles$acc_name[which(eagles$gps_name == unique(HRdf$individual_id))])])] <- 1
  # timestamps greater than date of fledging & less than date of emigration are classfied as 2 (fledgling)
  HRdf$stage[which(HRdf$timestamp > stages$dt_fledge[which(stages$id == eagles$acc_name[which(eagles$gps_name == unique(HRdf$individual_id))])] & 
                     HRdf$timestamp < stages$dt_emigrate[which(stages$id == eagles$acc_name[which(eagles$gps_name == unique(HRdf$individual_id))])])] <- 2
  # timesamps greater than date of emigration are classfied as 3 (emigrant)
  HRdf$stage[which(HRdf$timestamp > stages$dt_emigrate[which(stages$id == eagles$acc_name[which(eagles$gps_name == unique(HRdf$individual_id))])])] <- 3
  # save the file again
  save(HRdf, file = paste0("segments/", seg_fls[i]))
  # signal
  print(paste0("Added life stage information to ", unique(HRdf$local_identifier), "."), quote = F)
}

