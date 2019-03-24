# By: Vincent van Hees 2019
rm(list=ls())
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
library(Sleepsight)
library(data.table)
#==============================================================
# input variables
overwrite = FALSE # whether to overwrite previously generated output with this R code.
do.plot = FALSE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
desiredtz = "Europe/London"
studyfolder = "/media/vincent/sleepsight"
# Note: Assumption that all zip-files have been unzipped

#==============================================================
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)

foldersInStudyFolder = "/media/vincent/sleepsight/SS08"
for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))

  cat("\n* Preprocess")
  outputfolder = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite)
  
  cat("\n* Export to csv")
  tmpID = unlist(strsplit(outputfolder,"/"))
  personID = unlist(strsplit(tmpID[length(tmpID)],"_"))[2]
  csvfile = paste0("Sleepsight_overview_",personID,".csv")
  export2csv(outputfolder,csvfile,desiredtz)
  
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    cat("\n* Create histograms")
    testplot(outputfolder, csvfile)
  }
  #------------------------
  # Code in progress (to be moved to separate function)
  D = data.table::fread(paste0(outputfolder,"/",csvfile))
  D = as.data.frame(D)
  # Simplify Withings data. Use direct download, if not pdk, and if not available NA.
  CDF = colnames(D)
  if ("withingsMove_dd" %in% CDF) {
    D$withingsactive = D$withingsMove_dd
    D$withingsleep = rowSums(cbind(D$deepsleep_dd, D$lightsleep_dd),na.rm=TRUE) #D$awake_dd
  } else {
    if ("withingsMove_dd" %in% CDF) {
      D$withingsactive = D$withingsMove_pdk
      D$withingsleep = rowSums(cbind(D$deepsleep_pdk, D$lightsleep_pdk),na.rm=TRUE) #, D$awake_pdk
    } else {
      D$withingsactive = NA
      D$withingsleep = NA
    }
  }
  # Simplify all data to a single status indicator
  D$status = 0 # inconclusive, if logic belows works correctly then there should be no inconclusive timestamps

  active = which(D$PSGmove == TRUE | D$batinteract == TRUE | D$phoneacc == TRUE | D$withingsactive == TRUE) # ... withings
  D$status[active] = 1 # any movement (phone app activity is not sufficient)
  
  inactive = which(is.na(D$PSGmove) == TRUE & is.na(D$batinteract) == TRUE &
                     is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE &
                     (D$withingsleep == 0 | is.na(D$withingsleep) == TRUE) & is.na(D$AppAct) == TRUE)
  D$status[inactive] = 2 # lack of movement, but no sleep detected with Withings (phone app activity is not allowed)
  
  sleep = which(D$withingsleep == 1 & is.na(D$PSGmove) == TRUE & is.na(D$batinteract) == TRUE &
                  is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE & is.na(D$AppAct) == TRUE)
  D$status[sleep] = 3 # withings sleep, but also no movement, and no app activity.
  
  missing = which(is.na(D$lighton) == TRUE & is.na(D$screenon) == TRUE &
                    is.na(D$PSGmove) == TRUE & is.na(D$AppAct == TRUE) &
                    is.na(D$batinteract == TRUE) & is.na(D$phoneacc) == TRUE &
                    is.na(D$AppHalted) == TRUE & is.na(D$withingsactive) == TRUE & is.na(D$withingsleep) == TRUE)
  D$status[missing] = 4 # if no data is available from any channels then label as missing, which indicates that both
  
  # Create new dataframe with only status and timestamps
  D2 = D[,c("time","status")]
  
  # Create continuous timeseries
  time.POSIX = as.POSIXlt(D2$time,tz=desiredtz)
  D2$time.POSIX = time.POSIX
  
  D2$time_num = as.numeric(D2$time.POSIX)
  complete_time = seq(min(D2$time_num),max(D2$time_num),by=60)
  D_complete_time = data.frame(time_num=complete_time)
  D3 = merge(D_complete_time,D2,by="time_num")
  missingstatus = which(is.na(D3$status) == TRUE)
  if (length(missingstatus) > 0) D$status[missingstatus] = 0
  # Aggregate per day
  D3$date = as.Date(D3$time)
  D4_active = aggregate(x = D3[,"status"],by = list(date = D3$date),FUN = function(x) length(which(x==1)))
  colnames(D4_active) = c("date", "active")
  D4_inactive = aggregate(x = D3[,"status"],by = list(date = D3$date),FUN = function(x) length(which(x==2)))
  colnames(D4_inactive) = c("date", "inactive")
  D4_sleep = aggregate(x = D3[,"status"],by = list(date = D3$date),FUN = function(x) length(which(x==3)))
  colnames(D4_sleep) = c("date", "sleep")
  D4_missing = aggregate(x = D3[,"status"],by = list(date = D3$date),FUN = function(x) length(which(x==4)))
  colnames(D4_missing) = c("date", "missing")
  D4_inconclusive = aggregate(x = D3[,"status"],by = list(date = D3$date),FUN = function(x) length(which(x==0)))
  colnames(D4_inconclusive) = c("date", "inconclusive")
  D5 = merge(D4_active,D4_inactive,by="date")
  D5 = merge(D5,D4_sleep,by="date")
  D5 = merge(D5,D4_missing,by="date")
  D5 = merge(D5,D4_inconclusive,by="date")
  
  # TO DO:
  # * Exclude days with less than 10 minutes active and/or less than 10 minutes sleep
  D5 = D5[which(D5$active > 10 & D5$sleep > 10),]
  # * Go back to D3 (1 minute resolution data)
  #   and infer sleep/wake rythm with rolling window only for days with reliable data in D5
  #    e.g. 1 hour of inactivity becomes sleep (accepting tiny fraction of missing or inconclusive data)
  # * Aggregate per 5 minutes
  # * Calculate basic descriptives across rolling 24 windows:
  #   - mean, L5, M10, entropy, sleeponset and waking up time.
  # * Calculate descriptives across rolling X days (7/14?)
  
  
  #------------------------------------------------------------------
  timer1 = Sys.time()
  deltatime = difftime(timer0,timer1,units = "secs")
  cat(paste0("\n     ",deltatime))
}

