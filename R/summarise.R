#' summarise
#'
#' @param outputfolder RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @return List with two data frames: D24HR and D5min
#' @export
summarise = function(outputfolder, csvfile, desiredtz) {
  if (file.exists(paste0(outputfolder,"/",csvfile))) {
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
    #----------------------------------------------------
    D$status = 4 # inconclusive, if logic belows works correctly then there should be no inconclusive timestamps
    sleep = which(D$withingsleep == 1 & is.na(D$PSGmove) == TRUE & is.na(D$batinteract) == TRUE &
                    is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE & is.na(D$AppAct) == TRUE)
    D$status[sleep] = -1 # withings sleep, but also no movement, and no app activity.
    #----------------------------------------------------
    inactive = which(is.na(D$PSGmove) == TRUE & is.na(D$batinteract) == TRUE &
                       is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE &
                       (D$withingsleep == 0 | is.na(D$withingsleep) == TRUE) & is.na(D$AppAct) == TRUE)
    D$status[inactive] = 0 # lack of movement, but no sleep detected with Withings (phone app activity is not allowed)
    #----------------------------------------------------
    active = which(D$PSGmove == TRUE | D$batinteract == TRUE | D$phoneacc == TRUE | D$withingsactive == TRUE) # ... withings
    D$status[active] = 1 # any movement (phone app activity is not sufficient)
    #----------------------------------------------------
    missing = which(is.na(D$lighton) == TRUE & is.na(D$screenon) == TRUE &
                      is.na(D$PSGmove) == TRUE & is.na(D$AppAct == TRUE) &
                      is.na(D$batinteract == TRUE) & is.na(D$phoneacc) == TRUE &
                      is.na(D$AppHalted) == TRUE & is.na(D$withingsactive) == TRUE & is.na(D$withingsleep) == TRUE)
    D$status[missing] = 4 # if no data is available from any channels then label as missing
    
    # Create new dataframe with only status and timestamps
    tmpmin = D[,c("time","status")]
    
    # Create continuous timeseries
    time.POSIX = as.POSIXlt(tmpmin$time,tz=desiredtz)
    tmpmin$time.POSIX = time.POSIX
    
    tmpmin$time_num = as.numeric(tmpmin$time.POSIX)
    complete_time = seq(min(tmpmin$time_num),max(tmpmin$time_num),by=60)
    D_complete_time = data.frame(time_num=complete_time)
    Dminute = merge(D_complete_time,tmpmin,by="time_num")
    missingstatus = which(is.na(Dminute$status) == TRUE) # missing or inconclusive
    if (length(missingstatus) > 0) D$status[missingstatus] = 4
    Dminute$date = as.Date(Dminute$time)
    # Aggregate per day to indentify 
    tmpday_active = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==1)))
    colnames(tmpday_active) = c("date", "active")
    tmpday_inactive = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==0)))
    colnames(tmpday_inactive) = c("date", "inactive")
    tmpday_sleep = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==-1)))
    colnames(tmpday_sleep) = c("date", "sleep")
    tmpday_missing = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==4)))
    colnames(tmpday_missing) = c("date", "missing")
    Dday = merge(tmpday_active,tmpday_inactive,by="date")
    Dday = merge(Dday,tmpday_sleep,by="date")
    Dday = merge(Dday,tmpday_missing,by="date")
    # Exclude days with more than 80% missing or inconclusive data or less than X minutes of active and sleep data
    daysexclude = which(Dday$active < 10 | Dday$sleep < 10 | Dday$missing > (1440*(1/3)) )
    if (length(daysexclude) > 0) {
      Dminute = Dminute[-which(Dminute$date %in% Dday$date[daysexclude] == TRUE),]
      Dday = Dday[-daysexclude,]
    }
    # Go back to Dminute (1 minute resolution data)
    # if status is missing then take most common value of either inactive or sleep (active criteria are not met) in 90 minute window
    D9 = Dminute$status
    is.na(D9[which(D9 %in% c(1,4))]) = TRUE
    D8 = zoo::rollapply(D9,FUN = function(x) mean(x, na.rm=TRUE), width = 90) # most common value
    index_missing = which(Dminute$status == 4)
    Dminute$status[index_missing] = round(D8[index_missing]) # replace missing values by most common value in window
    # turn value into sleep if 95% or more of 90 minutes is inactive
    D6 = zoo::rollapply(Dminute$status,FUN=function(x) length(which(x == 0)),width = 90)
    Dminute$status[which(D6 > (90*0.95))] = -1 
    #  Aggregate per 5 minutes (most common status is used)
    Dminute$time_num_5min = round(Dminute$time_num / 5) * 5 # time rounded to five minutes
    D5min = aggregate(x = Dminute[,"status"],by = list(time = Dminute$time_num_5min),FUN = function(x) {
      MM = pracma::Mode(x)
      if (length(MM) == 0) MM = NA
      return(MM)
    })
    colnames(D5min) = c("time", "status")
    
    D5min$time.POSIX = as.POSIXlt(D5min$time,tz=desiredtz,origin="1970-01-01")
    D5min$date = as.character(as.Date(D5min$time.POSIX))
    D5min$hour = D5min$time.POSIX$hour
    D5min$min = D5min$time.POSIX$min
    D5min$hour_in_day = D5min$hour + (D5min$min/60)
    D5minB = D5min
    # Estimate time spent in status classes per day
    sleepdur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==-1)))
    colnames(sleepdur_perday) = c("date","sleepdur")
    activedur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==1)))
    colnames(activedur_perday) = c("date","activedur")
    inactivedur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==0)))
    colnames(inactivedur_perday) = c("date","inactivedur")
    D24HR = merge(sleepdur_perday,activedur_perday,by="date")
    D24HR = merge(D24HR,inactivedur_perday,by="date")
    # simplify classes to be able to do L5M10 analysis
    D5minB$status[which(D5minB$status == -1)] = 0 #sleep becomes inactivity
    # Calculate basic active/inactive descriptives per day:
    D5minB$rollmean5HR = zoo::rollapply(D5minB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(5*12),fill=NA)
    D5minB$rollmean10HR = zoo::rollapply(D5minB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(10*12),fill=NA)
    tmp = aggregate(x = D5minB$rollmean5HR,by = list(date = D5minB$date),FUN = function(x) which.min(x))
    tmp$x = tmp$x / 12
    colnames(tmp) = c("date","L5hour")
    D24HR = merge(D24HR,tmp,by="date")
    
    tmp = aggregate(x = D5minB$rollmean5HR,by = list(date = D5minB$date),FUN = function(x) min(x))
    tmp$x = tmp$x / 12
    colnames(tmp) = c("date","L5value")
    D24HR = merge(D24HR,tmp,by="date")
    tmp = aggregate(x = D5minB$rollmean10HR,by = list(date = D5minB$date),FUN = function(x) which.max(x))
    tmp$x = tmp$x / 12
    colnames(tmp) = c("date","M10hour")
    D24HR = merge(D24HR,tmp,by="date")
    tmp = aggregate(x = D5minB$rollmean10HR,by = list(date = D5minB$date),FUN = function(x) max(x))
    tmp$x = tmp$x / 12
    colnames(tmp) = c("date","M10value")
    D24HR = merge(D24HR,tmp,by="date")
    tmp = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) mean(x,na.rm = TRUE))
    colnames(tmp) = c("date","mean")
    D24HR = merge(D24HR,tmp,by="date")
    return(list(D24HR=D24HR,D5min=D5min))
  }
}