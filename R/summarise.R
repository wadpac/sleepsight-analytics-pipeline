#' summarise
#'
#' @param outputfolder RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @return List with two data frames: D24HR and D5min
#' @export
summarise = function(outputfolder, csvfile, desiredtz) {
  D24HR = D5min = Dsurvey = c()
  if (file.exists(paste0(outputfolder,"/",csvfile))) {
    D = data.table::fread(paste0(outputfolder,"/",csvfile))
    D = as.data.frame(D)
    # Simplify Withings data. Use direct download, if not pdk, and if not available NA.
    CDF = colnames(D)
    do.withings = TRUE
    if ("withingsMove_dd" %in% CDF) {
      D$withingsactive = D$withingsMove_dd
      D$withingsleep = rowSums(cbind(D$deepsleep_dd, D$lightsleep_dd),na.rm=TRUE) #D$awake_dd
    } else {
      if ("withingsMove_pdk" %in% CDF) {
        D$withingsactive = D$withingsMove_pdk
        D$withingsleep = rowSums(cbind(D$deepsleep_pdk, D$lightsleep_pdk),na.rm=TRUE) #, D$awake_pdk
      } else {
        # D$withingsactive = NA
        # D$withingsleep = NA
        do.withings = FALSE
      }
    }
    if (do.withings == TRUE) { # only process file if there is Withingsdata
      
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
      
      surveyfilename = paste0(outputfolder,"/SleepSurvey.RData")
      if (file.exists(surveyfilename) == TRUE) {
        load(surveyfilename)
        Dsurvey = SleepSurvey[,c("positiveFeelings","negativeFeelings","Sleep.Quality.Value","surveytime")]
        Dsurvey$date = as.Date(as.POSIXlt(Dsurvey$surveytime,tz=desiredtz))
      }
      # Untill here the data may include gaps in time.
      # We will now create continuous time series to ease plotting
      time.POSIX = as.POSIXlt(tmpmin$time,tz=desiredtz)
      tmpmin$time.POSIX = time.POSIX
      tmpmin$time_num = as.numeric(tmpmin$time.POSIX)
      complete_time = seq(min(tmpmin$time_num),max(tmpmin$time_num),by=60)
      D_complete_time = data.frame(time_num=complete_time)
      tmpmin = tmpmin[,-c(which(colnames(tmpmin) %in% c("time","time.POSIX") == TRUE))]
      Dminute = base::merge(D_complete_time,tmpmin,by="time_num",all = TRUE)
      missingstatus = which(is.na(Dminute$status) == TRUE) # missing or inconclusive
      if (length(missingstatus) > 0) Dminute$status[missingstatus] = 4
      Dminute$time = as.POSIXlt(Dminute$time_num,origin="1970-1-1",tz=desiredtz)
      Dminute$date = as.Date(Dminute$time)
      #========================================================
      # Remove days with not enough data
      # To do this we have to aggregate per day
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
      # Exclude days with more than 80% missing or
      #  or inconclusive data
      #  or less than X minutes of active and sleep data
      #  or less than 1440 minutes in a day
      DAYLENGTH = aggregate(x = Dminute$status,by = list(date = Dminute$date),FUN = function(x) length(x))
      shortdays = Dminute$date[which(Dminute$date %in% DAYLENGTH$date[which(DAYLENGTH$x < 1440)] ==  TRUE)]
      daysexclude = which(Dday$active < 10 | Dday$sleep < 10 | Dday$missing > (1440*(1/3)))
      if (length(shortdays) > 0) { # exclude also days with less 
        daysexclude = unique(c(daysexclude,which(Dday$date %in% shortdays == TRUE)))
      }
      if (length(daysexclude) > 0) {
        Dminute = Dminute[-which(Dminute$date %in% Dday$date[daysexclude] == TRUE),]
        Dday = Dday[-daysexclude,]
      }
      
      
      
      # turn value into sleep if 95% or more of 90 minutes is inactive
      D6 = zoo::rollapply(Dminute$status,FUN=function(x) length(which(x == 0)),width = 90)
      Dminute$status[which(D6 > (90*0.95))] = -1
      
      # TO DO: This could be a good place to impute missing values, if we want to impute values
      #========================================================
      # Aggregate per 5 minutes (the most common status is used)
      Dminute$time_num_5min = round(Dminute$time_num / 300) * 300 # time rounded to five minutes (300 seconds)
      if (nrow(Dminute) > 1440) {
        calcmode = function(x) {
          MM = pracma::Mode(x)
          if (length(MM) == 0) MM = NA
          return(MM)
        }
        D5min = aggregate(x = Dminute$status,by = list(time = Dminute$time_num_5min),FUN = calcmode)
        colnames(D5min) = c("time", "status")
        D5min$time.POSIX = as.POSIXlt(D5min$time,tz=desiredtz,origin="1970-01-01")
        D5min$date = as.character(as.Date(D5min$time.POSIX))
        D5min$hour = D5min$time.POSIX$hour
        D5min$min = D5min$time.POSIX$min
        D5min$hour_in_day = D5min$hour + (D5min$min/60)
        D5minB = D5min
        #========================================================
        # Aggregate per day
        mydivfun = function(tmp,dn) {
          # divide tmp by dn while ignore NA values
          nax = which(is.na(tmp$x) == FALSE); tmp$x[nax] = tmp$x[nax] / dn 
          return(tmp)
        }
        sleepdur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==-1)))
        sleepdur_perday = mydivfun(sleepdur_perday,dn=12) # convert x-5-minute window of a day into hour
        colnames(sleepdur_perday) = c("date","sleepdur") # duration expressed in minutes
        
        activedur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==1)))
        activedur_perday = mydivfun(activedur_perday,dn=12)
        colnames(activedur_perday) = c("date","activedur")
        
        inactivedur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==0)))
        inactivedur_perday = mydivfun(inactivedur_perday,dn=12)
        colnames(inactivedur_perday) = c("date","inactivedur")
        
        missingdur_perday = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(which(x==4)))
        missingdur_perday = mydivfun(missingdur_perday,dn=12)
        colnames(missingdur_perday) = c("date","missingdur")
        
        D24HR = merge(sleepdur_perday,activedur_perday,by="date")
        D24HR = merge(D24HR,inactivedur_perday,by="date")
        D24HR = merge(D24HR,missingdur_perday,by="date")
        # simplify classes to be able to do L5M10 analysis
        D5minB$status[which(D5minB$status == -1)] = 0 #sleep becomes inactivity
        is.na(D5minB$status[which(D5minB$status == 4)]) = TRUE #missing data becomes NA
        # Calculate other summary statistics:
        #--------------------------------------------------------
        D5minB$rollmean5HR = zoo::rollapply(D5minB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(5*12),fill=NA)
        tmp = aggregate(x = D5minB$rollmean5HR,by = list(date = D5minB$date),FUN = function(x) which.min(x)[1])
        tmp = mydivfun(tmp,dn=12) # convert x-5-minute window of a day into hour
        colnames(tmp) = c("date","L5hour")
        D24HR = merge(D24HR,tmp,by="date")
        tmp = aggregate(x = D5minB$rollmean5HR,by = list(date = D5minB$date),FUN = function(x) {
          mv = min(x,na.rm = TRUE)
          if (mv == Inf | mv == Inf) mv = NA
          return(mv)
        })
        colnames(tmp) = c("date","L5value")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        D5minB$rollmean10HR = zoo::rollapply(D5minB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(10*12),fill=NA)
        tmp = aggregate(x = D5minB$rollmean10HR,by = list(date = D5minB$date),FUN = function(x) which.max(x)[1])
        tmp = mydivfun(tmp,dn=12)
        colnames(tmp) = c("date","M10hour")
        D24HR = merge(D24HR,tmp,by="date")
        tmp = aggregate(x = D5minB$rollmean10HR,by = list(date = D5minB$date),FUN = function(x) {
          mv = max(x,na.rm = TRUE)
          if (mv == Inf | mv == Inf) mv = NA
          return(mv)
        })
        colnames(tmp) = c("date","M10value")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        tmp = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) length(x))
        tmp = mydivfun(tmp,dn=12)
        colnames(tmp) = c("date","daylength")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        tmp = aggregate(x = D5minB$status,by = list(date = D5minB$date),FUN = function(x) mean(x,na.rm = TRUE))
        colnames(tmp) = c("date","mean")
        D24HR = merge(D24HR,tmp,by="date")
      }
    }
    return(list(D24HR=D24HR,D5min=D5min,Dsurvey=Dsurvey))
    
  }
}