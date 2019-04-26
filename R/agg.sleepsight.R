#' agg.sleepsight
#'
#' @param outputfolder RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @param minmisratio minimum missing ratio per day, day will be ignored if more data is missing
#' @param shortwindow short window length in minutes to aggregate to
#' @param longwindow long window length in minutes to aggregate to
#' @return List with data frames that hold the aggregated data: D24HR, Dshort, Dlong, and Dsurvey.
#' @export
agg.sleepsight = function(outputfolder, csvfile, desiredtz, minmisratio=1/3, shortwindow = 1, longwindow = 30) {
  D24HR = Dshort = Dlong = Dsurvey = c()
  if (file.exists(paste0(outputfolder,"/",csvfile))) {
    # Load data
    D = data.table::fread(paste0(outputfolder,"/",csvfile))
    D = as.data.frame(D)
    # Potentially Withings data is available from both Direct download and from PDK.
    # Use direct download if available, if not then use pdk, if neither is available then do not process 
    # any data from this person.
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
        do.withings = FALSE
      }
    }
    if (do.withings == TRUE) { # only process file if there is Withingsdata
      # Define status categories
      #-------------------------------------------------------
      D$status = 5 # no data is default
      #----------------------------------------------------
      # SLEEP
      sleep = which(D$withingsleep == 1 | (is.na(D$GPSmove) == TRUE & is.na(D$batinteract) == TRUE &
                      is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE)) #& is.na(D$AppAct) == TRUE)
      D$status[sleep] = -1 # withings sleep AND no phone movement AND no phone app activity AND no withings activity
      #----------------------------------------------------
      # INACTIVE
      inactive = which(is.na(D$GPSmove) == TRUE & is.na(D$batinteract) == TRUE &
                         is.na(D$withingsactive) == TRUE & is.na(D$phoneacc) == TRUE &
                         (D$withingsleep == 0 | is.na(D$withingsleep) == TRUE))
      D$status[inactive] = 0 # lack of movement, but no sleep detected with Withings (phone app activity is allowed)
      #----------------------------------------------------
      # ACTIVE
      active = which(D$GPSmove == TRUE | D$batinteract == TRUE | D$phoneacc == TRUE | D$withingsactive == TRUE)
      D$status[active] = 1 # any movement (phone app activity is not sufficient, only phone movement or Withings activity counts)
      #----------------------------------------------------
      # INCONCLUSIVE
      inconclusive = which(is.na(D$lighton) == TRUE & is.na(D$screenon) == TRUE &
                        is.na(D$GPSmove) == TRUE & is.na(D$AppAct == TRUE) &
                        is.na(D$batinteract == TRUE) & is.na(D$phoneacc) == TRUE &
                        is.na(D$AppHalted) == TRUE & is.na(D$withingsactive) == TRUE & is.na(D$withingsleep) == TRUE)
      D$status[inconclusive] = 4 

      #----------------------------------------------------
      # Create new dataframe with only status and timestamps
      tmpmin = D[,c("time","status")]
      # Untill here the data may include gaps in time.
      # We will now create continuous time series to ease plotting
      time.POSIX = as.POSIXlt(tmpmin$time,tz=desiredtz)
      tmpmin$time.POSIX = time.POSIX
      tmpmin$time_num = as.numeric(tmpmin$time.POSIX)
      complete_time = seq(min(tmpmin$time_num),max(tmpmin$time_num),by=60)
      D_complete_time = data.frame(time_num=complete_time)
      tmpmin = tmpmin[,-c(which(colnames(tmpmin) %in% c("time","time.POSIX") == TRUE))]
      Dminute = base::merge(D_complete_time,tmpmin,by="time_num",all = TRUE)
      missing_status = which(is.na(Dminute$status) == TRUE) # missing
      if (length(missing_status) > 0) Dminute$status[missing_status] = 5
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
      tmpday_inconclusive = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==4)))
      colnames(tmpday_inconclusive) = c("date", "inconclusive")
      tmpday_missing = aggregate(x = Dminute[,"status"],by = list(date = Dminute$date),FUN = function(x) length(which(x==5)))
      colnames(tmpday_missing) = c("date", "missing")
      
      Dday = merge(tmpday_active,tmpday_inactive,by="date")
      Dday = merge(Dday,tmpday_sleep,by="date")
      Dday = merge(Dday,tmpday_missing,by="date")
      Dday = merge(Dday,tmpday_inconclusive,by="date")
      # Exclude days with more than minmisratio missing data
      #  or less than 10 minutes of both active and sleep data
      #  or less than 1440 minutes in a day
      DAYLENGTH = aggregate(x = Dminute$status,by = list(date = Dminute$date),FUN = function(x) length(x))
      shortdays = Dminute$date[which(Dminute$date %in% DAYLENGTH$date[which(DAYLENGTH$x < 1440)] ==  TRUE)]
      daysexclude = which((Dday$active < 10 & Dday$sleep < 10) | Dday$missing > (1440*(minmisratio)))
      if (length(shortdays) > 0) { # exclude also days with less 
        daysexclude = unique(c(daysexclude,which(Dday$date %in% shortdays == TRUE)))
      }
      if (length(daysexclude) > 0) {
        Dminute = Dminute[-which(Dminute$date %in% Dday$date[daysexclude] == TRUE),]
        Dday = Dday[-daysexclude,]
      }

      #========================================================
      # create new category sustained inactivity, if 95% or more of 90 minutes is inactive
      D6 = zoo::rollapply(Dminute$status,FUN=function(x) length(which(x == 0)),width = 90)
      replacestatus = which(D6 > (90*0.95))
      statuszero = which(Dminute$status == 0)
      replacestatus = replacestatus[which(replacestatus %in% statuszero == TRUE)]
      if (length(replacestatus) > 0) Dminute$status[replacestatus] = -2 # sustained inactive
      #========================================================
      #
      # NOTE: This is probably a good place to impute missing values, if we want to impute values...
      #
      #========================================================
      # Aggregate per shortwindow and longwindow (the most common status is used if windowlength is larger than 1)
      Dminute$time_num_short = round(Dminute$time_num / (shortwindow *60)) * (shortwindow *60)
      Dminute$time_num_long = round(Dminute$time_num / (longwindow *60)) * (longwindow*60)
      if (nrow(Dminute) > 1440) {
        calcmode = function(x) {
          if (length(x) == 1) {
            MM = x
          } else {
            MM = pracma::Mode(x)
            if (length(MM) == 0) MM = NA
          }
          return(MM)
        }
        if (shortwindow == 1) {
          Dshort = Dminute[,c("time", "status")]
        } else {
          Dshort = aggregate(x = Dminute$status,by = list(time = Dminute$time_num_short),FUN = calcmode)
        }
        Dlong = aggregate(x = Dminute$status,by = list(time = Dminute$time_num_long),FUN = calcmode)
        colnames(Dshort) = c("time", "status")
        colnames(Dlong) = c("time", "status")
        
        AddTimeToDF = function(df) {
          df$time.POSIX = as.POSIXlt(df$time,tz=desiredtz,origin="1970-01-01")
          df$date = as.character(as.Date(df$time.POSIX))
          df$hour = df$time.POSIX$hour
          df$min = df$time.POSIX$min
          df$hour_in_day = df$hour + (df$min/60)
          df$month = paste0(format(df$time.POSIX,"%m"),"-",format(df$time.POSIX,"%Y"))
          df$day = format(df$time.POSIX,"%d")
          df$weekday = weekdays(df$time.POSIX)
          return(df)
        }
        status2factor = function(df) {
          # To ease intrerpretation, replace numeric coding by factor names
          df$status = as.character(df$status)
          df$status[which(df$status == "1")] = "active"
          df$status[which(df$status == "0")] = "inactive"
          df$status[which(df$status == "-1")] = "sleep"
          df$status[which(df$status == "-2")] = "sustained inactive"
          df$status[which(df$status == "4")] = "inconclusive"
          df$status[which(df$status == "5")] = "no data"
          df$status = as.factor(df$status)
          return(df)
        }
        Dshort = AddTimeToDF(Dshort) 
        DshortB = Dshort # used only for calculating D24HR
        Dshort = status2factor(Dshort) # used as output
        Dlong = AddTimeToDF(Dlong)
        Dlong = status2factor(Dlong)
        #========================================================
        # Aggregate per day
        mydivfun = function(tmp,dn) {
          # divide tmp by dn while ignore NA values
          nax = which(is.na(tmp$x) == FALSE); tmp$x[nax] = tmp$x[nax] / dn 
          return(tmp)
        }
        
        NSinH = 60 / shortwindow #number of shortwindows in an hours
        
        sleepdur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==-1)))
        sleepdur_perday = mydivfun(sleepdur_perday,dn=NSinH) # convert x-5-minute window of a day into hour
        colnames(sleepdur_perday) = c("date","sleepdur") # duration expressed in minutes
        
        susindur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==-2)))
        susindur_perday = mydivfun(susindur_perday,dn=NSinH) # convert x-5-minute window of a day into hour
        colnames(susindur_perday) = c("date","susindur") # duration expressed in minutes
        
        activedur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==1)))
        activedur_perday = mydivfun(activedur_perday,dn=NSinH)
        colnames(activedur_perday) = c("date","activedur")
        
        inactivedur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==0)))
        inactivedur_perday = mydivfun(inactivedur_perday,dn=NSinH)
        colnames(inactivedur_perday) = c("date","inactivedur")
        
        inconclusive_dur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==4)))
        inconclusive_dur_perday = mydivfun(inconclusive_dur_perday,dn=NSinH)
        colnames(inconclusive_dur_perday) = c("date","inconclusive_dur")
        
        missing_dur_perday = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(which(x==5)))
        missing_dur_perday = mydivfun(missing_dur_perday,dn=NSinH)
        colnames(missing_dur_perday) = c("date","missing_dur")
        
        D24HR = merge(sleepdur_perday,activedur_perday,by="date")
        D24HR = merge(D24HR,inactivedur_perday,by="date")
        D24HR = merge(D24HR,susindur_perday,by="date")
        D24HR = merge(D24HR,inconclusive_dur_perday,by="date")
        D24HR = merge(D24HR,missing_dur_perday,by="date")
        #--------------------------------------------------------
        # Calculate other summary statistics:
        # simplify classes to be able to do L5M10 analysis
        DshortB$status[which(DshortB$status == -1)] = 0 #sleep becomes inactivity
        DshortB$status[which(DshortB$status == -2)] = 0 #possible sleep becomes inactivity
        is.na(DshortB$status[which(DshortB$status == 4 | DshortB$status == 5)]) = TRUE #inconclusive and missing data become NA
        
        DshortB$rollmean5HR = zoo::rollapply(DshortB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(5*NSinH),fill=NA)
        tmp = aggregate(x = DshortB$rollmean5HR,by = list(date = DshortB$date),FUN = function(x) which.min(x)[1])
        tmp = mydivfun(tmp,dn=NSinH) # convert x-5-minute window of a day into hour
        colnames(tmp) = c("date","L5hour")
        D24HR = merge(D24HR,tmp,by="date")
        tmp = aggregate(x = DshortB$rollmean5HR,by = list(date = DshortB$date),FUN = function(x) {
          mv = min(x,na.rm = TRUE)
          if (mv == Inf | mv == Inf) mv = NA
          return(mv)
        })
        colnames(tmp) = c("date","L5value")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        DshortB$rollmean10HR = zoo::rollapply(DshortB$status,FUN=function(x) mean(x,na.rm=TRUE),width=(10*NSinH),fill=NA)
        tmp = aggregate(x = DshortB$rollmean10HR,by = list(date = DshortB$date),FUN = function(x) which.max(x)[1])
        tmp = mydivfun(tmp,dn=NSinH)
        colnames(tmp) = c("date","M10hour")
        D24HR = merge(D24HR,tmp,by="date")
        tmp = aggregate(x = DshortB$rollmean10HR,by = list(date = DshortB$date),FUN = function(x) {
          mv = max(x,na.rm = TRUE)
          if (mv == Inf | mv == Inf) mv = NA
          return(mv)
        })
        colnames(tmp) = c("date","M10value")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        tmp = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) length(x))
        tmp = mydivfun(tmp,dn=NSinH)
        colnames(tmp) = c("date","daylength")
        D24HR = merge(D24HR,tmp,by="date")
        #--------------------------------------------------------
        tmp = aggregate(x = DshortB$status,by = list(date = DshortB$date),FUN = function(x) mean(x,na.rm = TRUE))
        colnames(tmp) = c("date","mean")
        D24HR = merge(D24HR,tmp,by="date")
        #----------------------------------------------
        # Also load survey data
        surveyfilename = paste0(outputfolder,"/SleepSurvey.RData")
        if (file.exists(surveyfilename) == TRUE) {
          load(surveyfilename)
          Dsurvey = SleepSurvey[,c("positiveFeelings","negativeFeelings","Sleep.Quality.Value","surveytime")]
          Dsurvey$date = as.Date(as.POSIXlt(Dsurvey$surveytime,tz=desiredtz))
        }
        
      }
    }
    return(list(D24HR=D24HR,Dshort=Dshort,Dlong=Dlong,Dsurvey=Dsurvey))
  }
}