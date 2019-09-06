#' export2csv
#'
#' @param outputfolder RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @param overwrite.preprocess2csv whether to overwrite existing csvfile
#' @param startDate Character,  optional argument to specify the startDate of the recording, e.g. 2019-01-01
#' @param endDate Character, optional argument to specify the startDate of the recording, e.g. 2019-01-01
#' @return no output, just a file is stored
#' @export
#' @importFrom utils write.csv
export2csv = function(outputfolder, csvfile, desiredtz, overwrite.preprocess2csv,
                      startDate = c(), endDate = c()) {
  if (!file.exists(csvfile) | overwrite.preprocess2csv == TRUE) {
    cat("\n* Export to csv")
    # to avoid warning "no visible binding for global variable " by R CMD check
    # when it sees that we are using undeclared objects, which actually are loaded
    PhoneAcc =withings_act = withings_actDD = AppHalted = c()
    rm(PhoneAcc, withings_act, withings_actDD, AppHalted)
    
    RDAfiles = dir(outputfolder,full.names = TRUE)
    for (RDAfile in RDAfiles) {
      if (length(unlist(strsplit(RDAfile,"[.]cs"))) < 2 & length(unlist(strsplit(RDAfile,"[.]pn"))) < 2) {
        load(file=RDAfile)
      }
    }
    #----------------------------------------------------------------
    # Round resolution of all channels to 1 minute to simplify merging
    aggregatePerMinute = function(x, desiredtz) {
      # aggregate input timestamps per minute
      x_num = round(as.numeric(x)/ 60)*60
      x_num = unique(x_num)
      x_60sec = as.POSIXlt(x_num,origin="1970-01-01",tz=desiredtz)
      return(x_60sec)
    }
    addToDF = function(x=c(),y=c(), desiredtz) {
      if (length(x) == 0) {
        dat = y
      } else {
        # conversion from POSIX to character to POSIX takes time
        # but not doing this seems to cause Windows/Linux consistency issues
        # when merging the timestamps
        x$time = as.character(x$time)
        y$time = as.character(y$time)
        dat = merge(x, y, by="time", all = TRUE)
        dat$time = as.POSIXlt(dat$time,origin="1970-01-01",tz=desiredtz, format="%Y-%m-%d %H:%M:%OS")
      }
      return(dat)
    }
    #------------------------------------------------------------------------
    # standardise time resolution (1 minute)
    # and merge all channels in one data.frame df
    df = c()
    if ("lightOnTimes" %in% ls()) {
      time_num = round(as.numeric(lightOnTimes)/60) * 60 # needed for aggregating the lightLevel further down
      # aggregate to minute level
      lightdf = data.frame(time_num = time_num, lightLevel = lightLevel)
      lightdf_min = aggregate(lightdf, by = list(lightdf$time_num),FUN = mean)
      # tidy up data.frame to enable merging to df
      lightdf_min$time = as.character(as.POSIXlt(lightdf_min$time_num, origin="1970-1-1", tz= desiredtz))
      lightdf_min = lightdf_min[,c("time","lightLevel")]
      lightdf_min$lightLevel = as.integer(round(lightdf_min$lightLevel))
      lightdf_min$lighton = TRUE
      df = addToDF(df,lightdf_min, desiredtz)
    }
    if ("ScreenOnTimes" %in% ls()) {
      ScreenOnTimes = aggregatePerMinute(ScreenOnTimes, desiredtz) # from 1 to 60 seconds
      ScreenOn = data.frame(time = ScreenOnTimes,screenon = TRUE)
      df = addToDF(df,ScreenOn, desiredtz)
    }
    if ("MovementGPSTimes" %in% ls()) {
      MovementGPSTimes = aggregatePerMinute(MovementGPSTimes, desiredtz) #from 1 to 60 seconds
      GPSmove = data.frame(time = MovementGPSTimes,GPSmove = TRUE)
      df = addToDF(df,GPSmove, desiredtz)
    }
    if ("AppActiveTimes" %in% ls()) {
      AppActiveTimes = aggregatePerMinute(AppActiveTimes, desiredtz) # from 5 to 60 seconds
      appactive = data.frame(time = AppActiveTimes,AppAct = TRUE)
      df = addToDF(df,appactive, desiredtz)
    }
    if ("batInteractTimes" %in% ls()) {
      batInteractTimes = aggregatePerMinute(batInteractTimes, desiredtz) # from 1 to 60 seconds
      batinteract = data.frame(time = batInteractTimes, batinteract = TRUE)
      df = addToDF(df,batinteract, desiredtz)
    }
    if ("PhoneAcc" %in% ls()) {
      PhoneAccTimes = PhoneAcc$Created.Date.POSIX[which((PhoneAcc$acceleration/9.81) > 0.03)]
      phoneacc = data.frame(time = PhoneAccTimes, phoneacc = TRUE)
      df = addToDF(df,phoneacc, desiredtz)
    }
    aggregate_withingsact = function(x) {
      x$timestamp = as.POSIXlt(x$timestamp,origin="1970-1-1",tz=desiredtz)
      # aggregate to 1 minute
      x$timestamp_num = round(as.numeric(x$timestamp)/ 60)*60
      x = x[,-which(colnames(x) == "timestamp")]
      mysum = function(x) {
        if (length(which(is.na(x) == FALSE)) > 0) {
          S = sum(x, na.rm = TRUE)
        } else {
          S = 0
        }
        return(S)
      }
      mydf = x
      x = aggregate(mydf[,"steps"],by = list(mydf$timestamp_num),FUN = mysum)
      colnames(x) = c("timestamp_num","steps")
      x$time = as.POSIXlt(x$timestamp_num,origin="1970-1-1",tz=desiredtz)
      x = x[,-which(colnames(x) == "timestamp_num")]
      return(x)
    }
    if ("withings_act" %in% ls()) { # PDK
      if ("infoentered" %in% colnames(withings_act)) {
        movei = which(withings_act$infoentered == TRUE | withings_act$movement == TRUE)
      } else {
        movei = which(withings_act$movement == TRUE)
      }
      WithingsMovement = withings_act[movei,which(colnames(withings_act) %in% c("timestamp","steps") == TRUE)]
      WithingsMovement = aggregate_withingsact(WithingsMovement)
      WithingsMovement$withingsMove_pdk = TRUE
      CLWM = colnames(WithingsMovement)
      colnames(WithingsMovement)[which(CLWM == "steps")] = "steps_pdk"
      df = addToDF(df, WithingsMovement, desiredtz)
      rm(WithingsMovement)
    }
    if ("withings_actDD" %in% ls()) { # Direct download
      if ("infoentered" %in% colnames(withings_actDD)) {
        movei = which(withings_actDD$infoentered == TRUE | withings_actDD$movement == TRUE)
      } else {
        movei = which(withings_actDD$movement == TRUE)
      }
      withings_actDD$steps = as.numeric(as.character(withings_actDD$steps)) # steps are stored as factor
      WithingsMovement = withings_actDD[movei,which(colnames(withings_actDD) %in% c("timestamp","steps") == TRUE)]
      WithingsMovement = aggregate_withingsact(WithingsMovement)
      WithingsMovement$withingsMove_dd = TRUE
      CLWM = colnames(WithingsMovement)
      colnames(WithingsMovement)[which(CLWM == "steps")] = "steps_dd"
      df = addToDF(df, WithingsMovement, desiredtz)
    }
    if ("withings_sleep" %in% ls()) { # PDK
      withings_sleep=withings_sleep[,-which(colnames(withings_sleep) == "statecode")]
      WSN = colnames(withings_sleep)
      colnames(withings_sleep)[which(WSN=="statename")] = "sleepstate"
      withings_sleep$sleepstate =as.character(withings_sleep$sleepstate)
      sleep_deep_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="deep-sleep")],deepsleep_pdk=TRUE)
      sleep_light_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="light-sleep")],lightsleep_pdk=TRUE)
      sleep_awake_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="awake")],awake_pdk=TRUE)
      df = addToDF(df,sleep_deep_pdk,desiredtz)
      df = addToDF(df,sleep_light_pdk,desiredtz)
      df = addToDF(df,sleep_awake_pdk,desiredtz)
    }
    if ("withings_sleepDD" %in% ls()) { # Direct download
      WSN = colnames(withings_sleepDD)
      colnames(withings_sleepDD)[which(WSN=="statecode")] = "sleepstate"
      withings_sleepDD$sleepstate =as.character(withings_sleepDD$sleepstate)
      sleep_deep_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="deep-sleep")],deepsleep_dd=TRUE)
      sleep_light_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="light-sleep")],lightsleep_dd=TRUE)
      sleep_awake_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="awake")],awake_dd=TRUE)
      df = addToDF(df,sleep_deep_dd,desiredtz)
      df = addToDF(df,sleep_light_dd, desiredtz)
      df = addToDF(df,sleep_awake_dd,desiredtz)
    }
    if ("AppHalted" %in% ls()) {
      AppHaltedTimes = aggregatePerMinute(AppHalted, desiredtz) # from 1 to 60 seconds
      AppHaltedTimes = data.frame(time = AppHaltedTimes, AppHalted  = TRUE)
      df = addToDF(df,AppHaltedTimes,desiredtz)
    }
    if ("SunSetRise" %in% ls()) {
      SunSetRise$time = as.POSIXlt(SunSetRise$timestamp,origin="1970-1-1",tz=desiredtz)
      SunSetRise$time = aggregatePerMinute(SunSetRise$time, desiredtz) # from 1 to 60 seconds
      CS = colnames(SunSetRise)
      colnames(SunSetRise)[which(CS=="event")] = "SunSetRise"
      SunSetRise=SunSetRise[,-which(colnames(SunSetRise) == "timestamp")]
      df = addToDF(df,SunSetRise,desiredtz)
    }
    if ("SleepSurvey" %in% ls()) {
      #------------------------
      # Try to correct unlogical rise- before bed-times (commented out, we can revisit this later)
      # rst = as.POSIXlt(SleepSurvey$risetime)$hour
      # bdt = as.POSIXlt(SleepSurvey$bedtime)$hour
      # srt = as.POSIXlt(SleepSurvey$surveytime)$hour
      # bedtime_min24 = which(bdt > 18 & rst < bdt) # bedtime -24
      # bedtime_min12 = which(bdt <= 18 & rst < bdt) # bedtime -12
      # risetime_min12 = which(rst > 18 & rst < bdt) # risetime -12
      # risetime_plus24 = which(rst >= srt & rst < (srt + 4) & rst < bdt) # risetime +24
      # if (length(risetime_min12) > 0) SleepSurvey$risetime[risetime_min12] = SleepSurvey$risetime[risetime_min12] - (12*3600)
      # if (length(bedtime_min12) > 0) SleepSurvey$bedtime[bedtime_min12] = SleepSurvey$bedtime[bedtime_min12] - (12*3600)
      # if (length(bedtime_min24) > 0) SleepSurvey$bedtime[bedtime_min24] = SleepSurvey$bedtime[bedtime_min24] - (24*3600)
      # if (length(risetime_plus24) > 0) SleepSurvey$risetime[risetime_plus24] = SleepSurvey$risetime[risetime_plus24] + (24*3600)
      #------------------------
      # Add time in bed (self-reported)
      SleepSurvey$bedtime_num = as.numeric(SleepSurvey$bedtime) # work with numeric time
      SleepSurvey$risetime_num = as.numeric(SleepSurvey$risetime) # work with numeric time
      inplausible = which(SleepSurvey$risetime_num < SleepSurvey$bedtime_num)
      if (length(inplausible) > 0) SleepSurvey = SleepSurvey[-inplausible,]
      InBedTimes = c()
      for (jj in 1:nrow(SleepSurvey)) {
        InBedTimes = c(InBedTimes, seq(SleepSurvey$bedtime_num[jj],SleepSurvey$risetime_num[jj],by=60))
        # Note this is the place where double timestamps can originate, because some reported times in bed overlap
      }
      InBedTimes = as.POSIXlt(InBedTimes, origin="1970-1-1", tz=desiredtz)
      inbed = data.frame(time=InBedTimes, InBed=TRUE)
      inbed = inbed[!duplicated(inbed),] # remove double entries
      df = addToDF(df,inbed,desiredtz)
      df$time = as.POSIXlt(df$time, origin="1970-1-1", tz=desiredtz)
      # add survey
      survey = SleepSurvey[,c("surveytime","positiveFeelings","negativeFeelings","Sleep.Quality.Value","Sleep.Duration")]
      colnames(survey)[which(colnames(survey) == "surveytime")] = "time"
      df = addToDF(df,survey,desiredtz)
    }
    # add hour in the day to ease plotting
    df$hour = as.POSIXlt(df$time, tz = desiredtz, origin= "1970-1-1")$hour
    df$min = as.POSIXlt(df$time, tz = desiredtz, origin= "1970-1-1")$min
    df$min_inday = df$hour * 60 + df$min
    clock_char = strftime(df$time,format="%H:%M:%S",tz=desiredtz)
    df= df[!duplicated(df),] # remove double entries
    if (length(startDate) > 0 & length(endDate) > 0) {
      startDateNum = as.numeric(as.Date(startDate)) * 3600*24
      endDateNum = as.numeric(as.Date(endDate)) * 3600*24
      timeNum = as.numeric(df$time)
      validdates = which(timeNum > startDateNum & timeNum < endDateNum)
      if (length(validdates) != 0) df = df[validdates,]
    }
    df$time = as.POSIXlt(df$time,origin="1970-01-01",tz=desiredtz)
    

    write.csv(df,file=csvfile,row.names = FALSE)
  }
}
