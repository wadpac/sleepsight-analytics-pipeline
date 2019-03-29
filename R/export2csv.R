#' export2csv
#'
#' @param outputfolder RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @return no output, just a file is stored
#' @export
#' @importFrom utils write.csv
export2csv = function(outputfolder, csvfile, desiredtz) {
  if (!file.exists(paste0(outputfolder,"/",csvfile))) {
    
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
    addToDF = function(x=c(),y=c()) {
      if (length(x) == 0) {
        dat = y
      } else {
        dat = merge(x, y, by="time", all = TRUE)
      }
      return(dat)
    }
    
    #------------------------------------------------------------------------
    # standardise time resolution (1 minute)
    # and merge all channels in one data.frame df
    df = c()
    if ("lightOnTimes" %in% ls()) {
      lightOnTimes = aggregatePerMinute(lightOnTimes, desiredtz) # from 5 to 60 seconds
      lightOn = data.frame(time = lightOnTimes,lighton=TRUE)
      df = addToDF(df,lightOn)
    }
    if ("ScreenOnTimes" %in% ls()) {
      ScreenOnTimes = aggregatePerMinute(ScreenOnTimes, desiredtz) # from 1 to 60 seconds
      ScreenOn = data.frame(time = ScreenOnTimes,screenon = TRUE)
      df = addToDF(df,ScreenOn)
    }
    if ("MovementPSGTimes" %in% ls()) {
      MovementPSGTimes = aggregatePerMinute(MovementPSGTimes, desiredtz) #from 1 to 60 seconds
      PSGmove = data.frame(time = MovementPSGTimes,PSGmove = TRUE)
      df = addToDF(df,PSGmove)
    }
    if ("AppActiveTimes" %in% ls()) {
      AppActiveTimes = aggregatePerMinute(AppActiveTimes, desiredtz) # from 5 to 60 seconds
      appactive = data.frame(time = AppActiveTimes,AppAct = TRUE)
      df = addToDF(df,appactive)
    }
    if ("batInteractTimes" %in% ls()) {
      batInteractTimes = aggregatePerMinute(batInteractTimes, desiredtz) # from 1 to 60 seconds
      batinteract = data.frame(time = batInteractTimes, batinteract = TRUE)
      df = addToDF(df,batinteract)
    }
    if ("PhoneAcc" %in% ls()) {
      PhoneAccTimes = PhoneAcc$Created.Date.POSIX[which((PhoneAcc$acceleration/9.81) > 0.03)]
      phoneacc = data.frame(time = PhoneAccTimes, phoneacc = TRUE)
      df = addToDF(df,phoneacc)
    }
    if ("withings_act" %in% ls()) { # PDK
      if ("infoentered" %in% colnames(withings_act)) {
        select = which(withings_act$infoentered == TRUE | withings_act$movement == TRUE)
      } else {
        select = which(withings_act$movement == TRUE)
      }
      WithingsMoveTimes = withings_act$timestamp[select] 
      WithingsMoveTimes = as.POSIXlt(WithingsMoveTimes,origin="1970-1-1",tz=desiredtz)
      WithingsMoveTimes = aggregatePerMinute(WithingsMoveTimes, desiredtz) # from 1 to 60 seconds
      withingsMove = data.frame(time = WithingsMoveTimes, withingsMove_pdk  = TRUE)
      df = addToDF(df,withingsMove)
    }
    if ("withings_actDD" %in% ls()) { # Direct download
      if ("infoentered" %in% colnames(withings_actDD)) {
        select = which(withings_actDD$infoentered == TRUE | withings_actDD$movement == TRUE)
      } else {
        select = which(withings_actDD$movement == TRUE)
      }
      WithingsMoveTimes = withings_actDD$timestamp[select] 
      WithingsMoveTimes = as.POSIXlt(as.character(WithingsMoveTimes),origin="1970-1-1",tz=desiredtz)
      WithingsMoveTimes = aggregatePerMinute(WithingsMoveTimes, desiredtz) # from 1 to 60 seconds
      withingsMove = data.frame(time = WithingsMoveTimes, withingsMove_dd  = TRUE)
      df = addToDF(df,withingsMove)
    }
    if ("withings_sleep" %in% ls()) { # PDK
      withings_sleep=withings_sleep[,-which(colnames(withings_sleep) == "statecode")]
      WSN = colnames(withings_sleep)
      colnames(withings_sleep)[which(WSN=="statename")] = "sleepstate"
      withings_sleep$sleepstate =as.character(withings_sleep$sleepstate)
      sleep_deep_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="deep-sleep")],deepsleep_pdk=TRUE)
      sleep_light_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="light-sleep")],lightsleep_pdk=TRUE)
      sleep_awake_pdk = data.frame(time=withings_sleep$timestamp[which(withings_sleep$sleepstate=="awake")],awake_pdk=TRUE)
      df = addToDF(df,sleep_deep_pdk)
      df = addToDF(df,sleep_light_pdk)
      df = addToDF(df,sleep_awake_pdk)
    }
    if ("withings_sleepDD" %in% ls()) { # Direct download
      WSN = colnames(withings_sleepDD)
      colnames(withings_sleepDD)[which(WSN=="statecode")] = "sleepstate"
      withings_sleepDD$sleepstate =as.character(withings_sleepDD$sleepstate)
      sleep_deep_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="deep-sleep")],deepsleep_dd=TRUE)
      sleep_light_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="light-sleep")],lightsleep_dd=TRUE)
      sleep_awake_dd = data.frame(time=withings_sleepDD$timestamp[which(withings_sleepDD$sleepstate=="awake")],awake_dd=TRUE)
      df = addToDF(df,sleep_deep_dd)
      df = addToDF(df,sleep_light_dd)
      df = addToDF(df,sleep_awake_dd)
    }
    if ("AppHalted" %in% ls()) {
      AppHaltedTimes = aggregatePerMinute(AppHalted, desiredtz) # from 1 to 60 seconds
      AppHaltedTimes = data.frame(time = AppHaltedTimes, AppHalted  = TRUE)
      df = addToDF(df,AppHaltedTimes)
    }
    if ("SunSetRise" %in% ls()) {
      SunSetRise$time = as.POSIXlt(SunSetRise$timestamp,origin="1970-1-1",tz=desiredtz)
      SunSetRise$time = aggregatePerMinute(SunSetRise$time, desiredtz) # from 1 to 60 seconds
      CS = colnames(SunSetRise)
      colnames(SunSetRise)[which(CS=="event")] = "SunSetRise"
      SunSetRise=SunSetRise[,-which(colnames(SunSetRise) == "timestamp")]
      df = addToDF(df,SunSetRise)
    }
    if ("SleepSurvey" %in% ls()) {
      # Try to unlogical some rise- before bed-times
      rst = as.POSIXlt(SleepSurvey$risetime)$hour
      bdt = as.POSIXlt(SleepSurvey$bedtime)$hour
      srt = as.POSIXlt(SleepSurvey$surveytime)$hour
      bedtime_min24 = which(bdt > 18 & rst < bdt) # bedtime -24
      bedtime_min12 = which(bdt <= 18 & rst < bdt) # bedtime -12
      risetime_min12 = which(rst > 18 & rst < bdt) # risetime -12
      risetime_plus24 = which(rst >= srt & rst < (srt + 4) & rst < bdt) # risetime +24
      if (length(risetime_min12) > 0) SleepSurvey$risetime[risetime_min12] = SleepSurvey$risetime[risetime_min12] - (12*3600)
      if (length(bedtime_min12) > 0) SleepSurvey$bedtime[bedtime_min12] = SleepSurvey$bedtime[bedtime_min12] - (12*3600)
      if (length(bedtime_min24) > 0) SleepSurvey$bedtime[bedtime_min24] = SleepSurvey$bedtime[bedtime_min24] - (24*3600)
      if (length(risetime_plus24) > 0) SleepSurvey$risetime[risetime_plus24] = SleepSurvey$risetime[risetime_plus24] + (24*3600)
    
      # Add time in bed (self-reported)
      SleepSurvey$bedtime_num = as.numeric(SleepSurvey$bedtime) # work with numeric time
      SleepSurvey$risetime_num = as.numeric(SleepSurvey$risetime) # work with numeric time
      inplausible = which(SleepSurvey$risetime_num < SleepSurvey$bedtime_num)
      if (length(inplausible) > 0) SleepSurvey = SleepSurvey[-inplausible,]
      InBedTimes = c()
      for (jj in 1:nrow(SleepSurvey)) {
        InBedTimes = c(InBedTimes, seq(SleepSurvey$bedtime_num[jj],SleepSurvey$risetime_num[jj],by=60))
      }
      InBedTimes = as.POSIXlt(InBedTimes, origin="1970-1-1", tz=desiredtz)
      inbed = data.frame(time=InBedTimes, InBed=TRUE)
      df = addToDF(df,inbed)
      # add survey
      survey = SleepSurvey[,c("surveytime","positiveFeelings","negativeFeelings","Sleep.Quality.Value","Sleep.Duration")]
      colnames(survey)[which(colnames(survey) == "surveytime")] = "time"
      df = addToDF(df,survey)
    }
    # add hour in the day to ease plotting
    df$hour = as.POSIXlt(df$time)$hour
    df$min = as.POSIXlt(df$time)$min
    df$min_inday = df$hour * 60 + df$min
    clock_char = strftime(df$time,format="%H:%M:%S",tz=desiredtz)
    write.csv(df,file=paste0(outputfolder,"/",csvfile),row.names = FALSE)
  }
}
