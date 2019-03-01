#' export2csv
#'
#' @param RDatafile RData datafile where the output of preprocess is stored.
#' @param csvfile file name where data overview needs to be stored
#' @param desiredtz timezone (character) in Europe/London format
#' @return no output, just a file is stored
#' @export
export2csv = function(RDatafile, csvfile, desiredtz) {
  load(file=RDatafile)
  #----------------------------------------------------------------
  # Round resolution of all channels to 1 minute to simplify merging
  aggregatePerMinute = function(x, desiredtz) {
    # aggregate input timestamps per minute
    x_num = round(as.numeric(x)/ 60)*60
    x_num = unique(x_num)
    x_60sec = as.POSIXlt(x_num,origin="1970-01-01",tz=desiredtz)
    return(x_60sec)
  }
  addToDF = function(x,y) {
    if (length(df) == 0) {
      df = y
    } else {
      df = merge(x,y,by="time", all = TRUE)
    }
    return(df)
  }
  
  #------------------------------------------------------------------------
  # standardise time resolution (1 minute)
  # and merge all channels in one data.frame df
  
  if ("lightOnTimes" %in% ls()) {
    lightOnTimes = aggregatePerMinute(lightOnTimes, desiredtz) # from 5 to 60 seconds
    lightOn = data.frame(time = lightOnTimes,lighton=TRUE)
    df = addToDF(df,lightOn)
  }
  if ("ScreenOnTimes" %in% ls()) {
    ScreenOnTimes = aggregatePerMinute(ScreenOnTimes, desiredtz) # from 1 to 60 seconds
    ScreenOn = data.frame(time = ScreenOnTimes,sreenon = TRUE)
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
  if ("WithingsActivity" %in% ls()) {
    WithingsMoveTimes = WithingsActivity$timestamp[which(WithingsActivity$infoentered == TRUE | WithingsActivity$movement == TRUE)] 
    WithingsMoveTimes = aggregatePerMinute(WithingsMoveTimes, desiredtz) # from 1 to 60 seconds
    withingsMove = data.frame(time = WithingsMoveTimes, withingsMove  = TRUE)
    df = addToDF(df,withingsMove)
  }
  if ("WithingsSleep" %in% ls()) {
    WithingsSleep=WithingsSleep[,-which(colnames(WithingsSleep) == "statecode")]
    WSN = colnames(WithingsSleep)
    colnames(WithingsSleep)[which(WSN=="statename")] = "withingsSleepState"
    WithingsSleep$withingsSleepState =as.character(WithingsSleep$withingsSleepState )
    sleep_deep = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="deep-sleep")],deepsleep=TRUE)
    sleep_light = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="light-sleep")],lightsleep=TRUE)
    sleep_awake = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="awake")],awake=TRUE)
    df = addToDF(df,sleep_deep)
    df = addToDF(df,sleep_light)
    df = addToDF(df,sleep_awake)
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
  # add hour in the day to ease plotting
  df$hour = as.POSIXlt(df$time)$hour
  df$min = as.POSIXlt(df$time)$min
  df$min_inday = df$hour * 60 + df$min
  clock_char = strftime(df$time,format="%H:%M:%S",tz=desiredtz)
  df$clock = as.POSIXct(df$time,format="%H:%M:%S",tz=desiredtz)
  write.csv(df,file=csvfile,row.names = FALSE)
}