#' getSystemHalted
#'
#' @param filename name of file where pdk-system-status is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return TimeStamps when phone was halted.
#' @export
getSystemHalted = function(filename, desiredtz) {
  status = data.table::fread(file=filename,sep="\t")
  status= as.data.frame(status)
  status = replaceVarWithSpace(status)
  status = addPOSIX(status, desiredtz)
  # Look for drops in RunTime as indicator of when Sleepsight app was restarted
  deltaRunTime = diff(status$App.Runtime) 
  startTimeDrop = which(deltaRunTime < 0)
  endTimeDrop = startTimeDrop + 1
  timesStart = status$Created.Date.POSIX[startTimeDrop]
  timesEnd = status$Created.Date.POSIX[endTimeDrop]
  dropDuration = as.numeric(difftime(timesEnd,timesStart,units = "secs"),units="secs")
  AppHalted = data.frame(starttime=timesStart,endtime=timesEnd,duration=dropDuration)
  # round times to 60 seconds
  AppHalted$starttime_num = as.numeric(AppHalted$starttime) # work with numeric time
  AppHalted$starttime_num_60sec = round(AppHalted$starttime_num / 60) * 60 # round time to nearest 60 seconds
  AppHalted$endtime_num = as.numeric(AppHalted$endtime) # work with numeric time
  AppHalted$endtime_num_60sec = round(AppHalted$endtime_num / 60) * 60 # round time to nearest 60 seconds
  # aggregate per 60 seconds:
  AppHalted.per60sec = aggregate(x = AppHalted[,c("duration")],
                                 FUN = sum, by = list(start = AppHalted$starttime_num_60sec, ends = AppHalted$endtime_num_60sec),
                                 na.rm=TRUE, na.action=NULL)
  # create timeseries:
  AppHaltedTimes = c()
  for (jj in 1:nrow(AppHalted)) {
    AppHaltedTimes = c(AppHaltedTimes,seq(AppHalted$starttime[jj],AppHalted$endtime[jj],by=60))
  }
  AppHaltedTimes = as.POSIXlt(AppHaltedTimes, origin="1970-1-1", tz=desiredtz)
  
  return(AppHaltedTimes)
}