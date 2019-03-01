#' getWithingsSleep
#'
#' @param filefolder path to folder Withings-.... with Withings data files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return data.frame with timestamps (POSIX) for detected sleep stages by Withings.
#' @export
getWithingsSleep = function(filefolder, desiredtz) {
  fn_wit = dir(filefolder,recursive = T,full.names = T)
  txtfiles = grep(".tx",x = fn_wit)
  fn_wit = fn_wit[txtfiles] # assumption now that there are 4 txt files
  
  #-------------------------------------------
  # sleep measures
  devsle = data.table::fread(file=fn_wit[grep("device-sleep",x = fn_wit)],sep="\t")
  devsle = as.data.frame(devsle)
  devsle = replaceVarWithSpace(devsle)
  devsle = addPOSIX(devsle, desiredtz)
  WithingsSleep = devsle[,c("Source","Created.Date.POSIX","start_date","end_date","state")]
  # Now downsample to minute
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  WithingsSleep$start_date = round(WithingsSleep$start_date / 60) *60
  WithingsSleep$end_date = round(WithingsSleep$end_date / 60) *60
  WS60sec = aggregate(x = WithingsSleep[,c("state")],FUN = Mode, by = list(start_date = WithingsSleep$start_date,
                                                                           end_date = WithingsSleep$end_date))
  # Now generate timeseries rather than only begin and end times.
  # Initialize vectors:
  WSTimes = seq(WS60sec$start_date[1],WS60sec$end_date[nrow(WS60sec)],by=60)
  WSState = rep(0,length(WSTimes))
  WS60sec$x = as.character(WS60sec$x)
  statelabels = unique(WS60sec$x) # extract unique labels in this file.
  cnt = 1
  for (jj in 1:nrow(WS60sec)) {
    tmp_time = seq(WS60sec$start_date[jj],WS60sec$end_date[jj],by=60)
    LT = length(tmp_time)
    statecode = which(statelabels == WS60sec$x[jj]) # using numeric indicator of state to speed up code.
    tmp_state = rep(statecode,LT) 
    WSTimes[cnt:(cnt+LT-1)] = tmp_time
    WSState[cnt:(cnt+LT-1)] = tmp_state 
    cnt = cnt+LT
  }
  WithingsSleepTimes = data.frame(timestamps=WSTimes,statecode=WSState)
  # remove double timestamps
  WithingsSleepTimes = aggregate(x = WithingsSleepTimes[,c("statecode")],
                                 FUN = Mode, by = list(timestamp = WithingsSleepTimes$timestamp))
  CN = colnames(WithingsSleepTimes)
  colnames(WithingsSleepTimes)[which(CN == "x")] = "statecode"
  WithingsSleepTimes$statename = NA # when no label is available
  for (ki in 1:length(statelabels)) {
    print(statelabels[ki])
    WithingsSleepTimes$statename[which(WithingsSleepTimes$statecode == ki)] = statelabels[ki]
  }
  WithingsSleepTimes[,-which(colnames(WithingsSleepTimes) == "statecode")]
  WithingsSleepTimes$timestamp = as.POSIXlt(WithingsSleepTimes$timestamp,origin="1970-1-1",tz=desiredtz)
  return(WithingsSleepTimes)
}