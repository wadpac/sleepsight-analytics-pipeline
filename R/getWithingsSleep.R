#' getWithingsSleep
#'
#' @param filefolder path to folder Withings-.... with Withings data files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @param directdownload boolean whether to use the direct download Withings data (TRUE) or the pdk aggregates (FALSE)
#' @return data.frame with timestamps (POSIX) for detected sleep stages by Withings.
#' @export
getWithingsSleep = function(filefolder, desiredtz, directdownload = TRUE) {
  fn_wit = dir(filefolder,recursive = T,full.names = T) # filenames in Withings folder
  csvfiles = grep(".cs",x = fn_wit)
  txtfiles = grep(".tx",x = fn_wit)
  fn_wit_csv = fn_wit[csvfiles]
  fn_wit_txt = fn_wit[txtfiles]
  WithingsSleepTimes = c()
  #-------------------------------------------
  # sleep measures
  if (directdownload == TRUE) {
    if (length(fn_wit_csv) > 0) { # Withings direct download
      S = data.table::fread(file=fn_wit_csv[grep("tracker_sleep",x = fn_wit_csv)],sep=",")
      S = as.data.frame(S)
      # add timestamps in POSIX format to make them R friendly
      removeLastSemicol = function(x) {
        tmp = unlist(strsplit(x,":"))
        y = paste0(tmp[1],":",tmp[2],":",tmp[3],tmp[4])
        return(y)
      }
      ts_ISO8601 = sapply(S$start,removeLastSemicol)
      S$Created.Date.POSIX = iso8601chartime2POSIX(as.character(ts_ISO8601), tz = desiredtz)
      # reformat matrix to be one row per minute
      S0 = S
      getsleepepisodes = function(x) {
        dur = unlist(strsplit(as.character(x[which(names(x) == "duration")]), "\\[|\\]|\\,| "))
        dur = dur[which(dur != "" | dur != " ")]
        dur = as.numeric(dur[2:length(dur)])
        val = unlist(strsplit(as.character(x[which(names(x) == "value")]), "\\[|\\]|\\,| "))
        val = val[which(val != "" | val != " ")]
        val = as.numeric(val[2:length(val)])
        Created.Date.POSIX = as.character(x[which(names(x) == "Created.Date.POSIX")])
        out = data.frame(duration=dur,value = val,Created.Date = rep(Created.Date.POSIX,length(val)),
                         cumdur = cumsum(dur))
        return(out)
      }
      WW = data.table::rbindlist(apply(S, 1, FUN=getsleepepisodes))
      WW = as.data.frame(WW)
      WW$Created.Date.POSIX = as.POSIXlt(WW$Created.Date, origin = "1970-1-1", tz = desiredtz)
      WW$starttime = WW$Created.Date.POSIX + WW$cumdur - WW$duration[1]
      WW = WW[,-c(which(colnames(WW) %in% c("Created.Date","Created.Date.POSIX") == TRUE))]
      WW2 = sapply(WW, rep, times = WW$duration/60)
      WW3 = as.data.frame(WW2)
      ss = WW3$starttime
      deltazero = 1
      while(length(deltazero) > 0) {
        dt = diff(ss)
        cv = cumsum(ss)
        deltazero = which(dt == 0)
        ss[deltazero + 1] = ss[deltazero + 1] + 60
      }
      WW3$Created.Date.POSIX = as.POSIXlt(ss,origin="1970-1-1",tz="desiredtz")
      # now the data has one row per minute of sleep
      rm(S)
      S = WW3[,-which(colnames(WW3) %in% c("starttime","duration","cumdur") ==  TRUE)]
      # S$Date = as.Date(S$Created.Date.POSIX)
      S$sleepname = NA
      w0 = which(S$valu == 0)
      w1 = which(S$value == 1)
      w2 = which(S$value == 2)
      if (length(w0) > 0) S$sleepname[w0] = "awake"
      if (length(w1) > 0) S$sleepname[w1] = "light-sleep"
      if (length(w2) > 0) S$sleepname[w2] = "deep-sleep"
      
      WithingsSleepTimes = data.frame(timestamps=as.character(S$Created.Date.POSIX),statecode=S$sleepname)
    }
  } else {
    if (length(fn_wit_txt) > 0) {  # Withings pdk download
      devsle = data.table::fread(file=fn_wit_txt[grep("device-sleep",x = fn_wit_txt)],sep="\t")
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
      if (nrow(WithingsSleep) != 0) {
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
          WithingsSleepTimes$statename[which(WithingsSleepTimes$statecode == ki)] = statelabels[ki]
        }
        WithingsSleepTimes[,-which(colnames(WithingsSleepTimes) == "statecode")]
        WithingsSleepTimes$timestamp = as.POSIXlt(WithingsSleepTimes$timestamp,origin="1970-1-1",tz=desiredtz)
      }
    }
  }
  return(WithingsSleepTimes)
}
