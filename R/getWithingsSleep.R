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
      Nminutes = function(x) {
        x = unlist(strsplit(as.character(x), "\\[|\\]|\\,| "))
        x = x[which(x != "" | x != " ")]
        Nminutes = length(x) - 1
      }
      SplitValues = function(x,cumsum=FALSE) {
        x = unlist(strsplit(as.character(x), "\\[|\\]|\\,| "))
        x = x[which(x != "" | x != " ")]
        x = x[2:length(x)]
        if (cumsum == TRUE) {
          x[1] = 0
          x = cumsum(x)
        }
        return(x)
      }
      S$N = unlist(lapply(S$duration,Nminutes))
      SleepStagePerMinute = unlist(lapply(S$value,SplitValues,cumsum=FALSE))
      Duration = unlist(lapply(S$duration,SplitValues,cumsum=FALSE))
      CumDuration = unlist(lapply(S$duration,SplitValues,cumsum=TRUE))
      # S2 = mefa::rep.data.frame(S, times = S$N) # create new data.frame with extra rows
      S2 = as.data.frame(lapply(S, rep, times = S$N)) # create new data.frame with extra rows
      S2$SleepStage = SleepStagePerMinute
      S2$duration = Duration
      S2$cumdur = CumDuration 
      S2$Created.Date.POSIX = S2$Created.Date.POSIX + S2$cumdur

      rm(S)
      S = S2[,-which(colnames(S2) %in% c("start","duration","cumdur","N","value") ==  TRUE)]
      S$Date = as.Date(S$Created.Date.POSIX)
      S$sleepname = NA
      w0 = which(S$SleepStage == 0)
      w1 = which(S$SleepStage == 1)
      w2 = which(S$SleepStage == 2)
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
      
      # Explore whether timestamp issue in data collected after August 2018 can easily be correct (conclusion: NO)
      # # if start and end time are identical then pdk may have made a mistake:
      # start_is_end = which(WithingsSleep$end_date == WithingsSleep$start_date)
      # if (length(start_is_end) > 0) { # use start next as end for current
      #   sensible = which(WithingsSleep$end_date[start_is_end] < WithingsSleep$start_date[start_is_end+1])
      #   WithingsSleep$end_date[start_is_end[sensible]] = WithingsSleep$start_date[start_is_end[sensible]+1]
      # }
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
        # x11()
        # timeinplot = as.POSIXlt(WS60sec$start_date, origin="1970-1-1",tz=desiredtz)
        # plot(WS60sec$end_date - WS60sec$start_date,type="l", main="P2", axes = FALSE)
        # time2 = timeinplot[seq(1,length(timeinplot),by=1000)]
        # LABE = as.character(as.Date(timeinplot[which(timeinplot %in% time2 == TRUE)]))
        # axis(side = 1, at = which(timeinplot %in% time2 == TRUE), labels = LABE, las = 3)

        
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
