#' getWithingsActivity
#'
#' @param filefolder path to folder Withings-.... with Withings data files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @param directdownload boolean whether to use the direct download Withings data (TRUE) or the pdk aggregates (FALSE)
#' @return data.frame with timestamps (POSIX) on which body info was entered and/or there was movement.
#' @export
getWithingsActivity = function(filefolder, desiredtz, directdownload = TRUE) {
  fn_wit = dir(filefolder,recursive = T,full.names = T) # filenames in Withings folder
  csvfiles = grep(".cs",x = fn_wit)
  txtfiles = grep(".tx",x = fn_wit)
  fn_wit_csv = fn_wit[csvfiles]
  fn_wit_txt = fn_wit[txtfiles]
  withings_enterbodyinfo = withings_personactive = c()
  #-------------------------------------
  # Accumulated steps over large periods, probably not relevant:
  # actmea = data.table::fread(file=fn_wit[grep("activity-measures",x = fn_wit)],sep="\t")
  #----------------------------------------
  # manual entry of body info:
  #----------------------------------------
  if (directdownload == TRUE) {
    if (length(fn_wit_csv) > 0) { # Withings direct download
      devbodtimestamps = c()
      for (i in 1:length(fn_wit_csv)) {
        tmp =  unlist(strsplit(fn_wit_csv[i],"/"))
        filenameshort = tmp[length(tmp)]
        if (filenameshort %in% c("bp.csv", "height.csv", "oxy.csv", "pwv.csv", "weight.csv")) {
          devbod = data.table::fread(file=fn_wit_csv[i],sep=",",header=TRUE)
          if (nrow(devbod) > 0) {
            ts = as.character(as.POSIXlt(devbod$Date, tz = desiredtz, format="%Y-%m-%d %H:%M:%S"))
            ts = as.character(ts)
            devbodtimestamps = c(devbodtimestamps,ts)
          }
        }
      }
      withings_enterbodyinfo = data.frame(timestamp = devbodtimestamps,infoentered=rep(TRUE,length(devbodtimestamps)))
      withings_enterbodyinfo = withings_enterbodyinfo[order(withings_enterbodyinfo$timestamp),]
    }
  } else {
    if (length(fn_wit_txt) > 0) {  # Withings pdk download
      devbod = data.table::fread(file=fn_wit_txt[grep("device-body",x = fn_wit_txt)],sep="\t")
      devbod = as.data.frame(devbod)
      devbod = replaceVarWithSpace(devbod)
      devbod = addPOSIX(devbod, desiredtz)
      devbod = devbod[,c("Source","Created.Date.POSIX")]
      # timestamps when person entered information about weight, height, heart-pulse
      # devbod$Created.Date.POSIX
      withings_enterbodyinfo = data.frame(timestamp = devbod$Created.Date.POSIX,infoentered=rep(TRUE,nrow(devbod)))
    }
  }
  #-------------------------------------
  # time series of steps (max 1 min res):
  #----------------------------------------
  if (directdownload == TRUE) {
    if (length(fn_wit_csv) > 0) { # Withings direct download
      devint = data.table::fread(file=fn_wit_csv[grep("tracker_step",x = fn_wit_csv)],sep=",")
      devint = as.data.frame(devint)
      # add timestamps in POSIX format to make them R friendly
      removeLastSemicol = function(x) {
        tmp = unlist(strsplit(x,":"))
        y = paste0(tmp[1],":",tmp[2],":",tmp[3],tmp[4])
        return(y)
      }
      ts_ISO8601 = sapply(devint$start,removeLastSemicol)
      devint$Created.Date.POSIX = iso8601chartime2POSIX(as.character(ts_ISO8601), tz = desiredtz)
      # reformat matrix to be one row per minute
      devint0 = devint
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
      devint$N = unlist(lapply(devint$duration,Nminutes))
      StepsPerMinute = unlist(lapply(devint$value,SplitValues,cumsum=FALSE))
      Duration = unlist(lapply(devint$duration,SplitValues,cumsum=FALSE))
      CumDuration = unlist(lapply(devint$duration,SplitValues,cumsum=TRUE))
      # devint2 = mefa::rep.data.frame(devint, times = devint$N) # create new data.frame with extra rows
      devint2 = as.data.frame(lapply(devint, rep, times = devint$N)) # create new data.frame with extra rows
      devint2$steps = StepsPerMinute
      devint2$duration = Duration
      devint2$cumdur = CumDuration 
      devint2$Created.Date.POSIX = devint2$Created.Date.POSIX + devint2$cumdur
      rm(devint)
      devint = devint2[,-which(colnames(devint2) %in% c("start","duration","cumdur","N","value") ==  TRUE)]
      devint$Date = as.Date(devint$Created.Date.POSIX)
      withings_personactive = data.frame(timestamp = as.character(devint$Created.Date.POSIX),
                                         movement=rep(TRUE,nrow(devint)),
                                         date= devint$Date, # to facilitate calculating steps per day (see commented code above)
                                         steps=devint$steps)
    }
  } else {
    if (length(fn_wit_txt) > 0) {  # Withings pdk download
      devint = data.table::fread(file=fn_wit_txt[grep("device-intra",x = fn_wit_txt)],sep="\t")
      devint = as.data.frame(devint)
      devint = replaceVarWithSpace(devint)
      devint = addPOSIX(devint, desiredtz)
      devint = devint[,c("Source","Created.Date.POSIX","steps","swim_strokes","pool_laps","elevation_climbed","distance","calories")]
      devint =devint[is.na(devint$steps)==FALSE,]
      devint$Date = as.Date(devint$Created.Date.POSIX)
      # timestamps when person was active
      # devint$Created.Date.POSIX
      withings_personactive = data.frame(timestamp = as.character(devint$Created.Date.POSIX),
                                         movement=rep(TRUE,nrow(devint)),
                                         date= devint$Date, # to facilitate calculating steps per day (see commented code above)
                                         steps=devint$steps)
      
    }
  }
  NRWP = nrow(withings_personactive)
  NRWE = nrow(withings_enterbodyinfo)  
  if (NRWP > 0 & NRWE > 0) {
    withingsActivity = base::merge(withings_enterbodyinfo,withings_personactive,by ="timestamp",all=TRUE)
  } else if (NRWP > 0 & NRWE == 0){
    withingsActivity = withings_personactive
  } else if (NRWP == 0 & NRWE > 0){
    withingsActivity = withings_enterbodyinfo
  } else if (NRWP == 0 & NRWE == 0){
    withingsActivity = c()
  }
  return(withingsActivity)
}

