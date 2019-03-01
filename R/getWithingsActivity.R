#' getWithingsActivity
#'
#' @param filefolder path to folder Withings-.... with Withings data files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return data.frame with timestamps (POSIX) on which body info was entered and/or there was movement.
#' @export
getWithingsActivity = function(filefolder, desiredtz) {
  fn_wit = dir(filefolder,recursive = T,full.names = T)
  txtfiles = grep(".tx",x = fn_wit)
  fn_wit = fn_wit[txtfiles] # assumption now that there are 4 txt files
  
  #-------------------------------------
  # Accumulated steps over large periods, probably not relevant:
  # actmea = data.table::fread(file=fn_wit[grep("activity-measures",x = fn_wit)],sep="\t")
  #-------------------------------------
  # manual entry of body info:
  devbod = data.table::fread(file=fn_wit[grep("device-body",x = fn_wit)],sep="\t")
  devbod = as.data.frame(devbod)
  devbod = replaceVarWithSpace(devbod)
  devbod = addPOSIX(devbod, desiredtz)
  devbod = devbod[,c("Source","Created.Date.POSIX")]
  # x11() # Create plot to QC event detection
  # plot(devbod$Created.Date.POSIX,rep(1,nrow(devbod)),type="p",pch=20,col="red",ylab="info entered")
  # timestamps when person entered information about weight, height, heart-pulse
  # devbod$Created.Date.POSIX
  withings_enterbodyinfo = data.frame(timestamp = devbod$Created.Date.POSIX,infoentered=rep(TRUE,nrow(devbod)))
  
  #-------------------------------------
  # time series of steps (max 1 min res):
  devint = data.table::fread(file=fn_wit[grep("device-intra",x = fn_wit)],sep="\t")
  devint = as.data.frame(devint)
  devint = replaceVarWithSpace(devint)
  devint = addPOSIX(devint, desiredtz)
  devint = devint[,c("Source","Created.Date.POSIX","steps","swim_strokes","pool_laps","elevation_climbed","distance","calories")]
  devint =devint[is.na(devint$steps)==FALSE,]
  devint$Date = as.Date(devint$Created.Date.POSIX)
  # devint.perday = aggregate(x = devint[c("steps")],
  #                           FUN = sum, by = list(Group.date = devint$Date), na.rm=TRUE, na.action=NULL)
  # x11() # Create plot to QC event detection
  # plot(devint$Created.Date.POSIX,devint$steps,type="l",ylab="steps")
  # x11() # Create plot to QC event detection
  # plot(devint.perday$Group.date,devint.perday$steps,type="l",ylab="steps per day")
  
  # timestamps when person was active
  # devint$Created.Date.POSIX
  withings_personactive = data.frame(timestamp = devint$Created.Date.POSIX,
                                     movement=rep(TRUE,nrow(devint)),
                                     date= devint$Date, # to facilitate calculating steps per day (see commented code above)
                                     steps=devint$steps)
  
  
  
  withingsActivity = base::merge(withings_enterbodyinfo,withings_personactive,by ="timestamp",all=TRUE)
  return(withingsActivity)
}