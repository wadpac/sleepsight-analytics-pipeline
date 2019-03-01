#' preprocess
#'
#' @param studyfolder folder in which we expect all the pdk and Withings output folder.
#' @param desiredtz timezone (character) in Europe/London format
#' @param overwrite boolean if TRUE then it will reload all the data (default FALSE).
#' @return outputfolder the name of the RData file where all extracted data is stored.
#' @export
preprocess = function(studyfolder,desiredtz,overwrite=FALSE) {
  setwd(personfolder)
  folders = list.dirs(".", recursive=FALSE)
  withingsfolder = c()
  withingsi = grep(pattern = "Withings-",x = folders)
  if (length(withingsi) > 0) {
    withingsfolder = folders[withingsi]
  }
  outputfolder = paste0(studyfolder,"/processinginR")

  output = list()
  if (!dir.exists(outputfolder) | overwrite == TRUE) {
    if (!dir.exists(outputfolder)) dir.create(outputfolder)
    if ("./pdk-device-battery" %in% folders) {
      cat("\nloading pdk-device-battery")
      batfolder = "pdk-device-battery/"
      fn_bat = dir(batfolder)
      filename = paste0(batfolder,fn_bat)
      batInteractTimes = getBatInteract(filename, desiredtz) # timestamps when phone was either put on charged or plugged out.
      save(batInteractTimes,file=paste0(outputfolder,"/batInteractTimes.RData"))
    }
    if (length(withingsfolder) > 0) {
      cat("\nloading Withings-wearable-sleep-activity")
      WithingsSleep = WithingsActivity = c()
      for (wfi in 1:length(withingsfolder)) {
        filefolder = paste0(unlist(strsplit(withingsfolder[wfi],"./"))[2],"/")
        # note that function WithingsSleep and WithingsActivity search the filefolder recursevely for files that meet the description
        WithingsSleep = getWithingsSleep(filefolder, desiredtz)
        WithingsActivity = getWithingsActivity(filefolder, desiredtz)
      }
      if (length(WithingsSleep) > 0) save(WithingsSleep,file=paste0(outputfolder,"/WithingsSleep.RData"))
      if (length(WithingsActivity) > 0) save(WithingsActivity,file=paste0(outputfolder,"/WithingsActivity.RData"))
    }
    if ("./pdk-location" %in% folders) {
      cat("\nloading pdk-location")
      locfolder = "pdk-location/"
      fn_loc = dir(locfolder)
      filename = paste0(locfolder,fn_loc)
      MovementPSGTimes = getMovementPSG(filename, desiredtz)
      save(MovementPSGTimes,file=paste0(outputfolder,"/MovementPSGTimes.RData"))
    }
    if ("./pdk-system-status" %in% folders) {
      cat("\nloading pdk-system-status")  # Runtime information is relevant for checking data missingness
      statusfolder = "pdk-system-status/"
      fn_status = dir(statusfolder)
      filename = paste0(statusfolder,fn_status)
      AppHalted = getSystemHalted(filename, desiredtz)
      save(AppHalted,file=paste0(outputfolder,"/AppHalted.RData"))
    }
    if ("./pdk-sensor-accelerometer" %in% folders) {
      cat("\nloading pdk-sensor-accelerometer")
      accfolder = "pdk-sensor-accelerometer/"
      PhoneAcc = getPhoneAcc(accfolder, desiredtz) # Note that this function provides all acceleration information
      # Comments on usefulness:
      # - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
      # - Data is not collected in the absense of movement, which complicates autocalibraton.
      # - We cannot use the default time extraction because that would only reflect when data
      #   blocks are created. Instread used "Normalized Timestamp"
      # - It seems that timestamps are not ordered correctly, so first order timestamps.
      save(PhoneAcc,file=paste0(outputfolder,"/PhoneAcc.RData"))
    }
    if ("./pdk-sensor-light" %in% folders) {
      cat("\nloading pdk-sensor-light")
      # Light probably not useful, because light can change without the person change activity state
      filefolder = "pdk-sensor-light/"
      lightOnTimes = getLight(filefolder, desiredtz)
      save(lightOnTimes,file=paste0(outputfolder,"/lightOnTimes.RData"))
    }
    if ("./pdk-foreground-application" %in% folders) {
      cat("\nloading pdk-foreground-application")
      appfolder = "pdk-foreground-application/"
      fn_app = dir(appfolder)
      filename = paste0(appfolder,fn_app)
      AppActiveTimes = getAppActive(filename, desiredtz)
      save(AppActiveTimes,file=paste0(outputfolder,"/AppActiveTimes.RData"))
    }
    if ("./pdk-screen-state" %in% folders) {
      cat("\nloading pdk-screen-state")
      # Note: probably not relevant, because screen activity does not necessarily
      # say much about whether the person interacted with the phone, e.g. incoming call
      # or messages...?
      screstafolder = "pdk-screen-state/"
      fn_scresta = dir(screstafolder)
      filename = paste0(screstafolder,fn_scresta)
      ScreenOnTimes = getScreenState(filename, desiredtz)
      save(ScreenOnTimes,file=paste0(outputfolder,"/ScreenOnTimes.RData"))
    }
    if ("./pdk-time-of-day" %in% folders) {
      cat("\nloading pdk-time-of-day")
      todfolder = "pdk-time-of-day/"
      fn_tod = dir(todfolder)
      filename = paste0(todfolder,fn_tod)
      SunSetRise = getSunSetRise(filename, desiredtz)
      save(SunSetRise,file=paste0(outputfolder,"/SunSetRise.RData"))
    }
  }
  return(outputfolder)
}