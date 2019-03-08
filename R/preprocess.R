#' preprocess
#'
#' @param personfolder folder in which we expect all the pdk and Withings output folder.
#' @param desiredtz timezone (character) in Europe/London format
#' @param overwrite boolean if TRUE then it will reload all the data (default FALSE).
#' @return outputfolder the name of the RData file where all extracted data is stored.
#' @export
preprocess = function(personfolder,desiredtz,overwrite=FALSE) {
  setwd(personfolder)
  channelfolders = list.dirs(".", recursive=FALSE)
  
  outputfolder = paste0(personfolder,"/processinginR")
  if (!dir.exists(outputfolder)) dir.create(outputfolder)
  #============================
  # Withings
  withingsfolder = c()
  withingsi = grep(pattern = "Withings-",x = channelfolders)
  if (length(withingsi) > 0) withingsfolder = channelfolders[withingsi]
  if (length(withingsfolder) > 0) {
    cat("\nloading Withings-Direct-Download")
    directdownload = TRUE
    withings_sleepDD = withings_actDD = c()
    fullpathout = paste0(outputfolder,"/Withings-DirectDownload.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      for (wfi in 1:length(withingsfolder)) {
        filefolder = paste0(unlist(strsplit(withingsfolder[wfi],"./"))[2],"/")
        # note that function WithingsSleep and WithingsActivity search the filefolder recursevely for files that meet the description
        withings_actDD = getWithingsActivity(filefolder, desiredtz, directdownload)
        withings_sleepDD = getWithingsSleep(filefolder, desiredtz, directdownload)
      }
      if (length(withings_actDD) > 0 | length(withings_sleepDD) > 0) {
        save(withings_actDD, withings_sleepDD,file=fullpathout)
      }
    }
    cat("\nloading Withings-PDK")
    directdownload = FALSE
    withings_sleep = withings_act = c()
    fullpathout = paste0(outputfolder,"/Withings-PDK.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      for (wfi in 1:length(withingsfolder)) {
        filefolder = paste0(unlist(strsplit(withingsfolder[wfi],"./"))[2],"/")
        # note that function WithingsSleep and WithingsActivity search the filefolder recursevely for files that meet the description
        withings_act = getWithingsActivity(filefolder, desiredtz, directdownload)
        withings_sleep = getWithingsSleep(filefolder, desiredtz, directdownload)
      }
      if (length(withings_act) > 0 | length(withings_sleep) > 0) {
        save(withings_act, withings_sleep,file=fullpathout)
      }
      
    }
  }
  #==========================
  # Phone data
  if ("./pdk-device-battery" %in% channelfolders) {
    cat("\nloading pdk-device-battery")
    batfolder = "pdk-device-battery/"
    fn_bat = dir(batfolder)
    filename = paste0(batfolder,fn_bat)
    fullpathout = paste0(outputfolder,"/batInteractTimes.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      batInteractTimes = getBatInteract(filename, desiredtz) # timestamps when phone was either put on charged or plugged out.
      save(batInteractTimes,file=fullpathout)
    }
  }
  if ("./pdk-location" %in% channelfolders) {
    cat("\nloading pdk-location")
    locfolder = "pdk-location/"
    fn_loc = dir(locfolder)
    filename = paste0(locfolder,fn_loc)
    fullpathout = paste0(outputfolder,"/MovementPSGTimes.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      MovementPSGTimes = getMovementPSG(filename, desiredtz)
      save(MovementPSGTimes,file=fullpathout)
    }
  }
  if ("./pdk-system-status" %in% channelfolders) {
    cat("\nloading pdk-system-status")  # Runtime information is relevant for checking data missingness
    statusfolder = "pdk-system-status/"
    fn_status = dir(statusfolder)
    filename = paste0(statusfolder,fn_status)
    fullpathout = paste0(outputfolder,"/AppHalted.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      AppHalted = getSystemHalted(filename, desiredtz)
      save(AppHalted,file=fullpathout)
    }
  }
  if ("./pdk-sensor-accelerometer" %in% channelfolders) {
    cat("\nloading pdk-sensor-accelerometer")
    accfolder = "pdk-sensor-accelerometer/"
    fullpathout = paste0(outputfolder,"/PhoneAcc.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      PhoneAcc=c()
      PhoneAcc = getPhoneAcc(accfolder, desiredtz) # Note that this function provides all acceleration information
      # Comments on usefulness:
      # - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
      # - Data is not collected in the absense of movement, which complicates autocalibraton.
      # - We cannot use the default time extraction because that would only reflect when data
      #   blocks are created. Instread used "Normalized Timestamp"
      # - It seems that timestamps are not ordered correctly, so first order timestamps.
      save(PhoneAcc,file=fullpathout)
    }
  }
  if ("./pdk-sensor-light" %in% channelfolders) {
    cat("\nloading pdk-sensor-light")
    # Light probably not useful, because light can change without the person change activity state
    filefolder = "pdk-sensor-light/"
    fullpathout = paste0(outputfolder,"/lightOnTimes.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      lightOnTimes = getLight(filefolder, desiredtz)
      save(lightOnTimes,file=fullpathout)
    }
  }
  if ("./pdk-foreground-application" %in% channelfolders) {
    cat("\nloading pdk-foreground-application")
    appfolder = "pdk-foreground-application/"
    fn_app = dir(appfolder)
    filename = paste0(appfolder,fn_app)
    fullpathout = paste0(outputfolder,"/AppActiveTimes.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      AppActiveTimes = getAppActive(filename, desiredtz)
      save(AppActiveTimes,file=fullpathout)
    }
  }
  if ("./pdk-screen-state" %in% channelfolders) {
    cat("\nloading pdk-screen-state")
    # Note: probably not relevant, because screen activity does not necessarily
    # say much about whether the person interacted with the phone, e.g. incoming call
    # or messages...?
    screstafolder = "pdk-screen-state/"
    fn_scresta = dir(screstafolder)
    filename = paste0(screstafolder,fn_scresta)
    fullpathout = paste0(outputfolder,"/ScreenOnTimes.RData")
    if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
      ScreenOnTimes = getScreenState(filename, desiredtz)
      save(ScreenOnTimes,file=fullpathout)
    }
  }
  if ("./pdk-time-of-day" %in% channelfolders) {
    cat("\nloading pdk-time-of-day")
    todfolder = "pdk-time-of-day/"
    fn_tod = dir(todfolder)
    filename = paste0(todfolder,fn_tod)
    SunSetRise = getSunSetRise(filename, desiredtz)
    save(SunSetRise,file=paste0(outputfolder,"/SunSetRise.RData"))
  }
  # }
  return(outputfolder)
}