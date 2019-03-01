#' preprocess
#'
#' @param studyfolder folder in which we expect all the pdk and Withings output folder.
#' @param desiredtz timezone (character) in Europe/London format
#' @param overwrite boolean if TRUE then it will reload all the data (default FALSE).
#' @return outputfile the name of the RData file where all extracted data is stored.
#' @export
preprocess = function(studyfolder,desiredtz,overwrite=FALSE) {
  setwd(personfolder)
  folders = list.dirs(".", recursive=FALSE)
  print(folders)
  withingsfolder = c()
  withingsi = grep(pattern = "Withings-",x = folders)
  if (length(withingsi) > 0) {
    withingsfolder = folders[withingsi]
  }
  outputfile = paste0(studyfolder,"/sleepsight_wearables_data.RData")
  
  if (file.exists(outputfile) == FALSE | overwrite == TRUE) {
    
    if ("./pdk-device-battery" %in% folders) {
      cat("\nextract battery information")
      batfolder = "pdk-device-battery/"
      fn_bat = dir(batfolder)
      filename = paste0(batfolder,fn_bat)
      batInteractTimes = getBatInteract(filename, desiredtz) # timestamps when phone was either put on charged or plugged out.
    }
    if (length(withingsfolder) > 0) {
      cat("\nWithings device - sleep and activity")
      for (wfi in 1:length(withingsfolder)) {
        filefolder = paste0(unlist(strsplit(withingsfolder[wfi],"./"))[2],"/")
        # note that function WithingsSleep and WithingsActivity search the filefolder recursevely for files that meet the description
        WithingsSleep = getWithingsSleep(filefolder, desiredtz)
        WithingsActivity = getWithingsActivity(filefolder, desiredtz)
      }
    }
    if ("./pdk-location" %in% folders) {
      cat("\npdk-location")
      locfolder = "pdk-location/"
      fn_loc = dir(locfolder)
      filename = paste0(locfolder,fn_loc)
      MovementPSGTimes = getMovementPSG(filename, desiredtz)
    }
    if ("./pdk-system-status" %in% folders) {
      cat("\nSystem status")  # Runtime information is relevant for checking data missingness
      statusfolder = "pdk-system-status/"
      fn_status = dir(statusfolder)
      filename = paste0(statusfolder,fn_status)
      AppHalted = getSystemHalted(filename, desiredtz)
    }
    if ("./pdk-sensor-accelerometer" %in% folders) {
      cat("\npdk-sensor-accelerometer")
      accfolder = "pdk-sensor-accelerometer/"
      PhoneAcc = getPhoneAcc(accfolder, desiredtz) # Note that this function provides all acceleration information
      # Comments on usefulness:
      # - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
      # - Data is not collected in the absense of movement, which complicates autocalibraton.
      # - We cannot use the default time extraction because that would only reflect when data
      #   blocks are created. Instread used "Normalized Timestamp"
      # - It seems that timestamps are not ordered correctly, so first order timestamps.
    }
    if ("./pdk-sensor-light" %in% folders) {
      cat("\npdk-sensor-light")
      # Light probably not useful, because light can change without the person change activity state
      filefolder = "pdk-sensor-light/"
      lightOnTimes = getLight(filefolder, desiredtz)
    }
    if ("./pdk-foreground-application" %in% folders) {
      cat("\npdk-foreground-application")
      appfolder = "pdk-foreground-application/"
      fn_app = dir(appfolder)
      filename = paste0(appfolder,fn_app)
      AppActiveTimes = getAppActive(filename, desiredtz)
    }
    if ("./pdk-screen-state" %in% folders) {
      cat("\npdk-screen-state")
      # Note: probably not relevant, because screen activity does not necessarily
      # say much about whether the person interacted with the phone, e.g. incoming call
      # or messages...?
      screstafolder = "pdk-screen-state/"
      fn_scresta = dir(screstafolder)
      filename = paste0(screstafolder,fn_scresta)
      ScreenOnTimes = getScreenState(filename, desiredtz)
    }
    if ("./pdk-time-of-day" %in% folders) {
      cat("\nSunset Sunrise")
      todfolder = "pdk-time-of-day/"
      fn_tod = dir(todfolder)
      filename = paste0(todfolder,fn_tod)
      SunSetRise = getSunSetRise(filename, desiredtz)
    }
    
    save(WithingsActivity,WithingsSleep,
         lightOnTimes,PhoneAcc,
         ScreenOnTimes, MovementPSGTimes, AppActiveTimes,
         batInteractTimes, AppHalted, SunSetRise,
         file=outputfile)
  }
  return(outputfile)
}