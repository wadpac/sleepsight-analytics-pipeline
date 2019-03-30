#' preprocess
#'
#' @param personfolder folder in which we expect all the pdk and Withings output folder.
#' @param desiredtz timezone (character) in Europe/London format
#' @param overwrite boolean if TRUE then it will reload all the data (default FALSE).
#' @return outputfolder the name of the RData file where all extracted data is stored.
#' @export
preprocess = function(personfolder,desiredtz,overwrite=FALSE) {
  channelfolders = list.dirs(personfolder, recursive=FALSE)
  txt_files_in_folder = dir(personfolder, recursive=FALSE,pattern = ".tx",full.names = TRUE)
  outputfolder = ID = c()
  # get source id:
  if (length(txt_files_in_folder) > 0) { # Sleep Survey
    sleepsurveyFile = grep("sleep-survey",x = txt_files_in_folder)
    appEventFile = grep("pdk-app-event",x = txt_files_in_folder)
    tmp3 = unique(c(appEventFile,sleepsurveyFile))
    
    if (length(tmp3) > 0) {
      filename= txt_files_in_folder[tmp3[1]]
      temp = data.table::fread(file=filename,sep="\t")
      if (nrow(temp) == 0 & length(tmp3) > 0) {
        filename= txt_files_in_folder[tmp3[2]]
        temp = data.table::fread(file=filename,sep="\t")
      }
      tmpID = unlist(strsplit(temp$Source[1],"@"))[1]
      rem_study = unlist(strsplit(tmpID,"study"))
      if (length(rem_study) == 2) {
        ID = rem_study[2]
      } else {
        ID = tmpID
      }
    }
  }
  if (length(ID) == 0) {
    cat(paste0("\nWarning: Person ID number not retrieved"))
    cat(paste0("\n",personfolder))
  } else {
    
    outputfolder = paste0(personfolder,"/processinginR_SS",ID)
    if (!dir.exists(outputfolder)) dir.create(outputfolder)
    #============================
    # Withings
    withingsfolder = c()
    withingsi = grep(pattern = "Withings-",x = channelfolders)
    direct.download.withings = TRUE
    if (length(withingsi) > 0) {
      withingsfolder = channelfolders[withingsi]
    } else { # files are not in Withings folder yet, create the folder and copy them there
      cat("\n Copying files to new Withings-data folder")
      withingsfolder = paste0(personfolder,"/Withings-data" )
      dir.create(withingsfolder)
      fn_wit = dir(personfolder,recursive = F,full.names = F) # filenames in Withings folder
      withingsfiles = fn_wit[grep("withings",x = fn_wit)]
      for (ki in 1:length(withingsfiles)) {
        file.copy(from = paste0(personfolder,"/",withingsfiles[ki]),
                  to = paste0(withingsfolder,"/",withingsfiles[ki]))
      }
    }
    if (length(withingsfolder) > 0) {
      # check whether there is direct download data or pdk data
      fn_wit = dir(withingsfolder,recursive = T,full.names = T) # filenames in Withings folder
      csvfiles = grep("[.]cs",x = fn_wit)
      txtfiles = grep("[.]tx",x = fn_wit)
      fn_wit_csv = fn_wit[csvfiles]
      fn_wit_txt = fn_wit[txtfiles]
      if (length(fn_wit_csv) > 0) {
        cat("\nWithings-Direct-Download")
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
      }
      if (length(fn_wit_txt) > 0) {
        cat("\nWithings-PDK")
        directdownload = FALSE
        withings_sleep = withings_act = c()
        fullpathout = paste0(outputfolder,"/Withings-PDK.RData")
        if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
          for (wfi in 1:length(withingsfolder)) {
            filefolder = withingsfolder[wfi]
            # note that function WithingsSleep and WithingsActivity search the filefolder recursevely for files that meet the description
            withings_act = getWithingsActivity(filefolder, desiredtz, directdownload)
            withings_sleep = getWithingsSleep(filefolder, desiredtz, directdownload)
          }
          if (length(withings_act) > 0 | length(withings_sleep) > 0) {
            save(withings_act, withings_sleep,file=fullpathout)
          }
        }
      }
    }
    #==========================
    # Phone data
    if (paste0(personfolder,"/pdk-device-battery") %in% channelfolders) {
      cat("\npdk-device-battery")
      batfolder = paste0(personfolder,"/pdk-device-battery/")
      fn_bat = dir(batfolder)
      filename = paste0(batfolder,fn_bat)
      fullpathout = paste0(outputfolder,"/batInteractTimes.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        batInteractTimes = getBatInteract(filename, desiredtz) # timestamps when phone was either put on charged or plugged out.
        save(batInteractTimes,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-location") %in% channelfolders) {
      cat("\npdk-location")
      locfolder = paste0(personfolder,"/pdk-location/")
      fn_loc = dir(locfolder)
      filename = paste0(locfolder,fn_loc)
      fullpathout = paste0(outputfolder,"/MovementGPSTimes.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        MovementGPSTimes = getMovementGPS(filename, desiredtz)
        save(MovementGPSTimes,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-system-status") %in% channelfolders) {
      cat("\npdk-system-status")  # Runtime information is relevant for checking data missingness
      statusfolder = paste0(personfolder,"/pdk-system-status/")
      fn_status = dir(statusfolder)
      filename = paste0(statusfolder,fn_status)
      fullpathout = paste0(outputfolder,"/AppHalted.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        AppHalted = getSystemHalted(filename, desiredtz)
        save(AppHalted,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-sensor-accelerometer") %in% channelfolders) {
      cat("\npdk-sensor-accelerometer")
      accfolder = paste0(personfolder,"/pdk-sensor-accelerometer/")
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
    if (paste0(personfolder,"/pdk-sensor-light") %in% channelfolders) {
      cat("\npdk-sensor-light")
      # Light probably not useful, because light can change without the person change activity state
      filefolder = paste0(personfolder,"/pdk-sensor-light/")
      fullpathout = paste0(outputfolder,"/lightOnTimes.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        lightOnTimes = getLight(filefolder, desiredtz)
        save(lightOnTimes,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-foreground-application") %in% channelfolders) {
      cat("\npdk-foreground-application")
      appfolder = paste0(personfolder,"/pdk-foreground-application/")
      fn_app = dir(appfolder)
      filename = paste0(appfolder,fn_app)
      fullpathout = paste0(outputfolder,"/AppActiveTimes.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        AppActiveTimes = getAppActive(filename, desiredtz)
        save(AppActiveTimes,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-screen-state") %in% channelfolders) {
      cat("\npdk-screen-state")
      # Note: probably not relevant, because screen activity does not necessarily
      # say much about whether the person interacted with the phone, e.g. incoming call
      # or messages...?
      screstafolder = paste0(personfolder,"/pdk-screen-state/")
      fn_scresta = dir(screstafolder)
      filename = paste0(screstafolder,fn_scresta)
      fullpathout = paste0(outputfolder,"/ScreenOnTimes.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        ScreenOnTimes = getScreenState(filename, desiredtz)
        save(ScreenOnTimes,file=fullpathout)
      }
    }
    if (paste0(personfolder,"/pdk-time-of-day") %in% channelfolders) {
      cat("\npdk-time-of-day")
      todfolder = paste0(personfolder,"/pdk-time-of-day/")
      fn_tod = dir(todfolder)
      filename = paste0(todfolder,fn_tod)
      fullpathout = paste0(outputfolder,"/SunSetRise.RData")
      if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
        SunSetRise = getSunSetRise(filename, desiredtz)
        save(SunSetRise,file=fullpathout)
      }
    }
    if (length(txt_files_in_folder) > 0) { # Sleep Survey
      cat("\npdk-sleep-survey")
      sleepsurveyFile = grep("sleep-survey",x = txt_files_in_folder)
      if (length(sleepsurveyFile) > 0) {
        fullpathout = paste0(outputfolder,"/SleepSurvey.RData")
        if (!file.exists(fullpathout) | overwrite == TRUE) { # only load data if file does not exist yet
          filename = txt_files_in_folder[sleepsurveyFile]
          SleepSurvey = getSurvey(filename,desiredtz)
          save(SleepSurvey,file=paste0(outputfolder,"/SleepSurvey.RData"))
        }
      }
    }
  }
  return(outputfolder)
}