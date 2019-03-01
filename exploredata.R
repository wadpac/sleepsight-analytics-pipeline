# By: Vincent van Hees 2019
rm(list=ls())
graphics.off()
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
library(Sleepsight)
library(data.table)

# input variables
overwrite = FALSE # whether to overwrite previously generated output with this R code.
do.plot = TRUE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
desiredtz = "Europe/London"
studyfolder = "/media/vincent/sleepsight"
# Note: Assumption that all zip-files have been unzipped
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)

# foldersInStudyFolder = "/media/vincent/sleepsight/SS08"  # for testing (comment out otherwise)
for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  tmp = unlist(strsplit(personfolder,"/"))
  personID = tmp[length(tmp)]
  RDatafile = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite)
  csvfile = paste0(personfolder,"/Sleepsight_overview_",personID,".csv")
  export2csv(RDatafile,csvfile,desiredtz)
  
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    load(file=RDatafile)
    clocktimesX = c("00:00","06:00","12:00","18:00","24:00")
    Xposi = c(0,6,12,18,24) * 60 #as.POSIXct(clocktimesX, format="%H:%M",tz = desiredtz)
    Xlabe = clocktimesX
    
    png(filename = paste0(personfolder,"/histograms_test.png"),width = 12,height = 10,units = "in",res = 400)
    par(mfrow=c(3,4))
    CX = 0.05
    CDF = colnames(df)
    if ("screenon" %in% CDF)  {
      hist(df$min_inday[which(df$sreenon==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone screen is on",xlab="time")
    }
    if ("lighton" %in% CDF)  {
      hist(df$min_inday[which(df$lighton==TRUE)], axes= FALSE, xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone light level above 10",xlab="time")
    }
    if ("PSGmove" %in% CDF)  {
      hist(df$min_inday[which(df$PSGmove == TRUE)], axes = FALSE, xlab="",ylab="",main = "") 
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone PSG/Speed indicate movement",xlab="time")
    }
    if ("AppAct" %in% CDF)  {
      hist(df$min_inday[which(df$AppAct==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone foreground app is on",xlab="time")
    }
    if ("batinteract" %in% CDF)  {
      hist(df$min_inday[which(df$batinteract==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone battery put on/off charge",xlab="time")
    }
    if ("phoneacc" %in% CDF)  {
      hist(df$min_inday[which(df$phoneacc==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone accelerates > 0.03 times gravity",xlab="time")
    }
    if ("withingsMove" %in% CDF)  {
      hist(df$min_inday[which(df$withingsMove==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings moves / Body info entered",xlab="time")
    }
    if ("SunSetRise" %in% CDF)  {
      hist(df$min_inday[which(is.na(df$SunSetRise) == FALSE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="sunset or sunrise",xlab="time")
    }
    if ("AppHalted" %in% CDF)  {
      hist(df$min_inday[which(df$AppHalted==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="AppHalted",xlab="time")
    }
    if ("lightsleep" %in% CDF)  {
      hist(df$min_inday[which(df$lightsleep == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings LightSleep",xlab="time")
    }
    if ("deepsleep" %in% CDF)  {
      hist(df$min_inday[which(df$deepsleep == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings deep-sleep",xlab="time")
    }
    if ("awake" %in% CDF)  {
      hist(df$min_inday[which(df$awake == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
      axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings awake",xlab="time")
    }
    dev.off()
  }
  timer1 = Sys.time()
  print(timer1 - timer0)
}

