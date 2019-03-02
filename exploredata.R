# By: Vincent van Hees 2019
rm(list=ls())
# setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
# roxygen2::roxygenise()
library(Sleepsight)
library(data.table)
#==============================================================
# input variables
overwrite = FALSE # whether to overwrite previously generated output with this R code.
do.plot = TRUE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
desiredtz = "Europe/London"
studyfolder = "/media/vincent/sleepsight"
# Note: Assumption that all zip-files have been unzipped



#==============================================================
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)
plothist = function(namevar,CDF,df,plottitle) {
  if (namevar %in% CDF)  {
    hist(df$min_inday[which(df[,namevar]==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
    axis(side = 1,at = Xposi,labels = Xlabe); title(main=plottitle,xlab="time")
  }
}
for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  tmp = unlist(strsplit(personfolder,"/"))
  personID = tmp[length(tmp)] #now using personfolder name as personID.
  
  cat("\n* Preprocess")
  outputfolder = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite)
 
  cat("\n* Export to csv")
  csvfile = paste0("Sleepsight_overview_",personID,".csv")
  export2csv(outputfolder,csvfile,desiredtz)
  
  cat("\n* Create histograms")
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    RDAfiles = dir(outputfolder,full.names = TRUE)
    for (RDAfile in RDAfiles) {
      if (length(unlist(strsplit(RDAfile,"[.]cs"))) < 2 & length(unlist(strsplit(RDAfile,"[.]pn"))) < 2) {
        load(file=RDAfile)
      }
    }
    clocktimesX = c("00:00","06:00","12:00","18:00","24:00")
    Xposi = c(0,6,12,18,24) * 60
    Xlabe = clocktimesX
    df = read.csv(file=paste0(outputfolder,"/",csvfile))

    png(filename = paste0(outputfolder,"/histograms_test.png"),width = 12,height = 10,units = "in",res = 400)
    par(mfrow=c(3,4))
    CX = 0.05
    CDF = colnames(df)

    plothist(namevar="screenon",CDF=CDF,df=df,plottitle="Phone screen is on")
    plothist(namevar="lighton",CDF=CDF,df=df,plottitle="Phone light level above 10")
    plothist(namevar="PSGmove",CDF=CDF,df=df,plottitle="Phone PSG/Speed indicate movement")
    plothist(namevar="AppAct",CDF=CDF,df=df,plottitle="Phone foreground app is on")
    plothist(namevar="batinteract",CDF=CDF,df=df,plottitle="Phone battery put on/off charge")
    plothist(namevar="phoneacc" ,CDF=CDF,df=df,plottitle="Phone accelerates > 0.03 times gravity")
    plothist(namevar="withingsMove" ,CDF=CDF,df=df,plottitle="Withings moves / Body info entered")
    plothist(namevar="SunSetRise" ,CDF=CDF,df=df,plottitle="sunset or sunrise")
    plothist(namevar="AppHalted" ,CDF=CDF,df=df,plottitle="AppHalted (restarted)")
    plothist(namevar="lightsleep" ,CDF=CDF,df=df,plottitle="Withings LightSleep")
    plothist(namevar="deepsleep" ,CDF=CDF,df=df,plottitle="Withings deep-sleep")
    plothist(namevar="awake" ,CDF=CDF,df=df,plottitle="Withings awake")
    dev.off()
  }
  timer1 = Sys.time()
  cat(paste0("\n     ",timer1 - timer0))
}

