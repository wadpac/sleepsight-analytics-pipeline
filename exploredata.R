# By: Vincent van Hees 2019
rm(list=ls())
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
library(Sleepsight)
library(data.table)
#==============================================================
# input variables
overwrite.preprocess = FALSE# whether to overwrite previously generated preprocessing output with this R code.
overwrite.aggregate = TRUE
do.plot = TRUE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
desiredtz = "Europe/London"
studyfolder = "/media/vincent/sleepsight"
outputfolder = "/media/vincent/sleepsight/results"
# Note: see README for expected folder structure!

#==============================================================
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)
removei = grep(x = foldersInStudyFolder,pattern = "results")
if (length(removei) > 0) foldersInStudyFolder = foldersInStudyFolder[-removei]

for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  
  cat("\n* Preprocess")
  outputfolderID = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite.preprocess,
                            outputfolder=outputfolder)
  cat("\n* Export to csv")
  personID = unlist(strsplit(outputfolderID,"/preproces/SS"))[2]
  csvfolder = paste0(outputfolder,"/preproces2csv")
  if (!dir.exists(csvfolder)) dir.create(csvfolder)
  csvfile = paste0(csvfolder,"/Sleepsight_overview_",personID,".csv")
  export2csv(outputfolderID, csvfile, desiredtz)
  
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    cat("\n* Create histograms")
    histfolder = paste0(outputfolder,"/histograms")
    if (!dir.exists(histfolder)) dir.create(histfolder)
    histfile = paste0(histfolder,"/histogram_",personID,".png")
    testplot(histfile, csvfile)
  }
  cat("\n* Aggregate data per: day, 30 minutes, and 1 minute")
  aggfolder = paste0(outputfolder,"/aggregated")
  if (!dir.exists(aggfolder)) dir.create(aggfolder)
  aggregatefile = paste0(aggfolder,"/agg.sleepsight_",personID,".RData")
  
  surveyfile = paste0(outputfolderID,"/SleepSurvey")
  
  if (!file.exists(aggregatefile) | overwrite.aggregate == TRUE) {
    out = agg.sleepsight(aggregatefile, csvfile, surveyfile, desiredtz, minmisratio = 1/3, shortwindow = 1, longwindow = 30)
    D24HR = out$D24HR
    Dshort = out$Dshort # 1 minute
    Dlong = out$Dlong # 30 minutes
    Dsurvey = out$Dsurvey
    save(D24HR, Dshort, Dlong, Dsurvey, file = aggregatefile)
  } else {
    load(file = aggregatefile)
  }
  write.csv(Dshort, file = paste0(aggfolder,"/Aggregated_per_shortwindow_",personID,".csv"),row.names = FALSE)
  write.csv(Dlong, file = paste0(aggfolder,"/Aggregated_per_longwindow_",personID,".csv"),row.names = FALSE)
  write.csv(D24HR, file = paste0(aggfolder,"/Aggregated_per_day_",personID,".csv"),row.names = FALSE)
  write.csv(Dsurvey, file = paste0(aggfolder,"/Simplified_Survey_",personID,".csv"),row.names = FALSE)
  
  # TO DO: Move visualisation code below to separate function when it has matured
  cat("\n* Overview visualisations")
  
  heatmapsfolder = paste0(outputfolder,"/heatmaps")
  if (!dir.exists(heatmapsfolder)) dir.create(heatmapsfolder)
  heatmapsfile = paste0(heatmapsfolder,"/heatmap_",personID,".png")
  
  timeseriesfolder = paste0(outputfolder,"/timeseries")
  if (!dir.exists(timeseriesfolder)) dir.create(timeseriesfolder)
  timeseriesfile = paste0(timeseriesfolder,"/timeserie_",personID,".png")
  
  library(ggplot2)
  library(gridExtra)
  if (length(Dshort) > 0) {
    months = unique(Dshort$month)
    myplots = list() 
    if (months[1] != "NULL-NULL") {
      # heatmap
      png(filename = heatmapsfile ,width = 12,height = 10,units = "in",res = 400)
      par(mfrow=c(6,2))
      for (mi in 1:12) {
        data2plot = Dshort[which(Dshort$month == months[mi]),]
        myplots[[mi]] = ggplot(data2plot, aes(day, hour_in_day, colour= status)) + geom_tile(aes(fill=status)) +
          theme(axis.text.x = element_text(angle = 45)) + ggtitle(months[mi]) + ylab("time in day") + theme_bw()
      }
      grid.arrange(myplots[[1]], myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]],myplots[[6]],
                   myplots[[7]], myplots[[8]],myplots[[9]],myplots[[10]],myplots[[11]],myplots[[12]], nrow = 6)
      dev.off()
      # time series
      png(filename = timeseriesfile,width = 12,height = 10,units = "in",res = 400)
      par(mfrow=c(5,2),bty="l",pch=20)
      XL = "day"
      YLIM = c(0,24)
      plot(D24HR$sleepdur,type="p",main="sleep (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$susindur,type="p",main= "sustained inactivity (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$inactivedur,type="p",main= "inactive (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$activedur,type="p",main = "active (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$inconclusive_dur,type="p",main = "inconclusive (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$missing_dur,type="p",main= "missing data (hours)",xlab=XL, ylab="",ylim=YLIM)
      plot(D24HR$daylength, type="p",main= "length day (hours)",xlab=XL, ylab="",ylim=YLIM)
      if (length(Dsurvey) > 0) {
        plot(Dsurvey$surveytime, Dsurvey$positiveFeelings,type="l",main="Positive Feelings")
        plot(Dsurvey$surveytime, Dsurvey$negativeFeelings,type="l",main="Negative Feelings")
        plot(Dsurvey$surveytime, Dsurvey$Sleep.Quality.Value,type="l",main="Sleep Quality (reported)")
      }
      dev.off()
    }
  }
  #------------------------------------------------------------------
  timer1 = Sys.time()
  deltatime = difftime(timer0,timer1,units = "secs")
  cat(paste0("\n     ",deltatime))
}

