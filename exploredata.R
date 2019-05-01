# By: Vincent van Hees 2019
rm(list=ls())
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
library(Sleepsight)
library(data.table)
#==============================================================
# input variables
overwrite.preprocess = FALSE# whether to overwrite previously generated preprocessing output with this R code.
overwrite.aggregate = FALSE
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
    if (months[1] != "NULL-NULL") {
      # heatmap
      # color coding:
      coldic <- c("active" = "#D55E00",
                  "inactive" = "#F0E442",
                  "sleep" = "#56B4E9",
                  "sustained inactive" = "#009E73",
                  "inconsluvie" = "#CC79A7",
                  "no data" = "#999999")
      # Single heatmap per person
      data2plot = Dshort
      # simplify classes
      data2plot$status[which(data2plot$status %in% c("sustained inactive","sleep") == TRUE)] = "inactive"
      
      ddd = unique(data2plot$date)
      ddd = ddd[seq(1,length(ddd),15)]
      data2plot$date = as.Date(data2plot$date)
      doubleplot = data2plot
      doubleplot$hour_in_day = doubleplot$hour_in_day + 24
      doubleplot$date = doubleplot$date - 1
      data2plot = rbind(data2plot, doubleplot)
      data2plot = data2plot[order(data2plot$hour_in_day,data2plot$date),]
      data2plot$date = as.character(data2plot$date)
      png(filename = heatmapsfile ,width = 15, height = 7,units = "in",res = 400)
      myplot = ggplot(data2plot) +
        geom_tile(aes(date, hour_in_day, fill=status, color=status)) +
        xlab("") +
        ylab("Hour in day") +
        theme_bw() +
        scale_colour_manual(values = coldic) +
        scale_fill_manual(values = coldic) +
        scale_x_discrete(breaks = ddd) +
        theme(axis.text.x = element_text(angle = 45))
      print(myplot)
      dev.off()
      
      # Alternative heatmap, but now per month and presented as a grid:
      # myplots = list() 
      # Nmonths = length(unique(Dshort$month))
      # nCol = 2
      # nRow = ceiling(Nmonths / 2)
      # png(filename = heatmapsfile ,width = nCol*7, height = nRow * 2.5,units = "in",res = 400)
      # par(mfrow=c(6,2))
      # for (mi in 1:Nmonths) {
      #   data2plot = Dshort[which(Dshort$month == months[mi]),]
      #   myplots[[mi]] = ggplot(data2plot) +  #, colour = status
      #     geom_tile(aes(day, hour_in_day, fill=status, color=status)) +
      #     theme(axis.text.x = element_text(angle = 45)) +
      #     ggtitle(months[mi]) +
      #     ylab("time in day") +
      #     theme_bw() +
      #     scale_colour_manual(values = coldic) +
      #     scale_fill_manual(values = coldic)
      # }
      # do.call("grid.arrange", c(myplots, ncol=nCol, nrow = nRow))
      # dev.off()
      
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

