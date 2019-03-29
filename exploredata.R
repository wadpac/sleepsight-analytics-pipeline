# By: Vincent van Hees 2019
rm(list=ls())
graphics.off()
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
library(Sleepsight)
library(data.table)
#==============================================================
# input variables
overwrite = FALSE # whether to overwrite previously generated output with this R code.
do.plot = FALSE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
desiredtz = "Europe/London"
studyfolder = "/media/vincent/sleepsight"
# Note: Assumption that all zip-files have been unzipped

#==============================================================
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)

# foldersInStudyFolder = "/media/vincent/sleepsight/pdk-export_2019-03-24_352"#"/media/vincent/sleepsight/SS08"
for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  
  cat("\n* Preprocess")
  outputfolder = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite)
  cat("\n* Export to csv")
  tmpID = unlist(strsplit(outputfolder,"/"))
  personID = unlist(strsplit(tmpID[length(tmpID)],"_"))[2]
  csvfile = paste0("Sleepsight_overview_",personID,".csv")
  export2csv(outputfolder,csvfile,desiredtz)
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    cat("\n* Create histograms")
    testplot(outputfolder, csvfile)
  }
  
  cat("\n* Aggregate data to 5 minute and day level")
  aggregatefile = paste0(outputfolder,"/aggregated.RData")
  if (!file.exists(aggregatefile)) {
    out = summarise(outputfolder, csvfile, desiredtz)
    D24HR = out$D24HR
    D5min = out$D5min
    Dsurvey = out$Dsurvey
    save(D24HR, D5min, Dsurvey, file = aggregatefile)
  } else {
    load(file = aggregatefile)
  }
  
  write.csv(D5min, file = paste0(outputfolder,"/Aggregated_per_5minutes.csv"),row.names = FALSE)
  write.csv(D24HR, file = paste0(outputfolder,"/Aggregated_per_24hour.csv"),row.names = FALSE)
  write.csv(Dsurvey, file = paste0(outputfolder,"/Simplified_Survey.csv"),row.names = FALSE)
  
  # TO DO: remove dependency on setwd within functions
  # TO DO: expand documentation + rethink names of "aggregated.RData" and function "summarise"
  # TO DO: make data cleaning parameters in summarise() modifiable
  # TO DO: Investigate issues with failed ID identification
  # TO DO: Move plotting code to separate function
  # LATER: Add double plot
  
  # DISCUSS: Self-reported sleep extracted, but not used yet
  # DISCUSS: fix L5 M10 variables => not easy without imputations
  cat("\n* Overview plot")
  
  library(ggplot2)
  library(gridExtra)
  
  D5min$status2 = as.character(D5min$status)
  D5min$status2[which(D5min$status2 == "1")] = "active"
  D5min$status2[which(D5min$status2 == "0")] = "inactive"
  D5min$status2[which(D5min$status2 == "-1")] = "sleep"
  D5min$status2[which(D5min$status2 == "4")] = "missing"
  D5min$status = as.factor(D5min$status2)
  D5min$month = paste0(format(D5min$time.POSIX,"%m"),"-",format(D5min$time.POSIX,"%Y"))
  D5min$day = format(D5min$time.POSIX,"%d")
  months = unique(D5min$month)
  myplots = list() 
  if (months[1] != "NULL-NULL") {
    png(filename = paste0(outputfolder,"/missingness.png"),width = 12,height = 10,units = "in",res = 400)
    par(mfrow=c(6,2))
    for (mi in 1:12) {
      data2plot = D5min[which(D5min$month == months[mi]),]
      myplots[[mi]] = ggplot(data2plot, aes(day, hour_in_day, colour= status)) + geom_tile(aes(fill=status)) +
        theme(axis.text.x = element_text(angle = 45)) + ggtitle(months[mi]) + ylab("time") + theme_bw()
    }
    grid.arrange(myplots[[1]], myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]],myplots[[6]],
                 myplots[[7]], myplots[[8]],myplots[[9]],myplots[[10]],myplots[[11]],myplots[[12]], nrow = 6)
    dev.off()
    
    png(filename = paste0(outputfolder,"/timeseries.png"),width = 12,height = 10,units = "in",res = 400)
    par(mfrow=c(5,2),bty="l",pch=20)
    XL = "day"
    YLIM = c(0,24)
    plot(D24HR$sleepdur,type="p",main="sleep duration (hours)",xlab=XL, ylab="",ylim=YLIM)
    plot(D24HR$activedur,type="p",main = "active duration (hours)",xlab=XL, ylab="",ylim=YLIM)
    plot(D24HR$inactivedur,type="p",main= "inactive durations (hours)",xlab=XL, ylab="",ylim=YLIM)
    plot(D24HR$missingdur,type="p",main= "missing data durations (hours)",xlab=XL, ylab="",ylim=YLIM)
    # plot(D24HR$L5hour,type="p",main = "L5 hour",xlab=XL, ylab="",ylim=YLIM)
    plot(D24HR$M10hour,type="p",main = "M10 hour",xlab=XL, ylab="",ylim=YLIM)
    plot(D24HR$daylength, type="p",main= "length day (hours)",xlab=XL, ylab="",ylim=YLIM)
    # plot(D24HR$L5value/(5*12),type="p",main = "L5 value",xlab=XL, ylab="")
    plot(D24HR$M10value/(10*12),type="p",main = "M10 value",xlab=XL, ylab="")
    plot(Dsurvey$surveytime, Dsurvey$positiveFeelings,type="l",main="Positive Feelings")
    plot(Dsurvey$surveytime, Dsurvey$negativeFeelings,type="l",main="Negative Feelings")
    plot(Dsurvey$surveytime, Dsurvey$Sleep.Quality.Value,type="l",main="Sleep Quality (reported)")
    dev.off()
  } else {
    cat("\n WARNING: plot skipped!")
  }
  #------------------------------------------------------------------
  timer1 = Sys.time()
  deltatime = difftime(timer0,timer1,units = "secs")
  cat(paste0("\n     ",deltatime))
}

