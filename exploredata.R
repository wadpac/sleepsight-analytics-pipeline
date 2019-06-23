# By: Vincent van Hees 2019
rm(list=ls())
# graphics.off()
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()

list.of.packages <- c("devtools", "data.table","roxygen2", "zoo", "pracma", "bit64", "gridExtra", "ggplot2", "cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# library(devtools)
# install_github("wadpac/sleepsight-analytics-pipeline")
library(Sleepsight)
library(data.table)
library(ggplot2)
library(gridExtra)
#==============================================================
# Input arguments for this script:

overwrite.preprocess = FALSE # whether to overwrite previously generated preprocessing output with this R code.
overwrite.preprocess2csv = FALSE
overwrite.aggregate = TRUE
do.plot = TRUE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.
simplify.behavioralclasses = FALSE

withings.mode = "dd" # Either "pdk" or "dd" to indicate whether to prioritise pdk or dd

desiredtz = "Europe/London"

# Note: see README for expected folder structure!
studyfolder = "/media/vincent/sleepsight"
outputfolder = "/media/vincent/sleepsight/results"

dateRange = c("01","2016-02-01","2020-01-01",
              "02","2016-02-01","2020-01-01",
              "03","2016-02-01","2020-01-01",
              "04","2016-02-01","2020-01-01",
              "05","2016-02-01","2020-01-01",
              "06","2016-02-01","2020-01-01",
              "07","2016-02-01","2020-01-01",
              "08","2018-02-01","2018-06-01",
              "10","2016-02-01","2020-01-01",
              "15","2017-01-01","2019-01-01")

#==============================================================
# Create essential output folders

if (!dir.exists(outputfolder)) dir.create(outputfolder)
tmp = unlist(strsplit(outputfolder,"/"))
csvfolder = paste0(outputfolder,"/preproces2csv")
if (!dir.exists(csvfolder)) dir.create(csvfolder)
histfolder = paste0(outputfolder,"/histograms")
if (!dir.exists(histfolder)) dir.create(histfolder)
aggfolder = paste0(outputfolder,"/aggregated")
if (!dir.exists(aggfolder)) dir.create(aggfolder)
heatmapsfolder = paste0(outputfolder,"/heatmaps")
if (!dir.exists(heatmapsfolder)) dir.create(heatmapsfolder)
timeseriesfolder = paste0(outputfolder,"/timeseries")
if (!dir.exists(timeseriesfolder)) dir.create(timeseriesfolder)

# Find each person specific folder in the studyfolder
nameOfOutputFolder = tmp[length(tmp)]
foldersInStudyFolder = list.dirs(studyfolder, recursive=FALSE)
removei = grep(x = foldersInStudyFolder,pattern = nameOfOutputFolder)
if (length(removei) > 0) foldersInStudyFolder = foldersInStudyFolder[-removei]

if (length(foldersInStudyFolder) == 0) stop(paste0("\nNo folders found inside ",studyfolder))

# Reformat dateRange object
dateRange = as.data.frame(matrix(dateRange,ncol = 3,byrow = TRUE))
colnames(dateRange) = c("id","startDate","endDate")

# foldersInStudyFolder = c("/media/vincent/sleepsight/SS08")
# ,    "/media/vincent/sleepsight/SS25") #c("/media/vincent/sleepsight/SS08","/media/vincent/sleepsight/SS14")

for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  # check that the folder has expected structure and give feedback to user if this is not the case
  check_personfolder(personfolder)
  
  # preproces the data
  preproDataPerID = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite.preprocess,
                               outputfolder=outputfolder, ignore.light = TRUE)
  # extract ID and specify desired name of output csv and png files
  personID = unlist(strsplit(preproDataPerID,"/preproces/SS"))[2]
  if (length(personID) == 0) warning(paste0("\nParticipant specific folder does not have SS in name"))
  
  csvfile = paste0(csvfolder,"/Sleepsight_overview_",personID,".csv")
  timeseriesfile = paste0(timeseriesfolder,"/timeserie_",personID,".png")
  startDate = c(); endDate = c()
  if (personID %in% dateRange$id) {
    dateRange_rownr = which(personID %in% dateRange$id)
    startDate = dateRange$startDate[dateRange_rownr]
    endDate = dateRange$endDate[dateRange_rownr]
  }
  export2csv(preproDataPerID, csvfile, desiredtz, overwrite.preprocess2csv,
             startDate = startDate, endDate = endDate)
  
  # plot histograms as quick check on the data
  if (do.plot == TRUE) { # simple historgram of all available data channels within a person
    histfile = paste0(histfolder,"/histogram_",personID,".png")
    testplot(histfile, csvfile)
  }
  
  # aggregate the data per minute, 30 minutes and day
  aggregatefile = paste0(aggfolder,"/agg.sleepsight_",withings.mode,"_",personID,".RData")
  surveyfile = paste0(preproDataPerID,"/SleepSurvey.RData")
  if (!file.exists(aggregatefile) | overwrite.aggregate == TRUE) {
    cat("\n* Aggregate data per: day, 30 minutes, and 1 minute")
    out = agg.sleepsight(aggregatefile = aggregatefile, csvfile = csvfile, surveyfile = surveyfile, desiredtz = desiredtz, 
                         minmisratio = 1/3, shortwindow = 1, longwindow = 30,
                         withings.mode = withings.mode)
    D24HR = out$D24HR
    Dshort = out$Dshort # 1 minute
    Dlong = out$Dlong # 30 minutes
    Dsurvey = out$Dsurvey
    withings.mode = out$withings.mode
    save(D24HR, Dshort, Dlong, Dsurvey, withings.mode, file = aggregatefile)
  } else {
    load(file = aggregatefile)
  }
  
  # shorten Dsurvey to dateRange
  if (length(startDate) > 0 & length(endDate) > 0) {
    startDateNum = as.numeric(as.Date(startDate)) * 3600*24
    endDateNum = as.numeric(as.Date(endDate)) * 3600*24
    timeNum = as.numeric(Dsurvey$surveytime)
    validdates = which(timeNum > startDateNum & timeNum < endDateNum)
    if (length(validdates) != 0) Dsurvey = Dsurvey[validdates,]
  }
  # write aggregated data to csv files
  write.csv(Dshort, file = paste0(aggfolder,"/Aggregated_per_shortwindow_",withings.mode,"_",personID,".csv"),row.names = FALSE)
  write.csv(Dlong, file = paste0(aggfolder,"/Aggregated_per_longwindow_",withings.mode,"_",personID,".csv"),row.names = FALSE)
  write.csv(D24HR, file = paste0(aggfolder,"/Aggregated_per_day_",withings.mode,"_",personID,".csv"),row.names = FALSE)
  write.csv(Dsurvey, file = paste0(aggfolder,"/Simplified_Survey_",withings.mode,"_",personID,".csv"),row.names = FALSE)

  if (length(Dshort) > 0 & length(Dlong) > 0) {
    
    heatmapsfile = paste0(heatmapsfolder,"/heatmap_",withings.mode,"_",personID,".png")
    heatmapsfile_steps = paste0(heatmapsfolder,"/heatmap_steps_",withings.mode,"_",personID,".png")
    
    # heatmaps of status and steps
    heatmaps(Dshort, Dlong, heatmapsfile, heatmapsfile_steps, 
             simplify.behavioralclasses, Dsurvey)
    # time series
    plot_timeseries(D24HR, Dsurvey, timeseriesfile)
  }
  #------------------------------------------------------------------
  deltatime = difftime(Sys.time(), timer0, units = "secs")
  cat(paste0("\n__Time elapsed: ", abs(round(deltatime)), " seconds__"))
}
