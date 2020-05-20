# By: Vincent van Hees 2019
rm(list=ls())
#==============================================================
# Input arguments for this script:
development.mode = FALSE # Set to FALSE if you are using this script

overwrite.preprocess = TRUE # whether to overwrite previously generated preprocessing output with this R code.
overwrite.preprocess2csv = TRUE # whether to overwrite previously generated csv exports of the preprocessed data.
overwrite.aggregate = TRUE # whether to overwrite previously generated aggregates
do.plot = TRUE # whether to create a simple histogram of available data and write it to file "histograms_test" inside each data folder.

simplify.behavioralclasses = FALSE # set to FALSE otherwise inactivity and sleep arge merged into one class, which we explored earlier

withings.mode = "dd" # Either "pdk" or "dd" to indicate which data source for Withings data to prioritise pdk or dd
lightThreshold = 10 # Light value above which light is stored, and below which we assume darkness
ignore.light = FALSE #Note: If light is not available, this is ignored.
desiredtz = "Europe/Amsterdam"

# Note: see README for expected folder structure!
studyfolder = "/media/vincent/projects/sleepsight"
outputfolder = "/media/vincent/projects/sleepsight/results"

dateRange = c("01","2017-08-15","2018-08-14",
              "02","2017-08-18","2018-08-23",
              "03","2017-09-06","2018-09-05",
              "04","2017-09-08","2018-09-07",
              "05","2017-09-14","2018-03-30",
              "06","2017-09-25","2018-09-24",
              "07","2017-10-05","2018-06-30",
              "08","2017-09-17","2019-05-18",
              "09","2017-10-17","2018-10-23",
              "10","2017-11-02","2018-11-01",
              "11","2017-11-07","2018-11-06",
              "12","2017-11-10","2017-12-22",
              "13","2017-12-06","2018-12-05",
              "14","2017-12-08","2018-12-07",
              "15","2017-12-08","2019-02-01",
              "16","2018-01-22","2019-01-21",
              "17","2018-02-08","2019-04-18",
              "18","2018-03-06","2019-03-05",
              "19","2018-03-20","2019-05-05",
              "20","2018-03-28","2018-05-11",
              "21","2018-03-29","2019-02-01",
              "22","2018-04-18","2019-04-17",
              "23","2018-04-27","2019-04-26",
              "24","2018-05-15","2019-05-14",
              "25","2018-05-30","2019-05-29",
              "26","2018-06-07","2019-06-07",
              "27","2018-06-28","2019-06-20",
              "28","2018-07-12","2019-06-20",
              "29","2018-07-25","2019-06-20",
              "30","2018-03-01","2018-11-01",
              "31","2018-08-15","2019-06-20",
              "32","2018-08-21","2019-06-20",
              "33","2018-09-12","2019-06-20",
              "34","2018-10-17","2019-06-20",
              "35","2019-02-13","2019-06-20",
              "36","2019-02-27","2019-06-20")

#==============================================================
if (development.mode == FALSE) {
  list.of.packages <- c("devtools", "data.table","roxygen2", "zoo", "pracma", "bit64", "gridExtra", "ggplot2", "cowplot")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(devtools)
  install_github("wadpac/sleepsight-analytics-pipeline")
  library(Sleepsight)
} else {
  roxygen2::roxygenise()
  locationRcode = "/home/vincent/projects/sleepsight-analytics-pipeline/R" 
  ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
  for (i in 1:length(ffnames)) {
    source(paste(locationRcode,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
  }
}
library(data.table)
library(ggplot2)
library(gridExtra)

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

if (development.mode == TRUE) {
  foldersInStudyFolder = c("/media/vincent/projects/sleepsight/SS08")
  # ,    "/media/vincent/sleepsight/SS25") #c("/media/vincent/sleepsight/SS08","/media/vincent/sleepsight/SS14")
  foldersInStudyFolder = c("/media/vincent/projects/sleepsight/SS34")
}
for (personfolder in foldersInStudyFolder) {
  timer0 = Sys.time()
  cat("\n==================================================================================")
  cat(paste0("\n",personfolder))
  # check that the folder has expected structure and give feedback to user if this is not the case
  check_personfolder(personfolder)
  
  # preproces the data
  preproDataPerID = preprocess(personfolder,desiredtz = desiredtz, overwrite=overwrite.preprocess,
                               outputfolder=outputfolder, ignore.light = ignore.light, lightThreshold = lightThreshold)
  # extract ID and specify desired name of output csv and png files
  personID = unlist(strsplit(preproDataPerID,"/preproces/SS"))[2]
  if (length(personID) == 0) warning(paste0("\nParticipant specific folder does not have SS in name"))
  
  csvfile = paste0(csvfolder,"/Sleepsight_overview_",personID,".csv")
  timeseriesfile = paste0(timeseriesfolder,"/timeserie_",personID,".png")
  startDate = c(); endDate = c()
  if (personID %in% dateRange$id) {
    dateRange_rownr = which(dateRange$id %in% personID)
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
                         withings.mode = withings.mode, startDate = startDate, endDate = endDate)
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
             simplify.behavioralclasses, Dsurvey, startDate, endDate, desiredtz)
    # time series
    plot_timeseries(D24HR, Dsurvey, timeseriesfile, desiredtz, startDate = startDate, endDate = endDate)
  }
  #------------------------------------------------------------------
  deltatime = difftime(Sys.time(), timer0, units = "secs")
  cat(paste0("\n__Time elapsed: ", abs(round(deltatime)), " seconds__"))
}
