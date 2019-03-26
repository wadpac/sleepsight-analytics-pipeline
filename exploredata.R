# By: Vincent van Hees 2019
rm(list=ls())
setwd("/home/vincent/sleepsight-analytics-pipeline") # only needed for roxygen2 command on next line
roxygen2::roxygenise()
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
    save(D24HR, D5min, file = aggregatefile)
  } else {
    load(file = aggregatefile)
  }
  
  # TO DO: Calculate descriptives across rolling 7 or 14 days
  # TO DO: Merge in survey questions
  # TO DO: rethink names of "aggregated.RData" and function "summarise"
  # TO DO: avoid depending on setwd within functions
  # TO DO: expand documentation
  # TO DO: make data cleaning parameters in summarise() modifiable
  
  #------------------------------------------------------------------
  timer1 = Sys.time()
  deltatime = difftime(timer0,timer1,units = "secs")
  cat(paste0("\n     ",deltatime))
}

