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

# foldersInStudyFolder = "/media/vincent/sleepsight/pilotdata" #"/media/vincent/sleepsight/SS08"
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
  timer1 = Sys.time()
  deltatime = difftime(timer0,timer1,units = "secs")
  cat(paste0("\n     ",deltatime))
}

