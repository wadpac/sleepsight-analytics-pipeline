# By: Vincent van Hees 2019
rm(list=ls())
graphics.off()

setwd("/home/vincent/sleepsight-analytics-pipeline")
roxygen2::roxygenise()
# specify location of data
# path = "/media/vincent/sleepsight/pilotdata"
path = "/media/vincent/sleepsight/SS08" 
setwd(path)

# Note: Zip-extraction and file identification will need to be more
# streamlined in actual pipeline
# zipfile = "pdk-export_2019-01-25_313.zip"
outdir = "."
# unzip file
# unzip(zipfile,exdir = outdir)
desiredtz = "Europe/London"
#--------------------------------------------------
# Declare common functions:
library(sleepsanpl)
library(data.table)
library(bit64)
#-----------------------------------------
# extract battery information
batfolder = "pdk-device-battery/"
fn_bat = dir(batfolder)
filename = paste0(batfolder,fn_bat)
batInteractTimes = getBatInteract(filename) # timestamps when phone was either put on charged or plugged out.

#-------------------------------------
# pdk-foreground-application
appfolder = "pdk-foreground-application/"
fn_app = dir(appfolder)
filename = paste0(appfolder,fn_app)
AppActiveTimes = getAppActive(filename)

#-------------------------------------
# pdk-location
locfolder = "pdk-location/"
fn_loc = dir(locfolder)
filename = paste0(locfolder,fn_loc)
MovementPSGTimes = getMovementPSG(filename)

# # -------------------------------------
# # pdk-screen-state
# # Note: probably not relevant, because screen activity does not necessarily
# # say much about whether the person interacted with the phone, e.g. incoming call
# # or messages...?
screstafolder = "pdk-screen-state/"
fn_scresta = dir(screstafolder)
filename = paste0(screstafolder,fn_scresta)
ScreenOnTimes = getScreenState(filename)

#-------------------------------------
# pdk-sensor-accelerometer
accfolder = "pdk-sensor-accelerometer/"
PhoneAcc = getPhoneAcc(accfolder, desiredtz) # Note that this function provides all acceleration information
# Comments on usefulness:
# - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
# - Data is not collected in the absense of movement, which complicates autocalibraton.
# - We cannot use the default time extraction because that would only reflect when data
#   blocks are created. Instread used "Normalized Timestamp"
# - It seems that timestamps are not ordered correctly, so first order timestamps.

#-------------------------------------
# pdk-sensor-light
# Light probably not useful, because light can change without the person change activity state
filefolder = "pdk-sensor-light/"
lightOnTimes = getLight(filefolder, desiredtz)


##-------------------------------------
# Withings device
filefolder = "Withings-20190215T093727Z-001/" # note that function searches this folder recursevely for files that meet the description
withingsdata = getWithingsData(filefolder)


#======================================
# Information not used:
#======================================

## -------------------------------------
## pdk-system-status
## Gives storage level, not relevant for behavior
## sysstafolder = "pdk-system-status/"
## fn_syssta = dir(sysstafolder)
## syssta = data.table::fread(file=paste0(sysstafolder,fn_syssta),sep="\t")

## -------------------------------------
## pdk-time-of-day
## Gives sunrise, maybe useful as contextual data? Ignore for now, or better to retrieve this from other source?
## todfolder = "pdk-time-of-day/"
## fn_tod = dir(todfolder)
## tod = data.table::fread(file=paste0(todfolder,fn_tod),sep="\t")

##-------------------------------------
# awake, deep sle, light sleep:
# TO DO: Investigate how this can be used in activity profiling
# devsle = data.table::fread(file=fn_wit[grep("device-sleep",x = fn_wit)],sep="\t")