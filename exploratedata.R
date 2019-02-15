# By: Vincent van Hees 2019
rm(list=ls())
graphics.off()

library(data.table)
library(bit64)

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
iso8601chartime2POSIX = function(x,tz){
  return(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz))
}
addPOSIX = function(x) {
  # add timestamps in POSIX format to make them R friendly
  removeLastSemicol = function(x) {
    tmp = unlist(strsplit(x,":"))
    y = paste0(tmp[1],":",tmp[2],":",tmp[3],tmp[4])
    return(y)
  }
  x$Created.Date = as.character(x$Created.Date)
  ts_ISO8601 = sapply(x$Created.Date,removeLastSemicol)
  x$Created.Date.POSIX = iso8601chartime2POSIX(as.character(ts_ISO8601),tz = desiredtz)
  return(x)
}
replaceVarWithSpace = function(x) {
  if("Created Date" %in% colnames(x) == TRUE) x$Created.Date = x$`Created Date`
  if("Created Timestamp" %in% colnames(x) == TRUE) x$Created.Timestamp = x$`Created Timestamp`
  if("Screen Active" %in% colnames(x) == TRUE) x$Screen.Active = x$`Screen Active`
  if("Screen State" %in% colnames(x) == TRUE) x$Screen.State = x$`Screen State`
  if("Normalized Timestamp" %in% colnames(x) == TRUE) x$Normalized.Timestamp = x$`Normalized Timestamp`
  if("Light Level" %in% colnames(x) == TRUE) x$Light.Level = x$`Light Level`
  return(x)
}

#-----------------------------------------
# extract battery information
batfolder = "pdk-device-battery/"
fn_bat = dir(batfolder)
bat = data.table::fread(file=paste0(batfolder,fn_bat),sep="\t")
bat = as.data.frame(bat)
bat = replaceVarWithSpace(bat)
bat = addPOSIX(bat)
plugged = bat$Plugged != "unknown"
batInteract = which(abs(diff(plugged)) != 0)
x11() # Create plot to QC event detection
plot(bat$Created.Date.POSIX,bat$Level,type="l",ylab="battery level")
lines(bat$Created.Date.POSIX[batInteract],bat$Level[batInteract],type="p",col="red",pch=20)
# timestamps when phone was either put on charged or plugged out:
# bat$Created.Date.POSIX[batInteract]

#-------------------------------------
# pdk-foreground-application
appfolder = "pdk-foreground-application/"
fn_app = dir(appfolder)
app = data.table::fread(file=paste0(appfolder,fn_app),sep="\t")
app = as.data.frame(app)
app = replaceVarWithSpace(app)
app = addPOSIX(app)
NR = nrow(app)
# screenactive = app$Screen.Active != "False" & app$Screen.Active != "FALSE"
app$Screen.Active.binary = rep(0,NR)
app$Screen.Active.binary[which(app$Screen.Active == "True" | app$Screen.Active == "TRUE")] = 1
screenInteract = which(app$Screen.Active.binary != 0)
x11() # Create plot to QC event detection
plot(app$Created.Date.POSIX,app$Screen.Active.binary,type="l",ylab="screen active")
lines(app$Created.Date.POSIX[screenInteract],app$Screen.Active.binary[screenInteract],type="p",col="red",pch=20)
# timestamps when phone screen was active:
# app$Created.Date.POSIX[screenInteract]

#-------------------------------------
# pdk-location
locfolder = "pdk-location/"
fn_loc = dir(locfolder)
loc = data.table::fread(file=paste0(locfolder,fn_loc),sep="\t")
loc = as.data.frame(loc)
loc = replaceVarWithSpace(loc)
loc = addPOSIX(loc)
# movement based on speed
movement_speed = which(loc$Speed != 0 & is.na(loc$Speed) == FALSE)
# movement based on change in Latitude
loc$Latitude = scale(loc$Latitude,center = TRUE) # scale to ease plotting
dLat = abs(diff(loc$Latitude))
movement_lat = which(dLat > 0.01)
# movement based on change in Longitude
loc$Longitude = scale(loc$Longitude,center = TRUE) # scale to ease plotting
dLon = abs(diff(loc$Longitude))
movement_lon = which(dLon > 0.01)
# combined indicator of movement
move = unique(c(movement_speed,movement_lat,movement_lon))
YLIM = range(c(loc$Longitude,loc$Latitude))
loc$Speed[movement_speed] = (loc$Speed[movement_speed] / max(loc$Speed[movement_speed])) * YLIM[2]
x11() # Create plot to QC event detection
plot(loc$Created.Date.POSIX,loc$Latitude,type="l",col="black",pch=20, ylim=YLIM)
lines(loc$Created.Date.POSIX,loc$Longitude,type="l",col="blue",pch=20)
lines(loc$Created.Date.POSIX[movement_speed],loc$Speed[movement_speed],type="p",col="black",pch=20)
lines(loc$Created.Date.POSIX[move],loc$Latitude[move],type="p",col="red",pch=20)
# timestamps when phone was moving according location or speed
# loc$Created.Date.POSIX[move]

# -------------------------------------
# pdk-screen-state
# Note: probably not relevant, because screen activity does not necessarily
# say much about whether the person interacted with the phone, e.g. incoming call
# or messages...?
screstafolder = "pdk-screen-state/"
fn_scresta = dir(screstafolder)
scresta = data.table::fread(file=paste0(screstafolder,fn_scresta),sep="\t")
scresta = as.data.frame(scresta)
scresta = replaceVarWithSpace(scresta)
scresta = addPOSIX(scresta)
NR = nrow(scresta)
screenactive = scresta$Screen.State != "off"
scresta$Screen.State.binary = rep(0,NR)
scresta$Screen.State.binary[which(scresta$Screen.State == "on")] = 1
screenState = which(scresta$Screen.State.binary != 0)
x11() # Create plot to QC event detection
plot(scresta$Created.Date.POSIX,scresta$Screen.State.binary,type="l",ylab="screen on")
lines(scresta$Created.Date.POSIX[screenState],scresta$Screen.State.binary[screenState],type="p",col="red",pch=20)

#-------------------------------------
# pdk-sensor-accelerometer
accfolder = "pdk-sensor-accelerometer/"
fn_acc = dir(accfolder)
outputMatrixAccstore = matrix(0,0,3)
for (j in 1:length(fn_acc)) {
  cat(paste0("\nLoading file ",fn_acc[j]))
  acc = data.table::fread(file=paste0(accfolder,fn_acc[j]),sep="\t")
  acc = as.data.frame(acc)
  acc = replaceVarWithSpace(acc)
  NR = nrow(acc)
  acc$Created.Date.POSIX = as.POSIXlt((acc$Normalized.Timestamp)/(1e+9),tz=desiredtz,origin="1970-1-1")
  acc = acc[order(acc$Created.Date.POSIX),]
  # calculate indicator of movement (sometimes referred to as ENMO):
  acc$acceleration = pmax(0,sqrt(acc$X^2+acc$Y^2+acc$Z^2)-9.81)
  acc$dt = 0
  acc$dt[2:NR] = diff(as.numeric(acc$Created.Date.POSIX))
  acc$dt[1] = acc$dt[2] # dt = deltatime
  acc$sf = 1/acc$dt
  acc = acc[which(acc$sf != Inf & acc$sf != 0 & acc$sf < 200)]
  timer0 = Sys.time() # keep track of how long it taks to process the file
  acc$num_time = as.numeric(acc$Created.Date.POSIX) # work with numeric time
  acc$num_time_5sec = round(acc$num_time / 5) * 5 # round time to nearest 5 seconds
  # aggregate per 5 seconds:
  acc.per5sec = aggregate(x = acc[c("acceleration","sf")],
                          FUN = mean, by = list(Group.time = acc$num_time_5sec),
                          na.rm=TRUE, na.action=NULL)
  outputMatrixAccstore = rbind(outputMatrixAccstore,acc.per5sec)
}
outputMatrixAcc = outputMatrixAccstore
acc = data.frame(Created.Date.POSIX = as.POSIXlt(outputMatrixAcc[,1],origin = "1970-1-1",tz=desiredtz),
                 acceleration=outputMatrixAcc[,2],sf=outputMatrixAcc[,3],
                 Source=rep(acc$Source[1],nrow(outputMatrixAcc)))
x11()
par(mfrow=c(2,1))
plot(acc$Created.Date.POSIX,acc$acceleration,type="l",main="acceleration")
plot(acc$Created.Date.POSIX,acc$sf,type="l",main="acceleration")

sf2 = acc$sf
sf2 = sf2[-which(sf2 <= 1 | sf2 == Inf | sf2 > 110)]
sf2 = round(sf2 * 5) / 5
x11();hist(sf2,breaks=100,xlab = "Sample frequency")
# acc$acceleration[which(acc$acceleration < 0)] = 0

# Comments on usefulness:
# - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
# - Data is not collected in the absense of movement, whcih complicates autocalibraton.
# - We cannot use the default time extraction because that would only reflect when data
#   blocks are created. Instread used "Normalized Timestamp"
# - It seems that timestamps are not ordered correctly, so first order timestamps.

#-------------------------------------
# pdk-sensor-light
# Light probably not useful, because light can change without the person change activity state
lightfolder = "pdk-sensor-light/"
fn_light = dir(lightfolder)
lightstore = c()
for (j in 1:length(fn_light)) {
  print(j)
  light = data.table::fread(file=paste0(lightfolder,fn_light[j]),sep="\t")
  light = as.data.frame(light)
  light = replaceVarWithSpace(light)
  # we cannot use the default time extraction because that would only reflect
  # when data blocks are created. Instread used "Normalized Timestamp"
  light = addPOSIX(light)
  light$Created.Date.POSIX = as.POSIXlt((light$Normalized.Timestamp)/(1e+9),
                                        tz=desiredtz,origin="1970-1-1")
  light = light[order(light$Created.Date.POSIX),]
  light = light[,c("Source","Created.Date.POSIX","Light.Level")]
  lightstore = rbind(lightstore,light)
}
light = lightstore
NR = nrow(light)
light$Light.binary = rep(0,NR)
light$Light.binary[which(light$Light.Level > 10)] = 1
x11() # Create plot to QC event detection
plot(light$Created.Date.POSIX,light$Light.Level,type="l",ylab="screen on")

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
# Withings device
wit = "Withings-20190215T093727Z-001/"
fn_wit = dir(wit,recursive = T,full.names = T)
txtfiles = grep(".tx",x = fn_wit)
fn_wit = fn_wit[txtfiles] # assumption now that there are 4 txt files
#-------------------------------------
# Accumulated steps over large periods, probably not relevant:
# actmea = data.table::fread(file=fn_wit[grep("activity-measures",x = fn_wit)],sep="\t")
#-------------------------------------
# manual entry of body info:
devbod = data.table::fread(file=fn_wit[grep("device-body",x = fn_wit)],sep="\t")
devbod = as.data.frame(devbod)
devbod = replaceVarWithSpace(devbod)
devbod = addPOSIX(devbod)
devbod = devbod[,c("Source","Created.Date.POSIX")]
x11() # Create plot to QC event detection
plot(devbod$Created.Date.POSIX,rep(1,nrow(devbod)),type="p",pch=20,col="red",ylab="info entered")
# timestamps when person entered information about weight, height, heart-pulse
# devbod$Created.Date.POSIX
#-------------------------------------
# time series of steps (max 1 min res):
devint = data.table::fread(file=fn_wit[grep("device-intra",x = fn_wit)],sep="\t")
devint = as.data.frame(devint)
devint = replaceVarWithSpace(devint)
devint = addPOSIX(devint)
devint = devint[,c("Source","Created.Date.POSIX","steps","swim_strokes","pool_laps","elevation_climbed","distance","calories")]
devint =devint[is.na(devint$steps)==FALSE,]
devint$Date = as.Date(devint$Created.Date.POSIX)
devint.perday = aggregate(x = devint[c("steps")],
                          FUN = sum, by = list(Group.date = devint$Date), na.rm=TRUE, na.action=NULL)
x11() # Create plot to QC event detection
plot(devint$Created.Date.POSIX,devint$steps,type="l",ylab="steps")
x11() # Create plot to QC event detection
plot(devint.perday$Group.date,devint.perday$steps,type="l",ylab="steps per day")

# timestamps when person was active
# devint$Created.Date.POSIX

##-------------------------------------
# awake, deep sle, light sleep:
# TO DO: Investigate how this can be used in activity profiling
# devsle = data.table::fread(file=fn_wit[grep("device-sleep",x = fn_wit)],sep="\t")