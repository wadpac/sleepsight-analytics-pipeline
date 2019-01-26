rm(list=ls())
graphics.off()

# specify location of data
path = "/media/vincent/sleepsight/pilotdata" 
setwd(path)

# identify available zip-files and unzip them within a new (?) subfolder
# TO DO: make sure extraction happens inside single folder per participant
# for now hardcoded filename
zipfile = "pdk-export_2019-01-25_313.zip"
outdir = "."
# unzip file
# unzip(zipfile,exdir = outdir)

desiredtz = "Europe/London"

# Generic function
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
#-----------------------------------------
# extract battery information
batfolder = "pdk-device-battery/"
fn_bat = dir(batfolder)
bat = read.csv(file=paste0(batfolder,fn_bat),sep="\t")
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
app = read.csv(file=paste0(appfolder,fn_app),sep="\t")
app = addPOSIX(app)
NR = nrow(app)
screenactive = app$Screen.Active != "False"
app$Screen.Active.binary = rep(0,NR)
app$Screen.Active.binary[which(app$Screen.Active == "True")] = 1
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
loc = read.csv(file=paste0(locfolder,fn_loc),sep="\t")
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
#-------------------------------------
# pdk-screen-state

#-------------------------------------
# pdk-sensor-accelerometer

#-------------------------------------
# pdk-sensor-light

#-------------------------------------
# pdk-system-status

#-------------------------------------
# pdk-time-of-day