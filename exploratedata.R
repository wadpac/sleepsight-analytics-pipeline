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
#-----------------------------------------
# extract battery information
batfolder = "pdk-device-battery/"
fn_bat = dir(batfolder)
bat = read.csv(file=paste0(batfolder,fn_bat),sep="\t")
plugged = bat$Plugged != "unknown"
batInteract = which(abs(diff(plugged)) != 0)

# convert timestamps to POSIX format to make them R friendly
iso8601chartime2POSIX = function(x,tz){
  return(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz))
}
bat$Created.Date = as.character(bat$Created.Date)
removeLastSemicol = function(x) {
  tmp = unlist(strsplit(x,":"))
  y = paste0(tmp[1],":",tmp[2],":",tmp[3],tmp[4])
  return(y)
}
ts_ISO8601 = sapply(bat$Created.Date,removeLastSemicol)
bat$Created.Date.POSIX = iso8601chartime2POSIX(as.character(ts_ISO8601),tz = desiredtz)

x11() # Create plot to QC event detection
plot(bat$Created.Date.POSIX,bat$Level,type="l",ylab="battery level")
lines(bat$Created.Date.POSIX[batInteract],bat$Level[batInteract],type="p",col="red",pch=20)

# timestamps when phone was either put on charged or plugged out:
# bat$Created.Date.POSIX[batInteract]

#-------------------------------------
# pdk-foreground-application

#-------------------------------------
# pdk-location

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