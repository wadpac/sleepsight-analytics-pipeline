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
# -------------------------------------
# pdk-screen-state
# Note: probably not relevant, because screen activity does not necessarily
# say much about whether the person interacted with the phone, e.g. incoming call
# or messages...?
screstafolder = "pdk-screen-state/"
fn_scresta = dir(screstafolder)
scresta = read.csv(file=paste0(screstafolder,fn_scresta),sep="\t")
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
acc = read.csv(file=paste0(accfolder,fn_acc),sep="\t")
NR = nrow(acc)
# we cannot use the default time extraction because that would only reflect
# when data blocks are created. Instread used "Normalized Timestamp"
# acc = addPOSIX(acc) 
acc$Created.Date.POSIX = as.POSIXlt((acc$Normalized.Timestamp)/(1e+9),tz=desiredtz,origin="1970-1-1")
acc = acc[order(acc$Created.Date.POSIX),]
# calculate indicator of movement
acc$enmo = pmax(0,sqrt(acc$X^2+acc$Y^2+acc$Z^2)-9.81)
acc$dt = 0
acc$dt[2:NR] = diff(as.numeric(acc$Created.Date.POSIX))
acc$dt[1] = acc$dt[2]
acc$sf = 1/acc$dt
# To handle variation in sampling rate try:
# - identify continuous blocks of data separated by at least a second in time
# - aggregate per second
trans = which(acc$dt > 1)
t1 = trans-1
t0 = c(1,trans[1:(length(trans)-1)]+1)
#remove all intervals that are shorter or equal to 10 samples
tooshort = which(t0 >= (t1-10))
if (length(tooshort) > 0) {
  t0 = t0[-tooshort]
  t1 = t1[-tooshort]
}
#estimate number of new values
Nest = sum(acc$Created.Date.POSIX[t1]-acc$Created.Date.POSIX[t0])
# initialize output matrix
accnew = matrix(0,Nest,3)
cnt = 1
timer0 = Sys.time()
acc$num_time = as.numeric(acc$Created.Date.POSIX) # work with numeric time for speed
for (i in 1:length(t0)) {
  # fit splines
  x = acc$num_time[t0[i]:t1[i]]
  y = acc$enmo[t0[i]:t1[i]]
  y2 = acc$sf[t0[i]:t1[i]]
  funcenmo = splinefun(x = x, y = y)
  funcsf = splinefun(x = x, y = y2)
  # interpolate
  time = seq(acc$num_time[t0[i]],acc$num_time[t1[i]],by=5)
  enmo2 = funcenmo(time)
  sf2 = funcsf(time)
  # store
  accnew[cnt:(cnt+length(enmo2)-1),1:3] = cbind(enmo2,sf2,time)
  cnt = cnt + length(enmo2)
}
if (nrow(accnew) > (cnt)) {
  accnew = accnew[-c((cnt):nrow(accnew)),]
}
# # ignore very low sample rates?
# lowsf = which(accnew[,2]<0.1)
# if (length(lowsf) > 1) accnew = accnew[-lowsf,]
timer1 = Sys.time()
print(timer1 - timer0)

x11()
par(mfrow=c(2,1))
plot(accnew[,1],type="l",main="accnew")
plot(accnew[,2],type="l",main="sample frequency (Hz)")
acc = data.frame(Created.Date.POSIX = as.POSIXlt(accnew[,3],origin = "1970-1-1",tz=desiredtz),
                 enmo=accnew[,1],sf=accnew[,2])
acc$enmo[which(acc$enmo < 0)] = 0

x11()
par(mfrow=c(2,1))
plot(acc$Created.Date.POSIX,acc$enmo,type="l",main="acceleration")
plot(acc$Created.Date.POSIX,acc$sf,type="l",main="underlying sample rate")

# TO DO: tidy up code and improve variable names
# TO DO: add source (participant identifier)
# TO DO: high-pass filtering or auto-calibration seems unrealistic.
# TO DO: check that no samples are unnecessarily deleted
# TO DO: It seems that timestamps are not ordered correctly. Investigate.
# TO DO: Are timestamps reliable at all?

#-------------------------------------
# pdk-sensor-light

#-------------------------------------
# pdk-system-status

#-------------------------------------
# pdk-time-of-day