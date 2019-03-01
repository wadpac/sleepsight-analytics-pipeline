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
reload = FALSE

if (reload == TRUE) {
  timer0 = Sys.time()
  #-----------------------------------------
  print("extract battery information")
  batfolder = "pdk-device-battery/"
  fn_bat = dir(batfolder)
  filename = paste0(batfolder,fn_bat)
  batInteractTimes = getBatInteract(filename, desiredtz) # timestamps when phone was either put on charged or plugged out.
  timer1 = Sys.time()
  print(timer1-timer0)
  #-------------------------------------
  print("Withings device - sleep")
  filefolder = "Withings-20190215T093727Z-001/" # note that function searches this folder recursevely for files that meet the description
  WithingsSleep = getWithingsSleep(filefolder, desiredtz)
  timer2 = Sys.time()
  print(timer2-timer1)
  #-------------------------------------
  print("pdk-location")
  locfolder = "pdk-location/"
  fn_loc = dir(locfolder)
  filename = paste0(locfolder,fn_loc)
  MovementPSGTimes = getMovementPSG(filename, desiredtz)
  timer3 = Sys.time()
  print(timer3-timer2)
  # -------------------------------------
  print("System status")
  # Runtime information is relevant for checking data missingness
  statusfolder = "pdk-system-status/"
  fn_status = dir(statusfolder)
  filename = paste0(statusfolder,fn_status)
  AppHalted = getSystemHalted(filename, desiredtz)
  timer4 = Sys.time()
  print(timer4-timer3)
  #-------------------------------------
  print("pdk-sensor-accelerometer")
  accfolder = "pdk-sensor-accelerometer/"
  PhoneAcc = getPhoneAcc(accfolder, desiredtz) # Note that this function provides all acceleration information
  # Comments on usefulness:
  # - Data has varying sample rates, which complicates high-pass filtering if we wanted to.
  # - Data is not collected in the absense of movement, which complicates autocalibraton.
  # - We cannot use the default time extraction because that would only reflect when data
  #   blocks are created. Instread used "Normalized Timestamp"
  # - It seems that timestamps are not ordered correctly, so first order timestamps.
  timer5 = Sys.time()
  print(timer5-timer4)
  #-------------------------------------
  print("pdk-sensor-light")
  # Light probably not useful, because light can change without the person change activity state
  filefolder = "pdk-sensor-light/"
  lightOnTimes = getLight(filefolder, desiredtz)
  timer6 = Sys.time()
  print(timer6-timer5)
  
  #-------------------------------------
  print("Withings device - activity")
  filefolder = "Withings-20190215T093727Z-001/" # note that function searches this folder recursevely for files that meet the description
  WithingsActivity = getWithingsActivity(filefolder, desiredtz)
  timer7 = Sys.time()
  print(timer7-timer6)
  #-------------------------------------
  print("pdk-foreground-application")
  appfolder = "pdk-foreground-application/"
  fn_app = dir(appfolder)
  filename = paste0(appfolder,fn_app)
  AppActiveTimes = getAppActive(filename, desiredtz)
  timer8 = Sys.time()
  print(timer8-timer7)
  #-------------------------------------
  print("pdk-screen-state")
  # Note: probably not relevant, because screen activity does not necessarily
  # say much about whether the person interacted with the phone, e.g. incoming call
  # or messages...?
  screstafolder = "pdk-screen-state/"
  fn_scresta = dir(screstafolder)
  filename = paste0(screstafolder,fn_scresta)
  ScreenOnTimes = getScreenState(filename, desiredtz)
  timer9 = Sys.time()
  print(timer9-timer8)
  # -------------------------------------
  print("Sunset Sunrise")
  todfolder = "pdk-time-of-day/"
  fn_tod = dir(todfolder)
  filename = paste0(todfolder,fn_tod)
  SunSetRise = getSunSetRise(filename, desiredtz)
  timer10 = Sys.time()
  print(timer10-timer9)
  #-------------------------------------
  print("Total")
  print(timer10-timer0)
  save(WithingsActivity,WithingsSleep,
       lightOnTimes,PhoneAcc,
       ScreenOnTimes, MovementPSGTimes, AppActiveTimes,
       batInteractTimes, AppHalted, SunSetRise,
       file=paste0(path,"/sleepanpl_output.RData"))
} else {
  load(file=paste0(path,"/sleepanpl_output.RData"))
}
#----------------------------------------------------------------
# Round resolution of all channels to 1 minute to simplify merging
aggregatePerMinute = function(x, desiredtz) {
  # aggregate input timestamps per minute
  x_num = round(as.numeric(x)/ 60)*60
  x_num = unique(x_num)
  x_60sec = as.POSIXlt(x_num,origin="1970-01-01",tz=desiredtz)
  return(x_60sec)
}

print("Standardise resolution")
# lightOnTimes - 5 seconds
lightOnTimes = aggregatePerMinute(lightOnTimes, desiredtz) # now 60 seconds
# ScreenOnTimes - 1 second
ScreenOnTimes = aggregatePerMinute(ScreenOnTimes, desiredtz) # now 60 seconds
# MovementPSGTimes - 1 second
MovementPSGTimes = aggregatePerMinute(MovementPSGTimes, desiredtz) # now 60 seconds
# AppActiveTimes - 5 seconds
AppActiveTimes = aggregatePerMinute(AppActiveTimes, desiredtz) # now 60 seconds
# batInteractTimes - 1 second
batInteractTimes = aggregatePerMinute(batInteractTimes, desiredtz) # now 60 seconds
# PhoneAcc - 60 seconds
PhoneAccTimes = PhoneAcc$Created.Date.POSIX[which((PhoneAcc$acceleration/9.81) > 0.03)]
# WithingsActivity - 1 second for body info, 1 minute for movement
WithingsMoveTimes = WithingsActivity$timestamp[which(WithingsActivity$infoentered == TRUE | WithingsActivity$movement == TRUE)] 
WithingsMoveTimes = aggregatePerMinute(WithingsMoveTimes, desiredtz) # now 60 seconds for both
# WithingsSleep - 60 seconds
# no need to downsample
WithingsSleep=WithingsSleep[,-which(colnames(WithingsSleep) == "statecode")]
WSN = colnames(WithingsSleep)
colnames(WithingsSleep)[which(WSN=="statename")] = "withingsSleepState"
# AppHalted - 1 second
AppHaltedTimes = aggregatePerMinute(AppHalted, desiredtz) # now 60 seconds
# SunSetRise - 1 second
SunSetRise$timestamp = as.POSIXlt(SunSetRise$timestamp,origin="1970-1-1",tz=desiredtz)
SunSetRise$timestamp = aggregatePerMinute(SunSetRise$timestamp, desiredtz) # now 60 seconds
CS = colnames(SunSetRise)
colnames(SunSetRise)[which(CS=="event")] = "SunSetRise"


print("Merge into one data.frame")
#------------------------------------------------------------------------
# merge all channels in one data.frame
lightOn = data.frame(time = lightOnTimes,lighton=TRUE)
ScreenOn = data.frame(time = ScreenOnTimes,sreenon = TRUE)
df = merge(lightOn,ScreenOn,by="time", all = TRUE)
PSGmove = data.frame(time = MovementPSGTimes,PSGmove = TRUE)
df = merge(df,PSGmove,by="time", all = TRUE)
appactive = data.frame(time = AppActiveTimes,AppAct = TRUE)
df = merge(df,appactive,by="time", all = TRUE)
batinteract = data.frame(time = batInteractTimes, batinteract = TRUE)
df = merge(df,batinteract,by="time", all = TRUE)
phoneacc = data.frame(time = PhoneAccTimes, phoneacc = TRUE)
df = merge(df,phoneacc,by="time", all = TRUE)
withingsMove = data.frame(time = WithingsMoveTimes, withingsMove  = TRUE)
df = merge(df, withingsMove, by="time", all = TRUE)
AppHaltedTimes = data.frame(time = AppHaltedTimes, AppHalted  = TRUE)
df = merge(df, AppHaltedTimes, by="time", all = TRUE)
df = merge(df, SunSetRise, by.x="time",by.y="timestamp", all = TRUE)
WithingsSleep$withingsSleepState =as.character(WithingsSleep$withingsSleepState )
sleep_deep = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="deep-sleep")],deepsleep=TRUE)
sleep_light = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="light-sleep")],lightsleep=TRUE)
sleep_awake = data.frame(time=WithingsSleep$timestamp[which(WithingsSleep$withingsSleepState=="awake")],awake=TRUE)
df = merge(df, sleep_deep, by = "time", all = TRUE)
df = merge(df, sleep_light, by = "time", all = TRUE)
df = merge(df, sleep_awake, by = "time", all = TRUE)
#--------------------------------------------------------
# add hour in the day to ease plotting
df$hour = as.POSIXlt(df$time)$hour
df$min = as.POSIXlt(df$time)$min
df$min_inday = df$hour * 60 + df$min
clock_char = strftime(df$time,format="%H:%M:%S",tz=desiredtz)
df$clock = as.POSIXct(df$time,format="%H:%M:%S",tz=desiredtz)
#--------------------------------------------------------
# prepare plotting variables
clocktimesX = c("00:00","06:00","12:00","18:00","24:00")
Xposi = c(0,6,12,18,24) * 60 #as.POSIXct(clocktimesX, format="%H:%M",tz = desiredtz)
Xlabe = clocktimesX

# x11()
# rannum = rnorm(n = nrow(df),mean = 0,sd=5)
# par(mfrow=c(3,2))
# CX = 0.05
# plot(df$min_inday,df$sreenon+rannum, axes = FALSE, type="p",pch=20,cex=CX,xlab="",ylab="")
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone Screen is on",xlab="time")
# plot(df$min_inday,df$lighton+rannum, axes= FALSE, type="p",pch=20,cex=CX,xlab="",ylab="")
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="Light level above 10",xlab="time")
# plot(df$min_inday,df$PSGmove+rannum, axes = FALSE, type="p",pch=20,cex=CX,xlab="",ylab="") 
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="PSG or Speed indicates movement of phone",xlab="time")
# plot(df$min_inday,df$AppAct+rannum, axes = FALSE, type="p",pch=20,cex=CX,xlab="",ylab="")
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="Foreground app is on",xlab="time")
# plot(df$min_inday,df$batinteract+rannum, axes = FALSE,type="p",pch=20,cex=CX,xlab="",ylab="")
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="Put battery On/Off charge",xlab="time")
# plot(df$min_inday,df$phoneacc+rannum, axes = FALSE,type="p",pch=20,cex=CX,xlab="",ylab="")
# axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone accelerates more than 0.03 times gravity",xlab="time")

png(filename = paste0(path,"/histograms_test.png"),width = 12,height = 10,units = "in",res = 400)
par(mfrow=c(3,4))
CX = 0.05
hist(df$min_inday[which(df$sreenon==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone screen is on",xlab="time")

hist(df$min_inday[which(df$lighton==TRUE)], axes= FALSE, xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone light level above 10",xlab="time")

hist(df$min_inday[which(df$PSGmove == TRUE)], axes = FALSE, xlab="",ylab="",main = "") 
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone PSG/Speed indicate movement",xlab="time")

hist(df$min_inday[which(df$AppAct==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone foreground app is on",xlab="time")

hist(df$min_inday[which(df$batinteract==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone battery put on/off charge",xlab="time")

hist(df$min_inday[which(df$phoneacc==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Phone accelerates > 0.03 times gravity",xlab="time")

hist(df$min_inday[which(df$withingsMove==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings moves / Body info entered",xlab="time")

hist(df$min_inday[which(is.na(df$SunSetRise) == FALSE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="sunset or sunrise",xlab="time")

hist(df$min_inday[which(df$AppHalted==TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="AppHalted",xlab="time")

hist(df$min_inday[which(df$lightsleep == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings LightSleep",xlab="time")

hist(df$min_inday[which(df$deepsleep == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings deep-sleep",xlab="time")

hist(df$min_inday[which(df$awake == TRUE)], axes = FALSE,xlab="",ylab="",main = "")
axis(side = 1,at = Xposi,labels = Xlabe); title(main="Withings awake",xlab="time")
dev.off()

