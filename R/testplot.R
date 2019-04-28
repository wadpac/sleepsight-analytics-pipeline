#' testplot
#'
#' @param histfile file where to store the plot
#' @param csvfile csvfile with the aggregated data
#' @return No output, a png file is stored in the outputfolder
#' @export
testplot = function(histfiler, csvfile) {

  plothist = function(namevar="",CDF,x="",plottitle="", xpos=Xposi, xlabe=Xlabe) {
    if (namevar %in% CDF)  {
      if (namevar == "SunSetRise") {
        hist(x$min_inday[which(is.na(x$SunSetRise) == FALSE)], axes = FALSE,xlab="",ylab="",main = "")
      } else {
        hist(x$min_inday[which(x[,namevar]==TRUE)], axes = FALSE, xlab="",ylab="",main = "")
      }
      axis(side = 1,at = xpos,labels = xlabe); title(main=plottitle,xlab="time")
    }
  }
  Xlabe= c("00:00","06:00","12:00","18:00","24:00") # x label for plots
  Xposi = c(0,6,12,18,24) * 60 # x positions for plots
  df = read.csv(file=csvfile)
  CDF = colnames(df)
  png(filename = histfile,width = 12,height = 10,units = "in",res = 400)
  # adjust plot grid to number of expected plots:
  Nplots = length(which(CDF %in% c("screenon", "lighton", "PSGmove", "AppAct", "batinteract",
                                   "phoneacc", "withingsMove_pdk","withingsMove_dd","SunSetRise", 
                                   "AppHalted", "lightsleep_pdk", "deepsleep_pdk", "awake_pdk", 
                                   "lightsleep_dd", "deepsleep_dd","awake_dd","InBed") == TRUE))
  DimPlots = ceiling(sqrt(Nplots))
  if ((DimPlots-1)*DimPlots > Nplots) {
    par(mfrow=c(DimPlots-1,DimPlots))
  } else {
    par(mfrow=c(DimPlots,DimPlots))
  }
  plothist(namevar="screenon",CDF=CDF,x=df,plottitle="Phone screen is on")
  plothist(namevar="lighton",CDF=CDF,x=df,plottitle="Phone light level above 10")
  plothist(namevar="PSGmove",CDF=CDF,x=df,plottitle="Phone PSG/Speed indicate movement")
  plothist(namevar="AppAct",CDF=CDF,x=df,plottitle="Phone foreground app is on")
  plothist(namevar="batinteract",CDF=CDF,x=df,plottitle="Phone battery put on/off charge")
  plothist(namevar="phoneacc" ,CDF=CDF,x=df,plottitle="Phone accelerates > 0.03 times gravity")
  plothist(namevar="withingsMove_pdk" ,CDF=CDF,x=df,plottitle="Withings moves / Body info entered PDK")
  plothist(namevar="withingsMove_dd" ,CDF=CDF,x=df,plottitle="Withings moves / Body info entered DD")
  plothist(namevar="SunSetRise" ,CDF=CDF,x=df,plottitle="sunset or sunrise")
  plothist(namevar="AppHalted" ,CDF=CDF,x=df,plottitle="AppHalted (restarted)")
  plothist(namevar="lightsleep_pdk" ,CDF=CDF,x=df,plottitle="Withings LightSleep PDK")
  plothist(namevar="deepsleep_pdk" ,CDF=CDF,x=df,plottitle="Withings deep-sleep PDK")
  plothist(namevar="awake_pdk" ,CDF=CDF,x=df,plottitle="Withings awake PDK")
  plothist(namevar="lightsleep_dd" ,CDF=CDF,x=df,plottitle="Withings LightSleep DD")
  plothist(namevar="deepsleep_dd" ,CDF=CDF,x=df,plottitle="Withings deep-sleep DD")
  plothist(namevar="awake_dd" ,CDF=CDF,x=df,plottitle="Withings awake DD")
  plothist(namevar="InBed" ,CDF=CDF,x=df,plottitle="In Bed (survey)")
  dev.off()
  
}
