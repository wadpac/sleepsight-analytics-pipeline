#' plot_timeseries
#'
#' @param D24HR data.frame with corresponding name produced by agg.sleepsight.
#' @param Dsurvey data.frame with corresponding name produced by agg.sleepsight.
#' @param timeseriesfile png.file to which the plot should be written to
#' @param desiredtz timezone (character) in Europe/London format
#' @param startDate Character,  optional argument to specify the startDate of the recording, e.g. 2019-01-01
#' @param endDate Character, optional argument to specify the startDate of the recording, e.g. 2019-01-01
#' @return no output, just a file is stored
#' @export
plot_timeseries = function(D24HR, Dsurvey, timeseriesfile, desiredtz, startDate = c(), endDate = c()) {
  if (length(startDate) > 0 & length(endDate) > 0) {
    startDateNum = as.numeric(as.Date(startDate)) * 3600*24
    endDateNum = as.numeric(as.Date(endDate)) * 3600*24
    timeNumLong = as.numeric(as.POSIXlt(D24HR$date,"1970-01-01",tz=desiredtz))
    timeNumShort = as.numeric(as.POSIXlt(Dshort$time,"1970-01-01",tz=desiredtz))
    validdatesLong = which(timeNumLong > startDateNum & timeNumLong < endDateNum)
    validdatesShort = which(timeNumShort > startDateNum & timeNumShort < endDateNum)
    if (length(validdatesLong) != 0) Dlong = Dlong[validdatesLong,]
    if (length(validdatesShort) != 0) Dshort = Dshort[validdatesShort,]
  }
  cat("\n* plot timeseries")
  png(filename = timeseriesfile,width = 12,height = 10,units = "in",res = 400)
  par(mfrow=c(5,2),bty="l",pch=20)
  XL = "day"
  YLIM = c(0,24)
  plot(D24HR$sleepdur_night,type="p",main="sleep (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$susindur,type="p",main= "sustained inactivity (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$inactivedur,type="p",main= "inactive (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$activedur,type="p",main = "active (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$inconclusive_dur,type="p",main = "inconclusive (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$missing_dur,type="p",main= "missing data (hours)",xlab=XL, ylab="",ylim=YLIM)
  plot(D24HR$daylength, type="p",main= "length day (hours)",xlab=XL, ylab="",ylim=YLIM)
  if (length(Dsurvey) > 0) {
    plot(Dsurvey$surveytime, Dsurvey$positiveFeelings,type="l",main="Positive Feelings")
    plot(Dsurvey$surveytime, Dsurvey$negativeFeelings,type="l",main="Negative Feelings")
    plot(Dsurvey$surveytime, Dsurvey$Sleep.Quality.Value,type="l",main="Sleep Quality (reported)")
  }
  dev.off()
}