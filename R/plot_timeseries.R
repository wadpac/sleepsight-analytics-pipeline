#' plot_timeseries
#'
#' @param D24HR data.frame with corresponding name produced by agg.sleepsight.
#' @param Dsurvey data.frame with corresponding name produced by agg.sleepsight.
#' @param timeseriesfile png.file to which the plot should be written to
#' @return no output, just a file is stored
#' @export
plot_timeseries = function(D24HR, Dsurvey, timeseriesfile) {
  cat("\n* plot timeseries")
  png(filename = timeseriesfile,width = 12,height = 10,units = "in",res = 400)
  par(mfrow=c(5,2),bty="l",pch=20)
  XL = "day"
  YLIM = c(0,24)
  plot(D24HR$sleepdur,type="p",main="sleep (hours)",xlab=XL, ylab="",ylim=YLIM)
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