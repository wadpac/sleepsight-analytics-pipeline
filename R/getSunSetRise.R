#' getSunSetRise
#'
#' @param filefolder name of file where pdk-time-of-day is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return data.frame with timestamps (POSIX) for detected events (sunset/sunrise).
#' @export
getSunSetRise = function(filename, desiredtz) {
  tod = data.table::fread(file=filename,sep="\t")
  tod = as.data.frame(tod)
  tod = replaceVarWithSpace(tod)
  tod = addPOSIX(tod, desiredtz)
  
  deltaIsDay = diff(tod$Is.Day)
  
  sunset = tod$Created.Date.POSIX[which(deltaIsDay == -1)]
  sunrise = tod$Created.Date.POSIX[which(deltaIsDay == 1)]
  SunSetRise1 = data.frame(timestamp=sunset,event="sunset")
  SunSetRise2 = data.frame(timestamp=sunrise,event="sunrise")
  SunSetRise = merge(SunSetRise1,SunSetRise2,by=c("timestamp","event"),all=TRUE)
}