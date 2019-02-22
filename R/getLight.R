#' getLight
#'
#' @param filefolder path to folder pdk-sensor-light with Phone light sensor files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return timestamps (POSIX) on which the light was on.
#' @export
#' @importFrom stats aggregate

getLight = function(filefolder, desiredtz) {
  fn_light = dir(filefolder)
  lightstore = c()
  for (j in 1:length(fn_light)) {
    print(j)
    light = data.table::fread(file=paste0(filefolder,fn_light[j]),sep="\t")
    light = as.data.frame(light)
    light = replaceVarWithSpace(light)
    # we cannot use the default time extraction because that would only reflect
    # when data blocks are created. Instread used "Normalized Timestamp"
    light = addPOSIX(light, desiredtz)
    light$Created.Date.POSIX = as.POSIXlt((light$Normalized.Timestamp)/(1e+9),
                                          tz=desiredtz,origin="1970-1-1")
    light = light[order(light$Created.Date.POSIX),]
    light = light[,c("Source","Created.Date.POSIX","Light.Level")]
    lightstore = rbind(lightstore,light)
  }
  light = lightstore
  NR = nrow(light)
  light$Light.binary = rep(0,NR)
  lightOn = which(light$Light.Level > 10)
  light$Light.binary[lightOn] = 1
  # x11() # Create plot to QC event detection
  # plot(light$Created.Date.POSIX,light$Light.Level,type="l",ylab="screen on")
  lightOnTimes = light$Created.Date.POSIX[lightOn]
  return(lightOnTimes)
}