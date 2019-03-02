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
  LFN = length(fn_light)
  lastp = 0
  for (j in 1:LFN ) {
    progress = (j/LFN) * 100
    if ((progress - lastp) > 1) {
      cat(paste0(" ",round(progress,digits=1),"%"))
      lastp = progress
    }
    light = data.table::fread(file=paste0(filefolder,fn_light[j]),sep="\t",showProgress = FALSE)
    light = as.data.frame(light)
    light = replaceVarWithSpace(light)
    

    # Forcing data to be numeric or NA to address:
    # Error message:
    # Error in (light$Normalized.Timestamp)/(1e+09) : 
    #   non-numeric argument to binary operator
    # In (caused error in set326/file113)
    light$Normalized.Timestamp = as.numeric(light$Normalized.Timestamp)    
    light = light[which(is.na(light$Normalized.Timestamp) == FALSE),]
    # we cannot use the default time extraction because that would only reflect
    # when data blocks are created. Instread used "Normalized Timestamp"
    light$Normalized.Timestamp.POSIX = as.POSIXlt((light$Normalized.Timestamp)/(1e+9),
                                          tz=desiredtz,origin="1970-1-1")
    light = light[order(light$Normalized.Timestamp.POSIX),] #<= Note: actual needed or caused by DST?
    Source = light$Source[1]
    light$timeindex = round(as.numeric(light$Normalized.Timestamp.POSIX) / 5) * 5 #<= Note: Would go wrong DST in autumn
    light = light[,c("Light.Level","timeindex")]
    # aggregate per 5 seconds, because this is probably not going to be a key variable for which we need second level data
    light = aggregate(light,FUN = mean, by = list(timeindex=light$timeindex),
                      na.rm=TRUE, na.action=NULL)
    light = light[,1:2] #ignore timeindex in the third column
    lightstore = rbind(lightstore,light)
  }
  light = lightstore
  light$Normalized.Timestamp.POSIX = as.POSIXlt(light$timeindex,tz=desiredtz,origin="1970-1-1")
  light = light[,-which(colnames(light) == "timeindex")]
  
  # Note: Across a year light can have multiple values per second. Is this related to double entries with DST? => Investigate
  NR = nrow(light)
  light$Light.binary = rep(0,NR)
  lightOn = which(light$Light.Level > 10)
  light$Light.binary[lightOn] = 1
  # x11() # Create plot to QC event detection
  # plot(light$Created.Date.POSIX,light$Light.Level,type="l",ylab="screen on")
  lightOnTimes = light$Normalized.Timestamp.POSIX[lightOn]
  return(lightOnTimes)
}