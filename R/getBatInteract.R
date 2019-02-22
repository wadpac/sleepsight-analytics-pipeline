#' getBatInteract
#'
#' @param filename name of file where pdk-device-battery is stored (txt).
#' @return timestamps (POSIX) on which the battery was either being connected or unplugged.
#' @export
getBatInteract = function(filename) {
  bat = data.table::fread(file=filename,sep="\t")
  bat = as.data.frame(bat)
  bat = replaceVarWithSpace(bat)
  bat = addPOSIX(bat)
  plugged = bat$Plugged != "unknown"
  batInteract = which(abs(diff(plugged)) != 0)
  batInteractTimes = bat$Created.Date.POSIX[batInteract]
  # x11() # Create plot to QC event detection
  # plot(bat$Created.Date.POSIX,bat$Level,type="l",ylab="battery level")
  # lines(bat$Created.Date.POSIX[batInteract],bat$Level[batInteract],type="p",col="red",pch=20)
  return(batInteractTimes)
}