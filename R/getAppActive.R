#' getAppActive
#'
#' @param filename name of file where pdk-foreground-application is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return timestamps (POSIX) on which the apps were on.
#' @export
getAppActive = function(filename, desiredtz) {
  app = data.table::fread(file=filename,sep="\t")
  app = as.data.frame(app)
  app = replaceVarWithSpace(app)
  app = addPOSIX(app, desiredtz)
  NR = nrow(app)
  app$Screen.Active.binary = rep(0,NR)
  app$Screen.Active.binary[which(app$Screen.Active == "True" | app$Screen.Active == "TRUE")] = 1
  screenInteract = which(app$Screen.Active.binary != 0)
  # x11() # Create plot to QC event detection
  # plot(app$Created.Date.POSIX,app$Screen.Active.binary,type="l",ylab="screen active")
  # lines(app$Created.Date.POSIX[screenInteract],app$Screen.Active.binary[screenInteract],type="p",col="red",pch=20)
  # timestamps when phone screen was active:
  AppActiveTimes = app$Created.Date.POSIX[screenInteract]
  return(AppActiveTimes)
}