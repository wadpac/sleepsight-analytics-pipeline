#' getScreenState
#'
#' @param filename name of file where pdk-screen-state is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return timestamps (POSIX) on which the screen was on.
#' @export
getScreenState = function(filename, desiredtz) {
  scresta = data.table::fread(file=filename,sep="\t")
  scresta = as.data.frame(scresta)
  scresta = replaceVarWithSpace(scresta)
  scresta = addPOSIX(scresta, desiredtz)
  NR = nrow(scresta)
  screenactive = scresta$Screen.State != "off"
  scresta$Screen.State.binary = rep(0,NR)
  scresta$Screen.State.binary[which(scresta$Screen.State == "on")] = 1
  screenState = which(scresta$Screen.State.binary != 0)
  # x11() # Create plot to QC event detection
  # plot(scresta$Created.Date.POSIX,scresta$Screen.State.binary,type="l",ylab="screen on")
  # lines(scresta$Created.Date.POSIX[screenState],scresta$Screen.State.binary[screenState],type="p",col="red",pch=20)
  ScreenOnTimes = scresta$Created.Date.POSIX[screenState]
  return(ScreenOnTimes)
}