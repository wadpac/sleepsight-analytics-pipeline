#' getAppActive
#'
#' @param filename name of file where pdk-location is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return timestamps (POSIX) on which the phone moved based on PSG/Speed information.
#' @export
getMovementPSG = function(filename, desiredtz) {
  loc = data.table::fread(file=filename,sep="\t")
  loc = as.data.frame(loc)
  loc = replaceVarWithSpace(loc)
  loc = addPOSIX(loc, desiredtz)
  # movement based on speed
  movement_speed = which(loc$Speed != 0 & is.na(loc$Speed) == FALSE)
  # movement based on change in Latitude
  loc$Latitude = scale(loc$Latitude,center = TRUE) # scale to ease plotting
  dLat = abs(diff(loc$Latitude))
  movement_lat = which(dLat > 0.01)
  # movement based on change in Longitude
  loc$Longitude = scale(loc$Longitude,center = TRUE) # scale to ease plotting
  dLon = abs(diff(loc$Longitude))
  movement_lon = which(dLon > 0.01)
  # combined indicator of movement
  move = unique(c(movement_speed,movement_lat,movement_lon))
  YLIM = range(c(loc$Longitude,loc$Latitude))
  loc$Speed[movement_speed] = (loc$Speed[movement_speed] / max(loc$Speed[movement_speed])) * YLIM[2]
  # x11() # Create plot to QC event detection
  # plot(loc$Created.Date.POSIX,loc$Latitude,type="l",col="black",pch=20, ylim=YLIM)
  # lines(loc$Created.Date.POSIX,loc$Longitude,type="l",col="blue",pch=20)
  # lines(loc$Created.Date.POSIX[movement_speed],loc$Speed[movement_speed],type="p",col="black",pch=20)
  # lines(loc$Created.Date.POSIX[move],loc$Latitude[move],type="p",col="red",pch=20)
  # timestamps when phone was moving according location or speed
  MovementPSGTimes = loc$Created.Date.POSIX[move]
  return(MovementPSGTimes)
}