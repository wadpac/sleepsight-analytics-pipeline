#' addPOSIX
#'
#' @param x data.frame with column "Created.Date" that has timestamps in sleepsight format
#' @param desiredtz timezone (character) in Europe/London format
#' @return data.frame x with timestamps added in POSIX format
#' @export
addPOSIX = function(x, desiredtz) {
  # add timestamps in POSIX format to make them R friendly
  removeLastSemicol = function(x) {
    tmp = unlist(strsplit(x,":"))
    y = paste0(tmp[1],":",tmp[2],":",tmp[3],tmp[4])
    return(y)
  }
  x$Created.Date = as.character(x$Created.Date)
  ts_ISO8601 = sapply(x$Created.Date,removeLastSemicol)
  x$Created.Date.POSIX = iso8601chartime2POSIX(as.character(ts_ISO8601),tz = desiredtz)
  return(x)
}