#' iso8601chartime2POSIX
#'
#' @param x timestamp (character) in iso801 format
#' @param tz timezone (character) in Europe/London format
#' @return timestamp in POSIX format
#' @examples
#' iso8601chartime2POSIX(x="2017-05-07T13:00:00+0200",tz="Europe/Amsterdam")
iso8601chartime2POSIX = function(x,tz){
  return(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz))
}