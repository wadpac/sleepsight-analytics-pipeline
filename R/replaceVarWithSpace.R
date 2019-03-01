#' replaceVarWithSpace
#'
#' @param x data.frame
#' @return same data.frame but with specific column names with a space in them replaced by column names with a dot
#' @export
replaceVarWithSpace = function(x) {
  if("Created Date" %in% colnames(x) == TRUE) x$Created.Date = x$`Created Date`
  if("Created Timestamp" %in% colnames(x) == TRUE) x$Created.Timestamp = x$`Created Timestamp`
  if("Screen Active" %in% colnames(x) == TRUE) x$Screen.Active = x$`Screen Active`
  if("Screen State" %in% colnames(x) == TRUE) x$Screen.State = x$`Screen State`
  if("Normalized Timestamp" %in% colnames(x) == TRUE) x$Normalized.Timestamp = x$`Normalized Timestamp`
  if("Light Level" %in% colnames(x) == TRUE) x$Light.Level = x$`Light Level`
  if("App Runtime" %in% colnames(x) == TRUE) x$App.Runtime = x$`App Runtime`
  if("Is Day" %in% colnames(x) == TRUE) x$Is.Day = x$`Is Day`
  return(x)
}