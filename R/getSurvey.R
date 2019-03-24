#' getSurvey
#'
#' @param filename name of file where pdk-system-status is stored (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @return TimeStamps and Answers survey.
#' @export
getSurvey = function(filename, desiredtz) {
  ssu = data.table::fread(file=filename,sep="\t")
  ssu = as.data.frame(ssu)
  ssu = replaceVarWithSpace(ssu)
  ssu = addPOSIX(ssu, desiredtz)
  # add bedtime and risetime
  ssu$bedtime = as.POSIXlt(paste0(as.Date(ssu$Created.Date.POSIX,tz=desiredtz)," ",
                                  ssu$Bedtime.Hour,":",ssu$Bedtime.Minute),tz=desiredtz)
  ssu$risetime = as.POSIXlt(paste0(as.Date(ssu$Created.Date.POSIX,tz=desiredtz)," ",
                                   ssu$Risetime.Hour,":",ssu$Risetime.Minute),tz=desiredtz)
  
  correctRise = which(ssu$risetime > ssu$Created.Date.POSIX)
  if (length(correctRise) > 0) {
    ssu$risetime[correctRise] = ssu$risetime[correctRise] - (24 *3600)
  }
  correctBed = which(ssu$bedtime > ssu$Created.Date.POSIX)
  if (length(correctBed) > 0) {
    ssu$bedtime[correctBed] = ssu$bedtime[correctBed] - (24 *3600)
  }
  ssu$positiveFeelings = ssu$Cheerful + ssu$Relaxed + ssu$In.Control
  ssu$negativeFeelings = ssu$Anxious + ssu$Irritable + ssu$Sad + ssu$Stressed + ssu$Suspicious +
    ssu$Trouble.Concentrating + ssu$Preoccupied.By.Thoughts + ssu$Others.Dislike.Me + ssu$Confused +
    ssu$Others.Influence.My.Thoughts + ssu$Unusual.Sights.AND.Sounds
  
  colnames(ssu)[which(colnames(ssu) == "Created.Date.POSIX")] = "surveytime"
  ssu = ssu[,c("positiveFeelings","negativeFeelings","bedtime","risetime","Sleep.Quality",
               "Sleep.Duration","Sleep.Quality.Value","surveytime")]
  return(ssu)
}