#' getPhoneAcc
#'
#' @param filefolder path to folder pdk-sensor-accelerometer with Phone acceleration files (txt).
#' @return acc a dataframe with timestamps (POSIX), acceleration, sf sample frequency and source.
#' @export
getPhoneAcc = function(filefolder, desiredtz) {
  fn_acc = dir(accfolder)
  outputMatrixAccstore = matrix(0,0,3)
  for (j in 1:length(fn_acc)) {
    cat(paste0("\nLoading file ",fn_acc[j]))
    acc = data.table::fread(file=paste0(accfolder,fn_acc[j]),sep="\t")
    acc = as.data.frame(acc)
    acc = replaceVarWithSpace(acc)
    NR = nrow(acc)
    acc$Created.Date.POSIX = as.POSIXlt((acc$Normalized.Timestamp)/(1e+9),tz=desiredtz,origin="1970-1-1")
    acc = acc[order(acc$Created.Date.POSIX),]
    # calculate indicator of movement (sometimes referred to as ENMO):
    acc$acceleration = pmax(0,sqrt(acc$X^2+acc$Y^2+acc$Z^2)-9.81)
    acc$dt = 0
    acc$dt[2:NR] = diff(as.numeric(acc$Created.Date.POSIX))
    acc$dt[1] = acc$dt[2] # dt = deltatime
    acc$sf = 1/acc$dt
    acc = acc[which(acc$sf != Inf & acc$sf != 0 & acc$sf < 200),]
    timer0 = Sys.time() # keep track of how long it taks to process the file
    acc$num_time = as.numeric(acc$Created.Date.POSIX) # work with numeric time
    acc$num_time_5sec = round(acc$num_time / 5) * 5 # round time to nearest 5 seconds
    # aggregate per 5 seconds:
    acc.per5sec = aggregate(x = acc[c("acceleration","sf")],
                            FUN = mean, by = list(Group.time = acc$num_time_5sec),
                            na.rm=TRUE, na.action=NULL)
    outputMatrixAccstore = rbind(outputMatrixAccstore,acc.per5sec)
  }
  outputMatrixAcc = outputMatrixAccstore
  acc = data.frame(Created.Date.POSIX = as.POSIXlt(outputMatrixAcc[,1],origin = "1970-1-1",tz=desiredtz),
                   acceleration=outputMatrixAcc[,2],sf=outputMatrixAcc[,3],
                   Source=rep(acc$Source[1],nrow(outputMatrixAcc)))
  # x11()
  # par(mfrow=c(2,1))
  # plot(acc$Created.Date.POSIX,acc$acceleration,type="l",main="acceleration")
  # plot(acc$Created.Date.POSIX,acc$sf,type="l",main="acceleration")
  
  # sf2 = acc$sf
  # sf2 = sf2[-which(sf2 <= 1 | sf2 == Inf | sf2 > 110)]
  # sf2 = round(sf2 * 5) / 5
  # x11();hist(sf2,breaks=100,xlab = "Sample frequency")
  # acc$acceleration[which(acc$acceleration < 0)] = 0
  return(acc)
}