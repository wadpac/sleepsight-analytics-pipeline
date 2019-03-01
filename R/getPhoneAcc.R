#' getPhoneAcc
#'
#' @param filefolder path to folder pdk-sensor-accelerometer with Phone acceleration files (txt).
#' @param desiredtz timezone (character) in Europe/London format
#' @param test_run boolean if TRUE then it will only load the first block of data
#' @return acc a dataframe with timestamps (POSIX), acceleration, sf sample frequency and source.
#' @export
#' @importFrom stats aggregate
getPhoneAcc = function(filefolder, desiredtz, test_run=FALSE) {
  fn_acc = dir(filefolder)
  outputMatrixAccstore = matrix(0,0,3)
  
  blocksize = 2000000
  for (j in 1:length(fn_acc)) {
    cat(paste0("\nLoading file ",fn_acc[j]))
    endlastblock = 0
    acc=c()
    stopprocess = FALSE
    fname = paste0(filefolder,fn_acc[j])
    varnames = colnames(data.table::fread(file=fname,sep="\t",nrows = 3))
    blocki = 1
    while(stopprocess == FALSE) { # while loop iterates over the file
      try(expr={acc = data.table::fread(file=fname,sep="\t",nrows = blocksize, skip = endlastblock)},silent=TRUE)
      
      if (test_run == TRUE) {
        if (blocki == 2) stopprocess = TRUE
      }
      
      if (length(acc) == 0) {
        stopprocess = TRUE
      } else {
        # cat(" ",length(acc)," ")
        # cat(" ",paste0(nrow(acc)," ",endlastblock," "))
        colnames(acc) = varnames
        cat(".")
        endlastblock = blocki * blocksize
        blocki = blocki + 1
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
        acc$num_time_60sec = round(acc$num_time / 60) * 60 # round time to nearest 60 seconds
        # aggregate per 60 seconds:
        acc.per60sec = aggregate(x = acc[,c("acceleration","sf")],
                                FUN = mean, by = list(Group.time = acc$num_time_60sec),
                                na.rm=TRUE, na.action=NULL)
        outputMatrixAccstore = rbind(outputMatrixAccstore,acc.per60sec)
        if (NR < blocksize) stopprocess = TRUE
        SourceID = acc$Source[1]
        rm(acc)
      }
    }
  }
  outputMatrixAcc = outputMatrixAccstore
  acc = data.frame(Created.Date.POSIX = as.POSIXlt(outputMatrixAcc[,1],origin = "1970-1-1",tz=desiredtz),
                   acceleration=outputMatrixAcc[,2],sf=outputMatrixAcc[,3],
                   Source=rep(SourceID,nrow(outputMatrixAcc)))
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