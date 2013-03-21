CM1992 <-
function(FC,data=NULL) {
  log.estimate<-2.411*log10(FC)-0.065
  estimate.CM1992<-round(10^log.estimate,2)
  return(cbind(data,estimate.CM1992))
}
