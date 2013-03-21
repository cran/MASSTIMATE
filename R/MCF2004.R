MCF2004 <-
function(FC,data=NULL) {
  log.estimate<-2.955*log10(FC)-4.166
  estimate.MCF2004<-round((10^log.estimate)*1000,2)
  return(cbind(data,estimate.MCF2004))
}
