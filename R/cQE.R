cQE <-
function(FC,equation=c("raw","phylocor"),cor=2,data=NULL) {
  if(equation=="raw") {
    log.cQE<-2.749*log10(FC*sqrt(cor))-1.104
    cQE<-round(10^log.cQE,1)
    ppe.err<-cQE*0.2563
    upper.cQE<-round(cQE+ppe.err,1)
    lower.cQE<-round(cQE-ppe.err,1)
  }
  if(equation=="phylocor") {
    log.cQE<-2.754*log10(FC*sqrt(cor))-1.097
    cQE<-round(10^log.cQE,1)
    ppe.err<-cQE*0.2503
    upper.cQE<-round(cQE+ppe.err,1)
    lower.cQE<-round(cQE-ppe.err,1)
  }
  return(cbind(data,log.cQE,cQE,upper.cQE,lower.cQE))
}
