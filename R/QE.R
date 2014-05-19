QE <-
function(HC,FC,equation=c("raw","phylocor"),data=NULL) {
  if(equation=="raw") {
    log.QE<-2.749*log10(HC+FC)-1.104
    QE<-round(10^log.QE,1)
    ppe.err<-QE*0.2563
    upper.QE<-round(QE+ppe.err,1)
    lower.QE<-round(QE-ppe.err,1)
  }
  if(equation=="phylocor") {
    log.QE<-2.754*log10(HC+FC)-1.097
    QE<-round(10^log.QE,1)
    ppe.err<-QE*0.2503
    upper.QE<-round(QE+ppe.err,1)
    lower.QE<-round(QE-ppe.err,1)
  }
  return(cbind(data,log.QE,QE,upper.QE,lower.QE))
}
