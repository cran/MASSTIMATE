cQE <-
function(FC,equation=c("raw","phylocor"),cor=2,data=NULL) {    
  if(equation=="raw") {
    log.masstimate<-2.749*log10(FC*sqrt(cor))-1.104
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2563
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="phylocor") {
    log.masstimate<-2.754*log10(FC*sqrt(cor))-1.097
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2503
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  return(cbind(data,log.masstimate,masstimate,upper,lower))
}
