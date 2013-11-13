MR <-
function(HC,FC,equation=c("raw","phylocor"),data=NULL) {
  if(equation=="raw") {
    log.masstimate<-1.78*log10(HC)+0.939*log10(FC)-0.215
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.24932
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="phylocor") {
    log.masstimate<-1.54*log10(HC)+1.195*log10(FC)-0.234
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.24624
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }    
  return(cbind(data,log.masstimate,masstimate,upper,lower))
}
