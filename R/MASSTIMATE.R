MASSTIMATE <-
function(HC=NULL,FC,equation=c("quad.raw","quad.phylocor","mult.raw","mult.phylocor","bip.raw","bip.phylocor"),data=NULL) {
  if(equation=="quad.raw") {
    log.masstimate<-2.749*log10(HC+FC)-1.104
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2563
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="quad.phylocor") {
    log.masstimate<-2.754*log10(HC+FC)-1.097
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2503
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="mult.raw") {
    log.masstimate<-1.78*log10(HC)+0.939*log10(FC)-0.215
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.24932
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="mult.phylocor") {
    log.masstimate<-1.54*log10(HC)+1.195*log10(FC)-0.234
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.24624
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }    
  if(equation=="bip.raw") {
    log.masstimate<-2.749*log10(FC*sqrt(2))-1.104
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2563
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  if(equation=="bip.phylocor") {
    log.masstimate<-2.754*log10(FC*sqrt(2))-1.097
    masstimate<-round(10^log.masstimate,1)
    ppe.err<-masstimate*0.2503
    upper<-round(masstimate+ppe.err,1)
    lower<-round(masstimate-ppe.err,1)
  }
  return(cbind(data,log.masstimate,masstimate,upper,lower))
}
