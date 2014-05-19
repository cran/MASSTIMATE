bipeds <-
function(FC,cQE.eq=c("raw","phylocor"),cQE.cor=2,data=NULL) {    
  #cQE estimate
  if(cQE.eq=="raw") {
    log.masstimate<-2.749*log10(FC*sqrt(cQE.cor))-1.104
    cQE<-round(10^log.masstimate,1)
    ppe.err<-cQE*0.2563
    upper.cQE<-round(cQE+ppe.err,1)
    lower.cQE<-round(cQE-ppe.err,1)
  }
  if(cQE.eq=="phylocor") {
    log.masstimate<-2.754*log10(FC*sqrt(cQE.cor))-1.097
    cQE<-round(10^log.masstimate,1)
    ppe.err<-cQE*0.2503
    upper.cQE<-round(cQE+ppe.err,1)
    lower.cQE<-round(cQE-ppe.err,1)
  }
  #Anderseon et al 1985 estimate
  AHR1985<-round(0.16*(FC)^2.73,2)
  #Christiansen and Farina 2004 estimate
  log.estimate<-2.738*log10(FC)-3.607
  CF2004<-round((10^log.estimate)*1000,2)
  #Campbell and Marcus 1992 estimate
  log.estimate<-2.411*log10(FC)-0.065
  CM1992<-round(10^log.estimate,2)
  return(cbind(data,cQE,upper.cQE,lower.cQE,AHR1985,CF2004,CM1992))
}
