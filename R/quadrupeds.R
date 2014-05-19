quadrupeds <-
function(HC,FC,QE_MR.eq=c("raw","phylocor"),data=NULL) {
  #QE and MR estimate
  if(QE_MR.eq=="raw") {
    QE.masstimate<-2.749*log10(HC+FC)-1.104
    QE<-round(10^QE.masstimate,1)
    QE.ppe.err<-QE*0.2563
    upper.QE<-round(QE+QE.ppe.err,1)
    lower.QE<-round(QE-QE.ppe.err,1)
    MR.masstimate<-1.78*log10(HC)+0.939*log10(FC)-0.215
    MR<-round(10^MR.masstimate,1)
    MR.ppe.err<-MR*0.24932
    upper.MR<-round(MR+MR.ppe.err,1)
    lower.MR<-round(MR-MR.ppe.err,1)
  }
  if(QE_MR.eq=="phylocor") {
    QE.masstimate<-2.754*log10(HC+FC)-1.097
    QE<-round(10^QE.masstimate,1)
    QE.ppe.err<-QE*0.2503
    upper.QE<-round(QE+QE.ppe.err,1)
    lower.QE<-round(QE-QE.ppe.err,1)
    MR.masstimate<-1.54*log10(HC)+1.195*log10(FC)-0.234
    MR<-round(10^MR.masstimate,1)
    MR.ppe.err<-MR*0.24624
    upper.MR<-round(MR+MR.ppe.err,1)
    lower.MR<-round(MR-MR.ppe.err,1)
  }
  #Anderseon et al 1985 estimate
  AHR1985<-round(0.078*(HC+FC)^2.73,2)
  #Mazzette et al 2004 estimate
  log.estimate<-2.955*log10(FC)-4.166
  MCF2004<-round((10^log.estimate)*1000,2)
  return(cbind(data,QE,upper.QE,lower.QE,MR,upper.MR,lower.MR,AHR1985,MCF2004))
}
