ppe <-
function(true, pred, abs = TRUE) {
  if(abs==TRUE) {
    ppe<-abs(((true-pred)/pred)*100)
  }
  if(abs==FALSE) {
    ppe<-((true-pred)/pred)*100
  }
  mean.ppe<-mean(ppe,na.rm=TRUE)
  ci.ppe<-ci(ppe,alpha=0.05)
  range.ppe<-range(ppe,na.rm=TRUE)
  sd.ppe<-sd(ppe,na.rm=TRUE)
  result <- list(ppe.list=ppe,mean=mean.ppe,conf.int=ci.ppe[5:6],range=range.ppe,st.dev=sd.ppe)
  return(result)
}
