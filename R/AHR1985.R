AHR1985 <-
function(HC=NULL,FC,equation=c("bip","quad"),data=NULL) {
  if(equation=="quad") {
    estimate.AHR1985<-round(0.078*(HC+FC)^2.73,2)
  }
  if(equation=="bip") {
    estimate.AHR1985<-round(0.16*(FC)^2.73,2)
  }
  return(cbind(data,estimate.AHR1985))
}
