#Ordinal Alpha function
OrdAlpha <- function(x){
  library(polycor)
  polymat<-as.matrix(hetcor(x, type="polychoric", pd=T))
  LowMTX<-polymat[lower.tri(polymat)]
  PolyMU<-mean(LowMTX)
  #run relability analysis
  AlphOrd<-(length(LowMTX)*PolyMU)/(1+(length(LowMTX)-1)*PolyMU)
  AlphOrd  
}
OrdAlpha(AugDat2[,26:29])#Yay it worked!
