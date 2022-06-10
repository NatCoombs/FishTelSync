DistList<-function(LCSList,DistMat){
  OutMat<-matrix(nrow = length(LCSList), ncol = 6)
  colnames(OutMat) <- c("DSynch","PSynch","SSynch","DDist","PDist","SDist")
  for(i in 1:length(LCSList)){
    LCSSub<-LCSList[[i]]
    OutMat[i,1] <- LCSSub[[7]]
    OutMat[i,2] <- LCSSub[[14]]
    OutMat[i,3] <- LCSSub[[21]]
    OutMat[i,4:6] <- DistWalk(LCSSub,DistMat)
  }
  return(OutMat)
}
