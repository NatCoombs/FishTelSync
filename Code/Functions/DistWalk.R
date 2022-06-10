DistWalk<-function(LCSOut,DistMat){
  DirLocs<-LCSOut[[2]]
  DirDist<-0
  PhaLocs<-LCSOut[[9]]
  PhaDist<-0
  SteLocs<-LCSOut[[16]]
  SteDist<-0
  CN <- colnames(DistMat)
  RN <- rownames(DistMat)
  if(length(DirLocs) > 1){
  for(i in 1:(length(DirLocs)-1)){
    FKey<-which(CN == DirLocs[i])
    TKey<-which(RN == DirLocs[i+1])
    DirDist<-DirDist + DistMat[FKey,TKey]
    
    FKey<-which(CN == PhaLocs[i])
    TKey<-which(RN == PhaLocs[i+1])
    PhaDist<-PhaDist + DistMat[FKey,TKey]
    
    FKey<-which(CN == SteLocs[i])
    TKey<-which(RN == SteLocs[i+1])
    SteDist<-SteDist + DistMat[FKey,TKey]
  }
  } 
  Out<-c(DirDist, PhaDist, SteDist)
  #print(Out)
  names(Out) <- c("Direct", "Phase", "Stepwise")
  
  return(Out)
}
