# LCSCalc
# 
# Function steps through each pair of fish and calculates the LCS synchrony metrics for them.
# 
# Args: 
# DenseTable = condensed data table of detections
# PairSet = set of pairs to evaluate

LCSCalc<-function(DenseTable, PairSet){
  LCSList<-list()
  cat(nrow(PairSet), " runs to complete. The completed run #:")
  Printdex <- 1
  for(i in 1:nrow(PairSet)){
    #cat("The fish pair: ", PairSet[i,1]," ", PairSet[i,2], "\n")
    
    #cat("The LCS info: \n")
    Key1 <- which(DenseTable[,"FishID"] %in% PairSet[i,1])
    Key2 <- which(DenseTable[,"FishID"] %in% PairSet[i,2])
    
    LCSList[[i]]<-LCSExtract(DenseTable[Key1,"Loc"],
                                     FWS07LocDenseSub[Key2,"Loc"],
                                     as.numeric(FWS07LocDenseSub[Key1,"FirstDet"]),
                                     as.numeric(FWS07LocDenseSub[Key1,"LastDet"]),
                                     as.numeric(FWS07LocDenseSub[Key2,"FirstDet"]),
                                     as.numeric(FWS07LocDenseSub[Key2,"LastDet"]),
                                     c("All"))
    cat(Printdex, " ")
    Printdex <- Printdex+1
  }
}
