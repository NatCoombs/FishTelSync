# Function DetCondense

# # # Description

# Takes cleaned acoustic telemetry data and condenses detection files into a data frame that consists of fish ID, location,
# first detection at the location, number of detections at the location (for this "visit"), and last detection at 
# said location.

# May incorporate both longest gap between detections and statistic re: does it make sense

# # # Args

# DetFrame: Detection frame to use to pull detection data
# FishID: Which fish to pull for condensing
# LocCol: Column for location
# tCol: Column for time
# IDCol: Column for ID

DetCondense<-function(DetFrame, FishID, LocCol, tCol, IDCol){
  if(!any(LocCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Location column selected not found in detection data frame")
  }
  if(!any(tCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Time column selected not found in detection data frame")
  }
  if(!any(IDCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Fish ID column selected not found in detection data frame")
  }
  if(!any(FishID %in% DetFrame[,IDCol])){
    stop("Error in DetCondense: FishID selected not found in detection data frame")
  }
  # if(any(is.na(as.numeric(DetFrame[,tCol])))){
  #   stop("Error in DetCondense: Times must be numeric")
  # }
  SubFrame<-DetFrame[which(!any(is.na(c(tCol,LocCol)))),which(IDCol %in% FishID)]
  DenseFrame<-data.frame("FishID" = NA, "Loc" = NA, "FirstDet" = NA, "LastDet" = NA, "NumDets" = NA)
  for(i in 1:length(FishID)){
    FullFishFrame<-DetFrame[which(DetFrame[,IDCol] == FishID[i]),c(IDCol,LocCol,tCol)]
    FullFishFrame<-FullFishFrame[which(complete.cases(FullFishFrame)),]
    OrdFishFrame<-FullFishFrame[order(FullFishFrame[,tCol]),]
    if(length(unique(OrdFishFrame[,LocCol])) > 1){
      
    if(dim(OrdFishFrame)[1] > 2){
    FirstDetVec<-vector(mode = "list")
    for(j in 2:length(OrdFishFrame[,tCol])){
      FirstDetVec[j] <- OrdFishFrame[j,LocCol] == OrdFishFrame[j-1,LocCol]
    }
    FirstDetInd<-vector(mode = "list")
    FirstDetInd<-c(1,which(FirstDetVec %in% FALSE))
    
    
    LastDetVec<-vector(mode = "list")
    for(j in 1:(length(OrdFishFrame[,tCol])-1)){
      LastDetVec[j]<-OrdFishFrame[j,LocCol] == OrdFishFrame[j+1,LocCol]
    }
    LastDetInd<-vector(mode = "list")
    LastDetInd<-c(which(LastDetVec %in% FALSE),length(OrdFishFrame[,tCol]))
    
    NumDets<-matrix()
    for(j in 1:length(FirstDetInd)){
      NumDets[j]<-length(FirstDetInd[j]:LastDetInd[j])
    }}
    else{
      FirstDetInd<-vector(mode = "list")
      FirstDetInd[1]<-OrdFishFrame[1,tCol]
      
      LastDetInd<-vector(mode = "list")
      LastDetInd[1]<-OrdFishFrame[1,tCol]
      
      NumDets<-matrix()
      NumDets[1]<-1
      NumDets[2]<-1
      
      FirstDetInd[2]<-OrdFishFrame[2,tCol]
      
      LastDetInd[2]<-OrdFishFrame[2,tCol]
    }
    
    FishFrame<-data.frame()
    FishFrame[1:length(FirstDetInd),"FishID"]<-FishID[i]
    FishFrame[1:length(FirstDetInd),"Loc"]<-OrdFishFrame[FirstDetInd,LocCol]
    FishFrame[1:length(FirstDetInd),"FirstDet"]<-OrdFishFrame[FirstDetInd,tCol]
    FishFrame[1:length(FirstDetInd),"LastDet"]<-OrdFishFrame[LastDetInd,tCol]
    FishFrame[1:length(FirstDetInd),"NumDets"]<-NumDets
    
    DenseFrame<-rbind(FishFrame,DenseFrame)
    }
  }
  return(DenseFrame)
}
